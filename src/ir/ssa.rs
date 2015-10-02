use time::precise_time_ns;

use ir::*;
use ir::util::subst;
use ir::liveness::LivenessAnalyzer;
use util::Name;
use std::collections::{BTreeMap, BTreeSet};

pub struct ToSSA;

// Get the generation of a variable and increment it.
fn next_gen(generations: &mut BTreeMap<Name, usize>, name: Name) -> Option<usize> {
    let gen = *generations.get(&name).unwrap_or(&0);
    generations.insert(name, gen+1);
    Some(gen+1)
}

// Get the generation of a variable.
fn gen_of(generations: &mut BTreeMap<Name, usize>, name: Name) -> Option<usize> {
    Some(*generations.get(&name).unwrap_or(&0))
}

// Fill in the generation of an RValElem.
fn ssa_rvalelem(generations: &mut BTreeMap<Name, usize>,
                rv_elem: &mut RValueElem) {
    match *rv_elem {
        Variable(ref mut var) =>
            var.generation = gen_of(generations, var.name),
        _ => {},
    }
}

// Fill in the generations of variables in the given BTreeSet, using the given
// gen_of function.
fn ssa_vars<F>(generations: &mut BTreeMap<Name, usize>, vars: &mut BTreeSet<Var>,
               gen_of: F)
    where F: Fn(&mut BTreeMap<Name, usize>, Name) -> Option<usize>
{
    let mut new_vars = BTreeSet::new();
    for var in vars.iter() {
        let mut new_var = var.clone();
        new_var.generation = gen_of(generations, new_var.name);
        new_vars.insert(new_var);
    }
    *vars = new_vars;
}

static mut opinfo_time: u64 = 0;
static mut param_time_1: u64 = 0;
static mut param_time_2: u64 = 0;

pub fn get_param_times() -> (u64, u64, u64) {
    unsafe {
        (opinfo_time, param_time_1, param_time_2)
    }
}

fn parameterize_labels(ops: &mut Vec<Op>) {
    // TODO: make it so we don't have to clone these.

    // TODO: we have to include assigned variables in the labels, too.
    let start = precise_time_ns();
    let opinfo = LivenessAnalyzer::analyze(ops);
    let end = precise_time_ns();
    unsafe {
        opinfo_time += end-start;
    }

    let start = precise_time_ns();
    let mut label_vars = BTreeMap::new();
    let len = ops.len();
    for i in 0 .. len {
        match ops.get_mut(i) {
            Some(&mut Op { id: _, val: OpNode::Label(ref label, ref mut vars)}) => {
                let ref live_vars = opinfo[i].live;
                label_vars.insert(*label,
                                  live_vars.clone());
                vars.extend(live_vars.iter().map(|x| (*x).clone()));
            },
            _ => {},
        }
    }
    let end = precise_time_ns();
    unsafe {
        param_time_1 += end-start;
    }

    let start = precise_time_ns();
    for op in ops.iter_mut() {
        match op.val {
            OpNode::Goto(i, ref mut vars) |
            OpNode::CondGoto(_, _, i, ref mut vars) => {
                let ref live_vars = label_vars[&i];
                vars.extend(live_vars.iter().map(|x| (*x).clone()));
            },
            _ => {}
        }
    }
    let end = precise_time_ns();
    unsafe {
        param_time_2 += end-start;
    }
}

fn minimize_once(ops: &mut Vec<Op>, verbose: bool) -> bool {
    // First, collect all jumps to a given label.
    // This maps the label to a vector of maps from variables to
    // generations.
    // So if we have:
    // - goto 1(Name(1)<1>, Name(2)<1>)
    // - goto 1(Name(1)<3>, Name(2)<1>)
    // then the map will have a single entry, taking
    // 1 to the vector of maps
    // <{Name(1):1, Name(2):1},  {Name(1):3, Name(2):1}>.
    let mut jump_table = BTreeMap::<usize, Vec<BTreeMap<Name, usize>>>::new();
    let mut label_table = BTreeMap::<usize, BTreeSet<Var>>::new();
    for op in ops.iter() {
        match op.val {
            OpNode::Goto(ref label, ref vars) |
            OpNode::CondGoto(_, _, ref label, ref vars) => {
                let mut map = BTreeMap::new();
                for var in vars.iter() {
                    map.insert(var.name, var.generation.unwrap());
                }

                // Note: we do it this way to avoid issues with the borrow
                // checker. If it weren't for the borrow checker we'd just
                // match on jump_table.find_mut.
                if jump_table.contains_key(label) {
                    jump_table.get_mut(label).unwrap().push(map)
                } else {
                    jump_table.insert(*label, vec!(map));
                }
            },
            OpNode::Label(ref label, ref vars) => {
                label_table.insert(*label, vars.clone());
            }
            _ => {}
        }
    }

    let mut substitutions = BTreeSet::<(Var, Var)>::new();
    let mut vars_at_labels_to_clear = BTreeMap::<usize, BTreeSet<Var>>::new();
    let mut labels_to_remove = BTreeSet::<usize>::new();
    let mut changed = false;

    // Next step: based on the list of jumps to each label, figure out
    // which variables can be eliminated/substituted. We record those
    // in the "substitutions" and "vars_at_labels_to_clear" tables.
    for (idx, ref item) in jump_table.iter() {
        if item.len() == 0 {
            // This label is not jumped to at all. We can entirely
            // remove it.
            labels_to_remove.insert(*idx);
            changed = true;
        } else if item.len() == 1 {
            // If we're here, we only jump to this label from one place.
            // We can entirely remove the variables, with a substitution.
            let ref label_vars = label_table[idx];
            for var in label_vars.iter() {
                let new_gen = *(**item)[0].get(&var.name).unwrap();
                substitutions.insert((var.clone(),
                                      Var { name: var.name,
                                            generation: Some(new_gen) } ));
                changed = true;
            }
            if label_vars.len() > 0 {
                vars_at_labels_to_clear.insert(*idx, label_vars.clone());
                changed = true;
            }
        } else {
            // There are multiple jumps. For each variable, look at the
            // set of generations. If there is only one generation other
            // than the generation in the label, we can do a substitution
            // and eliminate the variable.

            let ref label_vars = label_table[idx];
            let mut vars_to_clear = BTreeSet::new();

            for var in label_vars.iter() {
                let mut gens = BTreeSet::<usize>::new();
                let label_gen = var.generation.unwrap();

                // Collect the generations of this variable in all jumps
                // to this label.
                for jump in item.iter() {
                    let gen = *jump.get(&var.name).unwrap();
                    if gen != label_gen {
                        gens.insert(gen);
                    }
                }
                if gens.len() == 0 {
                    // No jumps to this label involve a different generation
                    // than the label itself. We can just remove it
                    // entirely.
                    vars_to_clear.insert(var.clone());
                    changed = true;
                } else if gens.len() == 1 {
                    // Aside from the generation in the label, only one
                    // generation is ever used for this variable in jumps
                    // to this label. We can substitute and eliminate it.
                    let other_gen = gens.iter().next().unwrap();
                    vars_to_clear.insert(var.clone());
                    substitutions.insert((var.clone(),
                                          Var { name: var.name,
                                                generation: Some(*other_gen)
                                          }));
                    changed = true;
                }
            }

            if vars_to_clear.len() > 0 {
                vars_at_labels_to_clear.insert(*idx, vars_to_clear.clone());
            }

        }
        if verbose {
            print!("{}:{:?}\n", idx, item);
        }
    }

    if verbose {
        print!("subs: ");
        for &(src, dest) in substitutions.iter() {
            print!("{}->{}, ", src, dest);
        }
        print!("\n");
        for (n, vars) in vars_at_labels_to_clear.iter() {
            print!("labels_to_clear: {}:", n);
            for var in vars.iter() {
                print!("{} ,", var);
            }
            print!("\n");
        }
    }

    // Perform all the substitutions. We do a (really inefficient)
    // topological sort, to ensure that we never substitute
    // B -> C followed by A -> B.
    // TODO: if this becomes a problem, do this more efficiently.
    while !substitutions.is_empty() {
        let mut targets = BTreeSet::<Var>::new();
        let mut new_substitutions = BTreeSet::<(Var, Var)>::new();
        for &(_, ref b) in substitutions.iter() {
            targets.insert(b.clone());
        }
        for &(ref a, ref b) in substitutions.iter() {
            if targets.contains(a) {
                new_substitutions.insert((a.clone(), b.clone()));
            } else {
                if verbose {
                    print!("{} -> {}\n", a, b);
                }
                subst(ops, a, &Variable(b.clone()));
            }
        }
        substitutions = new_substitutions;
    }

    // Remove any useless labels
    for op in ops.iter_mut() {
        // We do this in two steps, to avoid annoying the move checker.
        let new_op = match op.val {
            OpNode::Label(ref label, _) if labels_to_remove.contains(label) =>
                Some(OpNode::Nop),
            _ => None
        };
        match new_op {
            Some(n) => { op.val = n; }
            _ => {}
        }
    }

    // And, finally, clear any variables that need to be cleared.
    for op in ops.iter_mut() {
        match op.val {
            OpNode::Goto(ref label, ref mut vars) |
            OpNode::CondGoto(_, _, ref label, ref mut vars) |
            OpNode::Label(ref label, ref mut vars) => {
                let vars_to_clear_opt = vars_at_labels_to_clear.get(label);
                match vars_to_clear_opt {
                    Some(ref vars_to_clear) =>
                        for var in vars_to_clear.iter() {
                            let mut new_vars = BTreeSet::new();
                            for var2 in vars.iter() {
                                if var2.name != var.name {
                                    new_vars.insert(var2.clone());
                                }
                            }
                            *vars = new_vars;
                        },
                    None => {},
                }
            },
            _ => {}
        }
    }

    changed
}

fn minimize(ops: &mut Vec<Op>, verbose: bool) {
    while minimize_once(ops, verbose) {
        if verbose {
            print!("Minimize step:\n");
            for op in ops.iter() {
                print!("{}", op);
            }
        }
    }
}

static mut label_time: u64 = 0;
static mut gen_time: u64 = 0;
static mut minimize_time: u64 = 0;

pub fn get_times() -> (u64, u64, u64) {
    unsafe {
        (label_time, gen_time, minimize_time)
    }
}

impl ToSSA {
    pub fn to_ssa(ops: &mut Vec<Op>, verbose: bool) {
        if ops.len() == 1 { return; } // No instructions; probably extern.
                                      // TODO: this is somewhat fragile...
        let start = precise_time_ns();
        parameterize_labels(ops);
        let end = precise_time_ns();
        unsafe {
            label_time += end-start;
        }

        let ref mut gens = BTreeMap::<Name, usize>::new();

        let start = precise_time_ns();
        for op in ops.iter_mut() {
            match op.val {
                OpNode::UnOp(ref mut v, _, ref mut rve) => {
                    ssa_rvalelem(gens, rve);
                    v.generation = next_gen(gens, v.name);
                },
                OpNode::BinOp(ref mut v, _, ref mut rve1, ref mut rve2, _) => {
                    ssa_rvalelem(gens, rve1);
                    ssa_rvalelem(gens, rve2);
                    v.generation = next_gen(gens, v.name);
                },
                OpNode::Call(ref mut v, ref mut f, ref mut args) => {
                    ssa_rvalelem(gens, f);
                    for arg in args.iter_mut() {
                        arg.generation = gen_of(gens, arg.name);
                    }
                    v.generation = next_gen(gens, v.name);
                },
                OpNode::Store(ref mut v, ref mut other_v, _) => {
                    other_v.generation = gen_of(gens, other_v.name);
                    v.generation = gen_of(gens, v.name);
                },
                OpNode::Load(ref mut v, ref mut other_v, _) => {
                    other_v.generation = gen_of(gens, other_v.name);
                    v.generation = next_gen(gens, v.name);
                },
                OpNode::Label(_, ref mut vars) => {
                    ssa_vars(gens, vars, |x, y| next_gen(x, y));
                }
                OpNode::CondGoto(_, ref mut rv, _, ref mut vars) => {
                    ssa_rvalelem(gens, rv);
                    ssa_vars(gens, vars, |x, y| gen_of(x, y));
                },
                OpNode::Goto(_, ref mut vars) => {
                    ssa_vars(gens, vars, |x, y| gen_of(x, y));
                },
                OpNode::Return(ref mut rv) => {
                    ssa_rvalelem(gens, rv);
                },
                OpNode::Func(_, ref mut vars, _) => {
                    for var in vars.iter_mut() {
                        *var = Var {
                            name: var.name.clone(),
                            generation: next_gen(gens, var.name),
                        }
                    }
                }
                OpNode::Alloca(ref mut v, _) => {
                    v.generation = next_gen(gens, v.name);;
                }
                _ => {}
            }
        }
        let end = precise_time_ns();
        unsafe {
            gen_time += end-start;
        }

        if verbose {
            print!("Before minimizing:\n");
            for op in ops.iter() {
                print!("{}", op);
            }
        }

        let start = precise_time_ns();
        minimize(ops, verbose);
        let end = precise_time_ns();
        unsafe {
            minimize_time = end-start;
        }
    }

}
