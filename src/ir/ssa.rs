use time::precise_time_ns;

use ir::*;
use ir::util::subst;
use ir::liveness::LivenessAnalyzer;
use std::collections::{BTreeMap, BTreeSet};

pub struct ToSSA;

// Get the generation of a variable and increment it.
fn next_gen(generations: &mut BTreeMap<VarName, usize>, name: VarName) -> Option<usize> {
    let gen = *generations.get(&name).unwrap_or(&0);
    generations.insert(name, gen+1);
    Some(gen+1)
}

// Get the generation of a variable.
fn gen_of(generations: &mut BTreeMap<VarName, usize>, name: VarName) -> Option<usize> {
    Some(*generations.get(&name).unwrap_or(&0))
}

// Fill in the generation of an RValElem.
fn ssa_rvalelem(generations: &mut BTreeMap<VarName, usize>,
                rv_elem: &mut RValueElem) {
    if let Variable(ref mut var) = *rv_elem {
        var.generation = gen_of(generations, var.name);
    }
}

// Fill in the generations of variables in the given BTreeSet, using the given
// gen_of function.
fn ssa_vars<F>(generations: &mut BTreeMap<VarName, usize>, vars: &mut BTreeSet<Var>,
               gen_of: F)
    where F: Fn(&mut BTreeMap<VarName, usize>, VarName) -> Option<usize>
{
    let mut new_vars = BTreeSet::new();
    for var in vars.iter() {
        let mut new_var = *var;
        new_var.generation = gen_of(generations, new_var.name);
        new_vars.insert(new_var);
    }
    *vars = new_vars;
}

#[allow(non_upper_case_globals)]
static mut opinfo_time: u64 = 0;
#[allow(non_upper_case_globals)]
static mut param_time_1: u64 = 0;
#[allow(non_upper_case_globals)]
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
        if let Some(&mut Op { id: _, val: OpNode::Label { label_idx: ref label, ref mut vars }}) = ops.get_mut(i) {
            let live_vars = &opinfo[i].live;
            label_vars.insert(*label,
                              live_vars.clone());
            vars.extend(live_vars.iter().cloned());
        }
    }
    let end = precise_time_ns();
    unsafe {
        param_time_1 += end-start;
    }

    let start = precise_time_ns();
    for op in ops.iter_mut() {
        match op.val {
            OpNode::Goto { label_idx: i, ref mut vars } |
            OpNode::CondGoto { label_idx: i, ref mut vars, .. } => {
                let live_vars = &label_vars[&i];
                vars.extend(live_vars.iter().cloned());
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
    let mut jump_table = BTreeMap::<usize, Vec<BTreeMap<VarName, usize>>>::new();
    let mut label_table = BTreeMap::<usize, BTreeSet<Var>>::new();
    for op in ops.iter() {
        match op.val {
            OpNode::Goto { label_idx: ref label, ref vars } |
            OpNode::CondGoto { label_idx: ref label, ref vars, .. } => {
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
            OpNode::Label { ref label_idx, ref vars } => {
                label_table.insert(*label_idx, vars.clone());
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
    for (idx, item) in &jump_table {
        if item.is_empty() {
            // This label is not jumped to at all. We can entirely
            // remove it.
            labels_to_remove.insert(*idx);
            changed = true;
        } else if item.len() == 1 {
            // If we're here, we only jump to this label from one place.
            // We can entirely remove the variables, with a substitution.
            let label_vars = &label_table[idx];
            for var in label_vars {
                let new_gen = (**item)[0][&var.name];
                substitutions.insert((*var,
                                      Var { name: var.name,
                                            generation: Some(new_gen) } ));
                changed = true;
            }
            if !label_vars.is_empty() {
                vars_at_labels_to_clear.insert(*idx, label_vars.clone());
                changed = true;
            }
        } else {
            // There are multiple jumps. For each variable, look at the
            // set of generations. If there is only one generation other
            // than the generation in the label, we can do a substitution
            // and eliminate the variable.

            let label_vars = &label_table[idx];
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
                if gens.is_empty() {
                    // No jumps to this label involve a different generation
                    // than the label itself. We can just remove it
                    // entirely.
                    vars_to_clear.insert(*var);
                    changed = true;
                } else if gens.len() == 1 {
                    // Aside from the generation in the label, only one
                    // generation is ever used for this variable in jumps
                    // to this label. We can substitute and eliminate it.
                    let other_gen = gens.iter().next().unwrap();
                    vars_to_clear.insert(*var);
                    substitutions.insert((*var,
                                          Var { name: var.name,
                                                generation: Some(*other_gen)
                                          }));
                    changed = true;
                }
            }

            if !vars_to_clear.is_empty() {
                vars_at_labels_to_clear.insert(*idx, vars_to_clear.clone());
            }

        }
        if verbose {
            println!("{}:{:?}", idx, item);
        }
    }

    if verbose {
        print!("subs: ");
        for &(src, dest) in &substitutions {
            print!("{}->{}, ", src, dest);
        }
        println!();
        for (n, vars) in &vars_at_labels_to_clear {
            print!("labels_to_clear: {}:", n);
            for var in vars.iter() {
                print!("{} ,", var);
            }
            println!();
        }
    }

    // Perform all the substitutions. We do a (really inefficient)
    // topological sort, to ensure that we never substitute
    // B -> C followed by A -> B.
    // TODO: if this becomes a problem, do this more efficiently.
    while !substitutions.is_empty() {
        let mut targets = BTreeSet::<Var>::new();
        let mut new_substitutions = BTreeSet::<(Var, Var)>::new();
        for &(_, b) in &substitutions {
            targets.insert(b);
        }
        for &(a, b) in &substitutions {
            if targets.contains(&a) {
                new_substitutions.insert((a, b));
            } else {
                if verbose {
                    print!("{} -> {}\n", a, b);
                }
                subst(ops, &a, &Variable(b));
            }
        }
        substitutions = new_substitutions;
    }

    // Remove any useless labels
    for op in ops.iter_mut() {
        // We do this in two steps, to avoid annoying the move checker.
        let new_op = match op.val {
            OpNode::Label { label_idx: ref label, .. } if labels_to_remove.contains(label) =>
                Some(OpNode::Nop {}),
            _ => None
        };
        if let Some(n) = new_op {
            op.val = n;
        }
    }

    // And, finally, clear any variables that need to be cleared.
    for op in ops.iter_mut() {
        match op.val {
            OpNode::Goto { label_idx: ref label, ref mut vars } |
            OpNode::CondGoto { label_idx: ref label, ref mut vars, .. } |
            OpNode::Label { label_idx: ref label, ref mut vars } => {
                let vars_to_clear_opt = vars_at_labels_to_clear.get(label);
                if let Some(vars_to_clear) = vars_to_clear_opt {
                    for var in vars_to_clear {
                        let mut new_vars = BTreeSet::new();
                        for var2 in vars.iter() {
                            if var2.name != var.name {
                                new_vars.insert(*var2);
                            }
                        }
                        *vars = new_vars;
                    }
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
            println!("Minimize step:");
            for op in ops.iter() {
                print!("{}", op);
            }
        }
    }
}

#[allow(non_upper_case_globals)]
static mut label_time: u64 = 0;
#[allow(non_upper_case_globals)]
static mut gen_time: u64 = 0;
#[allow(non_upper_case_globals)]
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

        let gens = &mut BTreeMap::<VarName, usize>::new();

        let start = precise_time_ns();
        for op in ops.iter_mut() {
            match op.val {
                OpNode::UnOp { target: ref mut v, operand: ref mut rve, .. } => {
                    ssa_rvalelem(gens, rve);
                    v.generation = next_gen(gens, v.name);
                },
                OpNode::BinOp { target: ref mut v, lhs: ref mut rve1, rhs: ref mut rve2, .. } => {
                    ssa_rvalelem(gens, rve1);
                    ssa_rvalelem(gens, rve2);
                    v.generation = next_gen(gens, v.name);
                },
                OpNode::Call { target: ref mut v_opt, func: ref mut f, ref mut args } => {
                    ssa_rvalelem(gens, f);
                    for arg in args.iter_mut() {
                        arg.generation = gen_of(gens, arg.name);
                    }
                    if let Some(ref mut v) = *v_opt {
                        v.generation = next_gen(gens, v.name);
                    }
                },
                OpNode::Store { addr: ref mut v, value: ref mut other_v, .. } => {
                    other_v.generation = gen_of(gens, other_v.name);
                    v.generation = gen_of(gens, v.name);
                },
                OpNode::Load { target: ref mut v, addr: ref mut other_v, .. } => {
                    other_v.generation = gen_of(gens, other_v.name);
                    v.generation = next_gen(gens, v.name);
                },
                OpNode::Label { ref mut vars, .. } => {
                    ssa_vars(gens, vars, |x, y| next_gen(x, y));
                }
                OpNode::CondGoto { cond: ref mut rv, ref mut vars, .. } => {
                    ssa_rvalelem(gens, rv);
                    ssa_vars(gens, vars, |x, y| gen_of(x, y));
                },
                OpNode::Goto { ref mut vars, .. } => {
                    ssa_vars(gens, vars, |x, y| gen_of(x, y));
                },
                OpNode::Return { retval: Some(ref mut rv) } => {
                    ssa_rvalelem(gens, rv);
                },
                OpNode::Func { args: ref mut vars, .. } => {
                    for var in vars.iter_mut() {
                        *var = Var {
                            name: var.name,
                            generation: next_gen(gens, var.name),
                        }
                    }
                }
                OpNode::Alloca { var: ref mut v, .. } => {
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
            println!("Before minimizing:");
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
