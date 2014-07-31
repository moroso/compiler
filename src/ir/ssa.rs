use ir::*;
use ir::util::subst;
use ir::liveness::LivenessAnalyzer;
use util::Name;
use std::collections::{TreeMap, SmallIntMap, TreeSet};

pub struct ToSSA;

// Get the generation of a variable and increment it.
fn next_gen(generations: &mut TreeMap<Name, uint>, name: Name) -> Option<uint> {
    let gen = *generations.find(&name).unwrap_or(&0);
    generations.insert(name, gen+1);
    Some(gen+1)
}

// Get the generation of a variable.
fn gen_of(generations: &mut TreeMap<Name, uint>, name: Name) -> Option<uint> {
    Some(*generations.find(&name).unwrap_or(&0))
}

// Fill in the generation of an RValElem.
fn ssa_rvalelem(generations: &mut TreeMap<Name, uint>,
                rv_elem: &mut RValueElem) {
    match *rv_elem {
        Variable(ref mut var) =>
            var.generation = gen_of(generations, var.name),
        _ => {},
    }
}

// Fill in the generations of variables in the given TreeSet, using the given
// gen_of function.
fn ssa_vars(generations: &mut TreeMap<Name, uint>, vars: &mut TreeSet<Var>,
                gen_of: |&mut TreeMap<Name, uint>, Name| -> Option<uint>) 
{
    let mut new_vars = TreeSet::new();
    for var in vars.iter() {
        let mut new_var = var.clone();
        new_var.generation = gen_of(generations, new_var.name);
        new_vars.insert(new_var);
    }
    *vars = new_vars;
}

fn parameterize_labels(ops: &mut Vec<Op>) {
    // TODO: make it so we don't have to clone these.

    // TODO: we have to include assigned variables in the labels, too.
    let opinfo = LivenessAnalyzer::analyze(ops);

    let mut label_vars = SmallIntMap::new();
    let len = ops.len();
    for i in range(0, len) {
        match *ops.get_mut(i) {
            Label(ref label, ref mut vars) => {
                let ref live_vars = opinfo[i].live;
                label_vars.insert(*label,
                                  live_vars.clone());
                vars.extend(live_vars.iter().map(|x| (*x).clone()));
            },
            _ => {},
        }
    }

    for op in ops.mut_iter() {
        match *op {
            Goto(ref i, ref mut vars) |
            CondGoto(_, _, ref i, ref mut vars) => {
                let ref live_vars = *label_vars.get(i);
                vars.extend(live_vars.iter().map(|x| (*x).clone()));
            },
            _ => {}
        }
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
    let mut jump_table = SmallIntMap::<Vec<TreeMap<Name, uint>>>::new();
    let mut label_table = SmallIntMap::<TreeSet<Var>>::new();
    for op in ops.iter() {
        match *op {
            Goto(ref label, ref vars) |
            CondGoto(_, _, ref label, ref vars) => {
                let mut map = TreeMap::new();
                for var in vars.iter() {
                    map.insert(var.name, var.generation.unwrap());
                }

                // Note: we do it this way to avoid issues with the borrow
                // checker. If it weren't for the borrow checker we'd just
                // match on jump_table.find_mut.
                if jump_table.contains_key(label) {
                    jump_table.find_mut(label).unwrap().push(map)
                } else {
                    jump_table.insert(*label, vec!(map));
                }
            },
            Label(ref label, ref vars) => {
                label_table.insert(*label, vars.clone());
            }
            _ => {}
        }
    }

    let mut substitutions = TreeSet::<(Var, Var)>::new();
    let mut vars_at_labels_to_clear = SmallIntMap::<TreeSet<Var>>::new();
    let mut labels_to_remove = TreeSet::<uint>::new();
    let mut changed = false;

    // Next step: based on the list of jumps to each label, figure out
    // which variables can be eliminated/substituted. We record those
    // in the "substitutions" and "vars_at_labels_to_clear" tables.
    for (ref idx, ref item) in jump_table.iter() {
        if item.len() == 0 {
            // This label is not jumped to at all. We can entirely
            // remove it.
            labels_to_remove.insert(idx.clone());
            changed = true;
        } else if item.len() == 1 {
            // If we're here, we only jump to this label from one place.
            // We can entirely remove the variables, with a substitution.
            let label_vars = label_table.get(idx);
            for var in label_vars.iter() {
                let new_gen = *(**item)[0].find(&var.name).unwrap();
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

            let label_vars = label_table.get(idx);
            let mut vars_to_clear = TreeSet::new();

            for var in label_vars.iter() {
                let mut gens = TreeSet::<uint>::new();
                let label_gen = var.generation.unwrap();

                // Collect the generations of this variable in all jumps
                // to this label.
                for jump in item.iter() {
                    let gen = *jump.find(&var.name).unwrap();
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
            print!("{}:{}\n", idx, item);
        }
    }

    if verbose {
        print!("subs: {}\n", substitutions);
        for x in vars_at_labels_to_clear.iter() {
            print!("labels_to_clear: {}\n", x);
        }
    }

    // Perform all the substitutions. We do a (really inefficient)
    // topological sort, to ensure that we never substitute
    // B -> C followed by A -> B.
    // TODO: if this becomes a problem, do this more efficiently.
    while !substitutions.is_empty() {
        let mut targets = TreeSet::<Var>::new();
        let mut new_substitutions = TreeSet::<(Var, Var)>::new();
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
    for op in ops.mut_iter() {
        // We do this in two steps, to avoid annoying the move checker.
        let new_op = match *op {
            Label(ref label, _) if labels_to_remove.contains(label) =>
                Some(Nop),
            _ => None
        };
        match new_op {
            Some(n) => {*op = n; }
            _ => {}
        }
    }

    // And, finally, clear any variables that need to be cleared.
    for op in ops.mut_iter() {
        match *op {
            Goto(ref label, ref mut vars) |
            CondGoto(_, _, ref label, ref mut vars) |
            Label(ref label, ref mut vars) => {
                let vars_to_clear_opt = vars_at_labels_to_clear.find(label);
                match vars_to_clear_opt {
                    Some(ref vars_to_clear) =>
                        for var in vars_to_clear.iter() {
                            let mut new_vars = TreeSet::new();
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
    while minimize_once(ops, verbose) {}
}

impl ToSSA {
    pub fn to_ssa(ops: &mut Vec<Op>, verbose: bool) {
        parameterize_labels(ops);

        let ref mut gens = TreeMap::<Name, uint>::new();

        for op in ops.mut_iter() {
            match *op {
                UnOp(ref mut v, _, ref mut rve) => {
                    ssa_rvalelem(gens, rve);
                    v.generation = next_gen(gens, v.name);
                },
                BinOp(ref mut v, _, ref mut rve1, ref mut rve2) => {
                    ssa_rvalelem(gens, rve1);
                    ssa_rvalelem(gens, rve2);
                    v.generation = next_gen(gens, v.name);
                },
                Call(ref mut v, ref mut f, ref mut args) => {
                    ssa_rvalelem(gens, f);
                    for arg in args.mut_iter() {
                        arg.generation = gen_of(gens, arg.name);
                    }
                    v.generation = next_gen(gens, v.name);
                },
                Store(ref mut v, ref mut other_v, _) => {
                    other_v.generation = gen_of(gens, other_v.name);
                    v.generation = gen_of(gens, v.name);
                },
                Load(ref mut v, ref mut other_v, _) => {
                    other_v.generation = gen_of(gens, other_v.name);
                    v.generation = next_gen(gens, v.name);
                },
                Label(_, ref mut vars) => {
                    ssa_vars(gens, vars, |x, y| next_gen(x, y));
                }
                CondGoto(_, ref mut rv, _, ref mut vars) => {
                    ssa_rvalelem(gens, rv);
                    ssa_vars(gens, vars, |x, y| gen_of(x, y));
                },
                Goto(_, ref mut vars) => {
                    ssa_vars(gens, vars, |x, y| gen_of(x, y));
                },
                Return(ref mut rv) => {
                    ssa_rvalelem(gens, rv);
                },
                Func(_, ref mut vars, is_extern) => {
                    if is_extern { return; }
                    for var in vars.mut_iter() {
                        *var = Var {
                            name: var.name.clone(),
                            generation: next_gen(gens, var.name),
                        }
                    }
                }
                Alloca(ref mut v, _) => {
                    v.generation = next_gen(gens, v.name);;
                }
                _ => {}
            }
        }

        minimize(ops, verbose);
    }

}