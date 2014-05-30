use ir::*;
use ir::liveness::LivenessAnalyzer;
use util::Name;
use collections::{TreeMap, SmallIntMap, TreeSet};

pub struct ToSSA {
    pub ops: Box<Vec<Op>>,
    generations: TreeMap<Name, uint>,
}


pub fn next_gen(generations: &mut TreeMap<Name, uint>, name: Name) -> Option<uint> {
    let gen = *generations.find(&name).unwrap_or(&0);
    generations.insert(name, gen+1);
    Some(gen+1)
}

pub fn gen_of(generations: &mut TreeMap<Name, uint>, name: Name) -> Option<uint> {
    Some(*generations.find(&name).unwrap_or(&0))
}

pub fn ssa_rvalelem(generations: &mut TreeMap<Name, uint>,
                    rv_elem: &mut RValueElem) {
    match *rv_elem {
        Variable(ref mut var) =>
            var.generation = gen_of(generations, var.name),
        _ => {},
    }
}

pub fn ssa_rvalue(generations: &mut TreeMap<Name, uint>, rv: &mut RValue) {
    match *rv {
        DirectRValue(ref mut rv_elem) => ssa_rvalelem(generations, rv_elem),
        BinOpRValue(_, ref mut lhs, ref mut rhs) => {
            ssa_rvalelem(generations, lhs);
            ssa_rvalelem(generations, rhs);
        }
    }
}

pub fn ssa_vars(generations: &mut TreeMap<Name, uint>, vars: &mut TreeSet<Var>,
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

impl ToSSA {
    pub fn new(ops: Box<Vec<Op>>) -> ToSSA {
        ToSSA {
            ops: ops,
            generations: TreeMap::new(),
        }
    }

    fn parameterize_labels(&mut self) {
        // TODO: make it so we don't have to clone these.
        let mut liveness = LivenessAnalyzer::new(self.ops.clone());
        liveness.seed();
        liveness.propagate();
        let opinfo = liveness.opinfo;

        let mut label_vars = SmallIntMap::new();
        let len = self.ops.len();
        for i in range(0, len) {
            match *self.ops.get_mut(i) {
                Label(ref label, ref mut vars) => {
                    let ref live_vars = opinfo.get(i).live;
                    label_vars.insert(*label,
                                      live_vars.clone());
                    vars.extend(live_vars.iter().map(|x| (*x).clone()));
                },
                _ => {},
            }
        }

        for op in self.ops.mut_iter() {
            match *op {
                Goto(ref i, ref mut vars) |
                CondGoto(_, ref i, ref mut vars) => {
                    let ref live_vars = *label_vars.get(i);
                    vars.extend(live_vars.iter().map(|x| (*x).clone()));
                },
                _ => {}
            }
        }
    }

    pub fn to_ssa(&mut self) {
        self.parameterize_labels();

        let ref mut gens = self.generations;

        for op in self.ops.mut_iter() {
            match *op {
                Assign(ref mut lv, ref mut rv) => {
                    ssa_rvalue(gens, rv);
                    match *lv {
                        VarLValue(ref mut var) | PtrLValue(ref mut var) =>
                            var.generation = next_gen(gens, var.name),
                    }
                },
                Label(_, ref mut vars) => {
                    ssa_vars(gens, vars, |x, y| next_gen(x, y));
                }
                CondGoto(ref mut rv, _, ref mut vars) => {
                    ssa_rvalelem(gens, rv);
                    ssa_vars(gens, vars, |x, y| gen_of(x, y));
                },
                Goto(_, ref mut vars) => {
                    ssa_vars(gens, vars, |x, y| gen_of(x, y));
                }
                _ => {}
            }
        }
    }

}