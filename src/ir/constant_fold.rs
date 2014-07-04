// Constant folding.

use ir::util::subst;

use std::collections::TreeSet;

use mc::ast::*;

use ir::*;
use values::{eval_binop, eval_unop};

pub struct ConstantFolder;

fn fold(op: &BinOpNode, e1: &RValueElem, e2: &RValueElem) ->
    Option<LitNode>
{
    let lit1 = match *e1 {
        Constant(ref l) => l.clone(),
        _ => return None,
    };
    let lit2 = match *e2 {
        Constant(ref l) => l.clone(),
        _ => return None,
    };

    Some(eval_binop(*op, lit1, lit2))
}

fn fold_unary(op: &UnOpNode, e: &RValueElem) -> Option<LitNode> {
    let lit = match *e {
        Constant(ref l) => l.clone(),
        _ => return None,
    };

    eval_unop(*op, lit)
}

fn constant_fold_once(ops: &mut Vec<Op>, vars_to_avoid: &TreeSet<Var>,
                      verbose: bool) -> bool {
    // Variables to replace with constants.
    let mut changes = vec!();

    for op in ops.iter() {
        match *op {
            BinOp(ref v, ref op, ref v1, ref v2) => {
                match fold(op, v1, v2) {
                    Some(c) => {
                        changes.push((v.clone(),
                                      Constant(c)));
                    },
                    _ => {}
                }
            },
            UnOp(ref v, ref op, ref rv) => {
                match fold_unary(op, rv) {
                    Some(c) => {
                        changes.push((v.clone(),
                                      Constant(c)));
                    },
                    _ => {}
                }
            }
            // TODO: unary ops, and anything else.
            _ => {},
        }
    }

    let mut changed = false;

    for (a, b) in changes.move_iter() {
        if !vars_to_avoid.contains(&a){
            if verbose {
                print!("Applying {}->{}\n", a, b);
            }
            subst(ops, &a, &b);
            if verbose {
                print!("{}\n", ops);
            }
            changed = true;
        }
    }

    changed
}


impl ConstantFolder {

    pub fn fold(ops: &mut Vec<Op>, verbose: bool) {
        // There are certain variables we are prohibited from substituting.
        // Those include any that appear in labels/gotos, as well as any
        // that is dereferenced as part of the left hand side of an assignment.
        let mut vars_to_avoid = TreeSet::<Var>::new();
        for op in ops.iter() {
            match *op {
                Label(_, ref vars) |
                Goto(_, ref vars) |
                CondGoto(_, _, _, ref vars) => {
                    for var in vars.iter() {
                        vars_to_avoid.insert(var.clone());
                    }
                },
                // We can't fold anything we take the address of.
                UnOp(_, AddrOf, ref rv) => {
                    match *rv {
                        Variable(ref v) => { vars_to_avoid.insert(v.clone()); },
                        _ => {},
                    }
                }
                Store(ref v1, ref v2, _) |
                Load(ref v1, ref v2, _) => {
                    vars_to_avoid.insert(v1.clone());
                    vars_to_avoid.insert(v2.clone());
                },
                Func(_, ref vars) => {
                    for var in vars.iter() {
                        vars_to_avoid.insert(var.clone());
                    }
                }
                _ => {},
            }
        }
        if verbose {
            print!("avoid: {}\n", vars_to_avoid);
        }
        while constant_fold_once(ops, &vars_to_avoid, verbose) {}
    }
}
