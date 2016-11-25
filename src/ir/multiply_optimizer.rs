use ir::*;
use mc::ast::*;
use util::IntKind::UnsignedInt;
use util::Name;
use util::Width::{AnyWidth, Width32};

pub struct MultiplyOptimizer;

fn log2(n: u64) -> Option<u64> {
    // Return Some(log_2(n)) if n is a power of 2, and None otherwise.
    for i in 0..63 {
        if n == 1 << i {
            return Some(i);
        }
    }

    None
}

fn popcount(n: u64) -> u8 {
    // Unoptimized bit count.
    let mut count = 0;

    for i in 0..63 {
        if n & (1 << i) != 0 {
            count += 1;
        }
    }

    count
}

impl MultiplyOptimizer {
    fn convert_to_software_ops(ops: &mut Vec<Op>, verbose: bool,
                               mul_func: Option<VarName>, div_func: Option<VarName>,
                               mod_func: Option<VarName>) {
        if verbose {
            print!("Using software ops\n");
        }
        // TODO: these can collide, right?
        let mut temp_count: usize = 0;
        fn gen_temp(temp_count: &mut usize) -> Var {
            let res = Var {
                name: VarName::OptTempVariable(*temp_count),
                generation: None,
            };
            *temp_count += 1;
            res
        }

        fn variable_for(rve: RValueElem, temp_count: &mut usize, node_id: IrNodeId
        ) -> (Vec<Op>, Var) {
            match rve {
                Variable(v) => {
                    let new_temp = gen_temp(temp_count);

                    (vec!(
                        Op {
                            val: OpNode::UnOp { target: new_temp, op: Identity, operand: Variable(v) },
                            // TODO: we really should generate a fresh ID for this, but
                            // at this point we no longer know which ones were used :(
                            id: node_id,
                        }
                    ), new_temp)
                },
                Constant(c) => {
                    let new_temp = gen_temp(temp_count);

                    (vec!(
                        Op {
                            val: OpNode::UnOp { target: new_temp, op: Identity, operand: Constant(c) },
                            // TODO: we really should generate a fresh ID for this, but
                            // at this point we no longer know which ones were used :(
                            id: node_id,
                        }
                    ), new_temp)
                }
            }
        }

        let mut old_ops: Vec<Op> = vec!();
        ::std::mem::swap(ops, &mut old_ops);

        // Software multiplication/division/mod was specified, so we need to convert
        // multiplies/divides/mods into function calls.
        for op in old_ops.into_iter() {
            let mut handled = false;
            if let OpNode::BinOp { target: var, op: binop, lhs: ref op1, rhs: ref op2, signed } = op.val {
                let func = match binop {
                    TimesOp => mul_func,
                    DivideOp => div_func,
                    ModOp => mod_func,
                    _ => None
                }.map(|x| Variable(Var { name: x, generation: Some(0) }));

                if func.is_some() {
                    let func = func.unwrap();

                    // func now contains the Variable for the function we need to call.

                    let (extra_ops, mut vars): (Vec<Vec<Op>>, Vec<Var>) =
                        vec!(op1.clone(), op2.clone()).into_iter()
                        .map(|x| variable_for(x, &mut temp_count, op.id)).unzip();

                    let signedness_var = gen_temp(&mut temp_count);
                    vars.push(signedness_var);
                    let nv = Op {
                        id: op.id,
                        val: OpNode::Call { target: Some(var), func: func, args: vars }
                    };
                    for oplist in extra_ops.into_iter() {
                        ops.extend(oplist);
                    }
                    ops.push(
                        Op {
                            id: op.id,
                            val: OpNode::UnOp {
                                target: signedness_var, op: Identity,
                                operand: Constant(
                                    NumLit(
                                        if signed { 1 } else { 0 },
                                        UnsignedInt(AnyWidth),
                                    )
                                )
                            },
                        }
                    );
                    ops.push(nv);
                    handled = true;
                }
            }

            if !handled {
                // Keep the old op if we didn't replace it.
                ops.push(op);
            }
        }
    }

    pub fn process(ops: &mut Vec<Op>, verbose: bool,
                   mul_func: Option<VarName>, div_func: Option<VarName>,
                   mod_func: Option<VarName>, const_mul_bit_limit: u8) {
        for op in ops.iter_mut() {
            match op.val {
                OpNode::BinOp { op: ref mut opnode, lhs: ref mut op1, rhs: ref mut op2, signed, .. } => {
                    if *opnode != TimesOp && *opnode != DivideOp && *opnode != ModOp {
                        // We only care about multiplies, divides, and mod.
                        continue;
                    }

                    if *opnode == TimesOp {
                        // In a multiplication, ensure that the constant (if any) comes second.
                        if let Constant(_) = *op1 {
                            ::std::mem::swap(op1, op2);
                        }
                    }

                    if verbose {
                        print!("{} {}\n", *opnode, signed);
                    }
                    if let Constant(NumLit(n, _)) = *op2 {
                        if verbose {
                            print!("Const: {}\n", n);
                        }
                        if let Some(log) = log2(n) {
                            if verbose {
                                print!("{} {} {}\n", *opnode, log, signed);
                            }
                            match *opnode {
                                TimesOp => {
                                    *opnode = LeftShiftOp;
                                    *op2 = Constant(NumLit(log, UnsignedInt(Width32)));
                                    continue;
                                },
                                DivideOp => {
                                    *opnode = RightShiftOp;
                                    *op2 = Constant(NumLit(log, UnsignedInt(Width32)));
                                    continue;
                                },
                                ModOp if !signed => {
                                    *opnode = BitAndOp;
                                    *op2 = Constant(NumLit(n - 1, UnsignedInt(Width32)));
                                    continue;
                                },
                                _ => {}
                            }
                        }
                    }
                },
                _ => {}
            }
        }

        // TODO: multi-bit constant divisions/multiplications.
        if const_mul_bit_limit > 1 { unimplemented!() }

        if verbose {
            print!("{:?} {:?} {:?}\n", mul_func, div_func, mod_func);
        }
        if mul_func.is_some() || div_func.is_some() || mod_func.is_some() {
            MultiplyOptimizer::convert_to_software_ops(ops, verbose,
                                                       mul_func, div_func, mod_func);
        }
    }
}
