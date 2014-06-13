use session::Interner;

use ast::*;
use ir::*;
use std::collections::TreeSet;

pub struct ASTToIntermediate<'a> {
    var_count: uint,
    label_count: uint,
    interner: &'a mut Interner,
}

impl<'a> ASTToIntermediate<'a> {
    pub fn new(interner: &'a mut Interner) -> ASTToIntermediate<'a> {
        ASTToIntermediate { var_count: 0,
                            label_count: 0,
                            interner: interner }
    }

    fn gen_temp(&mut self) -> Var {
        let res = Var {
            name: self.interner.intern(format!("TEMP{}", self.var_count)),
            generation: None,
        };
        self.var_count += 1;
        res
    }

    fn gen_label(&mut self) -> uint {
        let res = self.label_count;
        self.label_count += 1;
        res
    }

    pub fn convert_stmt(&mut self, stmt: &Stmt) -> (Vec<Op>, Var) {
        match stmt.val {
            ExprStmt(ref e) => self.convert_expr(e),
            SemiStmt(ref e) => self.convert_expr(e),
            LetStmt(ref pat, ref e_opt) => {
                let v = match pat.val {
                    IdentPat(ref ident, _) => {
                        Var { name: ident.val.name,
                              generation: None }
                    },
                    _ => fail!("Only ident patterns are supported right now.")
                };
                match *e_opt {
                    Some(ref e) => {
                        let (mut ops, other_v) = self.convert_expr(e);
                        ops.push(Assign(VarLValue(v),
                                        DirectRValue(Variable(other_v))));
                        (ops, v)
                    },
                    None => (vec!(), v)
                }
            }
        }
    }

    pub fn convert_block(&mut self, block: &Block) -> (Vec<Op>, Var) {
        let mut ops = vec!();
        for stmt in block.stmts.iter() {
            let (new_ops, _) = self.convert_stmt(stmt);
            ops.push_all_move(new_ops);
        }
        match block.expr {
            Some(ref e) => {
                let (new_ops, new_var) = self.convert_expr(e);
                ops.push_all_move(new_ops);
                (ops, new_var)
            },
            None => {
                (ops, self.gen_temp())
            }
        }
    }

    pub fn convert_item(&mut self, item: &Item) -> (Vec<Op>, Var) {
        match item.val {
            FuncItem(ref id, ref args, _, ref block, _) => {
                let mut ops = vec!(Func(id.val.name,
                                        args.iter()
                                        .map(
                                            |arg| Var {
                                                name: arg.ident.val.name,
                                                generation: None
                                            })
                                        .collect()));

                let (new_ops, v) = match *block {
                    Some(ref block) => self.convert_block(block),
                    None => (vec!(), self.gen_temp()),
                };

                ops.push_all_move(new_ops);
                ops.push(Return(Variable(v)));
                (ops, v)
            },
            _ => fail!("{}", item)//(vec!(), self.gen_temp())
        }
    }

    pub fn convert_expr(&mut self, expr: &Expr) -> (Vec<Op>, Var) {
        match expr.val {
            LitExpr(ref lit) => {
                let res_var = self.gen_temp();
                let insts = vec!(
                    Assign(VarLValue(res_var.clone()),
                           DirectRValue(Constant(lit.val.clone())))
                    );
                (insts, res_var)
            }
            BinOpExpr(ref op, ref e1, ref e2) => {
                let (mut insts1, var1) = self.convert_expr(*e1);
                let (insts2, var2) = self.convert_expr(*e2);
                insts1.push_all_move(insts2);
                let new_res = self.gen_temp();
                insts1.push(
                    Assign(VarLValue(new_res.clone()),
                           BinOpRValue(op.val.clone(),
                                       Variable(var1),
                                       Variable(var2))));
                (insts1, new_res)
            },
            PathExpr(ref path) => {
                //fail!("Need to do paths properly")
                (vec!(), Var { name: path.val.elems.last().unwrap().val.name,
                               generation: None })
            },
            AssignExpr(ref op, ref e1, ref e2) => {
                let (mut res, var2) = self.convert_expr(*e2);

                // The RHS might be wrapped in a GroupExpr.
                // Unwrap it.
                let mut e1val = e1.val.clone();
                loop {
                    match e1val {
                        GroupExpr(e) => e1val = e.val.clone(),
                        _ => { break; }
                    }
                }

                // The tuple elements are as follows:
                // binop_var is the variable to use on the left-hand side
                //     of a binary operation, if we decide to.
                //     So, in a+=b, "binop_var" will be a variable containing
                //     the value of a.
                // binop_insts are the instructions necessary to put the
                //     right value into binop_var.
                // dest is the LValue where everything is ultimately stored.
                let (binop_var, binop_insts, dest) = match e1val {
                    PathExpr(ref path) => {
                        let lhs_var = Var {
                            name: path.val.elems.last().unwrap().val.name,
                            generation: None
                        };
                        (lhs_var.clone(),
                         vec!(),
                         VarLValue(lhs_var.clone()))
                    },
                    UnOpExpr(ref lhs_op, ref e) => {
                        let (insts, var) = self.convert_expr(*e);
                        res.push_all_move(insts);
                        let res_var = self.gen_temp();
                        match lhs_op.val {
                            Deref => {
                                (res_var.clone(),
                                 vec!(Assign(VarLValue(res_var.clone()),
                                             UnOpRValue(Deref,
                                                        Variable(var.clone())
                                                        )
                                             )
                                      ),
                                 PtrLValue(var.clone()))
                            },
                            _ => fail!(),
                        }
                    },
                    _ => fail!("Got {}", e1.val),
                };
                let final_var =
                    match *op {
                        Some(ref inner_op) => {
                            // We actually have a binop!
                            // Start by including the instructions to get
                            // the value we're adding to.
                            // So, in 'a+=b', this pushes the instructions
                            // necessary to give us the value of 'a'.
                            res.push_all_move(binop_insts);
                            let binop_result_var = self.gen_temp();
                            res.push(Assign(
                                VarLValue(binop_result_var),
                                BinOpRValue(inner_op.val.clone(),
                                            Variable(binop_var),
                                            Variable(var2))));
                            binop_result_var
                        },
                        // No binop. We can just assign the result of
                        // the rhs directly.
                        None => var2
                    };
                // This generates a redundant store in some cases, but
                // the optimizer will eliminate them.
                res.push(Assign(dest, DirectRValue(Variable(final_var))));

                (res, final_var)
            }
            BlockExpr(ref b) => self.convert_block(*b),
            IfExpr(ref e, ref b1, ref b2) => {
                let (mut insts, if_var) = self.convert_expr(*e);
                let (b1_insts, b1_var) = self.convert_block(*b1);
                let (b2_insts, b2_var) = self.convert_block(*b2);
                let b1_label = self.gen_label();
                let end_var = self.gen_temp();
                insts.push(CondGoto(Variable(if_var),
                                    b1_label,
                                    TreeSet::new()));
                insts.push_all_move(b2_insts);
                insts.push(Assign(VarLValue(end_var),
                                  DirectRValue(Variable(b2_var))));
                insts.push(Goto(b1_label, TreeSet::new()));
                insts.push(Label(b1_label, TreeSet::new()));
                insts.push_all_move(b1_insts);
                insts.push(Assign(VarLValue(end_var),
                                  DirectRValue(Variable(b1_var))));
                (insts, end_var)
            },
            WhileExpr(ref e, ref b) => {
                let begin_label = self.gen_label();
                let middle_label = self.gen_label();
                let end_label = self.gen_label();
                let mut res = vec!(
                    Goto(begin_label, TreeSet::new()),
                    Label(begin_label, TreeSet::new()));
                let (cond_insts, cond_var) = self.convert_expr(*e);
                res.push_all_move(cond_insts);
                res.push(CondGoto(Variable(cond_var),
                                  middle_label, TreeSet::new()));
                res.push(Goto(end_label, TreeSet::new()));
                res.push(Label(middle_label, TreeSet::new()));
                let (block_insts, _) = self.convert_block(*b);
                res.push_all_move(block_insts);
                res.push(Goto(begin_label, TreeSet::new()));
                res.push(Label(end_label, TreeSet::new()));
                (res, self.gen_temp())
            },
            //ForExpr(ref init, ref cond, ref iter, ref body) => {
            //    let (mut insts, _) = self.convert_expr(*init);
            //    
            //}
            GroupExpr(ref e) => self.convert_expr(*e),
            CallExpr(ref f, ref args) => {
                let mut ops = vec!();
                let mut vars = vec!();
                for arg in args.iter() {
                    let (new_ops, new_var) = self.convert_expr(arg);
                    ops.push_all_move(new_ops);
                    vars.push(new_var);
                }
                let (new_ops, new_var) = self.convert_expr(*f);
                let result_var = self.gen_temp();
                ops.push(Assign(
                    VarLValue(result_var.clone()),
                    CallRValue(Variable(new_var),
                               vars.move_iter().map(
                                   |v| Variable(v)).collect())));
                (ops, result_var)
            }
            _ => (vec!(), self.gen_temp()),
        }
    }
}
