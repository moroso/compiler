use util::{Width32, UnsignedInt, Name};

use mc::session::Session;

use std::collections::TreeSet;

use mc::ast::*;

use intrinsics::size_of::*;
use ir::*;
use typechecker::*;

pub struct ASTToIntermediate<'a> {
    var_count: uint,
    label_count: uint,
    session: &'a mut Session,
    typemap: &'a mut Typemap,
}

impl<'a> ASTToIntermediate<'a> {
    pub fn new(session: &'a mut Session,
               typemap: &'a mut Typemap) -> ASTToIntermediate<'a> {
        ASTToIntermediate { var_count: 0,
                            label_count: 0,
                            session: session,
                            typemap: typemap,
        }
    }

    fn gen_temp(&mut self) -> Var {
        let res = Var {
            name: self.session.interner.intern(
                format!("TEMP{}", self.var_count)),
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
                let (v, ty) = match pat.val {
                    IdentPat(ref ident, ref ty_opt) => {
                        (Var { name: ident.val.name,
                               generation: None },
                         ty_opt.clone())
                    },
                    _ => fail!("Only ident patterns are supported right now.")
                };

                match *e_opt {
                    Some(ref e) => {
                        let (mut ops, other_v) = self.convert_expr(e);
                        ops.push(UnOp(v, Identity, Variable(other_v)));
                        (ops, v)
                    },
                    None => {
                        match ty {
                            None => fail!("No type available."),
                            Some(ref t) => {
                                match *self.typemap.types.get(&t.id.to_uint()) {
                                    StructTy(ref id, _) => {
                                        let size = size_of_def(self.session,
                                                               self.typemap,
                                                               id);
                                        print!("Allocate {}\n", size);
                                        (vec!(Alloca(v, size)), v)
                                    },
                                    _ => (vec!(), v)
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn convert_block(&mut self, block: &Block) -> (Vec<Op>, Var) {
        let mut ops = vec!();
        for stmt in block.val.stmts.iter() {
            let (new_ops, _) = self.convert_stmt(stmt);
            ops.push_all_move(new_ops);
        }
        match block.val.expr {
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
            StructItem(..) => (vec!(), self.gen_temp()),
            _ => fail!("{}", item)//(vec!(), self.gen_temp())
        }
    }

    fn while_helper(&mut self,
                    e: &Expr,
                    block_insts: Vec<Op>) -> (Vec<Op>, Var) {
        let begin_label = self.gen_label();
        let end_label = self.gen_label();
        let mut res = vec!(
            Goto(begin_label, TreeSet::new()),
            Label(begin_label, TreeSet::new()));
        let (cond_insts, cond_var) = self.convert_expr(e);
        res.push_all_move(cond_insts);
        res.push(CondGoto(true,
                          Variable(cond_var),
                          end_label, TreeSet::new()));
        res.push_all_move(block_insts);
        res.push(Goto(begin_label, TreeSet::new()));
        res.push(Label(end_label, TreeSet::new()));
        (res, self.gen_temp())
    }

    pub fn convert_expr(&mut self, expr: &Expr) -> (Vec<Op>, Var) {
        match expr.val {
            LitExpr(ref lit) => {
                let res_var = self.gen_temp();
                let insts = vec!(
                    UnOp(res_var.clone(), Identity, Constant(lit.val.clone()))
                    );
                (insts, res_var)
            }
            BinOpExpr(ref op, ref e1, ref e2) => {
                let (mut insts1, var1) = self.convert_expr(*e1);
                let (insts2, var2) = self.convert_expr(*e2);
                match op.val {
                    AndAlsoOp |
                    OrElseOp => {
                        let is_and = op.val == AndAlsoOp;
                        let end_label = self.gen_label();
                        insts1.push(CondGoto(is_and, // cond is negated for
                                                     // ands, not for ors.
                                             Variable(var1),
                                             end_label,
                                             TreeSet::new()));
                        insts1.push_all_move(insts2);
                        insts1.push(UnOp(var1,
                                         Identity,
                                         Variable(var2)));
                        insts1.push(Goto(end_label, TreeSet::new()));
                        insts1.push(Label(end_label, TreeSet::new()));
                        (insts1, var1)
                    },
                    _ => {
                        let new_res = self.gen_temp();
                        insts1.push_all_move(insts2);
                        insts1.push(
                            BinOp(new_res.clone(),
                                  op.val.clone(),
                                  Variable(var1),
                                  Variable(var2)));
                        (insts1, new_res)
                    }
                }
            },
            PathExpr(ref path) => {
                //fail!("Need to do paths properly")
                (vec!(), Var { name: path.val.elems.last().unwrap().val.name,
                               generation: None })
            },
            AssignExpr(ref op, ref e1, ref e2) => {
                // TODO: this will break in the case of structs and enums.

                let (mut res, var2) = self.convert_expr(*e2);

                // The LHS might be wrapped in a GroupExpr.
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
                // finalize does the assignment to the variable; lhs_var
                //     is passed into it.
                let (binop_var, binop_insts, lhs_var, finalize) = match e1val {
                    PathExpr(ref path) => {
                        let lhs_var = Var {
                            name: path.val.elems.last().unwrap().val.name,
                            generation: None
                        };
                        (lhs_var.clone(),
                         vec!(),
                         lhs_var,
                         |lv, v| UnOp(lv, Identity, Variable(v)))
                    },
                    UnOpExpr(ref lhs_op, ref e) => {
                        let (insts, var) = self.convert_expr(*e);
                        res.push_all_move(insts);
                        let res_var = self.gen_temp();
                        match lhs_op.val {
                            Deref => {
                                (res_var.clone(),
                                 vec!(Load(res_var.clone(),
                                           var.clone(),
                                           Width32 // TODO!!!! Determine width
                                           )
                                      ),
                                 var.clone(),
                                 |lv, v| Store(lv, v,
                                           Width32 // TODO!!!! Determine width
                                           ))
                            },
                            _ => fail!(),
                        }
                    },
                    DotExpr(ref e, ref name) => {
                        let (insts, added_addr_var) =
                            self.struct_helper(*e, name);

                        res.push_all_move(insts);

                        let binop_var = self.gen_temp();
                        (binop_var,
                         vec!(Load(binop_var.clone(),
                                   added_addr_var.clone(),
                                   Width32 // TODO!!!! Determine width
                                   )
                              ),
                         added_addr_var,
                         |lv, v| Store(lv, v,
                                   Width32 // TODO!!!! Determine width
                                   ))
                    }
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
                            res.push(BinOp(binop_result_var,
                                           inner_op.val.clone(),
                                           Variable(binop_var),
                                           Variable(var2)));
                            binop_result_var
                        },
                        // No binop. We can just assign the result of
                        // the rhs directly.
                        None => var2
                    };
                // This generates a redundant store in some cases, but
                // the optimizer will eliminate them.
                res.push(finalize(lhs_var, final_var));

                (res, final_var)
            }
            BlockExpr(ref b) => self.convert_block(*b),
            IfExpr(ref e, ref b1, ref b2) => {
                let (mut insts, if_var) = self.convert_expr(*e);
                let (b1_insts, b1_var) = self.convert_block(*b1);
                let (b2_insts, b2_var) = self.convert_block(*b2);
                let b1_label = self.gen_label();
                let end_var = self.gen_temp();
                insts.push(CondGoto(false,
                                    Variable(if_var),
                                    b1_label,
                                    TreeSet::new()));
                insts.push_all_move(b2_insts);
                insts.push(UnOp(end_var, Identity, Variable(b2_var)));
                insts.push(Goto(b1_label, TreeSet::new()));
                insts.push(Label(b1_label, TreeSet::new()));
                insts.push_all_move(b1_insts);
                insts.push(UnOp(end_var, Identity, Variable(b1_var)));
                (insts, end_var)
            },
            WhileExpr(ref e, ref b) => {
                let (block_insts, _) = self.convert_block(*b);
                self.while_helper(*e, block_insts)
            },
            ForExpr(ref init, ref cond, ref iter, ref body) => {
                let (mut init_insts, _) = self.convert_expr(*init);
                let (mut block_insts, _) = self.convert_block(*body);
                let (iter_insts, _) = self.convert_expr(*iter);
                block_insts.push_all_move(iter_insts);
                let (loop_insts, var) = self.while_helper(*cond, block_insts);
                init_insts.push_all_move(loop_insts);
                (init_insts, var)
            }
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
                ops.push_all_move(new_ops);
                let result_var = self.gen_temp();
                ops.push(Call(result_var.clone(),
                              Variable(new_var),
                              vars.move_iter().map(
                                  |v| Variable(v)).collect()));
                (ops, result_var)
            },
            DotExpr(ref e, ref name) |
            ArrowExpr(ref e, ref name) => {
                let (mut ops, added_addr_var) = self.struct_helper(*e, name);

                let res_var = self.gen_temp();
                ops.push(Load(res_var, added_addr_var,
                              Width32 // TODO!!!! Determine width
                              ));
                (ops, res_var)
            },
            CastExpr(ref e, _) => {
                self.convert_expr(*e)
            },
            UnitExpr => (vec!(), self.gen_temp()),
            SizeofExpr(ref t) => {
                let v = self.gen_temp();
                let ty = self.typemap.types.get(&t.id.to_uint());

                (vec!(UnOp(v,
                           Identity,
                           Constant(NumLit(size_of_ty(self.session,
                                                      self.typemap,
                                                      ty),
                                           UnsignedInt(Width32))))),
                 v)
            },
            UnOpExpr(ref op, ref e) => {
                let (mut insts, v) = self.convert_expr(*e);
                let res_v = self.gen_temp();
                insts.push(UnOp(res_v,
                                op.val.clone(),
                                Variable(v)));
                (insts, res_v)
            },
            ReturnExpr(ref e) => {
                let (mut insts, v) = self.convert_expr(*e);
                insts.push(Return(Variable(v)));
                (insts, self.gen_temp())
            }
            TupleExpr(..) => unimplemented!(),
            StructExpr(..) => unimplemented!(),
            IndexExpr(..) => unimplemented!(),
            BreakExpr(..) => unimplemented!(),
            ContinueExpr(..) => unimplemented!(),
            MatchExpr(..) => unimplemented!(),
            MacroExpr(..) => fail!("ICE: macros should have been expanded by now"),
        }
    }

    // Helper function for dealing with structs. Returns a list of ops and
    // a variable that, after the ops, will point to the field `name`
    // of the structure given by the expression `e`.
    fn struct_helper(&mut self, e: &Expr, name: &Name) -> (Vec<Op>, Var) {
        let (mut ops, var) = self.convert_expr(e);
        let id = match *self.typemap.types.get(&e.id.to_uint()) {
            StructTy(id, _) => id,
            PtrTy(ref p) => match p.val {
                StructTy(id, _) => id,
                _ => fail!("ICE: struct pointer doesn't have struct pointer type. Typechecker should have caught this.")
            },
            _ => fail!("ICE: struct doesn't have struct type. Typechecker should have caught this.")
        };
        let offs = offset_of_struct_field(
            self.session,
            self.typemap,
            &id,
            name);

        let added_addr_var = self.gen_temp();

        ops.push(BinOp(
            added_addr_var,
            PlusOp,
            Variable(var),
            Constant(NumLit(offs, UnsignedInt(Width32)))));

        (ops, added_addr_var)
    }
}
