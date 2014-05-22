use session::Interner;

use ast::*;
use ir::*;

pub struct ASTToIntermediate<'a> {
    var_count: uint,
    interner: &'a mut Interner,
}

impl<'a> ASTToIntermediate<'a> {
    pub fn new(interner: &'a mut Interner) -> ASTToIntermediate<'a> {
        ASTToIntermediate { var_count: 0, interner: interner }
    }

    fn gen_temp(&mut self) -> Var {
        let res = Var {
            name: self.interner.intern(format!("TEMP{}", self.var_count)),
            index: 0,
        };
        self.var_count += 1;
        res
    }

    pub fn convert_stmt(&mut self, stmt: &Stmt) -> (Vec<Op>, Var) {
        match stmt.val {
            ExprStmt(ref e) => self.convert_expr(e),
            SemiStmt(ref e) => self.convert_expr(e),
            _ => fail!(), // TODO: let statements.
        }
    }

    pub fn convert_block(&mut self, block: &Block) -> (Vec<Op>, Var) {
        let mut ops = vec!();
        for stmt in block.stmts.iter() {
            let (new_ops, new_var) = self.convert_stmt(stmt);
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
            IdentExpr(ref id) => {
                (vec!(), Var { name: id.val.name, index: 0 })
            },
            AssignExpr(ref e1, ref e2) => {
                let mut res;
                let (insts2, var2) = self.convert_expr(*e2);
                let (lhs, res_v) = match e1.val {
                    IdentExpr(ref id) => {
                        res = vec!();
                        (VarLValue(Var { name: id.val.name,
                                         index: 0 }),
                         Var { name: id.val.name,
                               index: 0 })
                    },
                    UnOpExpr(ref op, ref e) => {
                        let (insts, var) = self.convert_expr(*e);
                        res = insts;
                        match op.val {
                            Deref => {
                                (PtrLValue(var.clone()),
                                 var2.clone())
                            },
                            _ => fail!(),
                        }
                    },
                    _ => fail!(),
                };
                res.push_all_move(insts2);
                res.push(Assign(lhs, DirectRValue(Variable(var2))));
                (res, res_v)
            }
            BlockExpr(ref b) => self.convert_block(*b),
            _ => (vec!(), self.gen_temp()),
        }
    }
}
