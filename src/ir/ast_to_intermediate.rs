use util::{Width32, Width16, Width8, AnyWidth, Width, UnsignedInt, Name};

use mc::session::Session;
use mc::ast::defmap::{StructDef, EnumDef, VariantDef};

use std::collections::{TreeSet, TreeMap};
use std::vec::unzip;

use mc::ast::*;

use intrinsics::size_of::*;
use ir::*;
use typechecker::*;

pub struct ASTToIntermediate<'a> {
    var_count: uint,
    label_count: uint,
    session: &'a mut Session,
    typemap: &'a mut Typemap,
    continue_labels: Vec<uint>,
    break_labels: Vec<uint>,
}

fn ty_is_reference(ty: &Ty) -> bool {
    // Some types are never stored directly in registers; instead, we store
    // references to them. These types behave differently than others (for
    // example, dereferencing them, in the IR, is a no-op). This function
    // tells us what types these are.

    match *ty {
        // These types are stored by reference interally,
        // so taking the address once is a no-op in the IR.
        StructTy(..) |
        EnumTy(..) |
        ArrayTy(..) => true,
        _ => false,
    }
}

fn ty_width(ty: &Ty) -> Width {
    match *ty {
        BoolTy => Width8,
        IntTy(w) |
        UintTy(w) => w.clone(),
        _ => Width32,
    }
}

impl<'a> ASTToIntermediate<'a> {
    pub fn new(session: &'a mut Session,
               typemap: &'a mut Typemap) -> ASTToIntermediate<'a> {
        ASTToIntermediate { var_count: 0,
                            label_count: 0,
                            session: session,
                            typemap: typemap,
                            continue_labels: vec!(),
                            break_labels: vec!(),
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

    pub fn convert_stmt(&mut self, stmt: &Stmt) -> (Vec<Op>, Option<Var>) {
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
                        let other_v = other_v.expect(
                            "Must have a value when assigning");
                        ops.push(UnOp(v, Identity, Variable(other_v)));
                        (ops, Some(v))
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
                                        (vec!(Alloca(v, size)), Some(v))
                                    },
                                    ArrayTy(ref ty, ref nelem_opt) => {
                                        let size = size_of_ty(self.session,
                                                              self.typemap,
                                                              &ty.val);
                                        let nelem = nelem_opt.expect(
                                            "Array must have a size");
                                        (vec!(Alloca(v, size*nelem)), Some(v))
                                    }
                                    // TODO: enums
                                    _ => (vec!(), Some(v))
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn convert_block(&mut self, block: &Block) -> (Vec<Op>, Option<Var>) {
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
                (ops, None)
            }
        }
    }

    pub fn convert_item(&mut self, item: &Item) -> (Vec<Vec<Op>>,
                                                    Vec<StaticIRItem>) {
        match item.val {
            FuncItem(ref id, ref args, _, ref block, _) => {
                let (new_ops, v) = match *block {
                    Some(ref block) => self.convert_block(block),
                    // TODO: extern functions.
                    None => return (vec!(), vec!())
                };

                let vars: Vec<Var> = args
                    .iter()
                    .map(
                        |arg| Var {
                            name: arg.ident.val.name,
                            generation: None
                        })
                    .collect();

                let var_stores = vars.iter()
                    .map(|var| UnOp(var.clone(), Identity,
                                    Variable(var.clone()))).collect();

                let mut ops = vec!(Func(id.val.name, vars));
                ops.push_all_move(var_stores);
                ops.push_all_move(new_ops);

                match v {
                    Some(v) => ops.push(Return(Variable(v))),
                    // TODO: Return should take an Option.
                    None => {
                        let v = self.gen_temp();
                        ops.push(UnOp(v, Identity,
                                      Constant(NumLit(5,
                                                      UnsignedInt(Width32)))));
                        ops.push(Return(Variable(v)));
                    },
                }
                (vec!(ops), vec!())
            },
            ModItem(_, ref module) => self.convert_module(module),
            StructItem(..) |
            EnumItem(..) |
            UseItem(..) => (vec!(), vec!()),
            ConstItem(ref id, ref t, ref exp) => {
                self.static_item_helper(id, t, &Some(exp.clone()))
            },
            StaticItem(ref id, ref t, ref exp) => {
                self.static_item_helper(id, t, exp)
            }
            _ => fail!("{}", item)
        }
    }

    fn static_item_helper(&mut self,
                          id: &Ident,
                          t: &Type,
                          exp: &Option<Expr>) -> (Vec<Vec<Op>>,
                                                  Vec<StaticIRItem>) {
        let ty = self.typemap.types.get(&t.id.to_uint());

        let size = size_of_ty(self.session,
                              self.typemap,
                              ty);
        (vec!(),
         vec!(StaticIRItem {
             name: id.val.name,
             size: size as uint,
             is_ref: ty_is_reference(ty),
             expr: exp.clone(),
         }))
    }

    pub fn convert_module(&mut self, module: &Module) -> (Vec<(Vec<Op>)>,
                                                          Vec<StaticIRItem>) {
        let mut res = vec!();
        let mut static_res = vec!();
        for item in module.val.items.iter() {
            let (converted_items, converted_static_items) =
                self.convert_item(item);
            res.push_all_move(converted_items);
            static_res.push_all_move(converted_static_items);
        }
        (res, static_res)
    }

    pub fn allocate_globals(globals: Vec<StaticIRItem>
                            ) -> TreeMap<Name, (uint, uint,
                                                bool, Option<Expr>)> {
        let mut offs: uint = 0;
        let mut result = TreeMap::new();

        for global in globals.move_iter() {
            result.insert(global.name, (offs, global.size,
                                        global.is_ref, global.expr));
            offs += global.size;
        }

        result
    }

    /// Once we know where in memory we're putting the globals, we have to
    /// initialize them. Later on this should be moved to some other part of
    /// the object file, but for now we just generate code on startup to
    /// initalize all of the items.
    pub fn convert_globals(&mut self,
                           global_map: &TreeMap<Name,
                           (uint, uint, bool, Option<Expr>)>) -> Vec<Op> {
        let mut res = vec!(
            Func(self.session.interner.intern("__INIT_GLOBALS".to_string()),
                 vec!()));
        for (name, &(_, size, is_ref, ref en)) in global_map.iter() {
            match *en {
                Some(ref expr) => {
                    // First get the instructions for this expression.
                    let (ops, expr_var) = self.convert_expr(expr);
                    let expr_var = expr_var.expect(
                        "Global must have a non-unit value");
                    let ty = (*self.typemap.types.get(&expr.id.to_uint()))
                        .clone();
                    res.push_all_move(ops);
                    // If the type is a reference, then the instructions just
                    // produced will store the values into the correct memory
                    // location, and we have no work to do. But if not, then
                    // the value was stored into a register, and it's our
                    // responsibility to store it in memory.
                    assert_eq!(is_ref, ty_is_reference(&ty));
                    let result_var = Var { name: name.clone(),
                                           generation: None };
                    if ty_is_reference(&ty) {
                        let new_result_var = self.gen_temp();
                        res.push(UnOp(new_result_var.clone(), Identity,
                                      Variable(result_var.clone())));
                        res.push(UnOp(expr_var.clone(), Identity,
                                      Variable(expr_var.clone())));
                        let len_var = self.gen_temp();
                        res.push(UnOp(len_var, Identity,
                                      Constant(NumLit(size as u64,
                                                      UnsignedInt(Width32)))));
                        res.push(
                            Call(self.gen_temp(),
                                 Variable(
                                     Var { name: self.session.interner.intern(
                                         "memcpy".to_string()),
                                           generation: None }),
                                 vec!(new_result_var,
                                      expr_var.clone(),
                                      len_var)));
                    } else {
                        res.push(UnOp(result_var, Identity,
                                      Variable(expr_var)));
                    }
                },
                _ => {},
            }
        }
        res.push(Return(Variable(self.gen_temp())));

        res
    }

    // Helper function for for and while loops.
    // e is the expression in the while statement.
    // block_insts is the block of the while loop.
    // iter_insts, if present, is the iteration for the for loop (which
    //   will go after block_insts).
    // break_label and continue_label will be used as the labels that are
    //   appropriate for a break statement or a continue statement within
    //   the block.
    fn while_helper(&mut self,
                    e: &Expr,
                    block_insts: Vec<Op>,
                    iter_insts: Option<Vec<Op>>,
                    break_label: uint,
                    continue_label: uint) -> (Vec<Op>, Option<Var>) {
        // In the case of a while loop, a "continue" should go all the way
        // back to the beginning. But in the case of a for loop, the beginning
        // label will be different, and "continue" will jump to the end of
        // block_insts, but the beginning of iter_insts.
        let begin_label = match iter_insts {
            Some(..) => self.gen_label(),
            _ => continue_label,
        };
        // We always break to the very end of everything.
        let end_label = break_label;
        let mut res = vec!(
            Goto(begin_label, TreeSet::new()),
            Label(begin_label, TreeSet::new()));
        let (cond_insts, cond_var) = self.convert_expr(e);
        let cond_var = cond_var.expect(
            "Condition must have a non-unit value");
        res.push_all_move(cond_insts);
        res.push(CondGoto(true,
                          Variable(cond_var),
                          end_label, TreeSet::new()));
        res.push_all_move(block_insts);
        match iter_insts {
            Some(insts) => {
                // These are run in a for loop, where we need iter_insts.
                res.push(Goto(continue_label, TreeSet::new()));
                res.push(Label(continue_label, TreeSet::new()));
                res.push_all_move(insts);
            },
            // In a while loop, there's nothing more to do here.
            _ => {},
        }
        res.push(Goto(begin_label, TreeSet::new()));
        res.push(Label(end_label, TreeSet::new()));
        (res, None)
    }

    pub fn convert_expr(&mut self, expr: &Expr) -> (Vec<Op>, Option<Var>) {
        match expr.val {
            LitExpr(ref lit) => {
                let res_var = self.gen_temp();
                let insts = vec!(
                    UnOp(res_var.clone(), Identity, Constant(lit.val.clone()))
                    );
                (insts, Some(res_var))
            }
            BinOpExpr(ref op, ref e1, ref e2) => {
                let (mut insts1, var1) = self.convert_expr(&**e1);
                let (insts2, var2) = self.convert_expr(&**e2);
                let var1 = var1.expect(
                    "Binop argument must have a non-unit value");
                let var2 = var2.expect(
                    "Binop argument must have a non-unit value");
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
                        (insts1, Some(var1))
                    },
                    _ => {
                        let new_res = self.gen_temp();
                        insts1.push_all_move(insts2);
                        insts1.push(
                            BinOp(new_res.clone(),
                                  op.val.clone(),
                                  Variable(var1),
                                  Variable(var2)));
                        (insts1, Some(new_res))
                    }
                }
            },
            PathExpr(ref path) => {
                let defid = self.session.resolver.def_from_path(path);
                // We do this to avoid borrowing self.
                let def = {
                    let d = self.session.defmap.find(&defid).expect(
                        format!("Unable to find defid {}", defid).as_slice());
                    (*d).clone()
                };
                match def {
                    // Handle the case that this is an num variant,
                    // not a variable.
                    VariantDef(_, ref parent_id, _) => {
                        let idx_var = self.gen_temp();
                        let base_var = self.gen_temp();

                        let index = self.variant_index(&defid, parent_id);
                        let insts = vec!(
                            Alloca(base_var,
                                   size_of_def(self.session,
                                               self.typemap,
                                               parent_id)),
                            UnOp(idx_var, Identity,
                                 Constant(
                                     NumLit(index,
                                            UnsignedInt(Width32)))),
                            Store(base_var, idx_var, Width32)
                                );

                        return (insts, Some(base_var))
                    },
                    _ => {},
                }
                // TODO: mangling.
                (vec!(), Some(
                    Var { name: path.val.elems.last().expect(
                        format!("Empty path: {}", path).as_slice()
                        ).val.name,
                          generation: None }))
            },
            AssignExpr(ref op, ref e1, ref e2) => {
                let (mut res, var2) = self.convert_expr(&**e2);
                let var2 = var2.expect(
                    "RHS of assignment must have a non-unit value");

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
                // width is the width of the assignment,
                // finalize does the assignment to the variable; lhs_var
                //     and width are passed into it.
                let (binop_var, binop_insts, lhs_var, width,
                     finalize) = match e1val {
                    PathExpr(ref path) => {
                        let lhs_var = Var {
                            name: path.val.elems.last().expect(
                                format!("Empty path: {}", path).as_slice()
                                    ).val.name,
                            generation: None
                        };
                        (lhs_var.clone(),
                         vec!(),
                         lhs_var,
                         AnyWidth,
                         |lv, v, _| UnOp(lv, Identity, Variable(v)))
                    },
                    UnOpExpr(ref lhs_op, ref e) => {
                        let width = match *self.typemap.types.get(
                            &e2.id.to_uint()) {
                            BoolTy => Width8,
                            IntTy(w) |
                            UintTy(w) => w.clone(),
                            _ => Width32,
                        };
                        let (insts, var) = self.convert_expr(&**e);
                        let var = var.expect(
                            "Argument to unary op must have non-unit value");
                        res.push_all_move(insts);
                        let res_var = self.gen_temp();
                        match lhs_op.val {
                            Deref => {
                                (res_var.clone(),
                                 vec!(Load(res_var.clone(),
                                           var.clone(),
                                           width
                                           )
                                      ),
                                 var.clone(),
                                 width,
                                 |lv, v, w| Store(lv, v, w))
                            },
                            _ => fail!(),
                        }
                    },
                    ArrowExpr(ref e, ref name) |
                    DotExpr(ref e, ref name) => {
                        let (insts, added_addr_var, ty) =
                            self.struct_helper(&**e, name);
                        let width = ty_width(&ty);
                        let is_ref = ty_is_reference(&ty);

                        res.push_all_move(insts);

                        let binop_var = self.gen_temp();
                        (binop_var,
                         if is_ref {
                             vec!(UnOp(binop_var.clone(),
                                       Identity,
                                       Variable(added_addr_var.clone())))
                         } else {
                             vec!(Load(binop_var.clone(),
                                       added_addr_var.clone(),
                                       width.clone()
                                       )
                                  )
                         },
                         added_addr_var,
                         width,
                         |lv, v, w| Store(lv, v, w))
                    },
                    IndexExpr(ref arr, ref idx) => {
                        let ty = (*self.typemap.types.get(&e1.id.to_uint()))
                            .clone();

                        let (ops, ptr_var, width, is_ref) =
                            self.array_helper(&**arr, &**idx, &ty);
                        res.push_all_move(ops);

                        let binop_var = self.gen_temp();
                        (binop_var,
                         if is_ref {
                             vec!(UnOp(binop_var.clone(),
                                       Identity,
                                       Variable(ptr_var.clone())))
                         } else {
                             vec!(Load(binop_var.clone(),
                                       ptr_var.clone(),
                                       width))
                         },
                         ptr_var,
                         width,
                         |lv, v, w| Store(lv, v, w))
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
                res.push(finalize(lhs_var, final_var, width));

                (res, Some(final_var))
            }
            BlockExpr(ref b) => self.convert_block(&**b),
            IfExpr(ref e, ref b1, ref b2) => {
                let (mut insts, if_var) = self.convert_expr(&**e);
                let if_var = if_var.expect(
                    "Condition for if statement must have non-unit value");
                let (b1_insts, b1_var) = self.convert_block(&**b1);
                let (b2_insts, b2_var) = self.convert_block(&**b2);
                assert!(
                    b1_var.is_none() == b2_var.is_none(),
                    "ICE: one branch of if statement is unit but other isn't");
                let b1_label = self.gen_label();
                let end_label = self.gen_label();
                let end_var = self.gen_temp();
                insts.push(CondGoto(false,
                                    Variable(if_var),
                                    b1_label,
                                    TreeSet::new()));
                insts.push_all_move(b2_insts);
                match b2_var {
                    Some(b2_var) => 
                        insts.push(UnOp(end_var, Identity, Variable(b2_var))),
                    None => {},
                }
                insts.push(Goto(end_label, TreeSet::new()));
                insts.push(Label(b1_label, TreeSet::new()));
                insts.push_all_move(b1_insts);
                match b1_var {
                    Some(b1_var) =>
                        insts.push(UnOp(end_var, Identity, Variable(b1_var))),
                    None => {},
                }
                insts.push(Goto(end_label, TreeSet::new()));
                insts.push(Label(end_label, TreeSet::new()));
                match b1_var {
                    Some(..) => (insts, Some(end_var)),
                    None => (insts, None),
                }
            },
            WhileExpr(ref e, ref b) => {
                let break_label = self.gen_label();
                let continue_label = self.gen_label();
                self.break_labels.push(break_label);
                self.continue_labels.push(continue_label);
                let (block_insts, _) = self.convert_block(&**b);
                self.break_labels.pop();
                self.continue_labels.pop();
                self.while_helper(&**e,
                                  block_insts,
                                  None,
                                  break_label,
                                  continue_label)
            },
            ForExpr(ref init, ref cond, ref iter, ref body) => {
                let (mut init_insts, _) = self.convert_expr(&**init);
                let break_label = self.gen_label();
                let continue_label = self.gen_label();
                self.break_labels.push(break_label);
                self.continue_labels.push(continue_label);
                let (block_insts, _) = self.convert_block(&**body);
                self.break_labels.pop();
                self.continue_labels.pop();
                let (iter_insts, _) = self.convert_expr(&**iter);
                let (loop_insts, var) = self.while_helper(&**cond,
                                                          block_insts,
                                                          Some(iter_insts),
                                                          break_label,
                                                          continue_label);
                init_insts.push_all_move(loop_insts);
                (init_insts, var)
            }
            GroupExpr(ref e) => self.convert_expr(&**e),
            CallExpr(ref f, ref args) => {
                // We need to deal with actual function calls, as well as
                // enum constructors.
                match f.val {
                    // TODO: split a bunch of this off into a helper function.
                    PathExpr(ref path) => {
                        let defid = self.session.resolver.def_from_path(path);
                        // We do this to avoid borrowing self.
                        let def = {
                            let d = self.session.defmap.find(&defid).expect(
                                format!("Cannot find defid {}",
                                        defid).as_slice());
                            (*d).clone()
                        };
                        match def {
                            VariantDef(_, ref parent_id, ref types) => {
                                let idx_var = self.gen_temp();
                                let base_var = self.gen_temp();

                                let index = self.variant_index(&defid,
                                                               parent_id);
                                let mut insts = vec!(
                                    Alloca(base_var,
                                           size_of_def(self.session,
                                                       self.typemap,
                                                       parent_id)),
                                    UnOp(idx_var, Identity,
                                         Constant(
                                             NumLit(index,
                                                    UnsignedInt(Width32)))),
                                    Store(base_var, idx_var, Width32)
                                        );

                                let (ops, vars, widths) = self.variant_helper(
                                    types, &base_var);

                                insts.push_all_move(ops);

                                for i in range(0, vars.len()) {
                                    let var = vars[i];
                                    let width = &widths[i];
                                    let (expr_insts, expr_var) =
                                        self.convert_expr(&args[i]);
                                    let expr_var = expr_var.unwrap();
                                    insts.push_all_move(expr_insts);
                                    insts.push(Store(var,
                                                     expr_var,
                                                     width.clone()));
                                }

                                return (insts, Some(base_var))
                            },
                            _ => {},
                        }
                    },
                    _ => {}
                }

                // It's an actual function!
                let mut ops = vec!();
                let mut vars = vec!();
                for arg in args.iter() {
                    let (new_ops, new_var) = self.convert_expr(arg);
                    ops.push_all_move(new_ops);
                    vars.push(new_var.expect("Passing a unit to a function"));
                }
                let (new_ops, new_var) = self.convert_expr(&**f);
                let new_var = new_var.expect(
                    "Function pointer had no non-unit value");
                ops.push_all_move(new_ops);
                let result_var = self.gen_temp();

                // We add in a bunch of dummy assignments, so that we can
                // be sure that registers are assigned correctly. Many of
                // these will be optimized away later.
                let new_vars = Vec::from_fn(vars.len(),
                                            |_| self.gen_temp());
                for (idx, var) in vars.iter().enumerate() {
                    ops.push(UnOp(new_vars[idx], Identity,
                                  Variable(var.clone())));
                }
                ops.push(Call(result_var.clone(),
                              Variable(new_var),
                              new_vars.move_iter().collect()));
                // We add one more dummy assignment, for the result.
                ops.push(UnOp(result_var.clone(), Identity,
                              Variable(result_var.clone())));
                let this_ty = self.typemap.types.get(&expr.id.to_uint());
                match *this_ty {
                    UnitTy => (ops, None),
                    _ => (ops, Some(result_var))
                }
            },
            DotExpr(ref e, ref name) |
            ArrowExpr(ref e, ref name) => {
                let (mut ops,
                     added_addr_var,
                     ty) = self.struct_helper(&**e, name);
                let width = ty_width(&ty);
                let is_ref = ty_is_reference(&ty);

                let res_var = self.gen_temp();
                if is_ref {
                    ops.push(UnOp(res_var, Identity, Variable(added_addr_var)));
                } else {
                    ops.push(Load(res_var, added_addr_var, width));
                }
                (ops, Some(res_var))
            },
            CastExpr(ref e, _) => {
                self.convert_expr(&**e)
            },
            UnitExpr => (vec!(), None),
            SizeofExpr(ref t) => {
                let v = self.gen_temp();
                let ty = self.typemap.types.get(&t.id.to_uint());

                (vec!(UnOp(v,
                           Identity,
                           Constant(NumLit(size_of_ty(self.session,
                                                      self.typemap,
                                                      ty),
                                           UnsignedInt(Width32))))),
                 Some(v))
            },
            UnOpExpr(ref op, ref e) => {
                let ty = (*self.typemap.types.get(&e.id.to_uint())).clone();
                match e.val {
                    DotExpr(ref e, ref name) |
                    ArrowExpr(ref e, ref name) => {
                        let (ops,
                             added_addr_var,
                             _) = self.struct_helper(&**e, name);
                        return (ops, Some(added_addr_var));
                    },
                    IndexExpr(ref arr, ref idx) => {
                        let (ops, ptr_var, _, _) =
                            self.array_helper(&**arr, &**idx, &ty);
                        return (ops, Some(ptr_var));
                    },
                    _ => {}
                }

                let (mut insts, v) = self.convert_expr(&**e);
                let v = v.expect(
                    "Argument to unary op must have non-unit value");
                let res_v = self.gen_temp();
                let actual_op = match op.val {
                    AddrOf =>
                        if ty_is_reference(&ty) {
                            // These types are stored by reference interally,
                            // so taking the address once is a no-op in the IR.
                            Identity
                        } else {
                            op.val
                        },
                    Deref => {
                        match ty {
                            PtrTy(ref inner) =>
                                if ty_is_reference(&inner.val) {
                                    Identity
                                } else {
                                    // This case is a bit different, because
                                    // we emit a load instead of a UnOp.
                                    insts.push(Load(res_v,
                                                    v,
                                                    ty_width(&inner.val)));
                                    return (insts, Some(res_v));
                                },
                            _ => op.val,
                        }
                    }
                    _ => op.val,
                };
                insts.push(UnOp(res_v,
                                actual_op,
                                Variable(v)));
                (insts, Some(res_v))
            },
            ReturnExpr(ref e) => {
                let (mut insts, v) = self.convert_expr(&**e);
                let v = v.unwrap_or(self.gen_temp());
                insts.push(Return(Variable(v)));
                (insts, None)
            }
            TupleExpr(..) => unimplemented!(),
            StructExpr(ref p, ref fields) => {
                // TODO: make this more efficient.
                let defid = self.session.resolver.def_from_path(p);

                let base_var = self.gen_temp();
                let mut ops = vec!(Alloca(base_var,
                                          size_of_def(
                                              self.session,
                                              self.typemap,
                                              &defid)));

                for &(ref name, ref expr) in fields.iter() {
                    let offset_var = self.gen_temp();
                    let (expr_insts, expr_var) = self.convert_expr(expr);
                    ops.push_all_move(expr_insts);

                    let offs = offset_of_struct_field(
                        self.session,
                        self.typemap,
                        &defid,
                        name);

                    let ty = {
                        let def = self.session.defmap.find(&defid).expect(
                            format!("Cannot find defid {}", defid).as_slice());
                        match *def {
                            StructDef(_, ref fields, _) => {
                                let &(_, ref t) =
                                    fields.iter()
                                    .find(|&&(a, _)| a == *name)
                                    .expect("No struct field with given name");
                                self.typemap.types.get(&t.id.to_uint())
                            },
                            _ => fail!(),
                        }
                    };
                    let width = ty_width(ty);

                    ops.push(BinOp(
                        offset_var,
                        PlusOp,
                        Variable(base_var),
                        Constant(NumLit(offs, UnsignedInt(Width32)))));
                    ops.push(Store(offset_var, expr_var.expect(
                        "Expression of struct type must have non-unit value"
                        ), width));
                }

                (ops, Some(base_var))
            }
            IndexExpr(ref arr, ref idx) => {
                let ty = (*self.typemap.types.get(&expr.id.to_uint())).clone();

                let (mut ops, ptr_var, width, is_ref) =
                    self.array_helper(&**arr, &**idx, &ty);

                if is_ref {
                    (ops, Some(ptr_var))
                } else {
                    let result_var = self.gen_temp();
                    ops.push(Load(result_var, ptr_var, width));
                    (ops, Some(result_var))
                }
            }
            BreakExpr(..) => {
                (vec!(Goto(*self.break_labels.last().expect(
                    "Break with no label to break to"),
                           TreeSet::new())),
                 None)
            }
            ContinueExpr(..) => {
                (vec!(Goto(*self.continue_labels.last().expect(
                    "Continue with no label to continue to"),
                           TreeSet::new())),
                 None)
            }
            MatchExpr(ref e, ref arms) => {
                let (mut ops, base_var) = self.convert_expr(&**e);
                let base_var = base_var.expect(
                    "Match expression must have non-unit value");
                let variant_var = self.gen_temp();
                let end_label = self.gen_label();
                let mut result_var = None;
                ops.push(Load(variant_var, base_var, Width32));

                // These are the labels for each pattern. The are off by one:
                // we never need to jump to the beginning of the first variant.
                let mut begin_labels = Vec::from_fn(arms.len() - 1,
                                                    |_| self.gen_label());
                // We do, however, need to jump to to the end of the last, and
                // it's convenient to put the ending label at the end of this
                // list.
                begin_labels.push(end_label);

                for (pos, arm) in arms.iter().enumerate() {
                    // arm.pat, arm.body
                    let (path, pats) = match arm.pat.val {
                        VariantPat(ref path, ref pats) => (path, pats),
                        _ => fail!(),
                    };

                    let patid = self.session.resolver.def_from_path(path);
                    let def =
                        (*self.session.defmap.find(&patid).expect(
                            format!("Cannot find defid {}", patid).as_slice()
                                )).clone();
                    let (parent_id, types) = match def {
                        VariantDef(_, ref parent_id, ref types) =>
                            (parent_id, types),
                        _ => fail!(),
                    };

                    let (variant_ops, vars, widths) =
                        self.variant_helper(types, &base_var);
                    let index = self.variant_index(&patid, parent_id);

                    let compare_var = self.gen_temp();
                    // Check if the variant is the right one.
                    ops.push(BinOp(compare_var, EqualsOp,
                                   Variable(variant_var),
                                   Constant(NumLit(index,
                                                   UnsignedInt(Width32)))));
                    // If not, jump to the next one.
                    ops.push(CondGoto(true,
                                      Variable(compare_var),
                                      begin_labels[pos],
                                      TreeSet::new()));
                    // It is! Generate the code for this particular variant.
                    ops.push_all_move(variant_ops);
                    // Assign all the variables.
                    for (((var, pat), this_type), width) in
                        vars.iter().zip(pats.iter()).zip(types.iter())
                        .zip(widths.iter())
                    {
                        let this_ty = self.typemap.types.get(
                            &this_type.id.to_uint());
                        match pat.val {
                            IdentPat(ref ident, _) => {
                                // TODO: a move or a load, depending.
                                let this_var = Var { name: ident.val.name,
                                                     generation: None };
                                if ty_is_reference(this_ty) {
                                    ops.push(
                                        UnOp(this_var,
                                             Identity,
                                             Variable(*var)));
                                } else {
                                    ops.push(
                                        Load(this_var, *var, *width));
                                }
                            },
                            _ => fail!("Only ident patterns are supported right now.")
                        }
                    }
                    // Emit the body of the arm.
                    let (arm_insts, arm_var) = self.convert_expr(&arm.body);
                    ops.push_all_move(arm_insts);

                    // Assign the result, if necessary.
                    if result_var.is_none() {
                        result_var = arm_var;
                    } else {
                        ops.push(UnOp(result_var.unwrap(),
                                      Identity,
                                      Variable(arm_var.unwrap())));
                    }

                    // And skip to the end!
                    ops.push(Goto(end_label, TreeSet::new()));
                    // And finally, the label that goes before the next arm.
                    ops.push(Label(begin_labels[pos].clone(),
                                   TreeSet::new()));
                }

                (ops, result_var)
            }
            MacroExpr(..) => fail!("ICE: macros should have been expanded by now"),
        }
    }

    // Helper function for dealing with structs. Returns a list of ops and
    // a variable that, after the ops, will point to the field `name`
    // of the structure given by the expression `e`. Also returns the width
    // that must be used for the assignment.
    fn struct_helper(&mut self,
                     e: &Expr,
                     name: &Name) -> (Vec<Op>, Var, Ty) {
        let (mut ops, var) = self.convert_expr(e);
        let var = var.expect("Struct expr must have non-unit value");
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

        let ty = {
            let def = self.session.defmap.find(&id).expect(
                format!("Cannot find defid {}", id).as_slice());

            match *def {
                StructDef(_, ref fields, _) => {
                    let &(_, ref t) =
                        fields.iter()
                        .find(|&&(a, _)| a == *name)
                        .expect(format!("Cannot find name {}", name).as_slice()
                                );
                    self.typemap.types.get(&t.id.to_uint())
                },
                _ => fail!("Looking up struct field offset in a non-struct.")
            }
        }.clone();


        let added_addr_var = self.gen_temp();

        ops.push(BinOp(
            added_addr_var,
            PlusOp,
            Variable(var),
            Constant(NumLit(offs, UnsignedInt(Width32)))));

        (ops, added_addr_var, ty)
    }

    fn variant_index(&mut self, defid: &NodeId, parent_id: &NodeId) -> u64 {
        (match *self.session.defmap.find(parent_id).expect(
            format!("Cannot find defid {}", parent_id).as_slice()) {
            EnumDef(_, ref variants, _) =>
                variants.iter().position(|&n| n == *defid).expect(
                    format!("Cannot find defid {}", defid).as_slice()),
            _ => fail!(),
        }) as u64
    }

    // Given the list of types in this enum, and a variable
    // pointing to the start of the enum in memory, returns a list of ops,
    // and list of Vars, and a list of Widths, where (after the ops are
    // executed) the variables are pointers to each of the fields, and Width
    // is the width of a load or store to/from the corresponding field.
    fn variant_helper(&mut self,
                      types: &Vec<Type>,
                      base_var: &Var) -> (Vec<Op>, Vec<Var>, Vec<Width>) {
        let mut insts = vec!();
        let (widths, sizes) = unzip(types.iter()
            .map(|t| self.typemap.types.get(&t.id.to_uint()))
            .map(|ty| (ty_width(ty),
                       size_of_ty(self.session, self.typemap, ty))));

        let vars = Vec::from_fn(sizes.len(), |_| self.gen_temp());

        for i in range(0, sizes.len()) {
            let offs = enum_tag_size + offset_of(&sizes, i);

            let new_offs_var = &vars[i];
            insts.push(BinOp(new_offs_var.clone(),
                             PlusOp,
                             Variable(base_var.clone()),
                             Constant(NumLit(offs, UnsignedInt(Width32)))));
        }

        (insts, vars, widths)
    }

    fn array_helper(&mut self,
                    arr: &Expr,
                    idx: &Expr,
                    ty: &Ty) -> (Vec<Op>, Var, Width, bool) {
        let (mut ops, base_var) = self.convert_expr(arr);
        let base_var = base_var.expect("Array base must have non-unit value");
        let (idx_ops, idx_var) = self.convert_expr(idx);
        let idx_var = idx_var.expect("Array index must have non-unit value");
        ops.push_all_move(idx_ops);
        let size = size_of_ty(self.session, self.typemap, ty);
        let total_size = packed_size(&vec!(size));
        let offs_var = self.gen_temp();
        let ptr_var = self.gen_temp();
        ops.push(BinOp(offs_var,
                       TimesOp,
                       Variable(idx_var),
                       Constant(NumLit(total_size,
                                       UnsignedInt(Width32)))));
        ops.push(BinOp(ptr_var,
                       PlusOp,
                       Variable(base_var),
                       Variable(offs_var)));

        (ops, ptr_var, ty_width(ty), ty_is_reference(ty))
    }
}
