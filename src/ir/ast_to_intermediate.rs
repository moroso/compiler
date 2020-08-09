use util::align;
use util::{Name, Width};
use util::Width::{AnyWidth, Width32, Width16, Width8};
use util::IntKind::{GenericInt, SignedInt, UnsignedInt};
use mc::ast::defmap::Def::{StructDef, EnumDef, VariantDef, ConstDef};
use mc::session::Session;
use values::static_cast;

use std::collections::{BTreeSet, BTreeMap};

use mc::ast;
use mc::ast::*;

use intrinsics::size_of::*;
use ir::*;
use typechecker::{Typemap, Ty, enum_is_c_like};
use typechecker::Ty::*;
use typechecker::TyBounds::*;

use std::mem::swap;
use std::cmp::max;

pub struct ASTToIntermediate<'a, 'b: 'a> {
    var_count: usize,
    label_count: usize,
    next_id: usize,
    session: &'a mut Session<'b>,
    typemap: &'a mut Typemap,
    manglemap: &'a BTreeMap<NodeId, String>,
    continue_labels: Vec<usize>,
    break_labels: Vec<usize>,
    // TODO: IrNodeId here.
    sourcemap: &'a mut BTreeMap<IrNodeId, NodeId>,
}

fn ty_is_reference(session: &Session, ty: &Ty) -> bool {
    // Some types are never stored directly in registers; instead, we store
    // references to them. These types behave differently than others (for
    // example, dereferencing them, in the IR, is a no-op). This function
    // tells us what types these are.

    match *ty {
        // These types are stored by reference interally,
        // so taking the address once is a no-op in the IR.
        StructTy(..) |
        ArrayTy(..) => true,
        EnumTy(ref id, _, _) => {
            !enum_is_c_like(session, id)
        }
        _ => false,
    }
}

fn ty_width(ty: &Ty) -> Width {
    match *ty {
        BoolTy => Width8,
        IntTy(w) |
        UintTy(w) => w,
        _ => Width32,
    }
}

/// Do whatever sign extending we need to do to our constants.
fn adjust_constant(c: &LitNode) -> LitNode {
    // We're using LitNodes for convenience, but we never actually
    // use the type. So we'll make them all u32.
    let target_type = UnsignedInt(Width32);
    match *c {
        NumLit(n, ref k) => {
            match *k {
                UnsignedInt(..) => NumLit(n, target_type),
                GenericInt |
                SignedInt(AnyWidth) |
                SignedInt(Width32) => NumLit(n as i32 as u64, target_type),
                SignedInt(Width16) => NumLit(n as i16 as u64, target_type),
                SignedInt(Width8)  => NumLit(n as i8 as u64, target_type),
            }
        }
        _ => c.clone()
    }
}

impl<'a, 'b> ASTToIntermediate<'a, 'b> {
    pub fn new<'c,'d>(session: &'c mut Session<'d>,
                      typemap: &'c mut Typemap,
                      manglemap: &'c BTreeMap<NodeId, String>,
                      sourcemap: &'c mut BTreeMap<IrNodeId, NodeId>,
                      ) -> ASTToIntermediate<'c,'d> {
        ASTToIntermediate { var_count: 0,
                            label_count: 0,
                            next_id: 0,
                            session: session,
                            typemap: typemap,
                            manglemap: manglemap,
                            sourcemap: sourcemap,
                            continue_labels: vec!(),
                            break_labels: vec!(),
        }
    }

    fn new_id(&mut self) -> IrNodeId {
        let id = self.next_id;
        self.next_id += 1;
        IrNodeId(id)
    }

    fn add_id<U>(&mut self, val: U) -> WithIdT<IrNodeId, U> {
        let id = self.new_id();
        WithIdT { val: val, id: id }
    }

    fn add_source(&mut self, ops: &[Op], source_node_id: NodeId) {
        for op in ops.iter() {
            self.sourcemap.entry(op.id).or_insert(source_node_id);
        }
    }

    /// Does whatever needs to be done to a variable so that it has a correct
    /// value for some type that is smaller than 32 bits.
    fn contract(&mut self,
                v: Var,
                ty: &Ty) -> (Vec<Op>, Option<Var>) {
        match *ty {
            IntTy(..) |
            UintTy(..) |
            GenericIntTy => {},
            _ => return (vec!(), Some(v))
        }
        let width = ty_width(ty);
        let is_signed = ty.is_signed();

        match width {
            Width8 |
            Width16 => {
                let res = self.gen_temp();
                if is_signed {
                    (vec!(self.add_id(OpNode::UnOp {
                        target: res,
                        op: if width == Width8 {
                            SxbOp
                        } else {
                            SxhOp
                        },
                        operand: Variable(v)
                    })), Some(res))
                } else {
                    (vec!(self.add_id(OpNode::BinOp {
                        target: res,
                        op: BitAndOp,
                        lhs: Variable(v),
                        rhs: Constant(NumLit(
                            width.mask(),
                            UnsignedInt(Width32))),
                        signed: false,
                    })), Some(res))
                }
            },
            _ => (vec!(), Some(v))
        }
    }

    fn gen_temp(&mut self) -> Var {
        let res = Var {
            name: VarName::IRTempVariable(self.var_count),
            generation: None,
        };
        self.var_count += 1;
        res
    }

    fn gen_label(&mut self) -> usize {
        let res = self.label_count;
        self.label_count += 1;
        res
    }

    pub fn next_label(&self) -> usize {
        self.label_count
    }

    pub fn convert_stmt(&mut self, stmt: &Stmt) -> (Vec<Op>, Option<Var>) {
        let (ops, var) = match stmt.val {
            ExprStmt(ref e) |
            SemiStmt(ref e) => self.convert_expr(e),
            LetStmt(ref pat, ref e_opt) => {
                let (v, ty) = match pat.val {
                    IdentPat(ref ident, ref ty_opt) => {
                        (Var { name: VarName::NamedVariable(ident.val.name, ident.id),
                               generation: None },
                         ty_opt.clone())
                    },
                    _ => panic!("Only ident patterns are supported right now.")
                };

                match *e_opt {
                    Some(ref e) => {
                        let (mut ops, other_v) = self.convert_expr(e);
                        let other_v = other_v.expect(
                            "Must have a value when assigning");
                        ops.push(self.add_id(OpNode::UnOp { target: v, op: Identity,
                                                            operand: Variable(other_v) }));
                        (ops, Some(v))
                    },
                    None => {
                        match ty {
                            None => panic!("No type available."),
                            Some(ref t) => {
                                let (op, v) = match *self.lookup_ty(t.id) {
                                    StructTy(ref id, _) => {
                                        let size = size_of_def(self.session,
                                                               self.typemap,
                                                               id);
                                        (OpNode::Alloca { var: v, size: size }, Some(v))
                                    },
                                    ArrayTy(ref ty, ref nelem_opt) => {
                                        let size = size_of_ty(self.session,
                                                              self.typemap,
                                                              &ty.val);
                                        let nelem = nelem_opt.expect(
                                            "Array must have a size");
                                        (OpNode::Alloca { var: v, size: size*nelem }, Some(v))
                                    }
                                    // TODO: enums
                                    _ => // If no expression is given,
                                        // initialize to 0.
                                        (OpNode::UnOp {
                                            target: v,
                                            op: Identity,
                                            operand: Constant(NumLit(
                                                0,
                                                UnsignedInt(
                                                    Width32)))
                                        },
                                         Some(v))
                                };
                                (vec!(self.add_id(op)), v)
                            }
                        }
                    }
                }
            },
        };
        self.add_source(&ops, stmt.id);

        (ops, var)
    }

    pub fn convert_block(&mut self, block: &Block) -> (Vec<Op>, Option<Var>) {
        let mut ops = vec!();
        for stmt in &block.val.stmts {
            let (new_ops, _) = self.convert_stmt(stmt);
            ops.extend(new_ops.into_iter());
        }
        match block.val.expr {
            Some(ref e) => {
                let (new_ops, new_var) = self.convert_expr(e);
                ops.extend(new_ops.into_iter());
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
                    LocalFn(ref block) => self.convert_block(block),
                    // For externs with no block, we'll generate a Func() op
                    // but nothing else.
                    ExternFn(ref abi, ref block_opt) => {
                        match *block_opt {
                            Some(ref block) => {
                                // TODO: this should be a softer error.
                                assert_eq!(abi.to_string(), "bare");
                                self.convert_block(block)
                            },
                            None => {
                                assert_eq!(abi.to_string(), "C");
                                (vec!(), Some(self.gen_temp()))
                            }
                        }
                    }
                };

                let vars: Vec<Var> = args
                    .iter()
                    .map(
                        |arg| Var {
                            name: VarName::NamedVariable(arg.ident.val.name, arg.ident.id),
                            generation: None
                        })
                    .collect();

                let var_stores: Vec<Op> = vars.iter()
                    .map(|var| self.add_id(OpNode::UnOp {
                        target: *var,
                        op: Identity,
                        operand: Variable(*var)
                    })).collect();

                let op = OpNode::Func { name: self.mangled_ident(id), args: vars, abi: block.abi() };
                let mut ops = vec!(self.add_id(op));
                if block.is_local() {
                    ops.extend(var_stores.into_iter());
                }

                ops.extend(new_ops.into_iter());

                if block.is_local() {
                    match v {
                        Some(v) => ops.push(self.add_id(OpNode::Return { retval: Some(Variable(v)) })),
                        None => {
                            ops.push(self.add_id(OpNode::Return { retval: None }));
                        },
                    }
                }
                (vec!(ops), vec!(
                    StaticIRItem {
                        name: self.mangled_ident(id),
                        label: None, // Will be filled in later
                        size: 0,
                        offset: None, // Will be filled in later
                        is_ref: false,
                        is_func: true,
                        is_extern: block.is_extern(),
                        expr: None,
                    }
                    ))
            },
            ModItem(_, ref module) => self.convert_module(module),
            StructItem(..) |
            EnumItem(..) |
            ConstItem(..) |
            UseItem(..) => (vec!(), vec!()),
            StaticItem(ref id, ref t, ref exp, is_extern) => {
                self.static_item_helper(id, t, exp, is_extern)
            }
            TypeItem(..) => { (vec!(), vec!()) } // Nothing to do for typedefs.
            _ => panic!("{}", item)
        }
    }

    fn static_item_helper(&mut self,
                          id: &Ident,
                          t: &Type,
                          exp: &Option<Expr>,
                          is_extern: bool) -> (Vec<Vec<Op>>,
                                               Vec<StaticIRItem>) {
        let name = self.mangled_ident(id);
        let ty = self.lookup_ty(t.id);

        let size = size_of_ty(self.session,
                              self.typemap,
                              ty);
        (vec!(),
         vec!(StaticIRItem {
             name: name,
             label: None,
             size: size as usize,
             offset: None,
             is_ref: ty_is_reference(self.session, ty),
             is_func: false,
             is_extern: is_extern,
             expr: exp.clone(),
         }))
    }

    pub fn convert_module(&mut self, module: &Module) -> (Vec<Vec<Op>>,
                                                          Vec<StaticIRItem>) {
        let mut res = vec!();
        let mut static_res = vec!();
        for item in &module.val.items {
            let (converted_items, converted_static_items) =
                self.convert_item(item);
            res.extend(converted_items.into_iter());
            static_res.extend(converted_static_items.into_iter());
        }
        (res, static_res)
    }

    pub fn allocate_globals(session: &mut Session, globals: Vec<StaticIRItem>
                            ) -> BTreeMap<VarName, StaticIRItem> {
        let mut offs: usize = 0;
        let mut result = BTreeMap::new();

        // For now, align everything to 4 bytes.
        // TODO: smarter alignment/packing of globals.
        for mut global in globals {
            // Nothing to do for functions.
            if global.is_func {
                global.label = Some(session.interner.intern(
                    format!("{}", global.name.base_name())))
            } else {
                let mut size = global.size;
                global.offset = Some(offs);
                // TODO: this is a hack; this should be refactored (in the same
                // way that variable names should be).
                let label_name = if global.is_extern {
                    // For externs, it's essential that the name match the actual name.
                    session.interner.intern(
                        format!("{}", global.name.base_name()))
                } else {
                    session.interner.intern(
                        format!("GLOBAL_LABEL_{}", global.name.canonical_name()))
                };
                global.label = Some(label_name);

                // Ensure we end of a 4-byte boundary.
                size = align(size, 4);
                assert!(size % 4 == 0);
                offs += size;
                assert!(offs % 4 == 0);
            }
            result.insert(global.name, global);
        }

        result
    }

    /// Once we know where in memory we're putting the globals, we have to
    /// initialize them. Later on this should be moved to some other part of
    /// the object file, but for now we just generate code on startup to
    /// initalize all of the items.
    pub fn convert_globals(&mut self,
                           global_map: &BTreeMap<VarName, StaticIRItem>) -> Vec<Op> {
        let op = OpNode::Func {
            name: VarName::MangledVariable(
                self.session.interner.intern("_INIT_GLOBALS".to_string())),
            args: vec!(),
            abi: None };
        let mut res = vec!(self.add_id(op));
        for (name, global_item) in global_map.iter() {
            let is_ref = global_item.is_ref;
            let is_func = global_item.is_func;
            let en = &global_item.expr;
            if is_func { continue; } // No initialization to do for functions.
            if let Some(ref expr) = *en {
                // First get the instructions for this expression.
                let (ops, expr_var) = self.convert_expr(expr);
                let expr_var = expr_var.expect(
                    "Global must have a non-unit value");
                let ty = (*self.lookup_ty(expr.id))
                    .clone();
                res.extend(ops.into_iter());
                // If the type is a reference, then the instructions just
                // produced will store the values into the correct memory
                // location, and we have no work to do. But if not, then
                // the value was stored into a register, and it's our
                // responsibility to store it in memory.
                assert_eq!(is_ref, ty_is_reference(self.session, &ty));
                let result_var = Var { name: *name,
                                       generation: None };
                if ty_is_reference(self.session, &ty) {
                    res.extend(
                        self.gen_copy(&result_var, &expr_var, &ty).into_iter());
                } else {
                    res.push(self.add_id(OpNode::UnOp {
                        target: result_var,
                        op: Identity,
                        operand: Variable(expr_var)
                    }));
                }
            }
        }
        let op = OpNode::Return { retval: None };
        res.push(self.add_id(op));

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
    // If middle_label is non-None, it will be added as a label after the
    //   condition but before the block. (This is for do .. while loops).
    fn while_helper(&mut self,
                    e: &Expr,
                    block_insts: Vec<Op>,
                    iter_insts: Option<Vec<Op>>,
                    break_label: usize,
                    continue_label: usize,
                    middle_label: Option<usize>) -> (Vec<Op>, Option<Var>) {
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
            self.add_id(OpNode::Goto { label_idx: begin_label, vars: BTreeSet::new() }),
            self.add_id(OpNode::Label { label_idx: begin_label, vars: BTreeSet::new() }));
        let (cond_insts, cond_var) = self.convert_expr(e);
        let cond_var = cond_var.expect(
            "Condition must have a non-unit value");
        res.extend(cond_insts.into_iter());
        res.push(self.add_id(OpNode::CondGoto {
            negated: true,
            cond: Variable(cond_var),
            label_idx: end_label,
            vars: BTreeSet::new()
        }));
        if let Some(l) = middle_label {
            res.extend(
                vec!(self.add_id(OpNode::Goto { label_idx: l, vars: BTreeSet::new() }),
                     self.add_id(OpNode::Label { label_idx: l, vars: BTreeSet::new() })).into_iter());
        }
        res.extend(block_insts.into_iter());
        if let Some(insts) = iter_insts {
            // These are run in a for loop, where we need iter_insts.
            res.push(self.add_id(OpNode::Goto { label_idx: continue_label,
                                                vars: BTreeSet::new() }));
            res.push(self.add_id(OpNode::Label { label_idx: continue_label,
                                                 vars: BTreeSet::new() }));
            res.extend(insts.into_iter());
        }
        // In a while loop, there's nothing more to do here.

        res.push(self.add_id(OpNode::Goto { label_idx: begin_label, vars: BTreeSet::new() }));
        res.push(self.add_id(OpNode::Label { label_idx: end_label, vars: BTreeSet::new() }));
        (res, None)
    }

    fn mangled_path(&mut self, path: &Path) -> Name {
        let defid = self.session.resolver.def_from_path(path);

        let name_opt = self.manglemap.get(&defid);
        match name_opt {
            Some(n) => self.session.interner.intern((*n).clone()),
            None => path.val.elems.last().expect(
                &format!("Empty path: {}", path)[..]
                    ).val.name
        }
    }

    fn mangled_ident(&mut self, ident: &Ident) -> VarName {
        let name_opt = self.manglemap.get(&ident.id);
        VarName::NamedVariable(
            match name_opt {
                Some(n) => self.session.interner.intern((*n).clone()),
                None => ident.val.name
            },
            ident.id,
        )
    }

    pub fn lookup_ty(&self, id: NodeId) -> &Ty {
        let this_ty = &self.typemap.types[&id];
        match *this_ty {
            BoundTy(ref bid) => {
                match self.typemap.bounds[bid] {
                    Concrete(ref ty) => ty,
                    _ => panic!("Should have a concrete type."),
                }
            },
            _ => this_ty
        }
    }

    pub fn gen_copy(&mut self,
                    dest_var: &Var,
                    src_var: &Var,
                    ty: &Ty) -> Vec<Op> {
        let size = size_of_ty(self.session,
                              self.typemap,
                              ty);
        let mut ops = vec!();
        let new_result_var = self.gen_temp();
        let new_src_var = self.gen_temp();
        ops.push(self.add_id(OpNode::UnOp {
            target: new_result_var,
            op: Identity,
            operand: Variable(*dest_var)
        }));
        ops.push(self.add_id(OpNode::UnOp {
            target: new_src_var,
            op: Identity,
            operand: Variable(*src_var)
        }));
        let len_var = self.gen_temp();
        ops.push(self.add_id(OpNode::UnOp {
            target: len_var,
            op: Identity,
            operand: Constant(NumLit(size as u64,
                                     UnsignedInt(Width32)))
        }));
        let op = OpNode::Call {
            target: None,
            func: Variable(
                Var { name: VarName::MangledVariable(self.session.interner.intern(
                    "rt_memcpy".to_string())),
                      generation: None }),
            args: vec!(new_result_var,
                       new_src_var,
                       len_var)
        };
        ops.push(self.add_id(op));
        ops
    }

    pub fn binop_helper<'c>(&mut self, op: &'c ast::BinOp,
                            var1: Var, mut var2: Var,
                            e1ty: &'c Ty, e2ty: &'c Ty,
                            insts1: Vec<Op>, insts2: Vec<Op>,
                            dest_ty: &'c Ty) -> (Vec<Op>, Var) {
        let mut insts = vec!();
        insts.extend(insts1.into_iter());

        match op.val {
            AndAlsoOp |
            OrElseOp => {
                let is_and = op.val == AndAlsoOp;
                let end_label = self.gen_label();
                insts.push(self.add_id(OpNode::CondGoto {
                    negated: is_and, // cond is negated for ands, not for ors.
                    cond: Variable(var1),
                    label_idx: end_label,
                    vars: BTreeSet::new()
                }));
                insts.extend(insts2.into_iter());
                insts.push(self.add_id(OpNode::UnOp {
                    target: var1,
                    op: Identity,
                    operand: Variable(var2)
                }));
                insts.push(self.add_id(OpNode::Goto { label_idx: end_label,
                                                      vars: BTreeSet::new() }));
                insts.push(self.add_id(OpNode::Label { label_idx: end_label,
                                                       vars: BTreeSet::new() }));
                (insts, var1)
            },
            _ => {
                let new_res = self.gen_temp();
                insts.extend(insts2.into_iter());
                if (op.val == PlusOp || op.val == MinusOp) &&
                    !e2ty.is_ptr() {
                        // For pointer arithmetic, we want to scale
                        // by the size of the type being pointed to.
                        if let PtrTy(ref new_ty) = *e1ty {
                            let size = max(size_of_ty(self.session,
                                                      self.typemap,
                                                      &new_ty.val),
                                           1);
                            let total_size = packed_size(&[size]);
                            let new_var2 = self.gen_temp();
                            let signed = e2ty.is_signed();
                            let intkind = if signed { SignedInt(Width32) } else { UnsignedInt(Width32) };
                            insts.push(
                                self.add_id(OpNode::BinOp {
                                    target: new_var2,
                                    op: TimesOp,
                                    lhs: Variable(var2),
                                    rhs: Constant(NumLit(total_size,
                                                         intkind)),
                                    signed: signed
                                }));
                            var2 = new_var2;
                        }
                    }

                if !(e1ty.is_generic() || e2ty.is_generic() || e1ty.is_ptr() || e1ty.is_ptr()) {
                    assert_eq!(e1ty.is_signed(), e2ty.is_signed());
                }
                let signed = if e1ty.is_generic() || e1ty.is_ptr() {
                    e2ty.is_signed()
                } else {
                    e1ty.is_signed()
                };
                insts.push(
                    self.add_id(OpNode::BinOp {
                        target: new_res,
                        op: op.val,
                        lhs: Variable(var1),
                        rhs: Variable(var2),
                        signed: signed
                    }));
                let (new_ops, new_var) = self.contract(new_res,
                                                       dest_ty);
                insts.extend(new_ops.into_iter());

                // The only thing we need to deal with now is pointer
                // differences, where we need to divide by the size
                // of the type.
                if op.val == MinusOp && e1ty.is_ptr() && e2ty.is_ptr() {
                    match *e1ty {
                        PtrTy(ref new_ty) => {
                            let size = max(size_of_ty(self.session,
                                                      self.typemap,
                                                      &new_ty.val),
                                           1);
                            let total_size = packed_size(&[size]);
                            let new_result = self.gen_temp();
                            insts.push(
                                self.add_id(OpNode::BinOp {
                                    target: new_result,
                                    op: DivideOp,
                                    lhs: Variable(new_var.unwrap()),
                                    rhs: Constant(NumLit(total_size,
                                                         UnsignedInt(
                                                             Width32))),
                                    signed: false
                                }));
                            (insts, new_result)
                        },
                        _ => panic!(
                            "I was so sure this was a pointer..."),
                    }
                } else {
                    (insts, new_var.unwrap())
                }
            }
        }
    }

    pub fn convert_expr(&mut self, expr: &Expr) -> (Vec<Op>, Option<Var>) {
        match expr.val {
            LitExpr(ref lit) => {
                let res_var = self.gen_temp();
                let new_lit = match lit.val {
                    // A NULL is just a 0.
                    NullLit => NumLit(0,
                                      UnsignedInt(Width32)),
                    _ => adjust_constant(&lit.val),
                };

                let insts = vec!(
                    self.add_id(OpNode::UnOp {
                        target: res_var,
                        op: Identity,
                        operand: Constant(new_lit)
                    })
                );
                (insts, Some(res_var))
            }
            BinOpExpr(ref op, ref e1, ref e2) => {
                let dest_ty = &self.lookup_ty(expr.id).clone();

                let mut e1ty = self.lookup_ty(e1.id).clone();
                let mut e2ty = self.lookup_ty(e2.id).clone();
                let (new_e1, new_e2) =
                    if op.val == PlusOp && e2ty.is_ptr() {
                        // We'll always put the pointer first.
                        // Because addition is commutative!
                        assert!(!e1ty.is_ptr(),
                                concat!("Can't add two pointers. ",
                                        "Typechecker should have caught this."));
                        swap(&mut e1ty, &mut e2ty);

                        (e2, e1)
                    } else {
                        (e1, e2)
                    };

                if op.val == MinusOp {
                    assert!(!e2ty.is_ptr() || e1ty.is_ptr(),
                            concat!("Second argument to sub can't be pointer",
                                    " unless first is. Typechecker should have",
                                    " caught this."));
                }

                let (insts1, var1) = self.convert_expr(&**new_e1);
                let (insts2, var2) = self.convert_expr(&**new_e2);
                let var1 = var1.expect(
                    "Binop argument must have a non-unit value");
                let var2 = var2.expect(
                    "Binop argument must have a non-unit value");

                let (insts, var) = self.binop_helper(
                    op, var1, var2, &e1ty, &e2ty, insts1, insts2, dest_ty);
                (insts, Some(var))
            },
            PathExpr(ref path) => {
                let defid = self.session.resolver.def_from_path(path);
                // We do this to avoid borrowing self.
                let def = {
                    let d = self.session.defmap.find(&defid).expect(
                        &format!("Unable to find defid {}", defid)[..]);
                    (*d).clone()
                };
                match def {
                    // Handle the case that this is an enum variant,
                    // not a variable.
                    VariantDef(_, ref parent_id, _) => {
                        let idx_var = self.gen_temp();
                        let base_var = self.gen_temp();

                        let is_ref = ty_is_reference(self.session, self.lookup_ty(expr.id));
                        let index = self.variant_index(&defid, parent_id);
                        let insts = if is_ref {
                            // Note: here to avoid double borrowing self.
                            let alloca_op = OpNode::Alloca { var: base_var,
                                                             size: size_of_def(self.session,
                                                                               self.typemap,
                                                                               parent_id) };
                            vec!(
                                self.add_id(alloca_op),
                                self.add_id(OpNode::UnOp {
                                    target: idx_var,
                                    op: Identity,
                                    operand: Constant(
                                        NumLit(index,
                                               UnsignedInt(Width32)))
                                }),
                                self.add_id(OpNode::Store {
                                    addr: base_var, value: idx_var, width: Width32 })
                            )
                        } else {
                            vec!(self.add_id(OpNode::UnOp {
                                target: base_var,
                                op: Identity,
                                operand: Constant(
                                    NumLit(index,
                                           UnsignedInt(Width32)))
                            }))
                        };

                        return (insts, Some(base_var))
                    },
                    ConstDef(_) => {
                        let constval = self.typemap.consts.get(
                            &defid).expect("No folded constant found").clone()
                            .ok().unwrap();
                        let cast_constval = static_cast(&constval, self.lookup_ty(expr.id))
                            .expect("Invalid cast of constant value");
                        let ret_var = self.gen_temp();
                        return (vec!(self.add_id(OpNode::UnOp {
                            target: ret_var,
                            op: Identity,
                            operand: Constant(cast_constval)
                        })),
                                Some(ret_var));
                    },
                    _ => {},
                }
                (vec!(), Some(
                    Var { name: VarName::NamedVariable(self.mangled_path(path),
                                                       self.session.resolver.def_from_path(path)),
                          generation: None }))
            },
            AssignExpr(ref op, ref e1, ref e2) => {
                let mut res = vec!();

                // The LHS might be wrapped in a GroupExpr.
                // Unwrap it.
                let unwrapped = self.unwrap_group(&**e1);

                let rhs_ty = self.lookup_ty(e2.id).clone();

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
                     finalize): (Var, Vec<OpNode>, Var, Width,
                                 Box<dyn Fn(Var, Var, Width) -> OpNode>) = match unwrapped.val {
                    PathExpr(ref path) => {
                        let defid = self.session.resolver.def_from_path(path);
                        let lhs_var = Var {
                            name: VarName::NamedVariable(self.mangled_path(path), defid),
                            generation: None
                        };
                        (lhs_var,
                         vec!(),
                         lhs_var,
                         AnyWidth,
                         Box::new(|lv, v, _| OpNode::UnOp { target: lv, op: Identity, operand: Variable(v) }))
                    },
                    UnOpExpr(ref lhs_op, ref e) => {
                        let ty = match *self.lookup_ty(e.id) {
                            PtrTy(ref inner_ty) => &inner_ty.val,
                            _ => panic!(concat!("Expecting a deref, and can ",
                                               "only deref a pointer type")),
                        }.clone();
                        let width = ty_width(&ty);
                        let (insts, var) = self.convert_expr(&**e);
                        let var = var.expect(
                            "Argument to unary op must have non-unit value");
                        res.extend(insts.into_iter());
                        let res_var = self.gen_temp();
                        match lhs_op.val {
                            Deref => {
                                (res_var,
                                 vec!(OpNode::Load {
                                     target: res_var,
                                     addr: var,
                                     width: width
                                 }),
                                 var,
                                 width,
                                 Box::new(|lv, v, w| OpNode::Store {
                                     addr: lv, value: v, width: w }))
                            },
                            _ => panic!(),
                        }
                    },
                    ArrowExpr(ref e, ref name) |
                    DotExpr(ref e, ref name) => {
                        let (insts, added_addr_var, ty) =
                            self.struct_helper(&**e, name);
                        let width = ty_width(&ty);
                        let is_ref = ty_is_reference(self.session, &ty);

                        res.extend(insts.into_iter());

                        let binop_var = self.gen_temp();
                        (binop_var,
                         if is_ref {
                             vec!(OpNode::UnOp {
                                 target: binop_var,
                                 op: Identity,
                                 operand: Variable(added_addr_var)
                             })
                         } else {
                             vec!(OpNode::Load {
                                 target: binop_var,
                                 addr: added_addr_var,
                                 width: width
                             })
                         },
                         added_addr_var,
                         width,
                         Box::new(|lv, v, w| OpNode::Store { addr: lv, value: v, width: w }))
                    },
                    IndexExpr(ref arr, ref idx) => {
                        let ty = (*self.lookup_ty(e1.id)).clone();

                        let (ops, ptr_var, width, is_ref) =
                            self.array_helper(&**arr, &**idx, &ty);
                        res.extend(ops.into_iter());

                        let binop_var = self.gen_temp();
                        (binop_var,
                         if is_ref {
                             vec!(OpNode::UnOp {
                                 target: binop_var,
                                 op: Identity,
                                 operand: Variable(ptr_var)
                             })
                         } else {
                             vec!(OpNode::Load {
                                 target: binop_var,
                                 addr: ptr_var,
                                 width: width
                             })
                         },
                         ptr_var,
                         width,
                         Box::new(|lv, v, w| OpNode::Store { addr: lv, value: v, width: w }))
                    }
                    _ => panic!("Got {}", e1.val),
                };
                let final_var =
                    match *op {
                        Some(ref inner_op) => {
                            // We actually have a binop!
                            // Start by including the instructions to get
                            // the value we're adding to.
                            // So, in 'a+=b', this pushes the instructions
                            // necessary to give us the value of 'a'.
                            let e1ty = (*self.lookup_ty(e1.id)).clone();
                            let e2ty = (*self.lookup_ty(e2.id)).clone();
                            if !(e1ty.is_generic() || e2ty.is_generic()) {
                                assert_eq!(e1ty.is_signed(), e2ty.is_signed());
                            }

                            let (res2, var2) = self.convert_expr(&**e2);
                            let var2 = var2.expect(
                                "RHS of assignment must have a non-unit value");

                            let dest_ty = &self.lookup_ty(expr.id).clone();

                            let binop_insts_with_ids = binop_insts.into_iter()
                                .map(|x| self.add_id(x)).collect();

                            let (insts, var) = self.binop_helper(
                                inner_op,
                                binop_var, var2,
                                &e1ty, &e2ty,
                                binop_insts_with_ids,
                                res2, dest_ty);
                            res.extend(insts.into_iter());
                            var
                        },
                        // No binop. We can just assign the result of
                        // the rhs directly.
                        None => {
                            let (res2, var2) = self.convert_expr(&**e2);
                            res.extend(res2.into_iter());
                            var2.expect(
                                "RHS of assignment must have a non-unit value")
                        }
                    };
                if ty_is_reference(self.session, &rhs_ty) {
                    res.extend(
                        self.gen_copy(&lhs_var, &final_var, &rhs_ty).into_iter());
                } else {
                    // This generates a redundant store in some cases, but
                    // the optimizer will eliminate them.
                    res.push(self.add_id(finalize(lhs_var, final_var, width)));
                }


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
                insts.push(self.add_id(OpNode::CondGoto {
                    negated: false,
                    cond: Variable(if_var),
                    label_idx: b1_label,
                    vars: BTreeSet::new()
                }));
                insts.extend(b2_insts.into_iter());
                if let Some(b2_var) = b2_var {
                    insts.push(self.add_id(OpNode::UnOp {
                        target: end_var,
                        op: Identity,
                        operand: Variable(b2_var)
                    }));
                }
                insts.push(self.add_id(OpNode::Goto { label_idx: end_label, vars: BTreeSet::new() }));
                insts.push(self.add_id(OpNode::Label { label_idx: b1_label, vars: BTreeSet::new() }));
                insts.extend(b1_insts.into_iter());
                if let Some(b1_var) = b1_var {
                    insts.push(self.add_id(OpNode::UnOp {
                        target: end_var,
                        op: Identity,
                        operand: Variable(b1_var)
                    }));
                }
                insts.push(self.add_id(OpNode::Goto { label_idx: end_label, vars: BTreeSet::new() }));
                insts.push(self.add_id(OpNode::Label { label_idx: end_label, vars: BTreeSet::new() }));
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
                                  continue_label,
                                  None)
            },
            DoWhileExpr(ref e, ref b) => {
                let break_label = self.gen_label();
                let continue_label = self.gen_label();
                let middle_label = self.gen_label();
                self.break_labels.push(break_label);
                self.continue_labels.push(continue_label);
                let (block_insts, _) = self.convert_block(&**b);
                self.break_labels.pop();
                self.continue_labels.pop();
                let (loop_insts, var) =
                    self.while_helper(&**e,
                                      block_insts,
                                      None,
                                      break_label,
                                      continue_label,
                                      Some(middle_label));
                let mut insts = vec!(self.add_id(OpNode::Goto { label_idx: middle_label,
                                                                vars: BTreeSet::new() }));
                insts.extend(loop_insts.into_iter());
                (insts, var)
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
                                                          continue_label,
                                                          None);
                init_insts.extend(loop_insts.into_iter());
                (init_insts, var)
            }
            GroupExpr(ref e) => self.convert_expr(&**e),
            CallExpr(ref f, ref args) => {
                // We need to deal with actual function calls, as well as
                // enum constructors.
                if let PathExpr(ref path) = f.val {
                    // TODO: split a bunch of this off into a helper function.
                    let defid = self.session.resolver.def_from_path(path);
                    // We do this to avoid borrowing self.
                    let def = {
                        let d = self.session.defmap.find(&defid).expect(
                            &format!("Cannot find defid {}", defid)[..]);
                        (*d).clone()
                    };
                    if let VariantDef(_, ref parent_id, ref types) = def {
                        let idx_var = self.gen_temp();
                        let base_var = self.gen_temp();

                        let index = self.variant_index(&defid,
                                                       parent_id);
                        // Note: this is here to avoid a double borrow of self.
                        let alloca_inst = OpNode::Alloca { var: base_var,
                                                           size: size_of_def(self.session,
                                                                             self.typemap,
                                                                             parent_id) };
                        let mut insts = vec!(
                            self.add_id(alloca_inst),
                            self.add_id(OpNode::UnOp {
                                target: idx_var,
                                op: Identity,
                                operand: Constant(
                                    NumLit(index,
                                           UnsignedInt(Width32)))
                            }),
                            self.add_id(OpNode::Store { addr: base_var,
                                                        value: idx_var,
                                                        width: Width32 })
                        );

                        let (ops, vars, widths) = self.variant_helper(
                            types, &base_var);

                        insts.extend(ops.into_iter());

                        for i in 0 .. vars.len() {
                            let var = vars[i];
                            let width = &widths[i];
                            let (expr_insts, expr_var) =
                                self.convert_expr(&args[i]);
                            let expr_var = expr_var.unwrap();
                            insts.extend(expr_insts.into_iter());
                            insts.push(self.add_id(OpNode::Store {
                                addr: var,
                                value: expr_var,
                                width: *width
                            }));
                        }

                        return (insts, Some(base_var))
                    }
                }

                // It's an actual function!
                let mut ops = vec!();
                let mut vars = vec!();
                for arg in args.iter() {
                    let (new_ops, new_var) = self.convert_expr(arg);
                    ops.extend(new_ops.into_iter());
                    vars.push(new_var.expect("Passing a unit to a function"));
                }
                let (new_ops, new_var) = self.convert_expr(&**f);
                let new_var = new_var.expect(
                    "Function pointer had no non-unit value");
                ops.extend(new_ops.into_iter());
                let mut result_var = self.gen_temp();

                // We add in a bunch of dummy assignments, so that we can
                // be sure that registers are assigned correctly. Many of
                // these will be optimized away later. In the case of types
                // that are stored as references, we perform copies.
                let new_vars: Vec<Var> = (0..vars.len()).map(
                    |_| self.gen_temp()).collect();
                let mut move_ops = vec!();
                for (idx, var) in vars.iter().enumerate() {
                    let var_ty = self.lookup_ty(args[idx].id).clone();
                    if ty_is_reference(self.session, &var_ty) {
                        // The type is a reference type, and so lhs_var and
                        // final_var are pointers. Memcpy time!
                        let len = size_of_ty(self.session,
                                             self.typemap,
                                             &var_ty);

                        ops.push(self.add_id(OpNode::Alloca { var: new_vars[idx], size: len }));
                        ops.extend(self.gen_copy(&new_vars[idx],
                                                 var,
                                                 &var_ty).into_iter());
                        move_ops.push(self.add_id(OpNode::UnOp {
                            target: new_vars[idx],
                            op: Identity,
                            operand: Variable(new_vars[idx])
                        }));
                    } else {
                        move_ops.push(self.add_id(OpNode::UnOp {
                            target: new_vars[idx],
                            op: Identity,
                            operand: Variable(*var)
                        }));
                    }
                }
                ops.extend(move_ops.into_iter());
                let this_ty = self.lookup_ty(expr.id).clone();
                match this_ty {
                    UnitTy => {
                        ops.push(self.add_id(OpNode::Call {
                            target: None,
                            func: Variable(new_var),
                            args: new_vars.into_iter().collect()
                        }));
                        (ops, None)
                    },
                    _ => {
                        ops.push(self.add_id(OpNode::Call {
                            target: Some(result_var),
                            func: Variable(new_var),
                            args: new_vars.into_iter().collect()
                        }));
                        // We add one more dummy assignment, for the result, or a
                        // copy in the case of something in memory.
                        ops.push(self.add_id(OpNode::UnOp {
                            target: result_var,
                            op: Identity,
                            operand: Variable(result_var)
                        }));

                        if ty_is_reference(self.session, &this_ty) {
                            let len = size_of_ty(self.session,
                                                 self.typemap,
                                                 &this_ty);
                            let new_result_var = self.gen_temp();
                            ops.push(self.add_id(OpNode::Alloca {
                                var: new_result_var, size: len
                            }));
                            ops.extend(self.gen_copy(&new_result_var,
                                                     &result_var,
                                                     &this_ty).into_iter());
                            result_var = new_result_var;
                        }

                        (ops, Some(result_var))
                    }
                }
            },
            DotExpr(ref e, ref name) |
            ArrowExpr(ref e, ref name) => {
                let (mut ops,
                     added_addr_var,
                     ty) = self.struct_helper(&**e, name);
                let width = ty_width(&ty);
                let is_ref = ty_is_reference(self.session, &ty);

                let res_var = self.gen_temp();
                let final_var = if is_ref {
                    ops.push(self.add_id(OpNode::UnOp {
                        target: res_var,
                        op: Identity,
                        operand: Variable(added_addr_var)
                    }));
                    Some(res_var)
                } else {
                    ops.push(self.add_id(OpNode::Load {
                        target: res_var,
                        addr: added_addr_var,
                        width: width
                    }));

                    let dest_ty = &self.lookup_ty(expr.id).clone();
                    let (new_ops, new_var) = self.contract(res_var, dest_ty);
                    ops.extend(new_ops.into_iter());

                    new_var
                };
                (ops, final_var)
            },
            CastExpr(ref e, ref t) => {
                let (mut ops, res) = self.convert_expr(&**e);
                let dest_ty = &self.lookup_ty(t.id).clone();

                let res = res.expect("Casting a unit value is not allowed.");

                let (new_ops, new_var) = self.contract(res, dest_ty);
                ops.extend(new_ops.into_iter());
                (ops, new_var)
            },
            UnitExpr => (vec!(), None),
            SizeofExpr(ref t) => {
                let v = self.gen_temp();
                let ty_size = size_of_ty(self.session,
                                         self.typemap,
                                         self.lookup_ty(t.id));

                let op = OpNode::UnOp {
                    target: v,
                    op: Identity,
                    operand: Constant(NumLit(ty_size,
                                             UnsignedInt(Width32)))
                };
                (vec!(self.add_id(op)),
                 Some(v))
            },
            UnOpExpr(ref op, ref e) => {
                let ty = (*self.lookup_ty(e.id)).clone();
                // AddrOf needs to be special cased when it's applied to
                // certain kinds of expressions.
                if op.val == AddrOf {
                    let unwrapped = self.unwrap_group(&**e);
                    match unwrapped.val {
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
                }

                let (mut insts, v) = self.convert_expr(&**e);
                let v = v.expect(
                    "Argument to unary op must have non-unit value");
                let res_v = self.gen_temp();
                let actual_op = match op.val {
                    AddrOf =>
                        if ty_is_reference(self.session, &ty) {
                            // These types are stored by reference interally,
                            // so taking the address once is a no-op in the IR.
                            Identity
                        } else {
                            op.val
                        },
                    Deref => {
                        match ty {
                            PtrTy(ref inner) =>
                                if ty_is_reference(self.session, &inner.val) {
                                    Identity
                                } else {
                                    // This case is a bit different, because
                                    // we emit a load instead of a UnOp.
                                    insts.push(self.add_id(OpNode::Load {
                                        target: res_v,
                                        addr: v,
                                        width: ty_width(&inner.val)
                                    }));

                                    let dest_ty = &self.lookup_ty(expr.id).clone();
                                    let (new_ops, new_var) = self.contract(
                                        res_v,
                                        dest_ty);
                                    insts.extend(new_ops.into_iter());

                                    return (insts, new_var);
                                },
                            _ => op.val,
                        }
                    }
                    _ => op.val,
                };
                insts.push(self.add_id(OpNode::UnOp {
                    target: res_v,
                    op: actual_op,
                    operand: Variable(v)
                }));
                let dest_ty = &self.lookup_ty(expr.id).clone();
                let (new_ops, new_var) = self.contract(res_v, dest_ty);
                insts.extend(new_ops.into_iter());
                (insts, new_var)
            },
            ReturnExpr(ref e) => {
                let (mut insts, v) = self.convert_expr(&**e);
                let v = v.map(Variable);
                insts.push(self.add_id(OpNode::Return { retval: v }));
                (insts, None)
            }
            TupleExpr(..) => unimplemented!(),
            ArrayExpr(ref elems) => {
                let ty = &self.lookup_ty(expr.id).clone();
                let (inner_len, nelems, inner_ty) = match *ty {
                    ArrayTy(ref inner_ty, nelems) => {
                        (packed_size(&[size_of_ty(self.session,
                                                  self.typemap,
                                                  &inner_ty.val)]),
                         nelems.expect("Array needs a size"),
                         inner_ty.val.clone())
                    },
                    _ => panic!("Array constructor has non-array type"),
                };
                let is_reference = ty_is_reference(self.session, &inner_ty);
                let total_size = size_of_ty(self.session,
                                            self.typemap,
                                            ty);
                assert_eq!(elems.len() as usize, nelems as usize);
                assert_eq!(total_size, inner_len * nelems);
                let result_var = self.gen_temp();
                let mut ops = vec!(self.add_id(OpNode::Alloca { var: result_var,
                                                                size: total_size }));
                for (idx, elem) in elems.iter().enumerate() {
                    let offset_var = self.gen_temp();
                    let offs = idx as u64 * inner_len;

                    let (new_ops, expr_result) = self.convert_expr(elem);
                    let expr_result = expr_result.expect(
                        "Expression in array constructor can't be unit.");
                    ops.extend(new_ops.into_iter());

                    ops.push(self.add_id(OpNode::BinOp {
                        target: offset_var,
                        op: PlusOp,
                        lhs: Variable(result_var),
                        rhs: Constant(NumLit(offs, UnsignedInt(Width32))),
                        signed: false
                    }));
                    if is_reference {
                        self.gen_copy(&offset_var, &expr_result, &inner_ty);
                    } else {
                        let width = ty_width(&inner_ty);
                        ops.push(self.add_id(OpNode::Store {
                            addr: offset_var,
                            value: expr_result,
                            width: width
                        }));
                    }
                }
                (ops, Some(result_var))
            }
            StructExpr(ref p, ref fields) => {
                // TODO: make this more efficient.
                let defid = self.session.resolver.def_from_path(p);

                let base_var = self.gen_temp();
                let op = OpNode::Alloca { var: base_var,
                                          size: size_of_def(
                                              self.session,
                                              self.typemap,
                                              &defid) };
                let mut ops = vec!(self.add_id(op));

                for &(ref name, ref expr) in fields.iter() {
                    let offset_var = self.gen_temp();
                    let (expr_insts, expr_var) = self.convert_expr(expr);
                    ops.extend(expr_insts.into_iter());

                    let offs = offset_of_struct_field(
                        self.session,
                        self.typemap,
                        &defid,
                        name);

                    let ty = &{
                        let def = self.session.defmap.find(&defid).expect(
                            &format!("Cannot find defid {}", defid)[..]);
                        match *def {
                            StructDef(_, ref fields, _) => {
                                let &(_, ref t) =
                                    fields.iter()
                                    .find(|&&(a, _)| a == *name)
                                    .expect("No struct field with given name");
                                self.lookup_ty(t.id).clone()
                            },
                            _ => panic!(),
                        }
                    };
                    let width = ty_width(ty);

                    ops.push(self.add_id(OpNode::BinOp {
                        target: offset_var,
                        op: PlusOp,
                        lhs: Variable(base_var),
                        rhs: Constant(NumLit(offs, UnsignedInt(Width32))),
                        signed: false
                    }));

                    if ty_is_reference(self.session, ty) {
                        ops.extend(
                            self.gen_copy(&offset_var,
                                          &expr_var.unwrap(),
                                          ty).into_iter());
                    } else {
                        ops.push(self.add_id(OpNode::Store {
                            addr: offset_var,
                            value: expr_var.expect(
                                "Expression of struct type must have non-unit value"),
                            width: width
                        }));
                    }
                }

                (ops, Some(base_var))
            }
            IndexExpr(ref arr, ref idx) => {
                let ty = (*self.lookup_ty(expr.id)).clone();

                let (mut ops, ptr_var, width, is_ref) =
                    self.array_helper(&**arr, &**idx, &ty);

                if is_ref {
                    (ops, Some(ptr_var))
                } else {
                    let result_var = self.gen_temp();
                    ops.push(self.add_id(OpNode::Load {
                        target: result_var,
                        addr: ptr_var,
                        width: width
                    }));

                    let dest_ty = &self.lookup_ty(expr.id).clone();
                    let (new_ops, new_var) = self.contract(result_var, dest_ty);
                    ops.extend(new_ops.into_iter());

                    (ops, new_var)
                }
            }
            BreakExpr => {
                let op = OpNode::Goto {
                    label_idx: *self.break_labels.last().expect(
                        "Break with no label to break to"),
                    vars: BTreeSet::new()
                };
                (vec!(self.add_id(op)),
                 None)
            }
            ContinueExpr => {
                let op = OpNode::Goto {
                    label_idx: *self.continue_labels.last().expect(
                        "Continue with no label to continue to"),
                    vars: BTreeSet::new()
                };
                (vec!(self.add_id(op)),
                 None)
            }
            MatchExpr(ref e, ref arms) => {
                let (mut ops, base_var) = self.convert_expr(&**e);
                let base_var = base_var.expect(
                    "Match expression must have non-unit value");
                let variant_var = self.gen_temp();
                let end_label = self.gen_label();
                let mut result_var = None;

                if ty_is_reference(self.session, self.lookup_ty(e.id)) {
                    ops.push(self.add_id(OpNode::Load { target: variant_var,
                                                        addr: base_var,
                                                        width: Width32 }));
                } else {
                    // If this happens, the enum is c-like and is not by reference.
                    ops.push(self.add_id(OpNode::UnOp {
                        target:variant_var,
                        op: Identity,
                        operand: Variable(base_var)
                    }));
                }

                // These are the labels for each pattern. The are off by one:
                // we never need to jump to the beginning of the first variant.
                let mut begin_labels: Vec<usize> = (0..arms.len() - 1).map(
                    |_| self.gen_label()).collect();
                // We do, however, need to jump to to the end of the last, and
                // it's convenient to put the ending label at the end of this
                // list.
                begin_labels.push(end_label);

                for (pos, arm) in arms.iter().enumerate() {
                    // arm.pat, arm.body
                    let path_pats = match arm.pat.val {
                        VariantPat(ref path, ref pats) => Some((path, pats)),
                        DiscardPat(_) => None,
                        _ => { print!("{:?}\n", arm.pat.val); panic!() },
                    };

                    match path_pats {
                        // We only need to emit the code for the condition if there's a condition at all.
                        // If it's just being discarded, we can skip over all that!
                        Some((path, pats)) => {
                            let patid = self.session.resolver.def_from_path(path);
                            let def =
                                (*self.session.defmap.find(&patid).expect(
                                    &format!("Cannot find defid {}", patid)[..]
                                        )).clone();
                            let (parent_id, types) = match def {
                                VariantDef(_, ref parent_id, ref types) =>
                                    (parent_id, types),
                                _ => panic!(),
                            };

                            let (variant_ops, vars, widths) =
                                self.variant_helper(types, &base_var);
                            let index = self.variant_index(&patid, parent_id);

                            let compare_var = self.gen_temp();
                            // Check if the variant is the right one.
                            // With one exception: if this is the last arm, we can
                            // assume it matches, because at least one arm is
                            // *required* to match.
                            if pos != arms.len() - 1 {
                                ops.push(self.add_id(OpNode::BinOp {
                                    target: compare_var,
                                    op: EqualsOp,
                                    lhs: Variable(variant_var),
                                    rhs: Constant(NumLit(index,
                                                         UnsignedInt(Width32))),
                                    signed: false
                                }));
                                // If not, jump to the next one.
                                ops.push(self.add_id(OpNode::CondGoto {
                                    negated: true,
                                    cond: Variable(compare_var),
                                    label_idx: begin_labels[pos],
                                    vars: BTreeSet::new()
                                }));
                            }

                            // It is! Generate the code for this particular variant.
                            ops.extend(variant_ops.into_iter());
                            // Assign all the variables.
                            for (((var, pat), this_type), width) in
                                vars.iter().zip(pats.iter()).zip(types.iter())
                                .zip(widths.iter())
                            {
                                let this_ty_is_reference = ty_is_reference(self.session,
                                                                           self.lookup_ty(this_type.id));
                                match pat.val {
                                    IdentPat(ref ident, _) => {
                                        // TODO: a move or a load, depending.
                                        let this_var = Var {
                                            name: VarName::NamedVariable(ident.val.name, ident.id),
                                            generation: None };
                                        if this_ty_is_reference {
                                            ops.push(
                                                self.add_id(OpNode::UnOp {
                                                    target: this_var,
                                                    op: Identity,
                                                    operand: Variable(*var)
                                                }));
                                        } else {
                                            ops.push(
                                                self.add_id(OpNode::Load {
                                                    target: this_var,
                                                    addr: *var,
                                                    width: *width
                                                }));
                                        }
                                    },
                                    _ => panic!("Only ident patterns are supported right now.")
                                }
                            }
                        },
                        //TODO: We really should check in an earlier stage of the compiler that a DiscardPat can
                        // only appear as the last pattern, but I don't think we do right now.
                        None => { assert!(pos == arms.len() - 1); },
                    }
                    // Emit the body of the arm.
                    let (arm_insts, arm_var) = self.convert_expr(&arm.body);
                    ops.extend(arm_insts.into_iter());

                    // Assign the result, if necessary.
                    if result_var.is_none() {
                        result_var = arm_var;
                    } else {
                        ops.push(self.add_id(OpNode::UnOp {
                            target: result_var.unwrap(),
                            op: Identity,
                            operand: Variable(arm_var.unwrap())
                        }));
                    }

                    // And skip to the end!
                    ops.push(self.add_id(OpNode::Goto {
                        label_idx: end_label, vars: BTreeSet::new()
                    }));
                    // And finally, the label that goes before the next arm.
                    ops.push(self.add_id(OpNode::Label {
                        label_idx: begin_labels[pos], vars: BTreeSet::new() }));
                }

                (ops, result_var)
            }
            AsmExpr(ref x, ref l) => {
                (vec!(self.add_id(OpNode::AsmOp { insts: x.clone(), labels: l.clone() })), None)
            }
            MacroExpr(..) => panic!("ICE: macros should have been expanded by now"),
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
        let id = match *self.lookup_ty(e.id) {
            StructTy(id, _) => id,
            PtrTy(ref p) => match p.val {
                StructTy(id, _) => id,
                _ => panic!("ICE: struct pointer doesn't have struct pointer type. Typechecker should have caught this.")
            },
            _ => panic!("ICE: struct doesn't have struct type. Typechecker should have caught this.")
        };
        let offs = offset_of_struct_field(
            self.session,
            self.typemap,
            &id,
            name);

        let ty = {
            let def = self.session.defmap.find(&id).expect(
                &format!("Cannot find defid {}", id)[..]);

            match *def {
                StructDef(_, ref fields, _) => {
                    let &(_, ref t) =
                        fields.iter()
                        .find(|&&(a, _)| a == *name)
                        .expect(&format!("Cannot find name {}", name)[..]
                                );
                    self.lookup_ty(t.id)
                },
                _ => panic!("Looking up struct field offset in a non-struct.")
            }
        }.clone();


        let added_addr_var = self.gen_temp();

        ops.push(self.add_id(OpNode::BinOp {
            target: added_addr_var,
            op: PlusOp,
            lhs: Variable(var),
            rhs: Constant(NumLit(offs, UnsignedInt(Width32))),
            signed: false
        }));

        (ops, added_addr_var, ty)
    }

    fn variant_index(&mut self, defid: &NodeId, parent_id: &NodeId) -> u64 {
        (match *self.session.defmap.find(parent_id).expect(
            &format!("Cannot find defid {}", parent_id)[..]) {
            EnumDef(_, ref variants, _) =>
                variants.iter().position(|&n| n == *defid).expect(
                    &format!("Cannot find defid {}", defid)[..]),
            _ => panic!(),
        }) as u64
    }

    // Given the list of types in this enum, and a variable
    // pointing to the start of the enum in memory, returns a list of ops,
    // and list of Vars, and a list of Widths, where (after the ops are
    // executed) the variables are pointers to each of the fields, and Width
    // is the width of a load or store to/from the corresponding field.
    fn variant_helper(&mut self,
                      types: &[Type],
                      base_var: &Var) -> (Vec<Op>, Vec<Var>, Vec<Width>) {
        let mut insts = vec!();
        let (widths, sizes): (Vec<Width>, Vec<u64>) = Iterator::unzip(types.iter()
            .map(|t| self.lookup_ty(t.id))
            .map(|ty| (ty_width(ty),
                       size_of_ty(self.session, self.typemap, ty))));

        let vars: Vec<Var> = (0..sizes.len()).map(|_| self.gen_temp()).collect();

        for i in 0 .. sizes.len() {
            let offs = ENUM_TAG_SIZE + offset_of(&sizes, i);

            let new_offs_var = &vars[i];
            insts.push(self.add_id(OpNode::BinOp {
                target: *new_offs_var,
                op: PlusOp,
                lhs: Variable(*base_var),
                rhs: Constant(NumLit(offs, UnsignedInt(Width32))),
                signed: false
            }));
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
        ops.extend(idx_ops.into_iter());
        let size = size_of_ty(self.session, self.typemap, ty);
        let total_size = packed_size(&[size]);
        let offs_var = self.gen_temp();
        let ptr_var = self.gen_temp();
        ops.push(self.add_id(OpNode::BinOp {
            target: offs_var,
            op: TimesOp,
            lhs: Variable(idx_var),
            rhs: Constant(NumLit(total_size,
                                 UnsignedInt(Width32))),
            signed: false
        }));
        ops.push(self.add_id(OpNode::BinOp {
            target: ptr_var,
            op: PlusOp,
            lhs: Variable(base_var),
            rhs: Variable(offs_var),
            signed: false
        }));

        (ops, ptr_var, ty_width(ty), ty_is_reference(self.session, ty))
    }

    fn unwrap_group<'c>(&mut self,
                        grp: &'c Expr) -> &'c Expr {
        let mut unwrapped = grp;
        while let GroupExpr(ref e) = unwrapped.val {
            unwrapped = &**e;
        }
        unwrapped
    }
}
