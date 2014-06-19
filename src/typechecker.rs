use mc::resolver::Resolver;
use mc::session::Session;
use util::{IntKind, GenericInt, SignedInt, UnsignedInt};
use util::{Width, AnyWidth, Width8, Width16, Width32};
use span::Span;

use std::collections::{SmallIntMap, TreeMap, EnumSet};
use std::collections::enum_set::CLike;

use std::fmt;

use mc::ast::*;
use mc::ast::defmap::*;
use mc::ast::visit::*;

#[deriving(Eq, PartialEq, Show, Clone)]
struct BoundsId(uint);

impl BoundsId {
    pub fn to_uint(&self) -> uint {
        let BoundsId(bid) = *self;
        bid
    }
}

#[deriving(Eq, PartialEq, Show, Clone)]
pub enum Ty_ {
    BoolTy,
    GenericIntTy,
    IntTy(Width),
    UintTy(Width),
    UnitTy,
    PtrTy(Box<Ty>),
    ArrayTy(Box<Ty>, Option<u64>),
    TupleTy(Vec<Ty>),
    FuncTy(Vec<Ty>, Box<Ty>),
    StructTy(NodeId, Vec<Ty>),
    EnumTy(NodeId, Vec<Ty>),
    BoundTy(BoundsId),
    BottomTy,
}

#[deriving(Eq, PartialEq, Show, Clone)]
pub struct Ty {
    pub ty: Ty_,
    pub id: NodeId,
}

impl Ty_ {
    fn kinds(&self) -> EnumSet<Kind> {
        let mut set = EnumSet::empty();
        match *self {
            FuncTy(..) | ArrayTy(..) => {
                set.add(EqKind); // effectively pointer equality
            }
            BoolTy => {
                set.add(EqKind);
                set.add(AndKind);
                set.add(OrKind);
            }
            PtrTy(..) => {
                set.add(EqKind);
                set.add(CmpKind);
                set.add(AddKind);
                set.add(SubKind);
            }
            GenericIntTy | IntTy(..) | UintTy(..) => {
                set.add(EqKind);
                set.add(CmpKind);
                set.add(AddKind);
                set.add(SubKind);
                set.add(MulKind);
                set.add(DivKind);
                set.add(RemKind);
                set.add(BitAndKind);
                set.add(BitOrKind);
                set.add(BitXorKind);
                set.add(ShrKind);
                set.add(ShlKind);
            }
            _ => {}
        }

        set
    }

    fn is_of_kind(&self, k: Kind) -> bool {
        self.kinds().contains_elem(k)
    }
}
impl Ty {
    fn kinds(&self) -> EnumSet<Kind> {
        self.ty.kinds()
    }
    fn is_of_kind(&self, k: Kind) -> bool {
        self.ty.is_of_kind(k)
    }
}

fn mk_ty(ty: Ty_, src: NodeId) -> Ty {
    Ty { ty: ty, id: src }
}
fn mk_i(ty: Ty_) -> Ty {
    Ty { ty: ty, id: DUMMY_NODEID }
}

#[deriving(Eq, PartialEq, Clone)]
enum Kind {
    EqKind,
    CmpKind,
    AndKind,
    OrKind,
    AddKind,
    SubKind,
    MulKind,
    DivKind,
    RemKind,
    BitAndKind,
    BitOrKind,
    BitXorKind,
    ShrKind,
    ShlKind,
}

impl CLike for Kind {
    fn to_uint(&self) -> uint {
        *self as uint
    }

    fn from_uint(u: uint) -> Kind {
        match u {
            0 => EqKind,
            1 => CmpKind,
            2 => AndKind,
            3 => OrKind,
            4 => AddKind,
            5 => SubKind,
            6 => MulKind,
            7 => DivKind,
            8 => RemKind,
            9 => BitAndKind,
            10 => BitOrKind,
            11 => BitXorKind,
            12 => ShrKind,
            13 => ShlKind,
            _ => fail!(),
        }
    }
}

impl fmt::Show for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            EqKind     => write!(f, "Eq"),
            CmpKind    => write!(f, "Cmp"),
            AndKind    => write!(f, "And"),
            OrKind     => write!(f, "Or"),
            AddKind    => write!(f, "Add"),
            SubKind    => write!(f, "Sub"),
            MulKind    => write!(f, "Mul"),
            DivKind    => write!(f, "Div"),
            RemKind    => write!(f, "Rem"),
            BitAndKind => write!(f, "BitAnd"),
            BitOrKind  => write!(f, "BitOr"),
            BitXorKind => write!(f, "BitXor"),
            ShrKind    => write!(f, "Shr"),
            ShlKind    => write!(f, "Shl"),
        }
    }
}

#[deriving(Eq, PartialEq, Clone)]
pub enum TyBounds {
    Concrete(Ty),
    Constrained(EnumSet<Kind>),
    Unconstrained,
}

impl fmt::Show for TyBounds {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Concrete(ref t) => write!(f, "{}", t),
            Constrained(ref ks) => write!(f, "<kinds {}>", ks),
            Unconstrained => write!(f, "<unconstrained>"),
        }
    }
}

pub struct Typemap {
    pub types: SmallIntMap<Ty>,
    pub bounds: SmallIntMap<TyBounds>,
}

pub struct Typechecker<'a> {
    defs: TreeMap<NodeId, BoundsId>,
    generics: Vec<TreeMap<NodeId, Ty>>,
    session: &'a Session,
    next_bounds_id: uint,
    exits: Vec<Ty>,
    typemap: Typemap,
}

fn intkind_to_ty(ik: IntKind) -> Ty_ {
    match ik {
        GenericInt     => GenericIntTy,
        SignedInt(w)   => IntTy(w),
        UnsignedInt(w) => UintTy(w),
    }
}

macro_rules! save_ty {
    ($n:expr, $t:expr) => ({ let ty = $t; self.typemap.types.insert($n.id.to_uint(), ty.clone()); ty })
}
macro_rules! save_ty_ {
    ($n:expr, $t:expr) => ({ let ty = mk_ty($t, $n.id); self.typemap.types.insert($n.id.to_uint(), ty.clone()); ty })
}

macro_rules! enumset {
    ($($k:expr),+,) => (enumset!($($k),+));
    ($($k:expr),*) => (
        {
            let mut ks = EnumSet::empty();
            $(ks.add($k);)*
            ks
        }
    );
}

fn op_to_kind_set(op: &BinOp) -> EnumSet<Kind> {
    match op.val {
        EqualsOp | NotEqualsOp => enumset!(EqKind),
        LessOp | GreaterOp     => enumset!(CmpKind),
        LessEqOp | GreaterEqOp => enumset!(CmpKind, EqKind),
        AndAlsoOp | OrElseOp   => enumset!(AndKind),
        PlusOp                 => enumset!(AddKind),
        MinusOp                => enumset!(SubKind),
        TimesOp                => enumset!(MulKind),
        DivideOp               => enumset!(DivKind),
        ModOp                  => enumset!(RemKind),
        BitAndOp               => enumset!(BitAndKind),
        BitOrOp                => enumset!(BitOrKind),
        BitXorOp               => enumset!(BitXorKind),
        LeftShiftOp            => enumset!(ShlKind),
        RightShiftOp           => enumset!(ShrKind),
    }
}

impl<'a> Typechecker<'a> {
    pub fn new(session: &'a Session) -> Typechecker<'a> {
        Typechecker {
            defs: TreeMap::new(),
            generics: vec!(),
            session: session,
            next_bounds_id: 0,
            exits: vec!(),
            typemap: Typemap {
                types: SmallIntMap::new(),
                bounds: SmallIntMap::new(),
            }
        }
    }

    // Should probably live elsewhere.
    pub fn error_fatal<T: Str>(&self, nid: NodeId, msg: T) -> ! {
        self.session.error_fatal(nid, msg);
    }
    pub fn error<T: Str>(&self, nid: NodeId, msg: T) {
        self.session.error(nid, msg);
    }

    pub fn get_typemap(self) -> Typemap {
        self.typemap
    }

    fn tps_to_tys(&mut self, nid: NodeId,
                  tps: &Vec<NodeId>, ts: &Option<Vec<Type>>, infer: bool) -> Vec<Ty> {
        match *ts {
            Some(ref ts) if ts.len() == tps.len() =>
                ts.iter().map(|t| self.type_to_ty(t)).collect(),
            None if infer =>
                tps.iter().map(|id| mk_ty(BoundTy(self.add_bounds()), *id)).collect(),
            None if tps.len() == 0 =>
                vec!(),
            _ =>
                self.error_fatal(
                    nid,
                    format!("Expected {} type parameters, but found {}",
                            tps.len(), ts.as_ref().map_or(0, |ts| ts.len()))),
        }
    }

    fn with_generics<T>(&mut self, gs: TreeMap<NodeId, Ty>, f: |&mut Typechecker<'a>| -> T) -> T {
        self.generics.push(gs);
        let ret = f(self);
        self.generics.pop();
        ret
    }

    fn new_bounds_id(&mut self) -> BoundsId {
        let bid = self.next_bounds_id;
        self.next_bounds_id += 1;
        BoundsId(bid)
    }

    fn add_bounds(&mut self) -> BoundsId {
        let bid = self.new_bounds_id();
        self.typemap.bounds.insert(bid.to_uint(), Unconstrained);
        bid
    }

    fn get_bounds(&self, bid: BoundsId) -> TyBounds {
        self.typemap.bounds.find(&bid.to_uint()).take_unwrap().clone()
    }

    fn update_bounds(&mut self, bid: BoundsId, bounds: TyBounds) {
        self.typemap.bounds.swap(bid.to_uint(), bounds);
    }

    fn add_bound_ty(&mut self, nid: NodeId) -> Ty {
        let bid = self.add_bounds();
        self.defs.insert(nid, bid);
        mk_ty(BoundTy(bid), nid)
    }

    fn get_bound_ty(&mut self, nid: NodeId) -> Ty {
        let bid = *self.defs.find(&nid).take_unwrap();
        match self.get_bounds(bid) {
            Concrete(ty) => ty,
            _ => mk_ty(BoundTy(bid), nid),
        }
    }

    fn generic_to_ty(&mut self, nid: NodeId) -> Ty {
        match self.generics.iter().rev()
                                  .filter_map(|gs| gs.find(&nid))
                                  .next() {
            Some(ty) => ty.clone(),
            None => self.error_fatal(nid, "Missing generic"),
        }
    }

    pub fn type_to_ty(&mut self, t: &Type) -> Ty {
        save_ty_!(t, match t.val {
            BoolType => BoolTy,
            UnitType => UnitTy,
            DivergingType => BottomTy,
            IntType(ik) => intkind_to_ty(ik),
            PtrType(ref t) => PtrTy(box self.type_to_ty(*t)),
            NamedType(ref path) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take_unwrap() {
                    StructDef(_, _, ref tps) => {
                        let tys = self.tps_to_tys(
                            t.id, tps, &path.val.elems.last().unwrap().val.tps, false);
                        StructTy(nid, tys)
                    }
                    EnumDef(_, _, ref tps) => {
                        let tys = self.tps_to_tys(
                            t.id, tps, &path.val.elems.last().unwrap().val.tps, false);
                        EnumTy(nid, tys)
                    }
                    GenericDef => self.generic_to_ty(nid).ty,
                    TypeDef(ref t) => self.type_to_ty(t).ty,
                    _ => self.error_fatal(t.id, "Expected type name"),
                }
            },
            FuncType(ref args, ref t) => {
               let ret_ty = self.type_to_ty(*t);
               let arg_tys = args.iter().map(|arg| {
                   self.type_to_ty(arg)
               }).collect();

               FuncTy(arg_tys, box ret_ty)
            },
            ArrayType(ref t, len) => {
                let ty = self.type_to_ty(*t);
                ArrayTy(box ty, Some(len))
            }
            TupleType(ref ts) => {
               let tys = ts.iter().map(|t| {
                   self.type_to_ty(t)
               }).collect();

               TupleTy(tys)
            },
        })
    }

    fn pat_to_ty(&mut self, pat: &Pat) -> Ty {
        save_ty!(pat, match pat.val {
            DiscardPat(ref t) => {
                match *t {
                    Some(ref t) => self.type_to_ty(t),
                    None => mk_ty(BoundTy(self.add_bounds()), pat.id),
                }
            }
            IdentPat(ref ident, ref t) => {
                match *t {
                    Some(ref t) => self.type_to_ty(t),
                    None => self.add_bound_ty(ident.id),
                }
            }
            TuplePat(ref pats) => {
                let tys = pats.iter().map(|p| self.pat_to_ty(p)).collect();
                mk_ty(TupleTy(tys), pat.id)
            }
            VariantPat(ref path, ref pats) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take_unwrap() {
                    VariantDef(_, ref enum_nid, ref args) => {
                        let tps = match *self.session.defmap.find(enum_nid).take_unwrap() {
                            EnumDef(_, _, ref tps) => tps,
                            _ => self.error_fatal(nid, "Nonsensical enum id for variant"),
                        };

                        let tp_tys = self.tps_to_tys(
                            pat.id, tps, &path.val.elems.last().unwrap().val.tps, true);

                        let mut gs = TreeMap::new();
                        for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                            gs.insert(*tp, tp_ty.clone());
                        }

                        self.with_generics(gs, |me| {
                            for (arg, pat) in args.iter().zip(pats.iter()) {
                                let arg_ty = me.type_to_ty(arg);
                                let pat_ty = me.pat_to_ty(pat);
                                me.unify(arg_ty, pat_ty);
                            }
                        });

                        mk_ty(EnumTy(*enum_nid, tp_tys), pat.id)
                    }
                    _ => unreachable!(),
                }
            }
            StructPat(ref path, ref fps) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take_unwrap() {
                    StructDef(_, ref fields, ref tps) => {
                        let tp_tys = self.tps_to_tys(
                            pat.id, tps, &path.val.elems.last().unwrap().val.tps, true);

                        let mut gs = TreeMap::new();
                        for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                            gs.insert(*tp, tp_ty.clone());
                        }

                        self.with_generics(gs, |me| {
                            for (field, fp) in fields.iter().map(
                                    |x| x.clone().val1()).zip(fps.iter()) {
                                let field_ty = me.type_to_ty(&field);
                                let fp_ty = me.pat_to_ty(&fp.pat);
                                me.unify(field_ty, fp_ty);
                            }
                        });

                        mk_ty(StructTy(nid, tp_tys), pat.id)
                    }
                    _ => unreachable!(),
                }
            }
        })
    }

    fn lit_to_ty(&mut self, lit: &Lit) -> Ty {
        save_ty_!(lit, match lit.val {
            NumLit(_, ik) => intkind_to_ty(ik),
            StringLit(..) => PtrTy(box mk_ty(UintTy(Width8), lit.id)),
            BoolLit(..) => BoolTy,
            NullLit => PtrTy(box mk_ty(BottomTy, lit.id)),
        })
    }

    fn func_def_to_ty(&mut self, arg_ids: &Vec<NodeId>, ret_t: &Type) -> Ty {
        let ret_ty = self.type_to_ty(ret_t);
        let arg_tys = arg_ids.iter().map(|arg_id| {
            match *self.session.defmap.find(arg_id).take_unwrap() {
                FuncArgDef(ref t) => self.type_to_ty(t),
                _ => self.session.bug_span(*arg_id, "Nonsensical arg id for func def"),
            }
        }).collect();
        /* XXX: this is a bad nodeid to use */
        let nid = ret_t.id;
        mk_ty(FuncTy(arg_tys, box ret_ty), nid)
    }

    fn expr_to_ty(&mut self, expr: &Expr) -> Ty {
        let id = expr.id;
        let mk = |ty| mk_ty(ty, id);


        save_ty!(expr, match expr.val {
            UnitExpr => mk(UnitTy),
            LitExpr(ref l) => self.lit_to_ty(l),
            TupleExpr(ref es) => mk(TupleTy(es.iter().map(|e| self.expr_to_ty(e)).collect())),
            GroupExpr(ref e) => self.expr_to_ty(*e),
            PathExpr(ref path) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take_unwrap() {
                    FuncDef(ref args, ref t, ref tps) => {
                        let tp_tys = self.tps_to_tys(
                            expr.id, tps, &path.val.elems.last().unwrap().val.tps, true);
                        let mut gs = TreeMap::new();
                        for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                            gs.insert(*tp, tp_ty.clone());
                        }

                        self.with_generics(gs, |me| me.func_def_to_ty(args, t))
                    }
                    FuncArgDef(ref t) => {
                        self.type_to_ty(t)
                    }
                    VariantDef(_, ref enum_nid, ref args) => {
                        let tps = match *self.session.defmap.find(enum_nid).take_unwrap() {
                            EnumDef(_, _, ref tps) => tps,
                            _ => self.error_fatal(expr.id, "Nonsensical enum id for variant"),
                        };

                        let tp_tys = self.tps_to_tys(
                            expr.id, tps, &path.val.elems.last().unwrap().val.tps, true);

                        let ctor = |tp_tys| EnumTy(*enum_nid, tp_tys);
                        if args.len() == 0 {
                            mk(ctor(tp_tys))
                        } else {
                            let mut gs = TreeMap::new();
                            for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                                gs.insert(*tp, tp_ty.clone());
                            }

                            let arg_tys = self.with_generics(gs, |me|
                                args.iter().map(|t| me.type_to_ty(t)).collect()
                            );

                            // XXX: better spans?
                            let ret_ty = mk(ctor(tp_tys));
                            mk(FuncTy(arg_tys, box ret_ty))
                        }
                    }
                    PatDef(ref t) => {
                        match *t {
                            Some(ref t) => self.type_to_ty(t),
                            None => self.get_bound_ty(nid),
                        }
                    }
                    _ => self.error_fatal(expr.id,
                                          format!("{} does not name a value", path)),
                }
            }
            StructExpr(ref path, ref flds) => {
                let nid = self.session.resolver.def_from_path(path);
                let (fields, tps) = match *self.session.defmap.find(&nid).take_unwrap() {
                    StructDef(_, ref fields, ref tps) => (fields, tps),
                    _ => self.error_fatal(expr.id,
                                          format!("{} does not name a struct", path)),
                };

                let tp_tys = self.tps_to_tys(
                    expr.id, tps, &path.val.elems.last().unwrap().val.tps, true);

                let mut gs = TreeMap::new();
                for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                    gs.insert(*tp, tp_ty.clone());
                }

                self.with_generics(gs, |me| {
                    for (field, fld) in fields.iter().map(
                            |x| x.clone().val1()).zip(flds.iter()) {
                        let field_ty = me.type_to_ty(&field);
                        let fld_ty = me.expr_to_ty(fld.ref1());
                        me.unify(field_ty, fld_ty);
                    }
                });

                mk(StructTy(nid, tp_tys))
            }
            BinOpExpr(ref op, ref l, ref r) => {
                let l_ty = self.expr_to_ty(*l);
                let r_ty = self.expr_to_ty(*r);

                self.unify_with_binop(op, l_ty, r_ty)
            }
            UnOpExpr(ref op, ref e) => {
                let ty = self.expr_to_ty(*e);
                let expr_ty = match op.val {
                    Negate => self.check_ty_bounds(ty, Constrained(enumset!(SubKind))),
                    BitNot => self.check_ty_bounds(ty, Constrained(enumset!(BitXorKind))),
                    LogNot => self.check_ty_bounds(ty, Concrete(mk_i(BoolTy))),
                    AddrOf => mk(PtrTy(box ty)),
                    Deref => match ty.ty {
                        PtrTy(p_ty) => mk(p_ty.ty),
                        _ => unreachable!(),
                    }
                };

                expr_ty
            }
            IndexExpr(ref a, ref i) => {
                let a_ty = self.expr_to_ty(*a);
                let i_ty = self.expr_to_ty(*i);
                self.check_ty_bounds(i_ty, Concrete(mk_i(UintTy(AnyWidth))));

                match a_ty.ty {
                    ArrayTy(ty, _) | PtrTy(ty) => *ty,
                    _ => unreachable!(),
                }
            }
            IfExpr(ref c, ref tb, ref fb) => {
                let c_ty = self.expr_to_ty(*c);
                self.unify(mk_i(BoolTy), c_ty);
                let tb_ty = self.block_to_ty(*tb);
                let fb_ty = self.block_to_ty(*fb);
                self.unify(tb_ty, fb_ty)
            }
            CallExpr(ref e, ref args) => {
                let arg_tys = args.iter().map(|arg| self.expr_to_ty(arg)).collect();
                let e_ty = self.expr_to_ty(*e);
                match self.unify(mk(FuncTy(arg_tys, box mk_i(BottomTy))), e_ty).ty {
                    FuncTy(_, ret_ty) => *ret_ty,
                    _ => unreachable!(),
                }
            }
            BlockExpr(ref b) => {
                self.block_to_ty(*b)
            }
            ReturnExpr(ref e) => {
                let ty = self.expr_to_ty(*e);
                self.exits.push(ty);
                mk(BottomTy)
            }
            BreakExpr => mk(BottomTy),
            ContinueExpr => mk(BottomTy),
            CastExpr(ref e, ref t) => {
                let e_ty = self.expr_to_ty(*e);
                let t_ty = self.type_to_ty(t);

                match e_ty.ty {
                    GenericIntTy | UintTy(..) | IntTy(..) | PtrTy(..) => {}
                    _ => self.error(expr.id, "Cannot cast expression of non-integral type"),
                }

                match t_ty.ty {
                    GenericIntTy | UintTy(..) | IntTy(..) | PtrTy(..) => {},
                    _ => self.error(expr.id, "Cannot cast to non-integral/pointer type"),
                }

                t_ty
            }
            AssignExpr(ref op, ref lv, ref rv) => {
                let l_ty = match lv.val {
                    PathExpr(..) | UnOpExpr(WithId { val: Deref, .. }, _) | IndexExpr(..) | DotExpr(..) | ArrowExpr(..) => {
                        self.expr_to_ty(*lv)
                    }
                    _ => self.error_fatal(lv.id, "LHS of assignment is not an lvalue"),
                };

                let r_ty = self.expr_to_ty(*rv);

                match *op {
                    Some(ref op) => self.unify_with_binop(op, l_ty, r_ty),
                    None => self.unify(l_ty, r_ty),
                }
            }
            DotExpr(ref e, ref fld) => {
                let e_ty = self.expr_to_ty(*e);
                let (nid, tp_tys) = match self.unify(mk_i(BottomTy), e_ty).ty {
                    StructTy(nid, tp_tys) => (nid, tp_tys),
                    ty => self.error_fatal(e.id,
                                           format!("Expression is not a structure, got {}", ty)),
                };

                match *self.session.defmap.find(&nid).take_unwrap() {
                    StructDef(_, ref fields, ref tps) => {
                        let mut gs = TreeMap::new();
                        for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                            gs.insert(*tp, tp_ty.clone());
                        }

                        let (_, ref field) = *fields.iter().find(
                            |&&(ref a, _)| a == fld).unwrap();
                        self.with_generics(gs, |me| me.type_to_ty(field))
                    }
                    _ => unreachable!(),
                }
            }
            ArrowExpr(ref e, ref fld) => {
                let e_ty = self.expr_to_ty(*e);
                let (nid, tp_tys) = match self.unify(mk_i(BottomTy), e_ty).ty {
                    PtrTy(box Ty {ty: StructTy(nid, tp_tys), id: _}) => (nid, tp_tys),
                    _ => self.error_fatal(e.id, "Expression is not a pointer to a structure"),
                };

                match *self.session.defmap.find(&nid).take_unwrap() {
                    StructDef(_, ref fields, ref tps) => {
                        let mut gs = TreeMap::new();
                        for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                            gs.insert(*tp, tp_ty.clone());
                        }

                        let (_, ref field) = *fields.iter().find(
                            |&&(ref a, _)| a == fld).unwrap();
                        self.with_generics(gs, |me| me.type_to_ty(field))
                    }
                    _ => unreachable!(),
                }
            }
            WhileExpr(ref e, ref b) => {
                let e_ty = self.expr_to_ty(*e);

                self.unify(mk_i(BoolTy), e_ty);

                let b_ty = self.block_to_ty(*b);
                self.unify(mk_i(UnitTy), b_ty)
            }
            ForExpr(ref init, ref cond, ref step, ref b) => {
                let _ = self.expr_to_ty(*init);

                let c_ty = self.expr_to_ty(*cond);
                self.unify(mk_i(BoolTy), c_ty);

                let _ = self.expr_to_ty(*step);

                let b_ty = self.block_to_ty(*b);
                self.unify(mk_i(UnitTy), b_ty)
            }
            MatchExpr(ref e, ref arms) => {
                let mut e_ty = self.expr_to_ty(*e);
                let mut ty = mk(BottomTy);
                for arm in arms.iter() {
                    let pat_ty = self.pat_to_ty(&arm.pat);
                    let body_ty = self.expr_to_ty(&arm.body);
                    e_ty = self.unify(e_ty, pat_ty);
                    ty = self.unify(ty, body_ty);
                }
                ty
            }
        })
    }

    fn block_to_ty(&mut self, block: &Block) -> Ty {
        for item in block.items.iter() {
            self.visit_item(item);
        }
        for stmt in block.stmts.iter() {
            self.visit_stmt(stmt);
        }
        // XXX: No node ids??
        block.expr.as_ref().map(|e| self.expr_to_ty(e)).unwrap_or(mk_i(UnitTy))
    }

    fn merge_bounds(&mut self, b1: TyBounds, b2: TyBounds) -> TyBounds {
        match (b1, b2) {
            (Unconstrained, bs) | (bs, Unconstrained) => bs,
            (Concrete(t1), Concrete(t2)) => Concrete(self.unify(t1, t2)),
            (Constrained(ks), Concrete(ty)) | (Concrete(ty), Constrained(ks)) =>
                Concrete(self.check_ty_bounds(ty, Constrained(ks))),  // do something about Concrete(BoundTy)
            (Constrained(ks1), Constrained(ks2)) => {
                Constrained(ks1.union(ks2))
            }
        }
    }

    fn check_ty_bounds(&mut self, t1: Ty, bounds: TyBounds) -> Ty {
        match bounds {
            Unconstrained => t1,
            Concrete(t2) => self.unify(t1, t2),
            Constrained(ks) => {
                match t1.ty {
                    BoundTy(bid) => {
                        let b1 = self.get_bounds(bid);
                        let bounds = self.merge_bounds(b1, bounds);
                        self.update_bounds(bid, bounds);
                        mk_ty(BoundTy(bid), t1.id)
                    }
                    _ => {
                        if !t1.kinds().contains(ks) {
                            self.error(
                                t1.id,
                                format!("Expected type with bounds {} but found type {}",
                                        bounds, t1));
                        }
                        t1
                    }
                }
            }
        }
    }

    fn unify_with_binop(&mut self, op: &BinOp, l_ty: Ty, r_ty: Ty) -> Ty {
        fn is_integral_ty(t: &Ty_) -> bool {
            match *t {
                UintTy(..) | IntTy(..) | GenericIntTy => true,
                _ => false,
            }
        };

        let l_id = l_ty.id;
        let r_id = r_ty.id;

        match (op.val, l_ty.ty, r_ty.ty) {
            // Once again we appease the borrow checker...
            (PlusOp, ref t, ref mut p@PtrTy(..)) | (PlusOp, ref mut p@PtrTy(..), ref t) if is_integral_ty(t) =>
                mk_ty(::std::mem::replace(p, UnitTy), l_ty.id),
            (MinusOp, PtrTy(..), PtrTy(..)) =>
                // XXX: this span is bogus
                mk_ty(UintTy(Width32), l_ty.id), //TODO PtrWidth
            (_, l_ty, r_ty) => {
                let kinds = op_to_kind_set(op);
                let bounds = Constrained(kinds);
                let ty = self.check_ty_bounds(mk_ty(l_ty, l_id), bounds);

                if kinds.intersects(enumset!(EqKind, CmpKind, AndKind, OrKind)) {
                    mk_ty(BoolTy, l_id)
                } else {
                    self.unify(ty, mk_ty(r_ty, r_id))
                }
            }
        }
    }

    fn unify(&mut self, ty1: Ty, ty2: Ty) -> Ty {
        let id1 = ty1.id;
        let id2 = ty2.id;
        let id = if id1 == DUMMY_NODEID { id2 } else { id1 };

        // TODO pointers and ints together
        match (ty1.ty, ty2.ty) {
            (BottomTy, t) => mk_ty(t, id2),
            (t, BottomTy) => mk_ty(t, id1),
            (BoundTy(b1), BoundTy(b2)) => {
                let bounds =
                    if b1 == b2 {
                        self.get_bounds(b1)
                    } else {
                        let bs1 = self.get_bounds(b1);
                        let bs2 = self.get_bounds(b2);
                        let bounds = self.merge_bounds(bs1, bs2);
                        self.update_bounds(b1, bounds.clone());
                        self.update_bounds(b2, bounds.clone());
                        bounds
                    };

                match bounds {
                    Concrete(ty) => ty,
                    _ => mk_ty(BoundTy(b1), id),
                }
            },
            (BoundTy(b), ty) | (ty, BoundTy(b)) => {
                let bounds = self.get_bounds(b);
                // XXX: bad span?
                let t = self.check_ty_bounds(mk_ty(ty, id), bounds);
                self.update_bounds(b, Concrete(t.clone()));
                t
            },
            // XXX: bad spans?
            (GenericIntTy, IntTy(w)) | (IntTy(w), GenericIntTy) => mk_ty(IntTy(w), id),
            (GenericIntTy, UintTy(w)) | (UintTy(w), GenericIntTy) => mk_ty(UintTy(w), id),
            (ref t@IntTy(ref w1), IntTy(ref w2)) | (ref t@UintTy(ref w1), UintTy(ref w2)) => {
                let ctor = match *t {
                    IntTy(..) => IntTy,
                    UintTy(..) => UintTy,
                    _ => unreachable!(),
                };

                let w = match (*w1, *w2) {
                    (AnyWidth, w) | (w, AnyWidth) => w,
                    (w1, w2) => {
                        if w1 == w2 {
                            w1
                        } else {
                            self.mismatch(id1, id2, &ctor(w1), &ctor(w2))
                        }
                    }
                };

                mk_ty(ctor(w), id)
            },
            (PtrTy(p1), PtrTy(p2)) =>
                mk_ty(PtrTy(box self.unify(*p1, *p2)), id),
            (ArrayTy(a1, l1), ArrayTy(a2, l2)) => {
                let l = match (l1, l2) {
                    (None, l) | (l, None) => l,
                    (Some(l1), Some(l2)) if l1 == l2 => Some(l1),
                    _ => self.mismatch(id1, id2, &ArrayTy(a1, l1), &ArrayTy(a2, l2)),
                };

                mk_ty(ArrayTy(box self.unify(*a1, *a2), l), id)
            },
            (TupleTy(ts1), TupleTy(ts2)) => {
                if ts1.len() == ts2.len() {
                    let t = TupleTy(ts1.move_iter().zip(
                                    ts2.move_iter()).map(
                                    |(t1, t2)| self.unify(t1, t2))
                                    .collect());
                    mk_ty(t, id)
                } else {
                    self.mismatch(id1, id2, &TupleTy(ts1), &TupleTy(ts2))
                }
            },
            (FuncTy(args1, t1), FuncTy(args2, t2)) => {
                if args1.len() == args2.len() {
                    let t =
                        FuncTy(args1.move_iter().zip(
                               args2.move_iter()).map(
                                   |(arg1, arg2)| self.unify(arg1, arg2))
                               .collect(), box self.unify(*t1, *t2));
                    mk_ty(t, id)
                } else {
                    self.mismatch(id1, id2, &FuncTy(args1, t1), &FuncTy(args2, t2))
                }
            },
            (StructTy(d1, ts1), StructTy(d2, ts2)) => {
                if d1 != d2 {
                    self.mismatch(id1, id2, &StructTy(d1, ts1), &StructTy(d2, ts2));
                } else {
                    if ts1.len() != ts2.len() {
                        self.error(
                            id1,
                            format!("Inconsistent number of type parameters for struct {}", d1));
                    }

                    let ts = ts1.move_iter().zip(ts2.move_iter()).map(|(t1, t2)| self.unify(t1, t2)).collect();
                    mk_ty(StructTy(d1, ts), id)
                }
            },
            (EnumTy(d1, ts1), EnumTy(d2, ts2)) => {
                if d1 != d2 {
                    self.mismatch(id1, id2, &EnumTy(d1, ts1), &EnumTy(d2, ts2));
                } else {
                    if ts1.len() != ts2.len() {
                        self.error(
                            id1,
                            format!("Inconsistent number of type parameters for enum {}", d1));
                    }

                    let ts = ts1.move_iter().zip(ts2.move_iter()).map(|(t1, t2)| self.unify(t1, t2)).collect();
                    mk_ty(EnumTy(d1, ts), id)
                }
            },
            (t1, t2) => if t1 == t2 { mk_ty(t1, id) } else { self.mismatch(id1, id2, &t1, &t2) }
        }
    }

    fn mismatch(&self, id1: NodeId, id2: NodeId, t1: &Ty_, t2: &Ty_) -> ! {
        self.session.errors_fatal([
            (id1, format!("Expected type: {}", t1)),
            (id2, format!("but got type: {}", t2))
        ]);
    }
}

impl<'a> Visitor for Typechecker<'a> {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.val {
            LetStmt(ref pat, ref e) => {
                match pat.val {
                    VariantPat(..) => self.error(
                        stmt.id,
                        "Cannot bind refutable pattern in let statement"),
                    _ => {}
                }

                let ty = self.pat_to_ty(pat);

                match *e {
                    Some(ref e) => {
                        let e_ty = self.expr_to_ty(e);
                        self.unify(ty, e_ty);
                    }
                    None => {}
                }
            }
            ExprStmt(ref e) => {
                let ty = self.expr_to_ty(e);
                self.unify(mk_i(UnitTy), ty);
            }
            SemiStmt(ref e) => {
                self.expr_to_ty(e);
            }
        }
    }

    fn visit_item(&mut self, item: &Item) {
        match item.val {
            UseItem(..) => {}
            FuncItem(_, _, ref t, ref b, ref tps) => {
                for b in b.iter() {
                    let tp_ids = tps.iter().map(|tp| tp.id).collect();
                    let tp_tys = self.tps_to_tys(item.id, &tp_ids, &None, true);
                    let mut gs = TreeMap::new();
                    for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                        gs.insert(tp.id, tp_ty.clone());
                    }

                    self.with_generics(gs, |me| {
                        me.exits.clear();
                        let ty = me.block_to_ty(b);
                        me.exits.push(ty);

                        let mut ty = me.type_to_ty(t);
                        let diverges = ty.ty == BottomTy;
                        for i in range(0, me.exits.len()).rev() {
                            let exit_ty = me.exits.swap_remove(i).take_unwrap();
                            ty = me.unify(ty, exit_ty);
                        }

                        if diverges && ty.ty != BottomTy {
                            me.error(item.id, "diverging function may return");
                        }
                    });
                }
            }
            ModItem(_, ref module) => {
                self.visit_module(module);
            }
            StaticItem(_, ref t, ref e) => {
                let ty = self.type_to_ty(t);

                match *e {
                    Some(ref e) => {
                        let e_ty = self.expr_to_ty(e);
                        self.unify(ty, e_ty);
                    }
                    None => {}
                }
            }
            StructItem(..) | EnumItem(..) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use mc::ast::NodeId;
    use mc::ast::visit::Visitor;
    use mc::parser::ast_from_str;
    use mc::session::Session;

    use super::Typechecker;

    use std::collections::TreeMap;

    #[test]
    fn basic_tyck_test() {
        let mut session = Session::new();
        let tree = session.parse_str(r"
enum Option<T> {
    Some(T),
    None,
}

fn lol<U>(mad: U) -> U {
    mad
}

fn wot(t: u32, flags: bool[3]) -> Option<bool> {
    let s = (t + 42) > 100 && false || flags[0];
    while true {
        let i;
        for ((); s && i < 10u; ()) {
            1337;
        }
    }
    Some(lol(s))
}

fn test_match<T>(flags: bool[3], o: Option<T>, f: fn(T) -> ()) -> Option<bool> {
    match o {
        Some(t) => f(t),
        None => {}
    }

    wot(42, flags)
}
");


        let mut tyck = Typechecker::new(&session);
        tyck.visit_module(&tree);
    }
}
