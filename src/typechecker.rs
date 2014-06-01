use collections::{SmallIntMap, TreeMap, EnumSet};
use collections::enum_set::CLike;
use resolver::Resolver;
use session::Session;
use util::{IntKind, GenericInt, SignedInt, UnsignedInt};
use util::{Width, AnyWidth, Width8, Width16, Width32};

use std::fmt;

use ast::*;
use ast::defmap::*;
use ast::visit::*;

#[deriving(TotalEq, PartialEq, Show, Clone)]
struct BoundsId(uint);

impl BoundsId {
    pub fn to_uint(&self) -> uint {
        let BoundsId(bid) = *self;
        bid
    }
}

#[deriving(TotalEq, PartialEq, Show, Clone)]
enum Ty {
    BoolTy,
    GenericIntTy,
    IntTy(Width),
    UintTy(Width),
    StrTy,
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

impl Ty {
    fn kinds(&self) -> EnumSet<Kind> {
        let mut set = EnumSet::empty();
        match *self {
            StrTy | UnitTy |
            TupleTy(..) | ArrayTy(..) | FuncTy(..) |
            StructTy(..) | EnumTy(..) | BoundTy(..) => {
                set.add(EqKind);
            }
            BoolTy => {
                set.add(EqKind);
            }
            PtrTy(..) => {
                set.add(EqKind);
                set.add(CmpKind);
                set.add(AddKind);
                set.add(SubKind);
                set.add(BitAndKind);
                set.add(BitOrKind);
                set.add(BitXorKind);
                set.add(ShrKind);
                set.add(ShlKind);
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

#[deriving(TotalEq, PartialEq, Clone)]
enum Kind {
    EqKind,
    CmpKind,
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
            2 => AddKind,
            3 => SubKind,
            4 => MulKind,
            5 => DivKind,
            6 => RemKind,
            7 => BitAndKind,
            8 => BitOrKind,
            9 => BitXorKind,
            10 => ShrKind,
            11 => ShlKind,
            _ => fail!(),
        }
    }
}

impl fmt::Show for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            EqKind => write!(f, "Eq"),
            CmpKind => write!(f, "Cmp"),
            AddKind => write!(f, "Add"),
            SubKind => write!(f, "Sub"),
            MulKind => write!(f, "Mul"),
            DivKind => write!(f, "Div"),
            RemKind => write!(f, "Rem"),
            BitAndKind => write!(f, "BitAnd"),
            BitOrKind => write!(f, "BitOr"),
            BitXorKind => write!(f, "BitXor"),
            ShrKind => write!(f, "Shr"),
            ShlKind => write!(f, "Shl"),
        }
    }
}

#[deriving(TotalEq, PartialEq, Clone)]
enum TyBounds {
    Concrete(Ty),
    Constrained(EnumSet<Kind>),
    Unconstrained,
}

impl fmt::Show for TyBounds {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Concrete(ref t) => write!(f, "{}", t),
            Constrained(ref ks) => write!(f, "<kinds {}>", ks),
            Unconstrained => write!(f, "<generic>"),
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

fn intkind_to_ty(ik: IntKind) -> Ty {
    match ik {
        GenericInt     => GenericIntTy,
        SignedInt(w)   => IntTy(w),
        UnsignedInt(w) => UintTy(w),
    }
}

macro_rules! save_ty {
    ($n:expr, $t:expr) => ({ let ty = $t; self.typemap.types.insert($n.id.to_uint(), ty.clone()); ty })
}

macro_rules! constrained {
    ($k:expr) => (
        {
            let mut ks = EnumSet::empty();
            ks.add($k);
            Constrained(ks)
        }
    )
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

    pub fn get_typemap(self) -> Typemap { 
        self.typemap
    }

    fn tps_to_tys(&mut self, tps: &Vec<NodeId>, ts: &Option<Vec<Type>>, infer: bool) -> Vec<Ty> {
        match *ts {
            Some(ref ts) if ts.len() == tps.len() =>
                ts.iter().map(|t| self.type_to_ty(t)).collect(),
            None if infer =>
                tps.iter().map(|_| BoundTy(self.add_bounds())).collect(),
            None if tps.len() == 0 =>
                vec!(),
            _ =>
                fail!("Expected {} type parameters, but found {}", tps.len(), ts.as_ref().map_or(0, |ts| ts.len())),
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
        BoundTy(bid)
    }

    fn get_bound_ty(&self, nid: NodeId) -> Ty {
        let bid = *self.defs.find(&nid).take_unwrap();
        BoundTy(bid)
    }

    fn generic_to_ty(&mut self, nid: NodeId) -> Ty {
        match self.generics.iter().rev()
                                  .filter_map(|gs| gs.find(&nid))
                                  .next() {
            Some(ty) => ty.clone(),
            None => fail!("Missing generic {}", nid),
        }
    }

    fn type_to_ty(&mut self, t: &Type) -> Ty {
        save_ty!(t, match t.val {
            BoolType => BoolTy,
            UnitType => UnitTy,
            IntType(ik) => intkind_to_ty(ik),
            PtrType(ref t) => PtrTy(box self.type_to_ty(*t)),
            NamedType(ref path) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take_unwrap() {
                    StructDef(_, ref tps) => {
                        let tys = self.tps_to_tys(tps, &path.val.elems.last().unwrap().val.tps, false);
                        StructTy(nid, tys)
                    }
                    EnumDef(_, ref tps) => {
                        let tys = self.tps_to_tys(tps, &path.val.elems.last().unwrap().val.tps, false);
                        EnumTy(nid, tys)
                    }
                    GenericDef => self.generic_to_ty(nid),
                    TypeDef(ref t) => self.type_to_ty(t),
                    _ => fail!("Expected type name"),
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
                    None => BoundTy(self.add_bounds()),
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
                TupleTy(tys)
            }
            VariantPat(ref path, ref pats) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take_unwrap() {
                    VariantDef(ref enum_nid, ref args) => {
                        let tps = match *self.session.defmap.find(enum_nid).take_unwrap() {
                            EnumDef(_, ref tps) => tps,
                            _ => fail!("Nonsensical enum id for variant"),
                        };

                        let tp_tys = self.tps_to_tys(tps, &path.val.elems.last().unwrap().val.tps, true);

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

                        EnumTy(*enum_nid, tp_tys)
                    }
                    _ => unreachable!(),
                }
            }
            StructPat(ref path, ref fps) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take_unwrap() {
                    StructDef(ref fields, ref tps) => {
                        let tp_tys = self.tps_to_tys(tps, &path.val.elems.last().unwrap().val.tps, true);

                        let mut gs = TreeMap::new();
                        for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                            gs.insert(*tp, tp_ty.clone());
                        }

                        self.with_generics(gs, |me| {
                            for (field, fp) in fields.iter().map(|x| x.val1()).zip(fps.iter()) {
                                let field_ty = me.type_to_ty(field);
                                let fp_ty = me.pat_to_ty(&fp.pat);
                                me.unify(field_ty, fp_ty);
                            }
                        });

                        StructTy(nid, tp_tys)
                    }
                    _ => unreachable!(),
                }
            }
        })
    }

    fn lit_to_ty(&mut self, lit: &Lit) -> Ty {
        save_ty!(lit, match lit.val {
            NumLit(_, ik) => intkind_to_ty(ik),
            StringLit(..) => StrTy,
            BoolLit(..) => BoolTy,
        })
    }

    fn func_def_to_ty(&mut self, arg_ids: &Vec<NodeId>, ret_t: &Type) -> Ty {
        let ret_ty = self.type_to_ty(ret_t);
        let arg_tys = arg_ids.iter().map(|arg_id| {
            match *self.session.defmap.find(arg_id).take_unwrap() {
                FuncArgDef(ref t) => self.type_to_ty(t),
                _ => fail!("Nonsensical arg id for func def"),
            }
        }).collect();
        FuncTy(arg_tys, box ret_ty)
    }

    fn expr_to_ty(&mut self, expr: &Expr) -> Ty {
        save_ty!(expr, match expr.val {
            UnitExpr => UnitTy,
            LitExpr(ref l) => self.lit_to_ty(l),
            TupleExpr(ref es) => TupleTy(es.iter().map(|e| self.expr_to_ty(e)).collect()),
            GroupExpr(ref e) => self.expr_to_ty(*e),
            PathExpr(ref path) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take_unwrap() {
                    FuncDef(ref args, ref t, ref tps) => {
                        let tp_tys = self.tps_to_tys(tps, &path.val.elems.last().unwrap().val.tps, true);
                        let mut gs = TreeMap::new();
                        for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                            gs.insert(*tp, tp_ty.clone());
                        }

                        self.with_generics(gs, |me| me.func_def_to_ty(args, t))
                    }
                    FuncArgDef(ref t) => {
                        self.type_to_ty(t)
                    }
                    VariantDef(ref enum_nid, ref args) => {
                        let tps = match *self.session.defmap.find(enum_nid).take_unwrap() {
                            EnumDef(_, ref tps) => tps,
                            _ => fail!("Nonsensical enum id for variant"),
                        };

                        let tp_tys = self.tps_to_tys(tps, &path.val.elems.last().unwrap().val.tps, true);

                        let ctor = |tp_tys| EnumTy(*enum_nid, tp_tys);
                        if args.len() == 0 {
                            ctor(tp_tys)
                        } else {
                            let mut gs = TreeMap::new();
                            for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                                gs.insert(*tp, tp_ty.clone());
                            }

                            let arg_tys = self.with_generics(gs, |me|
                                args.iter().map(|t| me.type_to_ty(t)).collect()
                            );

                            FuncTy(arg_tys, box ctor(tp_tys))
                        }
                    }
                    PatDef(ref t) => {
                        match *t {
                            Some(ref t) => self.type_to_ty(t),
                            None => self.get_bound_ty(nid),
                        }
                    }
                    _ => fail!("{} does not name a value", path),
                }
            }
            StructExpr(ref path, ref flds) => {
                let nid = self.session.resolver.def_from_path(path);
                let (fields, tps) = match *self.session.defmap.find(&nid).take_unwrap() {
                    StructDef(ref fields, ref tps) => (fields, tps),
                    _ => fail!("{} does not name a struct", path),
                };

                let tp_tys = self.tps_to_tys(tps, &path.val.elems.last().unwrap().val.tps, true);

                let mut gs = TreeMap::new();
                for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                    gs.insert(*tp, tp_ty.clone());
                }

                self.with_generics(gs, |me| {
                    for (field, fld) in fields.iter().map(|x| x.val1()).zip(flds.iter()) {
                        let field_ty = me.type_to_ty(field);
                        let fld_ty = me.expr_to_ty(fld.ref1());
                        me.unify(field_ty, fld_ty);
                    }
                });

                StructTy(nid, tp_tys)
            }
            BinOpExpr(ref op, ref l, ref r) => {
                let l_ty = self.expr_to_ty(*l);
                let r_ty = self.expr_to_ty(*r);

                enum TyOrKind {
                    Ty(Ty),
                    Kind(Kind),
                }

                let (expr_ty, ck) = match op.val {
                    EqualsOp | NotEqualsOp =>
                        (Some(BoolTy), Kind(EqKind)),
                    AndAlsoOp | OrElseOp =>
                        (Some(BoolTy), Ty(BoolTy)),
                    LessOp | LessEqOp | GreaterOp | GreaterEqOp =>
                        (Some(BoolTy), Kind(CmpKind)),
                    PlusOp =>
                        (None, Kind(AddKind)),
                    MinusOp =>
                        (None, Kind(SubKind)),
                    TimesOp =>
                        (None, Kind(MulKind)),
                    DivideOp =>
                        (None, Kind(DivKind)),
                    ModOp =>
                        (None, Kind(RemKind)),
                    BitAndOp =>
                        (None, Kind(BitAndKind)),
                    BitOrOp =>
                        (None, Kind(BitOrKind)),
                    BitXorOp =>
                        (None, Kind(BitXorKind)),
                    LeftShiftOp =>
                        (None, Kind(ShlKind)),
                    RightShiftOp =>
                        (None, Kind(ShrKind)),
                };

                let bounds = match ck {
                    Kind(k) => constrained!(k),
                    Ty(ty) => Concrete(ty),
                };

                let l_ty = self.check_ty_bounds(l_ty, bounds);
                let ty = self.unify(l_ty, r_ty);

                expr_ty.unwrap_or(ty)
            }
            UnOpExpr(ref op, ref e) => {
                let ty = self.expr_to_ty(*e);
                let expr_ty = match op.val {
                    Negate => self.check_ty_bounds(ty, constrained!(SubKind)),
                    BitNot => self.check_ty_bounds(ty, constrained!(BitXorKind)),
                    LogNot => self.unify(BoolTy, ty),
                    AddrOf => PtrTy(box ty),
                    Deref => {
                        match self.unify(PtrTy(box BottomTy), ty) {
                            PtrTy(ty) => *ty,
                            _ => unreachable!(),
                        }
                    }
                };

                expr_ty
            }
            IndexExpr(ref a, ref i) => {
                let a_ty = self.expr_to_ty(*a);
                let i_ty = self.expr_to_ty(*i);

                self.unify(UintTy(AnyWidth), i_ty);

                match self.unify(ArrayTy(box BottomTy, None), a_ty) {
                    ArrayTy(ty, _) => *ty,
                    _ => unreachable!(),
                }
            }
            IfExpr(ref c, ref tb, ref fb) => {
                let c_ty = self.expr_to_ty(*c);
                self.unify(BoolTy, c_ty);
                let tb_ty = self.block_to_ty(*tb);
                let fb_ty = self.block_to_ty(*fb);
                self.unify(tb_ty, fb_ty)
            }
            CallExpr(ref e, ref args) => {
                let arg_tys = args.iter().map(|arg| self.expr_to_ty(arg)).collect();
                let e_ty = self.expr_to_ty(*e);
                match self.unify(e_ty, FuncTy(arg_tys, box BottomTy)) {
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
                BottomTy
            }
            CastExpr(ref e, ref t) => {
                let e_ty = self.expr_to_ty(*e);
                let t_ty = self.type_to_ty(t);

                match self.unify(BottomTy, e_ty) {
                    GenericIntTy | UintTy(..) | IntTy(..) => {}
                    _ => fail!("Cannot cast expression of non-integral type"),
                }

                match self.unify(BottomTy, t_ty) {
                    ty@GenericIntTy | ty@UintTy(..) | ty@IntTy(..) => ty,
                    _ => fail!("Cannot cast to non-integral type"),
                }
            }
            AssignExpr(ref lv, ref rv) => {
                // TODO: need self.expr_is_lvalue(...)
                // For now we'll just assume lv is an lvalue.
                self.expr_to_ty(*rv)
            }
            DotExpr(ref e, ref fld) => {
                let e_ty = self.expr_to_ty(*e);
                let (nid, tp_tys) = match self.unify(BottomTy, e_ty) {
                    StructTy(nid, tp_tys) => (nid, tp_tys),
                    ty => fail!("Expression is not a structure, got {}", ty),
                };

                match *self.session.defmap.find(&nid).take_unwrap() {
                    StructDef(ref fields, ref tps) => {
                        let mut gs = TreeMap::new();
                        for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                            gs.insert(*tp, tp_ty.clone());
                        }

                        let field = fields.find(fld).unwrap();
                        self.with_generics(gs, |me| me.type_to_ty(field))
                    }
                    _ => unreachable!(),
                }
            }
            ArrowExpr(ref e, ref fld) => {
                let e_ty = self.expr_to_ty(*e);
                let (nid, tp_tys) = match self.unify(BottomTy, e_ty) {
                    PtrTy(box StructTy(nid, tp_tys)) => (nid, tp_tys),
                    _ => fail!("Expression is not a pointer to a structure"),
                };

                match *self.session.defmap.find(&nid).take_unwrap() {
                    StructDef(ref fields, ref tps) => {
                        let mut gs = TreeMap::new();
                        for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                            gs.insert(*tp, tp_ty.clone());
                        }

                        let field = fields.find(fld).unwrap();
                        self.with_generics(gs, |me| me.type_to_ty(field))
                    }
                    _ => unreachable!(),
                }
            }
            WhileExpr(ref e, ref b) => {
                let e_ty = self.expr_to_ty(*e);
                self.unify(BoolTy, e_ty);

                let b_ty = self.block_to_ty(*b);
                self.unify(UnitTy, b_ty)
            }
            ForExpr(ref init, ref cond, ref step, ref b) => {
                let i_ty = self.expr_to_ty(*init);
                // No unify here: we allow the init expression to have any type.
                //self.unify(UnitTy, i_ty);

                let c_ty = self.expr_to_ty(*cond);
                self.unify(BoolTy, c_ty);

                let s_ty = self.expr_to_ty(*step);
                // No unify here: we allow the iter expression to have any type.

                let b_ty = self.block_to_ty(*b);
                self.unify(UnitTy, b_ty)
            }
            MatchExpr(ref e, ref arms) => {
                let mut e_ty = self.expr_to_ty(*e);
                let mut ty = BottomTy;
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
        block.expr.as_ref().map(|e| self.expr_to_ty(e)).unwrap_or(UnitTy)
    }

    fn merge_bounds(&mut self, b1: TyBounds, b2: TyBounds) -> TyBounds {
        match (b1, b2) {
            (Unconstrained, bs) | (bs, Unconstrained) => bs,
            (Concrete(t1), Concrete(t2)) => Concrete(self.unify(t1, t2)),
            (Constrained(ks), Concrete(ty)) | (Concrete(ty), Constrained(ks)) =>
                Concrete(self.check_ty_bounds(ty, Constrained(ks))),
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
                match t1 {
                    BoundTy(bid) => {
                        let b1 = self.get_bounds(bid);
                        let bounds = self.merge_bounds(b1, bounds);
                        self.update_bounds(bid, bounds);
                        BoundTy(bid)
                    }
                    _ => if t1.kinds().contains(ks) { t1 }
                         else { fail!("Expected type with bounds {} but found type {}", bounds, t1) }
                }
            }
        }
    }

    fn unify(&mut self, t1: Ty, t2: Ty) -> Ty {
        // TODO pointers and ints together
        match (t1, t2) {
            // Bound types take priority over BottomTy
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
                    _ => BoundTy(b1),
                }
            },
            (BoundTy(b), t) | (t, BoundTy(b)) => {
                let bounds = self.get_bounds(b);
                let t = self.check_ty_bounds(t, bounds);
                self.update_bounds(b, Concrete(t.clone()));
                t
            },
            (BottomTy, t) | (t, BottomTy) => t,
            (GenericIntTy, IntTy(w)) | (IntTy(w), GenericIntTy) => IntTy(w),
            (GenericIntTy, UintTy(w)) | (UintTy(w), GenericIntTy) => UintTy(w),
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
                            self.mismatch(&ctor(w1), &ctor(w2))
                        }
                    }
                };

                ctor(w)
            },
            (PtrTy(p1), PtrTy(p2)) =>
                PtrTy(box self.unify(*p1, *p2)),
            (ArrayTy(a1, l1), ArrayTy(a2, l2)) => {
                let l = match (l1, l2) {
                    (None, l) | (l, None) => l,
                    (Some(l1), Some(l2)) if l1 == l2 => Some(l1),
                    _ => self.mismatch(&ArrayTy(a1, l1), &ArrayTy(a2, l2)),
                };

                ArrayTy(box self.unify(*a1, *a2), l)
            },
            (TupleTy(ts1), TupleTy(ts2)) => {
                if ts1.len() == ts2.len() {
                    TupleTy(ts1.move_iter().zip(
                            ts2.move_iter()).map(
                                |(t1, t2)| self.unify(t1, t2))
                        .collect())
                } else {
                    self.mismatch(&TupleTy(ts1), &TupleTy(ts2))
                }
            },
            (FuncTy(args1, t1), FuncTy(args2, t2)) => {
                if args1.len() == args2.len() {
                    FuncTy(args1.move_iter().zip(
                           args2.move_iter()).map(
                               |(arg1, arg2)| self.unify(arg1, arg2))
                       .collect(), box self.unify(*t1, *t2))
                } else {
                    self.mismatch(&FuncTy(args1, t1), &FuncTy(args2, t2))
                }
            },
            (StructTy(d1, ts1), StructTy(d2, ts2)) => {
                if d1 != d2 {
                    self.mismatch(&StructTy(d1, ts1), &StructTy(d2, ts2));
                } else {
                    if ts1.len() != ts2.len() {
                        fail!("Inconsistent number of type parameters for struct {}", d1);
                    }

                    let ts = ts1.move_iter().zip(ts2.move_iter()).map(|(t1, t2)| self.unify(t1, t2)).collect();
                    StructTy(d1, ts)
                }
            },
            (EnumTy(d1, ts1), EnumTy(d2, ts2)) => {
                if d1 != d2 {
                    self.mismatch(&EnumTy(d1, ts1), &EnumTy(d2, ts2));
                } else {
                    if ts1.len() != ts2.len() {
                        fail!("Inconsistent number of type parameters for enum {}", d1);
                    }

                    let ts = ts1.move_iter().zip(ts2.move_iter()).map(|(t1, t2)| self.unify(t1, t2)).collect();
                    EnumTy(d1, ts)
                }
            },
            (t1, t2) => if t1 == t2 { t1 } else { self.mismatch(&t1, &t2) }
        }
    }

    fn mismatch(&self, t1: &Ty, t2: &Ty) -> ! {
        fail!("Expected type {} but found type {}", t1, t2)
    }
}

impl<'a> Visitor for Typechecker<'a> {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.val {
            LetStmt(ref pat, ref e) => {
                match pat.val {
                    VariantPat(..) => fail!("Cannot bind refutable pattern in let statement"),
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
                self.unify(UnitTy, ty);
            }
            SemiStmt(ref e) => {
                self.expr_to_ty(e);
            }
        }
    }

    fn visit_item(&mut self, item: &Item) {
        match item.val {
            FuncItem(_, _, ref t, ref b, ref tps) => {
                let tp_ids = tps.iter().map(|tp| tp.id).collect();
                let tp_tys = self.tps_to_tys(&tp_ids, &None, true);
                let mut gs = TreeMap::new();
                for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                    gs.insert(tp.id, tp_ty.clone());
                }

                self.with_generics(gs, |me| {
                    me.exits.clear();
                    let ty = me.block_to_ty(b);
                    me.exits.push(ty);

                    let mut ty = me.type_to_ty(t);
                    for i in range(0, me.exits.len()).rev() {
                        let exit_ty = me.exits.swap_remove(i).take_unwrap();
                        ty = me.unify(ty, exit_ty);
                    }
                });
            }
            ModItem(_, ref module) => {
                self.visit_module(module);
            }
            StructItem(..) | EnumItem(..) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use session::Session;
    use ast::NodeId;
    use ast::visit::Visitor;
    use parser::ast_from_str;
    use collections::TreeMap;
    use super::Typechecker;

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