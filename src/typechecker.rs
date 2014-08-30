use mc::resolver::Resolver;
use mc::session::Session;
use util::{IntKind, GenericInt, SignedInt, UnsignedInt};
use util::{Width, AnyWidth, Width8, Width16, Width32};
use span::Span;

use std::collections::{SmallIntMap, TreeMap, EnumSet, TreeSet};
use std::collections::enum_set::CLike;

use std::fmt;
use values::eval_binop;

use util::graph::{Graph, VertexIndex};

use mc::ast::*;
use mc::ast::defmap::*;
use mc::ast::visitor::*;

use values;

#[deriving(Eq, PartialEq, Show, Clone)]
struct BoundsId(uint);

impl BoundsId {
    pub fn to_uint(&self) -> uint {
        let BoundsId(bid) = *self;
        bid
    }
}

#[deriving(Eq, PartialEq, Show, Clone)]
pub enum Ty {
    BoolTy,
    GenericIntTy,
    IntTy(Width),
    UintTy(Width),
    UnitTy,
    PtrTy(Box<WithId<Ty>>),
    ArrayTy(Box<WithId<Ty>>, Option<u64>),
    TupleTy(Vec<WithId<Ty>>),
    FuncTy(Vec<WithId<Ty>>, Box<WithId<Ty>>),
    StructTy(NodeId, Vec<WithId<Ty>>),
    EnumTy(NodeId, Vec<WithId<Ty>>),
    BoundTy(BoundsId),
    BottomTy,
}

type ConstGraph = Graph<NodeId, ()>;

struct ConstCollector<'a> {
    graph: ConstGraph,
    nodes: TreeMap<NodeId, VertexIndex>,
    consts: TreeMap<NodeId, &'a Expr>,
    session: &'a Session<'a>,
}

struct ConstGraphBuilder<'a> {
    graph: &'a mut ConstGraph,
    nodes: &'a TreeMap<NodeId, VertexIndex>,
    session: &'a Session<'a>,
}

type Constant = LitNode;
type ConstantResult = Result<Constant, (NodeId, &'static str)>;
pub type ConstantMap = TreeMap<NodeId, ConstantResult>;

fn constant_fold(session: &Session, map: &ConstantMap, expr: &Expr)
                  -> ConstantResult {
    match expr.val {
        LitExpr(ref lit) => {
            use std::mem::copy_lifetime;
            match lit.val {
                NullLit                 => Err((lit.id, "`null` is not valid here")),
                ref val                 => Ok(val.clone())
            }
        }
        GroupExpr(ref e) => constant_fold(session, map, &**e),
        PathExpr(ref path) => {
            let nid = session.resolver.def_from_path(path);
            match map.find(&nid) {
                Some(c) => c.clone(),
                None => Err((path.id, "Non-constant name where constant expected")),
            }
        }
        BinOpExpr(op, ref e1, ref e2) => {
            let r1 = try!(constant_fold(session, map, &**e1));
            let r2 = try!(constant_fold(session, map, &**e2));

            Ok(values::eval_binop(op.val, r1, r2))
        }
        UnOpExpr(op, ref e) => {
            let r = try!(constant_fold(session, map, &**e));

            // Unlike binops, some unops can't be folded
            match values::eval_unop(op.val, r) {
                Some(l) => Ok(l),
                None => Err((expr.id, "Non-constant expression where constant expected")),
            }
        }
        _ => Err((expr.id, "Non-constant expression where constant expected")),
    }
}

impl<'a> Visitor for ConstCollector<'a> {
    fn visit_item(&mut self, item: &Item) {
        match item.val {
            ConstItem(ref ident, _, ref e) => {
                use std::mem::copy_lifetime;
                let vid = self.graph.add_node(ident.id);
                self.nodes.insert(ident.id, vid);
                let expr = unsafe { copy_lifetime(self.session, e) };
                self.consts.insert(ident.id, expr);
            }
            _ => walk_item(self, item)
        }
    }
}

impl<'a> ConstCollector<'a> {
    fn new(session: &'a Session) -> ConstCollector<'a> {
        ConstCollector {
            nodes: TreeMap::new(),
            consts: TreeMap::new(),
            graph: Graph::new(),
            session: session,
        }
    }

    fn get_order(&mut self, module: &'a Module) -> Vec<NodeId> {
        use util::graph::GraphExt;

        let ConstCollector {
            nodes: ref nodes,
            consts: _,
            graph: ref mut graph,
            session: ref session,
        } = *self;

        ConstGraphBuilder::build_graph(graph, nodes, *session, module);

        match graph.toposort() {
            Ok(order) => order,
            Err(nid)  => session.error_fatal(nid, "Recursive constant"),
        }
    }

    fn map_constants(mut self, map: &mut ConstantMap, module: &'a Module) {
        self.visit_module(module);
        let order = self.get_order(module);
        for nid in order.iter() {
            let e = *self.consts.find(nid).unwrap();
            let c = constant_fold(self.session, map, e);
            map.insert(*nid, c);
        }
    }
}

impl<'a> Visitor for ConstGraphBuilder<'a> {
    fn visit_item(&mut self, item: &Item) {
        match item.val {
            ConstItem(ref ident, _, ref e) => {
                let srcidx = self.nodes.find(&ident.id).unwrap();
                match e.val {
                    PathExpr(ref path) => {
                        let nid = self.session.resolver.def_from_path(path);
                        match self.nodes.find(&nid) {
                            Some(destidx) => {
                                self.graph.add_edge(*srcidx, *destidx, ());
                            }
                            None => {}
                        }
                    }
                    _ => {}
                }
            }
            _ => walk_item(self, item)
        }
    }
}

impl<'a> ConstGraphBuilder<'a> {
    fn new(graph: &'a mut ConstGraph,
           nodes: &'a TreeMap<NodeId, VertexIndex>,
           session: &'a Session)
           -> ConstGraphBuilder<'a> {
        ConstGraphBuilder {
            graph: graph,
            nodes: nodes,
            session: session,
        }
    }

    fn build_graph(graph: &'a mut ConstGraph,
                   nodes: &'a TreeMap<NodeId, VertexIndex>,
                   session: &'a Session,
                   module: &'a Module) {
        let mut builder = ConstGraphBuilder::new(graph, nodes, session);
        builder.visit_module(module);
    }
}

impl Ty {
    fn with_id(self, nid: NodeId) -> WithId<Ty> {
        WithId { id: nid, val: self }
    }

    fn with_id_of<T>(self, node: &WithId<T>) -> WithId<Ty> {
        WithId { id: node.id, val: self }
    }

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

    pub fn is_ptr(&self) -> bool {
        match *self {
            PtrTy(..) => true,
            _ => false,
        }
    }

    pub fn is_signed(&self) -> bool {
        match *self {
            GenericIntTy |
            IntTy(..) => true,
            UintTy(..) |
            PtrTy(..) |
            BoolTy => false,
            _ => fail!("Non-integer type {}", self),
        }
    }

    pub fn is_generic(&self) -> bool {
        match *self {
            GenericIntTy => true,
            _ => false,
        }
    }
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

#[deriving(Eq, PartialEq, Clone)]
enum ErrorCause {
    InvalidArg,
    InvalidReturn,
    InvalidIndex,
    InvalidUnop,
    InvalidBinop,
    InvalidField,
    InvalidDimension,
    InvalidCond,
    InvalidIfBranches,
    InvalidCall,
    InvalidAssignment,
    InvalidLoopTy,
    InvalidPatBinding,
    InvalidMatchResult,
    InvalidStmt,
}

impl fmt::Show for ErrorCause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            InvalidArg => "incorrect arg type",
            InvalidReturn => "incorrect return type",
            InvalidIndex => "invalid index type",
            InvalidUnop => "invalid argument type to unary op",
            InvalidBinop => "invalid arguments to binary op",
            InvalidField => "invalid struct field in literal/pattern",
            InvalidDimension => "non-uint used as dimension for array type",
            InvalidCond => "non-bool used as predicate for conditional",
            InvalidIfBranches => "types of if branches don't match",
            // This needs to be broken up badly.
            InvalidCall => "incorrect function call",
            InvalidAssignment => "left and right hand sides of assignment don't match",
            InvalidLoopTy => "type of loop body not unit",
            InvalidPatBinding => "pattern does not have same type as expression",
            InvalidMatchResult => "arms in match have different types",
            InvalidStmt => "type of statement not unit",
        };

        write!(f, "{}", s)
    }
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

impl TyBounds {
    fn with_id(self, nid: NodeId) -> WithId<TyBounds> {
        WithId { id: nid, val: self }
    }

    fn with_id_of<T>(self, node: &WithId<T>) -> WithId<TyBounds> {
        WithId { id: node.id, val: self }
    }
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
    pub consts: ConstantMap,
}

pub struct Typechecker<'a> {
    defs: TreeMap<NodeId, BoundsId>,
    generics: Vec<TreeMap<NodeId, WithId<Ty>>>,
    session: &'a Session<'a>,
    next_bounds_id: uint,
    exits: Vec<WithId<Ty>>,
    typemap: Typemap,
    // This is hacky but it cuts down on plumbing
    current_cause: Option<(NodeId, ErrorCause)>,
    current_unify: Option<(WithId<Ty>, WithId<Ty>)>,
}

fn intkind_to_ty(ik: IntKind) -> Ty {
    match ik {
        GenericInt     => GenericIntTy,
        SignedInt(w)   => IntTy(w),
        UnsignedInt(w) => UintTy(w),
    }
}

macro_rules! save_ty {
    ($s:expr, $n:expr, $t:expr) => ({ let ty = $t; $s.typemap.types.insert($n.id.to_uint(), ty.clone()); ty.with_id_of($n) })
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
                consts: TreeMap::new(),
            },
            current_cause: None,
            current_unify: None,
        }
    }

    fn expr_to_const(&self, e: &Expr) -> Constant {
        let c = constant_fold(self.session, &self.typemap.consts, e);
        self.unwrap_const(c)
    }

    fn unwrap_const(&self, c: ConstantResult) -> Constant {
        match c {
            Ok(c) => c,
            Err((nid, e)) => self.session.error_fatal(nid, e),
        }
    }

    pub fn typecheck(&mut self, module: &'a Module) {
        let cc = ConstCollector::new(self.session);
        cc.map_constants(&mut self.typemap.consts, module);
        self.visit_module(module);
        for c in self.typemap.consts.iter() {
            self.unwrap_const(c.val1().clone());
        }
    }

    pub fn type_error_with_notes(&self, msg: String, notes: Vec<(NodeId, String)>) -> ! {
        let mut errors = vec!();
        match self.current_cause {
            None => fail!("ICE: type_error without cause"),
            Some((nid, cause)) => errors.push((nid, format!("{}: {}", cause, msg))),
        }

        errors.push_all_move(notes);

        self.session.errors_fatal(errors.as_slice())
    }

    pub fn type_error(&self, msg: String) -> ! {
        self.type_error_with_notes(msg, vec!())
    }

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
                  tps: &Vec<NodeId>, ts: &Option<Vec<Type>>, infer: bool) -> Vec<WithId<Ty>> {
        match *ts {
            Some(ref ts) if ts.len() == tps.len() =>
                ts.iter().map(|t| self.type_to_ty(t)).collect(),
            None if infer =>
                tps.iter().map(|id| {
                    WithId {
                        id: *id,
                        val: BoundTy(self.add_bounds()),
                    }
                }).collect(),
            None if tps.len() == 0 =>
                vec!(),
            _ =>
                self.error_fatal(
                    nid,
                    format!("Expected {} type parameters, but found {}",
                            tps.len(), ts.as_ref().map_or(0, |ts| ts.len()))),
        }
    }

    fn with_generics<T>(&mut self, gs: TreeMap<NodeId, WithId<Ty>>, f: |&mut Typechecker<'a>| -> T) -> T {
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
        self.typemap.bounds.find(&bid.to_uint()).take().unwrap().clone()
    }

    fn update_bounds(&mut self, bid: BoundsId, bounds: TyBounds) {
        self.typemap.bounds.swap(bid.to_uint(), bounds);
    }

    fn add_bound_ty(&mut self, nid: NodeId) -> Ty {
        let bid = self.add_bounds();
        self.defs.insert(nid, bid);
        BoundTy(bid)
    }

    fn get_bound_ty(&mut self, nid: NodeId) -> Ty {
        let bid = *self.defs.find(&nid).take().unwrap();
        match self.get_bounds(bid) {
            Concrete(ty) => ty,
            _ => BoundTy(bid),
        }
    }

    fn generic_to_ty(&mut self, nid: NodeId) -> WithId<Ty> {
        match self.generics.iter().rev()
                                  .filter_map(|gs| gs.find(&nid))
                                  .next() {
            Some(ty) => {
                WithId {
                    id: nid,
                    val: ty.val.clone(),
                }
            },
            None => self.error_fatal(nid, "Missing generic"),
        }
    }

    fn type_to_ty(&mut self, t: &Type) -> WithId<Ty> {
        save_ty!(self, t, match t.val {
            BoolType => BoolTy,
            UnitType => UnitTy,
            DivergingType => BottomTy,
            IntType(ik) => intkind_to_ty(ik),
            PtrType(ref t) => PtrTy(box self.type_to_ty(&**t)),
            NamedType(ref path) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take().unwrap() {
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
                    GenericDef => self.generic_to_ty(nid).val,
                    TypeDef(ref t) => self.type_to_ty(t).val,
                    _ => self.error_fatal(t.id, "Expected type name"),
                }
            },
            FuncType(ref args, ref t) => {
               let ret_ty = self.type_to_ty(&**t);
               let arg_tys = args.iter().map(|arg| {
                   self.type_to_ty(arg)
               }).collect();

               FuncTy(arg_tys, box ret_ty)
            },
            ArrayType(ref t, ref d) => {
                let ty = self.type_to_ty(&**t);
                let d_ty = self.expr_to_ty(&**d);
                self.check_ty_bounds_w(d.id, InvalidDimension, d_ty, Concrete(UintTy(AnyWidth)));
                let c = self.expr_to_const(&**d);
                let len = match c {
                    NumLit(u, UnsignedInt(_)) | NumLit(u, GenericInt) => u,
                    _ => self.error_fatal(t.id, "Array length must be a uint constant"),
                };
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

    fn pat_to_ty(&mut self, pat: &Pat) -> WithId<Ty> {
        let ty = match pat.val {
            DiscardPat(ref t) => {
                match *t {
                    Some(ref t) => self.type_to_ty(t).val,
                    None => BoundTy(self.add_bounds()),
                }
            }
            IdentPat(ref ident, ref t) => {
                match *t {
                    Some(ref t) => self.type_to_ty(t).val,
                    None => self.add_bound_ty(ident.id),
                }
            }
            TuplePat(ref pats) => {
                let tys = pats.iter().map(|p| self.pat_to_ty(p)).collect();
                TupleTy(tys)
            }
            VariantPat(ref path, ref pats) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take().unwrap() {
                    VariantDef(_, ref enum_nid, ref args) => {
                        let tps = match *self.session.defmap.find(enum_nid).take().unwrap() {
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
                                me.unify_with_cause(pat.id, InvalidArg, arg_ty, pat_ty);
                            }
                        });

                        EnumTy(*enum_nid, tp_tys)
                    }
                    _ => self.session.error_fatal(pat.id, "Not an enum variant"),
                }
            }
            StructPat(ref path, ref fps) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take().unwrap() {
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
                                me.unify_with_cause(fp.pat.id, InvalidField, field_ty, fp_ty);
                            }
                        });

                        StructTy(nid, tp_tys)
                    }
                    _ => self.session.error_fatal(pat.id, "Not a struct"),
                }
            }
        };

        save_ty!(self, pat, ty)
    }

    fn lit_to_ty(&mut self, lit: &Lit) -> WithId<Ty> {
        save_ty!(self, lit, match lit.val {
            NumLit(_, ik) => intkind_to_ty(ik),
            StringLit(..) => PtrTy(box UintTy(Width8).with_id_of(lit)),
            BoolLit(..) => BoolTy,
            NullLit => PtrTy(box BottomTy.with_id_of(lit)),
        })
    }

    fn func_def_to_ty(&mut self, arg_ids: &Vec<NodeId>, ret_t: &Type) -> Ty {
        let ret_ty = self.type_to_ty(ret_t);
        let arg_tys = arg_ids.iter().map(|arg_id| {
            match *self.session.defmap.find(arg_id).take().unwrap() {
                FuncArgDef(ref t) => self.type_to_ty(t),
                _ => self.session.bug_span(*arg_id, "Nonsensical arg id for func def"),
            }
        }).collect();
        FuncTy(arg_tys, box ret_ty)
    }

    fn expr_to_ty(&mut self, expr: &Expr) -> WithId<Ty> {
        save_ty!(self, expr, match expr.val {
            UnitExpr => UnitTy,
            LitExpr(ref l) => self.lit_to_ty(l).val,
            SizeofExpr(ref t) => {
                self.type_to_ty(t);
                UintTy(Width32)
            }
            TupleExpr(ref es) => TupleTy(es.iter().map(|e| self.expr_to_ty(e)).collect()),
            GroupExpr(ref e) => self.expr_to_ty(&**e).val,
            PathExpr(ref path) => {
                let nid = self.session.resolver.def_from_path(path);
                match *self.session.defmap.find(&nid).take().unwrap() {
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
                        self.type_to_ty(t).val
                    }
                    VariantDef(_, ref enum_nid, ref args) => {
                        let tps = match *self.session.defmap.find(enum_nid).take().unwrap() {
                            EnumDef(_, _, ref tps) => tps,
                            _ => self.error_fatal(expr.id, "Nonsensical enum id for variant"),
                        };

                        let tp_tys = self.tps_to_tys(
                            expr.id, tps, &path.val.elems.last().unwrap().val.tps, true);

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

                            let ret_ty = ctor(tp_tys).with_id_of(path);
                            FuncTy(arg_tys, box ret_ty)
                        }
                    }
                    PatDef(ref t) => {
                        match *t {
                            Some(ref t) => self.type_to_ty(t).val,
                            None => self.get_bound_ty(nid),
                        }
                    }
                    ConstDef(ref t) => {
                        self.type_to_ty(t).val
                    }
                    _ => self.error_fatal(expr.id,
                                          format!("{} does not name a value", path)),
                }
            }
            StructExpr(ref path, ref flds) => {
                let nid = self.session.resolver.def_from_path(path);
                let (fields, tps) = match *self.session.defmap.find(&nid).take().unwrap() {
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
                        me.unify_with_cause(fld.ref1().id, InvalidField, field_ty, fld_ty);
                    }
                });

                StructTy(nid, tp_tys)
            }
            BinOpExpr(ref op, ref l, ref r) => {
                let l_ty = self.expr_to_ty(&**l);
                let r_ty = self.expr_to_ty(&**r);

                self.unify_with_binop(expr.id, op, l_ty, r_ty)
            }
            UnOpExpr(ref op, ref e) => {
                let ty = self.expr_to_ty(&**e);
                let expr_ty = match op.val {
                    Negate => self.check_ty_bounds_w(e.id, InvalidUnop,
                                                     ty, Constrained(enumset!(SubKind))),
                    BitNot => self.check_ty_bounds_w(e.id, InvalidUnop,
                                                     ty, Constrained(enumset!(BitXorKind))),
                    LogNot => self.check_ty_bounds_w(e.id, InvalidUnop,
                                                     ty, Concrete(BoolTy)),
                    AddrOf => PtrTy(box ty),
                    Deref => match ty.val {
                        PtrTy(p_ty) => p_ty.val,
                        _ => unreachable!(),
                    },
                    Identity => ty.val,
                    _ => fail!("Operator {} should not appear here.",
                               op.val),
                };

                expr_ty
            }
            IndexExpr(ref a, ref i) => {
                let a_ty = self.expr_to_ty(&**a);
                let i_ty = self.expr_to_ty(&**i);
                self.check_ty_bounds_w(i.id, InvalidIndex,
                                       i_ty, Concrete(UintTy(AnyWidth)));

                match a_ty.val {
                    ArrayTy(ty, _) | PtrTy(ty) => ty.val,
                    ty => self.error_fatal(expr.id, format!("Cannot index into a {}", ty))
                }
            }
            IfExpr(ref c, ref tb, ref fb) => {
                let c_ty = self.expr_to_ty(&**c);
                self.check_ty_bounds_w(c.id, InvalidCond, c_ty, Concrete(BoolTy));
                let tb_ty = self.block_to_ty(&**tb);
                let fb_ty = self.block_to_ty(&**fb);
                self.unify_with_cause(expr.id, InvalidIfBranches, tb_ty, fb_ty)
            }
            CallExpr(ref e, ref args) => {
                let arg_tys: Vec<WithId<Ty>> = args.iter().map(|arg| self.expr_to_ty(arg)).collect();
                match self.expr_to_ty(&**e).val {
                    FuncTy(e_arg_tys, e_ret_ty) => {
                        if e_arg_tys.len() == arg_tys.len() {
                            for (e_arg_ty, arg_ty) in e_arg_tys.move_iter().zip(arg_tys.move_iter()) {
                                self.unify_with_cause(arg_ty.id, InvalidCall, e_arg_ty, arg_ty);
                            }
                            e_ret_ty.val
                        } else {
                            self.with_cause(expr.id, InvalidCall, proc(me)
                                             me.type_error(format!("expected {} parameters but found {}",
                                                                   e_arg_tys.len(), arg_tys.len())))
                        }
                    }
                    _ => {
                        self.error_fatal(expr.id, "Cannot call non-function");
                    }
                }
            }
            BlockExpr(ref b) => {
                self.block_to_ty(&**b).val
            }
            ReturnExpr(ref e) => {
                let ty = self.expr_to_ty(&**e);
                self.exits.push(ty);
                BottomTy
            }
            BreakExpr => BottomTy,
            ContinueExpr => BottomTy,
            CastExpr(ref e, ref t) => {
                let e_ty = self.expr_to_ty(&**e);
                let t_ty = self.type_to_ty(t);

                match e_ty.val {
                    GenericIntTy | UintTy(..) | IntTy(..) | PtrTy(..) | FuncTy(..) => {}
                    _ => self.error(expr.id, "Cannot cast expression of non-integral/pointer type"),
                }

                match t_ty.val {
                    GenericIntTy | UintTy(..) | IntTy(..) | PtrTy(..) | FuncTy(..) => {},
                    _ => self.error(expr.id, "Cannot cast to non-integral/pointer type"),
                }

                t_ty.val
            }
            AssignExpr(ref op, ref lv, ref rv) => {
                let l_ty = match lv.val {
                    PathExpr(..) | UnOpExpr(WithId { val: Deref, .. }, _) | IndexExpr(..) | DotExpr(..) | ArrowExpr(..) => {
                        self.expr_to_ty(&**lv)
                    }
                    _ => self.error_fatal(lv.id, "LHS of assignment is not an lvalue"),
                };

                let r_ty = self.expr_to_ty(&**rv);

                match *op {
                    Some(ref op) => self.unify_with_binop(expr.id, op, l_ty, r_ty),
                    None => self.unify_with_cause(expr.id, InvalidAssignment, l_ty, r_ty),
                }
            }
            DotExpr(ref e, ref fld) => {
                let e_ty = self.expr_to_ty(&**e);
                let (nid, tp_tys) = match e_ty.val { //self.unify(BottomTy, e_ty) {
                    StructTy(nid, tp_tys) => (nid, tp_tys),
                    ty => self.error_fatal(e.id,
                                           format!("Expression is not a structure, got {}", ty)),
                };

                match *self.session.defmap.find(&nid).take().unwrap() {
                    StructDef(ref name, ref fields, ref tps) => {
                        let mut gs = TreeMap::new();
                        for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                            gs.insert(*tp, tp_ty.clone());
                        }

                        let &(_, ref field) =
                            match fields.iter().find(|&&(ref a, _)| a == fld) {
                                Some(x) => x,
                                None => self.error_fatal(
                                    e.id,
                                    format!("The field {} is not a member of struct {}",
                                            fld,
                                            name.last().unwrap()))
                            };
                        self.with_generics(gs, |me| me.type_to_ty(field).val)
                    }
                    _ => unreachable!(),
                }
            }
            ArrowExpr(ref e, ref fld) => {
                let e_ty = self.expr_to_ty(&**e);
                let (nid, tp_tys) = match e_ty.val { //self.unify(BottomTy, e_ty) {
                    PtrTy(box WithId { id: _, val: StructTy(nid, tp_tys) }) => (nid, tp_tys),
                    _ => self.error_fatal(e.id, "Expression is not a pointer to a structure"),
                };

                // FIXME: code duplication with above
                match *self.session.defmap.find(&nid).take().unwrap() {
                    StructDef(ref name, ref fields, ref tps) => {
                        let mut gs = TreeMap::new();
                        for (tp, tp_ty) in tps.iter().zip(tp_tys.iter()) {
                            gs.insert(*tp, tp_ty.clone());
                        }

                        let &(_, ref field) =
                            match fields.iter().find(|&&(ref a, _)| a == fld) {
                                Some(x) => x,
                                None => self.error_fatal(
                                    e.id,
                                    format!("The field {} is not a member of struct {}",
                                            fld,
                                            name.last().unwrap()))
                            };
                        self.with_generics(gs, |me| me.type_to_ty(field).val)
                    }
                    _ => unreachable!(),
                }
            }
            DoWhileExpr(ref e, ref b) |
            WhileExpr(ref e, ref b) => {
                let e_ty = self.expr_to_ty(&**e);

                self.check_ty_bounds_w(e.id, InvalidCond, e_ty, Concrete(BoolTy));

                let b_ty = self.block_to_ty(&**b);
                self.check_ty_bounds_w(b.id, InvalidLoopTy, b_ty, Concrete(UnitTy))
            }
            ForExpr(ref init, ref cond, ref step, ref b) => {
                let _ = self.expr_to_ty(&**init);

                let c_ty = self.expr_to_ty(&**cond);
                self.check_ty_bounds_w(cond.id, InvalidCond, c_ty, Concrete(BoolTy));

                let _ = self.expr_to_ty(&**step);

                let b_ty = self.block_to_ty(&**b);
                self.check_ty_bounds_w(b.id, InvalidLoopTy, b_ty, Concrete(UnitTy))
            }
            MatchExpr(ref e, ref arms) => {
                let mut e_ty = self.expr_to_ty(&**e).val;
                let mut ty = BottomTy;
                for arm in arms.iter() {
                    let pat_ty = self.pat_to_ty(&arm.pat);
                    let body_ty = self.expr_to_ty(&arm.body);
                    e_ty = self.unify_with_cause(arm.pat.id, InvalidPatBinding, e_ty.with_id_of(&**e), pat_ty);
                    ty = self.unify_with_cause(arm.body.id, InvalidMatchResult, ty.with_id_of(expr), body_ty);
                }

                let eid = match e_ty {
                    EnumTy(eid, _) => eid,
                    _ => self.session.error_fatal(e.id, "Match expressions are only valid on enum types"),
                };

                let mut variants = TreeSet::new();
                match *self.session.defmap.find(&eid).unwrap() {
                    EnumDef(_, ref vs, _) => {
                        for v in vs.iter() {
                            variants.insert(*v);
                        }
                    }
                    _ => unreachable!(),
                };

                let mut has_discard_pat = false;
                for arm in arms.iter() {
                    match arm.pat.val {
                        DiscardPat(_) => {
                            has_discard_pat = true;
                            break;
                        }
                        VariantPat(ref path, _) => {
                            let vid = self.session.resolver.def_from_path(path);

                            if variants.contains(&vid) {
                                variants.remove(&vid);
                            } else {
                                self.session.error_fatal(arm.pat.id, "Duplicate variant in match arm");
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                if !has_discard_pat && !variants.is_empty() {
                    self.session.error_fatal(expr.id, "Non-exhaustive match");
                }

                ty
            }
            ArrayExpr(ref elems) => {
                let mut cur_ty = self.expr_to_ty(&elems[0]);
                for elem in elems.iter().skip(1) {
                    let new_ty = self.expr_to_ty(elem);
                    cur_ty = WithId { id: cur_ty.id,
                                      val: self.unify(cur_ty, new_ty) };
                }

                ArrayTy(box cur_ty, Some(elems.len() as u64))
            },
            MacroExpr(..) => fail!(),
        })
    }

    fn block_to_ty(&mut self, block: &Block) -> WithId<Ty> {
        save_ty!(self, block, {
            for item in block.val.items.iter() {
                self.visit_item(item);
            }
            for stmt in block.val.stmts.iter() {
                self.visit_stmt(stmt);
            }
            block.val.expr.as_ref().map_or(UnitTy, |e| self.expr_to_ty(e).val)
        })
    }

    fn merge_bounds(&mut self, b1: WithId<TyBounds>, b2: WithId<TyBounds>) -> TyBounds {
        let id1 = b1.id;
        let id2 = b2.id;
        match (b1.val, b2.val) {
            (Unconstrained, bs) | (bs, Unconstrained) => bs,
            (Concrete(t1), Concrete(t2)) => Concrete(self.unify(t1.with_id(id1), t2.with_id(id2))),
            (Concrete(ty), Constrained(ks)) =>
                Concrete(self.check_ty_bounds(ty.with_id(id1), Constrained(ks).with_id(id2))),  // do something about Concrete(BoundTy)
            (Constrained(ks), Concrete(ty)) =>
                Concrete(self.check_ty_bounds(ty.with_id(id2), Constrained(ks).with_id(id1))),  // do something about Concrete(BoundTy)
            (Constrained(ks1), Constrained(ks2)) => {
                Constrained(ks1.union(ks2))
            }
        }
    }


    fn check_ty_bounds(&mut self, t1: WithId<Ty>, bounds: WithId<TyBounds>) -> Ty {
        let idt = t1.id;
        let idb = bounds.id;
        match bounds.val {
            Unconstrained => t1.val,
            Concrete(t2) => self.unify(t1, t2.with_id(idb)),
            Constrained(ks) => {
                match t1.val {
                    BoundTy(bid) => {
                        let b1 = self.get_bounds(bid).with_id(idt);
                        let bounds = self.merge_bounds(b1, bounds);
                        self.update_bounds(bid, bounds);
                        BoundTy(bid)
                    }
                    _ => {
                        if !t1.val.kinds().contains(ks) {
                            self.type_error(
                                format!("Expected type with bounds {} but found type {}",
                                        bounds, t1.val));
                        }
                        t1.val
                    }
                }
            }
        }
    }

    fn unify_with_binop(&mut self, nid: NodeId, op: &BinOp, l_ty: WithId<Ty>, r_ty: WithId<Ty>) -> Ty {
        fn is_integral_ty(t: &Ty) -> bool {
            match *t {
                UintTy(..) | IntTy(..) | GenericIntTy => true,
                _ => false,
            }
        };

        match (op.val, l_ty.val, r_ty.val) {
            // Once again we appease the borrow checker...
            (PlusOp, ref t, ref mut p@PtrTy(..)) |
            (PlusOp, ref mut p@PtrTy(..), ref t) |
            (MinusOp, ref mut p@PtrTy(..), ref t) if is_integral_ty(t) =>
                ::std::mem::replace(p, UnitTy),
            (MinusOp, PtrTy(..), PtrTy(..)) =>
                UintTy(Width32), //TODO PtrWidth
            (_, l_ty_val, r_ty_val) => {
                let kinds = op_to_kind_set(op);
                let bounds = Constrained(kinds);

                let l_ty = WithId {
                    id: l_ty.id,
                    val: l_ty_val,
                };

                /* groan */
                let l_ty = WithId {
                    id: l_ty.id,
                    val: self.check_ty_bounds_w(nid, InvalidBinop, l_ty, bounds),
                };

                let r_ty = WithId {
                    id: r_ty.id,
                    val: r_ty_val,
                };


                let out_ty = self.unify_with_cause(nid, InvalidBinop, l_ty, r_ty);
                if kinds.intersects(enumset!(EqKind, CmpKind, AndKind, OrKind)) {
                    BoolTy
                } else {
                    out_ty
                }
            }
        }
    }

    fn with_cause<T>(&mut self, nid: NodeId, cause: ErrorCause, f: proc(&mut Typechecker) -> T) -> T {
        let mut current_cause = Some((nid, cause));
        ::std::mem::swap(&mut current_cause, &mut self.current_cause);
        let ret = f(self);
        ::std::mem::swap(&mut current_cause, &mut self.current_cause);
        ret
    }

    fn with_unify<T>(&mut self, ty1: WithId<Ty>, ty2: WithId<Ty>, f: proc(&mut Typechecker) -> T) -> T {
        let mut current_unify = Some((ty1, ty2));
        ::std::mem::swap(&mut current_unify, &mut self.current_unify);
        let ret = f(self);
        ::std::mem::swap(&mut current_unify, &mut self.current_unify);
        ret
    }

    fn unify_with_cause(&mut self, nid: NodeId, cause: ErrorCause, ty1: WithId<Ty>, ty2: WithId<Ty>) -> Ty {
        self.with_cause(nid, cause, proc(me) {
            me.with_unify(ty1.clone(), ty2.clone(), proc(me) {
                me.unify(ty1, ty2)
            })
        })
    }

    fn check_ty_bounds_w(&mut self, nid: NodeId, cause: ErrorCause,
                         t1: WithId<Ty>, bounds: TyBounds) -> Ty {
        let id = t1.id;
        self.with_cause(nid, cause, proc(me) me.check_ty_bounds(t1, bounds.with_id(id)))
    }

    fn unify(&mut self, ty1: WithId<Ty>, ty2: WithId<Ty>) -> Ty {
        let id1 = ty1.id;
        let id2 = ty2.id;
        // TODO pointers and ints together
        match (ty1.val, ty2.val) {
            (BottomTy, t) => t,
            (t, BottomTy) => t,
            (BoundTy(b1), BoundTy(b2)) => {
                let bounds =
                    if b1 == b2 {
                        self.get_bounds(b1)
                    } else {
                        let bs1 = WithId { id: ty1.id, val: self.get_bounds(b1) };
                        let bs2 = WithId { id: ty2.id, val: self.get_bounds(b2) };
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
            (BoundTy(b), ty) => {
                let bounds = self.get_bounds(b).with_id(id1);
                let t = self.check_ty_bounds(ty.with_id(id2), bounds);
                self.update_bounds(b, Concrete(t.clone()));
                t
            },
            (ty, BoundTy(b)) => {
                let bounds = self.get_bounds(b).with_id(id2);
                let t = self.check_ty_bounds(ty.with_id(id1), bounds);
                self.update_bounds(b, Concrete(t.clone()));
                t
            },
            (GenericIntTy, IntTy(w)) | (IntTy(w), GenericIntTy) => IntTy(w),
            (GenericIntTy, UintTy(w)) | (UintTy(w), GenericIntTy) => UintTy(w),
            (IntTy(w1), IntTy(w2)) => {
                let w = match (w1, w2) {
                    (AnyWidth, w) | (w, AnyWidth) => w,
                    (w1, w2) => {
                        if w1 == w2 {
                            w1
                        } else {
                            self.mismatch(&IntTy(w1).with_id(id1), &IntTy(w2).with_id(id2))
                        }
                    }
                };

                IntTy(w)
            },
            (UintTy(w1), UintTy(w2)) => {
                let w = match (w1, w2) {
                    (AnyWidth, w) | (w, AnyWidth) => w,
                    (w1, w2) => {
                        if w1 == w2 {
                            w1
                        } else {
                            self.mismatch(&UintTy(w1).with_id(id1), &UintTy(w2).with_id(id2))
                        }
                    }
                };

                UintTy(w)
            },
            (PtrTy(p1), PtrTy(p2)) => {
                // XXX might have the wrong id here
                let id = p1.id;
                PtrTy(box self.unify(*p1, *p2).with_id(id))
            }
            (ArrayTy(a1, l1), ArrayTy(a2, l2)) => {
                let l = match (l1, l2) {
                    (None, l) | (l, None) => l,
                    (Some(l1), Some(l2)) if l1 == l2 => Some(l1),
                    _ => self.mismatch(&ArrayTy(a1, l1).with_id(id1), &ArrayTy(a2, l2).with_id(id2)),
                };

                // XXX might have the wrong id here
                let id = a1.id;
                ArrayTy(box self.unify(*a1, *a2).with_id(id), l)
            },
            (TupleTy(ts1), TupleTy(ts2)) => {
                if ts1.len() == ts2.len() {
                    TupleTy(ts1.move_iter().zip(
                            ts2.move_iter()).map(
                                // XXX might have the wrong id here
                                |(t1, t2)| { let id = t1.id; self.unify(t1, t2).with_id(id) })
                            .collect())
                } else {
                    self.mismatch(&TupleTy(ts1).with_id(id1), &TupleTy(ts2).with_id(id2))
                }
            },
            (FuncTy(args1, t1), FuncTy(args2, t2)) => {
                if args1.len() == args2.len() {
                    // XXX might have the wrong id for ret_ty here
                    let ret_id = t1.id;
                    FuncTy(args1.move_iter().zip(
                           args2.move_iter()).map(
                                // XXX might have the wrong id here
                               |(arg1, arg2)| { let id = arg1.id; self.unify(arg1, arg2).with_id(id) })
                           .collect(), box self.unify(*t1, *t2).with_id(ret_id))
                } else {
                    self.mismatch(&FuncTy(args1, t1).with_id(id1), &FuncTy(args2, t2).with_id(id2))
                }
            },
            (StructTy(d1, ts1), StructTy(d2, ts2)) => {
                if d1 != d2 {
                    self.mismatch(&StructTy(d1, ts1).with_id(id1), &StructTy(d2, ts2).with_id(id2));
                } else {
                    if ts1.len() != ts2.len() {
                        self.type_error(format!("Inconsistent number of type parameters for struct {}", d1));
                    }

                    // XXX might have the wrong id here
                    let ts = ts1.move_iter().zip(ts2.move_iter()).map(|(t1, t2)| { let id = t1.id; self.unify(t1, t2).with_id(id) }).collect();
                    StructTy(d1, ts)
                }
            },
            (EnumTy(d1, ts1), EnumTy(d2, ts2)) => {
                if d1 != d2 {
                    self.mismatch(&EnumTy(d1, ts1).with_id(id1), &EnumTy(d2, ts2).with_id(id2));
                } else {
                    if ts1.len() != ts2.len() {
                        self.type_error(format!("Inconsistent number of type parameters for enum {}", d1));
                    }

                    // XXX might have the wrong id here
                    let ts = ts1.move_iter().zip(ts2.move_iter()).map(|(t1, t2)| { let id = t1.id; self.unify(t1, t2).with_id(id) }).collect();
                    EnumTy(d1, ts)
                }
            },
            (t1, t2) => if t1 == t2 { t1 } else { self.mismatch(&t1.with_id(id1), &t2.with_id(id2)) }
        }
    }

    fn mismatch(&mut self, ty1: &WithId<Ty>, ty2: &WithId<Ty>) -> ! {
        let current_unify = ::std::mem::replace(&mut self.current_unify, None);
        match current_unify {
            Some((ref full_ty1, ref full_ty2)) => {
                self.type_error_with_notes(
                    format!("Expected type {} but found type {}", full_ty1.val, full_ty2.val), vec!(
                    (ty1.id, format!("note: expected type: {}", ty1.val)),
                    (ty2.id, format!("note: but got type: {}", ty2.val))
                ))
            }
            _ => {
                self.type_error(format!("Expected type {} but found type {}", ty1.val, ty2.val))
            }
        }
    }
}

impl<'a> Visitor for Typechecker<'a> {
    fn visit_block(&mut self, b: &Block) {
        self.block_to_ty(b);
    }

    fn visit_expr(&mut self, e: &Expr) {
        self.expr_to_ty(e);
    }

    fn visit_type(&mut self, t: &Type) {
        self.type_to_ty(t);
    }

    fn visit_lit(&mut self, l: &Lit) {
        self.lit_to_ty(l);
    }

    fn visit_pat(&mut self, p: &Pat) {
        self.pat_to_ty(p);
    }

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
                        self.unify_with_cause(stmt.id, InvalidPatBinding, ty, e_ty);
                    }
                    None => {}
                }
            }
            ExprStmt(ref e) => {
                let ty = self.expr_to_ty(e);
                self.unify_with_cause(stmt.id, InvalidStmt, UnitTy.with_id_of(stmt), ty);
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

                        let mut ty = me.type_to_ty(t).val;
                        let diverges = ty == BottomTy;
                        for i in range(0, me.exits.len()).rev() {
                            let exit_ty = me.exits.swap_remove(i).take().unwrap();
                            ty = me.unify_with_cause(item.id, InvalidReturn, ty.with_id_of(item), exit_ty);
                        }

                        if diverges && ty != BottomTy {
                            me.error(item.id, "diverging function may return");
                        }
                    });
                }
            }
            ModItem(_, ref module) => {
                self.visit_module(module);
            }
            StaticItem(_, ref t, ref e, _) => {
                let ty = self.type_to_ty(t);

                match *e {
                    Some(ref e) => {
                        let e_ty = self.expr_to_ty(e);
                        self.unify_with_cause(item.id, InvalidPatBinding, ty, e_ty);
                    }
                    None => {}
                }
            }
            ConstItem(_, ref t, ref e) => {
                let ty = self.type_to_ty(t);
                let e_ty = self.expr_to_ty(e);
                self.unify_with_cause(item.id, InvalidPatBinding, ty, e_ty);
            }
            TypeItem(_, ref t, ref tps) => {
                let mut gs = TreeMap::new();
                for tp in tps.iter() {
                    gs.insert(tp.id, UnitTy.with_id_of(tp));
                }

                self.with_generics(gs, |me| {
                    me.visit_type(t);
                })
            }
            EnumItem(_, ref vs, ref tps) => {
                for v in vs.iter() {
                    let mut gs = TreeMap::new();
                    for tp in tps.iter() {
                        gs.insert(tp.id, UnitTy.with_id_of(tp));
                    }

                    self.with_generics(gs, |me| {
                        for t in v.args.iter() {
                            me.visit_type(t);
                        }
                    })
                }
            }
            StructItem(_, ref flds, ref tps) => {
                let mut gs = TreeMap::new();
                for tp in tps.iter() {
                    gs.insert(tp.id, UnitTy.with_id_of(tp));
                }

                self.with_generics(gs, |me| {
                    for fld in flds.iter() {
                        me.visit_type(&fld.fldtype);
                    }
                })
            }
            MacroDefItem(..) => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use mc::ast::NodeId;
    use mc::ast::visitor::Visitor;
    use mc::parser::ast_from_str;
    use mc::session::{Session, Options};

    use super::Typechecker;

    use std::collections::TreeMap;

    #[test]
    fn basic_tyck_test() {
        let mut session = Session::new(Options::new());
        let tree = session.parse_package_str(r"
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
        tyck.typecheck(&tree);
    }
}
