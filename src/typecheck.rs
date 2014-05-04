use ast::*;
use ast::defmap::*;
use ast::visit::*;
use collections::{SmallIntMap, HashMap, EnumSet};
use collections::enum_set::CLike;
use resolve::Resolver;
use std::fmt;

#[deriving(Eq, Show)]
struct BoundsId(uint);

impl BoundsId {
    pub fn to_uint(&self) -> uint {
        let BoundsId(bid) = *self;
        bid
    }
}

#[deriving(Eq, Show)]
enum Ty {
    BoolTy,
    GenericIntTy,
    IntTy(Width),
    UintTy(Width),
    StrTy,
    UnitTy,
    PtrTy(~Ty),
    ArrayTy(~Ty, u64),
    TupleTy(Vec<Ty>),
    FuncTy(Vec<Ty>, ~Ty),
    NamedTy(DefId),
    BoundTy(BoundsId),
    BottomTy,
}

impl Ty {
    fn kinds(&self) -> EnumSet<Kind> {
        let mut set = EnumSet::empty();
        match *self {
            StrTy | UnitTy |
            TupleTy(..) | ArrayTy(..) | FuncTy(..) |
            NamedTy(..) | BoundTy(..) => {
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

#[deriving(Eq)]
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
            EqKind => write!(f.buf, "Eq"),
            CmpKind => write!(f.buf, "Cmp"),
            AddKind => write!(f.buf, "Add"),
            SubKind => write!(f.buf, "Sub"),
            MulKind => write!(f.buf, "Mul"),
            DivKind => write!(f.buf, "Div"),
            RemKind => write!(f.buf, "Rem"),
            BitAndKind => write!(f.buf, "BitAnd"),
            BitOrKind => write!(f.buf, "BitOr"),
            BitXorKind => write!(f.buf, "BitXor"),
            ShrKind => write!(f.buf, "Shr"),
            ShlKind => write!(f.buf, "Shl"),
        }
    }
}

#[deriving(Eq)]
enum TyBounds {
    Concrete(Ty),
    Constrained(Kind),
    Unconstrained,
}

impl fmt::Show for TyBounds {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Concrete(ref t) => write!(f.buf, "{}", t),
            Constrained(ref k) => write!(f.buf, "<kind {}>", k),
            Unconstrained => write!(f.buf, "<generic>"),
        }
    }
}

pub struct Typechecker<'a> {
    bounds: SmallIntMap<TyBounds>,
    defmap: &'a DefMap,
    resolver: &'a Resolver,
    next_bounds_id: uint,
    exits: Vec<Ty>,
}

fn intkind_to_ty(ik: IntKind) -> Ty {
    match ik {
        GenericInt     => GenericIntTy,
        SignedInt(w)   => IntTy(w),
        UnsignedInt(w) => UintTy(w),
    }
}

impl<'a> Typechecker<'a> {
    pub fn new(defmap: &'a DefMap, resolver: &'a Resolver) -> Typechecker<'a> {
        Typechecker {
            bounds: SmallIntMap::new(),
            defmap: defmap,
            resolver: resolver,
            next_bounds_id: 0,
            exits: vec!(),
        }
    }

    fn new_bounds_id(&mut self) -> BoundsId {
        let bid = self.next_bounds_id;
        self.next_bounds_id += 1;
        BoundsId(bid)
    }

    fn type_to_ty(&mut self, t: &TypeNode) -> Ty {
        match *t {
            BoolType => BoolTy,
            UnitType => UnitTy,
            IntType(ik) => intkind_to_ty(ik),
            PtrType(ref t) => PtrTy(~self.type_to_ty(&t.val)),
            NamedType(ref id) => {
                let did = self.resolver.def_from_ident(id);
                match id.tps {
                    Some(ref tps) => {
                        /*
                        let bids = tps.iter().map(|t| {
                            let bid = self.new_bounds_id();
                            self.bounds.insert(bid.to_uint(), Unconstrained));
                            bid
                        }).collect();

                        BoundTy(did, bids)
                        */
                        fail!("Generics aren't ready yet")
                    }
                    None => NamedTy(did)
                }
            },
            FuncType(ref args, ref t) => {
               let ret_ty = self.type_to_ty(&t.val);
               let arg_tys = args.iter().map(|arg| {
                   self.type_to_ty(&arg.val)
               }).collect();

               FuncTy(arg_tys, ~ret_ty)
            },
            ArrayType(ref t, len) => {
                let ty = self.type_to_ty(&t.val);
                ArrayTy(~ty, len)
            }
            TupleType(ref ts) => {
               let tys = ts.iter().map(|t| {
                   self.type_to_ty(&t.val)
               }).collect();

               TupleTy(tys)
            },
        }
    }

    fn lit_to_ty(&mut self, lit: &Lit) -> Ty {
        match lit.val {
            NumLit(_, ik) => intkind_to_ty(ik),
            StringLit(..) => StrTy,
            BoolLit(..) => BoolTy,
        }
    }

    fn expr_to_ty(&mut self, expr: &Expr) -> Ty {
        match expr.val {
            UnitExpr => UnitTy,
            LitExpr(ref l) => self.lit_to_ty(l),
            TupleExpr(ref es) => TupleTy(es.iter().map(|e| self.expr_to_ty(e)).collect()),
            IdentExpr(ref id) => {
                let did = self.resolver.def_from_ident(id);
                let t = match *self.defmap.find(&did).take_unwrap() {
                    FuncDef(ref args, ref t, ref tps) => {
                        let ret_ty = self.type_to_ty(t);
                        let arg_tys = args.iter().map(|t| {
                            //self.type_to_ty()
                        });
                        ret_ty
                    }
                    FuncArgDef(ref t) => {
                        self.type_to_ty(t)
                    }
                    LetDef(ref t) => {
                        match *t {
                            Some(ref t) => self.type_to_ty(t),
                            None => fail!("Inference isn't ready yet"),
                        }
                    }
                    _ => fail!("Expected value name"),
                };

                t
            }
            BinOpExpr(ref op, ref l, ref r) => {
                let l_ty = self.expr_to_ty(*l);
                let r_ty = self.expr_to_ty(*r);

                let (expr_ty, ck) = match op.val {
                    EqualsOp | NotEqualsOp =>
                        (Some(BoolTy), Constrained(EqKind)),
                    AndAlsoOp | OrElseOp =>
                        (Some(BoolTy), Concrete(BoolTy)),
                    LessOp | LessEqOp | GreaterOp | GreaterEqOp =>
                        (Some(BoolTy), Constrained(CmpKind)),
                    PlusOp =>
                        (None, Constrained(AddKind)),
                    MinusOp =>
                        (None, Constrained(SubKind)),
                    TimesOp =>
                        (None, Constrained(MulKind)),
                    DivideOp =>
                        (None, Constrained(DivKind)),
                    ModOp =>
                        (None, Constrained(RemKind)),
                    BitAndOp =>
                        (None, Constrained(BitAndKind)),
                    BitOrOp =>
                        (None, Constrained(BitOrKind)),
                    BitXorOp =>
                        (None, Constrained(BitXorKind)),
                    LeftShiftOp =>
                        (None, Constrained(ShlKind)),
                    RightShiftOp =>
                        (None, Constrained(ShrKind)),
                };

                let l_ty = self.check_bounds(l_ty, ck);
                let ty = self.unify(l_ty, r_ty);

                expr_ty.unwrap_or(ty)
            }
            UnOpExpr(ref op, ref e) => {
                let ty = self.expr_to_ty(*e);

                let expr_ty = match op.val {
                    Deref => self.unify(PtrTy(~BottomTy), ty),
                    AddrOf => PtrTy(~ty),
                    Negate => self.unify(IntTy(AnyWidth), ty),
                };

                expr_ty
            }
            IndexExpr(ref a, ref i) => {
                let a_ty = self.expr_to_ty(*a);
                let i_ty = self.expr_to_ty(*i);

                let ty = match a_ty {
                    ArrayTy(ty, len) => *ty,
                    _ => self.unify(ArrayTy(~BottomTy, 0), a_ty),
                };

                let i_ty = self.unify(UintTy(AnyWidth), i_ty);

                ty
            }
            IfExpr(ref c, ref tb, ref fb) => {
                let c_ty = self.expr_to_ty(*c);
                self.unify(BoolTy, c_ty);
                let tb_ty = self.block_to_ty(*tb);
                let fb_ty = self.block_to_ty(*fb);
                self.unify(tb_ty, fb_ty)
            }
            CallExpr(..) => unimplemented!(),
            BlockExpr(ref b) => {
                self.block_to_ty(*b)
            }
            ReturnExpr(ref e) => {
                let ty = self.expr_to_ty(*e);
                self.exits.push(ty);
                BottomTy
            }
            CastExpr(..) => unimplemented!(),
            AssignExpr(..) => unimplemented!(),
            DotExpr(..) => unimplemented!(),
            ArrowExpr(..) => unimplemented!(),
            WhileExpr(ref e, ref b) => {
                let e_ty = self.expr_to_ty(*e);
                self.unify(BoolTy, e_ty);

                let b_ty = self.block_to_ty(*b);
                self.unify(UnitTy, b_ty)
            }
            ForExpr(ref init, ref cond, ref step, ref b) => {
                let i_ty = self.expr_to_ty(*init);
                self.unify(UnitTy, i_ty);

                let c_ty = self.expr_to_ty(*cond);
                self.unify(BoolTy, c_ty);

                let s_ty = self.expr_to_ty(*step);
                self.unify(UnitTy, s_ty);

                let b_ty = self.block_to_ty(*b);
                self.unify(UnitTy, b_ty)
            }
            MatchExpr(ref e, ref arms) => {
                unimplemented!() // need to implement match in defmap first, want to use patterns
            }
        }
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

    fn check_bounds(&mut self, t1: Ty, bounds: TyBounds) -> Ty {
        match bounds {
            Unconstrained => t1,
            Constrained(kind) if t1.is_of_kind(kind) => t1,
            Concrete(t2) => self.unify(t1, t2),
            _ => fail!("Expected type {} but found type {}", bounds, t1)
        }
    }

    fn unify(&self, t1: Ty, t2: Ty) -> Ty {
        // TODO pointers and ints together
        match (t1, t2) {
            (BottomTy, t) | (t, BottomTy) => t,
            (GenericIntTy, IntTy(w)) | (IntTy(w), GenericIntTy) => IntTy(w),
            (GenericIntTy, UintTy(w)) | (UintTy(w), GenericIntTy) => UintTy(w),
            (ref t@IntTy(ref w1), IntTy(ref w2)) | (ref t@UintTy(ref w1), UintTy(ref w2)) => {
                let ctor = match *t {
                    IntTy(..) => IntTy,
                    UintTy(..) => UintTy,
                    _ => fail!(),
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
            (BoundTy(..), _) | (_, BoundTy(..)) =>
                fail!("Generics aren't ready yet"), //TODO lookup and check bounds with other
            (PtrTy(p1), PtrTy(p2)) =>
                PtrTy(~self.unify(*p1, *p2)),
            (ArrayTy(a1, l1), ArrayTy(a2, l2)) => {
                if l1 == l2 {
                    ArrayTy(~self.unify(*a1, *a2), l1)
                } else {
                    self.mismatch(&ArrayTy(a1, l1), &ArrayTy(a2, l2))
                }
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
                       .collect(), ~self.unify(*t1, *t2))
                } else {
                    self.mismatch(&FuncTy(args1, t1), &FuncTy(args2, t2))
                }
            },
            (NamedTy(d1), NamedTy(d2)) if d1 == d2 =>
                NamedTy(d1), //TODO handle typedefs by getting def'd ty outside match
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
            LetStmt(ref id, ref t, ref e) => {
                match (t.as_ref(), e.as_ref()) {
                    (Some(t), Some(e)) => {
                        let ty = self.type_to_ty(&t.val);
                        let e_ty = self.expr_to_ty(e);
                        self.unify(ty, e_ty);
                    }
                    (Some(t), None) => {
                        self.type_to_ty(&t.val);
                    }
                    (None, Some(e)) => {
                        self.expr_to_ty(e);
                    }
                    (None, None) => fail!("Forward declaration not ready yet"),
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
            FuncItem(ref id, _, ref t, ref b, ref tps) => {
                if tps.len() > 0 {
                    fail!("Generics aren't ready yet");
                }

                self.exits.clear();

                let ty = self.block_to_ty(b);
                self.exits.push(ty);

                let mut ty = self.type_to_ty(&t.val);
                for i in range(0, self.exits.len()).rev() {
                    let exit_ty = self.exits.swap_remove(i).take_unwrap();
                    ty = self.unify(ty, exit_ty);
                }
            }
            StructItem(..) | EnumItem(..) => fail!("Structs and enums aren't ready yet")
        }
    }
}

#[cfg(test)]
mod tests {
    use resolve::Resolver;
    use ast::defmap::DefMap;
    use ast::DefId;
    use ast::visit::Visitor;
    use parser::new_from_string;
    use collections::TreeMap;
    use super::Typechecker;

    #[test]
    fn basic_tyck_test() {
        let mut parser = new_from_string(r"
fn wot(t: u32, flags: bool[3]) -> bool {
    let s: bool = (t + 42u) > 100u && false || flags[0u];
    while true {
        let i: u32;
        for ((); i < 10u; ()) {
            1337;
        }
    }
    s
}
".to_owned());

        let tree = parser.parse_module();

        let mut defmap = DefMap::new();
        defmap.visit_module(&tree);

        let mut resolver = Resolver::new();
        resolver.visit_module(&tree);

        let mut tyck = Typechecker::new(&defmap, &resolver);
        tyck.visit_module(&tree);
    }
}
