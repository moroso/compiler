use util::{IntKind, Name};
use super::lexer::Token;

use std::fmt;
use std::fmt::{Debug, Formatter, Display};

use mas::ast::InstNode;
use util::Escape;

pub use self::PatNode::*;
pub use self::TypeNode::*;
pub use self::BinOpNode::*;
pub use self::UnOpNode::*;
pub use self::LitNode::*;
pub use self::ExprNode::*;
pub use self::StmtNode::*;
pub use self::MacroToken::*;
pub use self::ImportSpec::*;
pub use self::ItemNode::*;
pub use self::FuncDef::*;

pub mod visitor;
pub mod mut_visitor;
pub mod defmap;
pub mod macros;
pub mod pathmap;

pub type WithId<T> = WithIdT<NodeId, T>;

#[derive(Clone, Eq, Copy)]
pub struct WithIdT<I, T> {
    pub id: I,
    pub val: T,
}

pub trait CanHaveId<I>
where Self: Sized {
    fn with_id(self: Self, id: I) -> WithIdT<I, Self> {
        WithIdT {
            val: self,
            id: id,
        }
    }
}

impl<I, T: PartialEq> PartialEq for WithIdT<I, T> {
    fn eq(&self, other: &WithIdT<I, T>) -> bool {
        self.val.eq(&other.val)
    }
}

impl<I, T: Debug> Debug for WithIdT<I, T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.val.fmt(f)
    }
}

impl<I, T: Display> Display for WithIdT<I, T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.val.fmt(f)
    }
}

macro_rules! with_id {
    ( $( $s:ident => $n:ident ),* ) => ( $( pub type $s = WithId<$n>; )* );
    ( $( $s:ident => $n:ident ),+, ) => ( with_id! { $( $s => $n ),+ } );
}

with_id! {
    Type     => TypeNode,
    BinOp    => BinOpNode,
    UnOp     => UnOpNode,
    Lit      => LitNode,
    Pat      => PatNode,
    Expr     => ExprNode,
    Block    => BlockNode,
    Stmt     => StmtNode,
    Item     => ItemNode,
    Ident    => IdentNode,
    Path     => PathNode,
    Module   => ModuleNode,
    Import   => ImportNode,
}

#[derive(Eq, PartialEq, Clone, Ord, PartialOrd, Debug, Copy)]
pub struct NodeId(pub usize);

allow_string!(NodeId);

impl NodeId {
    pub fn to_uint(&self) -> usize {
        let NodeId(did) = *self;
        did
    }
}

// This is a fully-type-applied reference to an identifier.
// e.g. "foo::<int,int*>"
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct IdentNode {
    pub tps: Option<Vec<Type>>, // type arguments. Option<Vec> to avoid alloc.
    pub name: Name,
}

impl Display for IdentNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        try!(write!(f, "{}", self.name));
        for tps in self.tps.iter() {
            if tps.len() > 0 {
                try!(write!(f, "<{:?}>", tps));
            }
        }
        Ok(())
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct PathNode {
    pub elems: Vec<Ident>,
    pub global: bool,
}

impl Display for PathNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let global = if self.global { "::" } else { "" };
        let elems: Vec<String> = self.elems.iter().map(|e| format!("{}", e)).collect();
        write!(f, "{}{}", global, elems.join("::"))
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct FieldPat {
    pub name: Name,
    pub pat:  Pat,
}

impl Display for FieldPat {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.pat)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum PatNode {
    DiscardPat(Option<Type>),
    IdentPat(Ident, Option<Type>),
    TuplePat(Vec<Pat>),
    VariantPat(Path, Vec<Pat>),
    StructPat(Path, Vec<FieldPat>),
}

impl Display for PatNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            DiscardPat(ref t)             => write!(f, "_{}",
                                                    t.as_ref().map(|t| format!(": {}", t)).unwrap_or_default()),
            IdentPat(ref id, ref t)       => write!(f, "{}{}", id,
                                                    t.as_ref().map(|t| format!(": {}", t)).unwrap_or_default()),
            TuplePat(ref args)            => write!(f, "({:?})", args),
            VariantPat(ref id, ref args)  => write!(f, "{}({:?})", id, args),
            StructPat(ref id, ref fields) => write!(f, "{} {{ {:?} }}", id, fields),
        }
    }
}

/// Types
#[derive(Eq, PartialEq, Clone, Debug)]
pub enum TypeNode {
    BoolType,
    UnitType,
    DivergingType,
    IntType(IntKind),
    PtrType(Box<Type>),
    NamedType(Path),
    FuncType(Vec<Type>, Box<Type>),
    ArrayType(Box<Type>, Box<Expr>),
    TupleType(Vec<Type>),
}

impl Display for TypeNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            BoolType                  => write!(f, "bool"),
            UnitType                  => write!(f, "()"),
            DivergingType             => write!(f, "!"),
            IntType(k)                => write!(f, "{}", k),
            PtrType(ref t)            => write!(f, "*({})", t),
            NamedType(ref p)          => write!(f, "{}", p),
            FuncType(ref d, ref r)    => write!(f, "({:?} -> {})", d, r),
            ArrayType(ref t, ref d)   => write!(f, "({})[{}]", t, d),
            TupleType(ref ts)         => write!(f, "({:?})", ts),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Copy)]
pub enum BinOpNode {
    PlusOp,
    MinusOp,
    TimesOp,
    DivideOp,
    ModOp,
    EqualsOp,
    NotEqualsOp,
    LessOp,
    LessEqOp,
    GreaterOp,
    GreaterEqOp,
    AndAlsoOp,
    OrElseOp,
    BitAndOp,
    BitOrOp,
    BitXorOp,
    LeftShiftOp,
    RightShiftOp,
}

impl BinOpNode {
    fn is_pred(&self) -> bool {
        match *self {
            BinOpNode::EqualsOp |
            BinOpNode::NotEqualsOp |
            BinOpNode::LessOp |
            BinOpNode::LessEqOp |
            BinOpNode::GreaterOp |
            BinOpNode::GreaterEqOp |
            BinOpNode::AndAlsoOp |
            BinOpNode::OrElseOp => true,
            _ => false,
        }
    }
}

impl Display for BinOpNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            PlusOp      => "+",
            MinusOp     => "-",
            TimesOp     => "*",
            DivideOp    => "/",
            ModOp       => "%",
            EqualsOp    => "==",
            LessOp      => "<",
            LessEqOp    => "<=",
            GreaterOp   => ">",
            GreaterEqOp => ">=",
            AndAlsoOp   => "&&",
            OrElseOp    => "||",
            BitAndOp    => "&",
            BitOrOp     => "|",
            BitXorOp    => "^",
            NotEqualsOp => "!=",
            LeftShiftOp => "<<",
            RightShiftOp=> ">>",
        })
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Copy)]
pub enum UnOpNode {
    Deref,
    AddrOf,
    Negate,
    LogNot,
    BitNot,
    Identity,
    SxbOp,
    SxhOp,
}

impl Display for UnOpNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Deref  => "*",
            AddrOf => "&",
            Negate => "-",
            LogNot => "!",
            BitNot => "~",
            Identity => "",
            SxbOp => "Sxb",
            SxhOp => "Sxh",
        })
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum LitNode {
    NumLit(u64, IntKind),
    StringLit(String),
    BoolLit(bool),
    NullLit,
}

impl Display for LitNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            NumLit(i, nt)     => write!(f, "{}{}", i, nt),
            StringLit(ref s)  => write!(f, "\"{}\"", s.escape_default()),
            BoolLit(b)        => write!(f, "BoolLit:{}", b),
            NullLit           => write!(f, "null"),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct MatchArm {
    pub pat: Pat,
    pub body: Expr,
}

impl Display for MatchArm {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.pat, self.body)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum ExprNode {
    UnitExpr,
    LitExpr(Lit),
    SizeofExpr(Type),
    GroupExpr(Box<Expr>),
    TupleExpr(Vec<Expr>),
    PathExpr(Path),
    StructExpr(Path, Vec<(Name, Expr)>),
    ArrayExpr(Vec<Expr>),
    BinOpExpr(BinOp, Box<Expr>, Box<Expr>),
    UnOpExpr(UnOp, Box<Expr>),
    IndexExpr(Box<Expr>, Box<Expr>),
    DotExpr(Box<Expr>, Name),
    ArrowExpr(Box<Expr>, Name),
    AssignExpr(Option<BinOp>, Box<Expr>, Box<Expr>),
    CallExpr(Box<Expr>, Vec<Expr>),
    CastExpr(Box<Expr>, Type),
    IfExpr(Box<Expr>, Box<Block>, Box<Block>),
    BlockExpr(Box<Block>),
    ReturnExpr(Box<Expr>),
    BreakExpr,
    ContinueExpr,
    WhileExpr(Box<Expr>, Box<Block>),
    DoWhileExpr(Box<Expr>, Box<Block>),
    ForExpr(Box<Expr>, Box<Expr>, Box<Expr>, Box<Block>),
    MatchExpr(Box<Expr>, Vec<MatchArm>),
    MacroExpr(Name, Vec<Vec<Token>>),
    AsmExpr(Vec<Vec<InstNode>>), // The inner Vec is to work around the fact that Rust arrays
                                 // only impl Clone if the inner type impls Copy.
}

impl Display for ExprNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            UnitExpr                            => write!(f, "()"),
            LitExpr(ref l)                      => write!(f, "{}", l),
            SizeofExpr(ref t)                   => write!(f, "{}", t),
            GroupExpr(ref e)                    => write!(f, "({})", e),
            TupleExpr(ref vs)                   => write!(f, "({:?})", vs),
            PathExpr(ref p)                     => write!(f, "{:?}", p),
            StructExpr(ref p, ref flds)         => write!(f, "{} {{ {:?} }}", p, flds),
            ArrayExpr(ref elems)                => write!(f, "[{:?}]", elems),
            BinOpExpr(op, ref l, ref r)         => write!(f, "({}{}{})", l, op, r),
            UnOpExpr(op, ref e)                 => write!(f, "({}{})", op, e),
            IndexExpr(ref e, ref i)             => write!(f, "{}[{}]", e, i),
            DotExpr(ref e, ref fld)             => write!(f, "{}.{}", e, fld),
            ArrowExpr(ref e, ref fld)           => write!(f, "{}->{}", e, fld),
            AssignExpr(ref op, ref lv, ref rv)  => write!(f, "({}{}={})", lv, op.map_or(String::new(), |op| format!("{}", op)), rv),
            CallExpr(ref e, ref args)           => write!(f, "{}({:?})", e, args),
            CastExpr(ref e, ref t)              => write!(f, "({} as {})", e, t),
            IfExpr(ref c, ref bt, ref bf)       => write!(f, "if {} {{\n    {}}} else {{\n    {}}}", c, bt, bf),
            BlockExpr(ref b)                    => write!(f, "{}", b),
            ReturnExpr(ref e)                   => write!(f, "return {}", e),
            BreakExpr                           => write!(f, "break"),
            ContinueExpr                        => write!(f, "continue"),
            WhileExpr(ref e, ref b)             => write!(f, "while {} {}", e, b),
            DoWhileExpr(ref e, ref b)           => write!(f, "do {} while {}", b, e),
            ForExpr(ref e1, ref e2, ref e3, ref b) => write!(f, "for ({};{};{}) {}", e1, e2, e3, b),
            MatchExpr(ref e, ref items) => {
                try!(write!(f, "match {} {{\n", e));
                for item in items.iter() {
                    try!(write!(f, "    {},\n", item));
                }
                write!(f, "{}", "}")
            },
            MacroExpr(n, ref args) => write!(f, "{}!({:?})", n, args),
            AsmExpr(ref packets) => {
                try!(write!(f, "Asm("));
                for packet in packets.iter() {
                    try!(write!(f, "[ "));
                    for inst in packet.iter() {
                        try!(write!(f, "{}", inst));
                    }
                    try!(write!(f, "]"));
                }
                write!(f, ")\n;")
            },
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum StmtNode {
    LetStmt(Pat, Option<Expr>),
    ExprStmt(Expr), // no trailing semicolon, must have unit type
    SemiStmt(Expr), // trailing semicolon, any type OK
}

impl Display for StmtNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            LetStmt(ref pat, ref expr) => {
                write!(f, "let {}{};", pat,
                       expr.as_ref().map(|e| format!(" = {}", e)).unwrap_or_default())
            },
            ExprStmt(ref e) => {
                write!(f, "{}", e)
            },
            SemiStmt(ref e) => {
                write!(f, "{};", e)
            },
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct BlockNode {
    pub items: Vec<Item>,
    pub stmts: Vec<Stmt>,
    pub expr:  Option<Expr>,
}

impl Display for BlockNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        try!(write!(f, "{}\n", "{"));
        for item in self.items.iter() {
            for line in (&format!("{}", item)[..]).lines() {
                try!(write!(f, "    {}\n", line));
            }
        }
        for stmt in self.stmts.iter() {
            for line in (&format!("{}", stmt)[..]).lines() {
                try!(write!(f, "    {};\n", line));
            }
        }
        match self.expr {
            Some(ref e) => {
                for line in (&format!("{}", e)[..]).lines() {
                    try!(write!(f, "    {}\n", line));
                }
            },
            None => {}
        }
        write!(f, "{}", "}")
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct FuncArg {
    pub ident:   Ident,
    pub argtype: Type,
}

impl Display for FuncArg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.ident, self.argtype)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Variant {
    pub ident: Ident,
    pub args: Vec<Type>,
}

impl Display for Variant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        try!(write!(f, "{}(", self.ident));
        for ref argtype in self.args.iter() {
            try!(write!(f, "{}, ", argtype));
        }
        write!(f, ")")
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Field {
    pub name:    Name,
    pub fldtype: Type,
}

impl Display for Field {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.fldtype)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MacroToken {
    MacroTok(Token),
    MacroVar(Name),
    MacroVarArgs,
}

allow_string!(MacroToken);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MacroDef {
    pub name: Name,
    pub args: Vec<Name>,
    pub body: Vec<MacroToken>,
}

impl MacroDef {
    pub fn with_id(self, nid: NodeId) -> WithId<MacroDef> {
        WithId { id: nid, val: self }
    }
}

allow_string!(MacroDef);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ImportSpec {
    ImportNames(Vec<Ident>),
    ImportAll
}

allow_string!(ImportSpec);

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ImportNode {
    pub elems: Vec<Ident>,
    pub import: ImportSpec,
    pub global: bool
}

allow_string!(ImportNode);

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum FuncDef {
    LocalFn(Block),
    ExternFn(Name, Option<Block>),
}

impl FuncDef {
    pub fn is_local(&self) -> bool {
        match *self {
            LocalFn(..) => true,
            ExternFn(..) => false,
        }
    }

    pub fn is_extern(&self) -> bool {
        match *self {
            LocalFn(..) => false,
            ExternFn(..) => true,
        }
    }

    pub fn abi(&self) -> Option<Name> {
        match *self {
            LocalFn(..) => None,
            ExternFn(name, _) => Some(name.clone())
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum ItemNode {
    FuncItem(Ident, Vec<FuncArg>, Type, FuncDef, Vec<Ident>),
    StructItem(Ident, Vec<Field>, Vec<Ident>),
    EnumItem(Ident, Vec<Variant>, Vec<Ident>),
    TypeItem(Ident, Type, Vec<Ident>),
    ModItem(Ident, Module),
    StaticItem(Ident, Type, Option<Expr>, bool /* is this extern? */),
    UseItem(Import),
    MacroDefItem(MacroDef),
    ConstItem(Ident, Type, Expr),
}

impl Display for ItemNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            FuncItem(ref id, ref args, ref t, LocalFn(ref block), ref tps) => {
                try!(write!(f, "fn {}", id));
                if tps.len() > 0 {
                    try!(write!(f, "<{:?}>", tps));
                }
                write!(f, "({:?}) -> {} {}", args, t, block)
            },
            FuncItem(ref id, ref args, ref t, ExternFn(ref abi, ref block_opt), ref tps) => {
                try!(write!(f, "extern \"{}\" fn {}", abi, id));
                if tps.len() > 0 {
                    try!(write!(f, "<{:?}>", tps));
                }
                try!(write!(f, "({:?}) -> {}", args, t));
                match *block_opt {
                    Some(ref block) => write!(f, "{{ {} }}", block),
                    None => write!(f, ";"),
                }
            },
            StructItem(ref id, ref fields, ref tps) => {
                try!(write!(f, "struct {}", id));
                if tps.len() > 0 {
                    try!(write!(f, "<{:?}>", tps));
                }
                try!(write!(f, "{}\n", " {"));
                for field in fields.iter() {
                    try!(write!(f, "    {},\n", field));
                }
                write!(f, "{}", "}")
            },
            EnumItem(ref id, ref items, ref tps) => {
                try!(write!(f, "enum {}", id));
                if tps.len() > 0 {
                    try!(write!(f, "<{:?}>", tps));
                }
                try!(write!(f, "{}\n", " {"));
                for ref item in items.iter() {
                    try!(write!(f, "    {},\n", item));
                }
                write!(f, "{}", "}")
            }
            TypeItem(ref id, ref ty, ref tps) => {
                try!(write!(f, "type {}", id));
                if tps.len() > 0 {
                    try!(write!(f, "<{:?}>", tps));
                }
                write!(f, " = {};", ty)
            }
            ModItem(ref ident, ref module) => {
                try!(write!(f, "mod {}", ident));
                try!(write!(f, "{}\n", " {"));
                for ref item in module.val.items.iter() {
                    try!(write!(f, "    {}\n", item));
                }
                write!(f, "{}", "}")
            }
            StaticItem(ref name, ref ty, ref expr, is_extern) => {
                write!(f, "{}static {}: {}{};", name,
                       if is_extern { "extern " } else { "" },
                       ty,
                       expr.as_ref().map(|e| format!(" = {}", e))
                       .unwrap_or_default())
            }
            ConstItem(ref name, ref ty, ref expr) => {
                write!(f, "const {}: {} = {};", name, ty, expr)
            }
            UseItem(ref path) => {
                write!(f, "use {};", path)
            }
            MacroDefItem(ref def) => write!(f, "macro {}", def),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ModuleNode {
    pub items: Vec<Item>
}

impl Display for ModuleNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for item in self.items.iter() {
            for line in (&format!("{}", item)[..]).lines() {
                try!(write!(f, "{}\n", line));
            }
        }
        Ok(())
    }
}
