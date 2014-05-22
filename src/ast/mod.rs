use lexer::{Token, SourceToken};
use util::{IntKind, Name, Width};

use std::fmt;
use std::fmt::{Formatter, Show};

pub mod visit;
pub mod defmap;

#[deriving(Clone)]
pub struct WithId<T> {
    pub id: NodeId,
    pub val: T,
}

impl<T: Eq> Eq for WithId<T> {
    fn eq(&self, other: &WithId<T>) -> bool {
        self.val.eq(&other.val)
    }
}

impl<T: Show> Show for WithId<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.val.fmt(f)
    }
}

macro_rules! with_id {
    ( $( $s:ident => $n:ident ),* ) => ( $( pub type $s = WithId<$n>; )* )
}

with_id! {
    Type     => TypeNode,
    BinOp    => BinOpNode,
    UnOp     => UnOpNode,
    Lit      => LitNode,
    Pat      => PatNode,
    Expr     => ExprNode,
    Stmt     => StmtNode,
    Item     => ItemNode,
    Ident    => IdentNode
}

#[deriving(Eq, Clone, Ord, TotalEq, TotalOrd, Show)]
pub struct NodeId(pub uint);

impl NodeId {
    pub fn to_uint(&self) -> uint {
        let NodeId(did) = *self;
        did
    }
}

#[deriving(Eq, Clone)]
pub struct IdentNode {
    pub tps: Option<Vec<Type>>,
    pub name: Name,
}

impl Show for IdentNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        try!(write!(f, "{}", self.name));
        for tps in self.tps.iter() {
            if tps.len() > 0 {
                try!(write!(f, "<{}>", tps));
            }
        }
        Ok(())
    }
}

#[deriving(Eq, Clone)]
pub struct FieldPat {
    pub name: Name,
    pub pat:  Pat,
}

impl Show for FieldPat {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.pat)
    }
}

#[deriving(Eq, Clone)]
pub enum PatNode {
    DiscardPat(Option<Type>),
    IdentPat(Ident, Option<Type>),
    TuplePat(Vec<Pat>),
    VariantPat(Ident, Vec<Pat>),
    StructPat(Ident, Vec<FieldPat>),
}

impl Show for PatNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            DiscardPat(ref t)             => write!(f, "_{}",
                                                    t.as_ref().map(|t| format!(": {}", t)).unwrap_or("".to_owned())),
            IdentPat(ref id, ref t)       => write!(f, "{}{}", id,
                                                    t.as_ref().map(|t| format!(": {}", t)).unwrap_or("".to_owned())),
            TuplePat(ref args)            => write!(f, "({})", args),
            VariantPat(ref id, ref args)  => write!(f, "{}({})", id, args),
            StructPat(ref id, ref fields) => write!(f, "{} \\{ {} \\}", id, fields),
        }
    }
}

/// Types
#[deriving(Eq, Clone)]
pub enum TypeNode {
    BoolType,
    UnitType,
    IntType(IntKind),
    PtrType(Box<Type>),
    NamedType(Ident),
    FuncType(Vec<Type>, Box<Type>),
    ArrayType(Box<Type>, u64),
    TupleType(Vec<Type>),
}

impl Show for TypeNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            BoolType                  => write!(f, "bool"),
            UnitType                  => write!(f, "()"),
            IntType(k)                => write!(f, "{}", k),
            PtrType(ref t)            => write!(f, "*({})", t),
            NamedType(ref n)          => write!(f, "{}", n),
            FuncType(ref d, ref r)    => write!(f, "({} -> {})", d, r),
            ArrayType(ref t, d)       => write!(f, "({})[{}]", t, d),
            TupleType(ref ts)         => write!(f, "({})", ts),
        }
    }
}

#[deriving(Eq, Clone)]
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

impl Show for BinOpNode {
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

#[deriving(Eq, Clone)]
pub enum UnOpNode {
    Deref,
    AddrOf,
    Negate,
    LogNot,
    BitNot,
}

impl Show for UnOpNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Deref  => "*",
            AddrOf => "&",
            Negate => "-",
            LogNot => "!",
            BitNot => "~",
        })
    }
}

#[deriving(Eq, Clone)]
pub enum LitNode {
    NumLit(u64, IntKind),
    StringLit(~str),
    BoolLit(bool),
}

impl Show for LitNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            NumLit(i, nt)     => write!(f, "{}{}", i, nt),
            StringLit(ref s)  => write!(f, "\"{}\"", s),
            BoolLit(b)        => write!(f, "BoolLit:{}", b),
        }
    }
}

#[deriving(Eq, Clone)]
pub struct MatchArm {
    pub pat: Pat,
    pub body: Expr,
}

impl Show for MatchArm {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.pat, self.body)
    }
}

#[deriving(Eq, Clone)]
pub enum ExprNode {
    UnitExpr,
    LitExpr(Lit),
    GroupExpr(Box<Expr>),
    TupleExpr(Vec<Expr>),
    IdentExpr(Ident),
    BinOpExpr(BinOp, Box<Expr>, Box<Expr>),
    UnOpExpr(UnOp, Box<Expr>),
    IndexExpr(Box<Expr>, Box<Expr>),
    DotExpr(Box<Expr>, Name),
    ArrowExpr(Box<Expr>, Name),
    AssignExpr(Box<Expr>, Box<Expr>),
    CallExpr(Box<Expr>, Vec<Expr>),
    CastExpr(Box<Expr>, Type),
    IfExpr(Box<Expr>, Box<Block>, Box<Block>),
    BlockExpr(Box<Block>),
    ReturnExpr(Box<Expr>),
    WhileExpr(Box<Expr>, Box<Block>),
    ForExpr(Box<Expr>, Box<Expr>, Box<Expr>, Box<Block>),
    MatchExpr(Box<Expr>, Vec<MatchArm>),
}

impl Show for ExprNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            UnitExpr                            => write!(f, "()"),
            LitExpr(ref l)                      => write!(f, "{}", l),
            GroupExpr(ref e)                    => write!(f, "({})", e),
            TupleExpr(ref vs)                   => write!(f, "({})", vs),
            IdentExpr(ref id)                   => write!(f, "{}", id),
            BinOpExpr(op, ref l, ref r)         => write!(f, "({}{}{})", l, op, r),
            UnOpExpr(op, ref e)                 => write!(f, "({}{})", op, e),
            IndexExpr(ref e, ref i)             => write!(f, "{}[{}]", e, i),
            DotExpr(ref e, ref fld)             => write!(f, "{}.{}", e, fld),
            ArrowExpr(ref e, ref fld)           => write!(f, "{}->{}", e, fld),
            AssignExpr(ref lv, ref rv)          => write!(f, "({}={})", lv, rv),
            CallExpr(ref e, ref args)           => write!(f, "{}({})", e, args),
            CastExpr(ref e, ref t)              => write!(f, "({} as {})", e, t),
            IfExpr(ref c, ref bt, ref bf)       => write!(f, "if {} \\{\n    {}\\} else \\{\n    {}\\}", c, bt, bf),
            BlockExpr(ref b)                    => write!(f, "{}", b),
            ReturnExpr(ref e)                   => write!(f, "return {}", e),
            WhileExpr(ref e, ref b)             => write!(f, "while {} {}", e, b),
            ForExpr(ref e1, ref e2, ref e3, ref b) => write!(f, "for ({};{};{}) {}", e1, e2, e3, b),
            MatchExpr(ref e, ref items) => {
                try!(write!(f, "match {} \\{\n", e));
                for item in items.iter() {
                    try!(write!(f, "    {},\n", item));
                }
                write!(f, "{}", "}")
            }
        }
    }
}

#[deriving(Eq, Clone)]
pub enum StmtNode {
    LetStmt(Pat, Option<Expr>),
    ExprStmt(Expr), // no trailing semicolon, must have unit type
    SemiStmt(Expr), // trailing semicolon, any type OK
}

impl Show for StmtNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            LetStmt(ref pat, ref expr) => {
                write!(f, "let {}{};", pat,
                       expr.as_ref().map(|e| format!(" = {}", e))
                       .unwrap_or("".to_owned()))
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

#[deriving(Eq, Clone)]
pub struct Block {
    pub items: Vec<Item>,
    pub stmts: Vec<Stmt>,
    pub expr:  Option<Expr>,
}

impl Show for Block {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        try!(write!(f, "{}\n", "{"));
        for item in self.items.iter() {
            for line in format!("{}", item).lines() {
                try!(write!(f, "    {}\n", line));
            }
        }
        for stmt in self.stmts.iter() {
            for line in format!("{}", stmt).lines() {
                try!(write!(f, "    {};\n", line));
            }
        }
        match self.expr {
            Some(ref e) => {
                for line in format!("{}", e).lines() {
                    try!(write!(f, "    {}\n", line));
                }
            },
            None => {}
        }
        write!(f, "{}", "}")
    }
}

#[deriving(Eq, Clone)]
pub struct FuncArg {
    pub ident:   Ident,
    pub argtype: Type,
}

impl Show for FuncArg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.ident, self.argtype)
    }
}

#[deriving(Eq, Clone)]
pub struct Variant {
    pub ident: Ident,
    pub args: Vec<Type>,
}

impl Show for Variant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        try!(write!(f, "{}(", self.ident));
        for ref argtype in self.args.iter() {
            try!(write!(f, "{}, ", argtype));
        }
        write!(f, ")")
    }
}

#[deriving(Eq, Clone)]
pub struct Field {
    pub name:    Name,
    pub fldtype: Type,
}

impl Show for Field {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.fldtype)
    }
}

#[deriving(Eq, Clone)]
pub enum ItemNode {
    FuncItem(Ident, Vec<FuncArg>, Type, Block, Vec<Ident>),
    StructItem(Ident, Vec<Field>, Vec<Ident>),
    EnumItem(Ident, Vec<Variant>, Vec<Ident>),
}

impl Show for ItemNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            FuncItem(ref id, ref args, ref t, ref def, ref tps) => {
                try!(write!(f, "fn {}", id));
                if tps.len() > 0 {
                    try!(write!(f, "<{}>", tps));
                }
                write!(f, "({}) -> {} {}", args, t, def)
            },
            StructItem(ref id, ref fields, ref tps) => {
                try!(write!(f, "struct {}", id));
                if tps.len() > 0 {
                    try!(write!(f, "<{}>", tps));
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
                    try!(write!(f, "<{}>", tps));
                }
                try!(write!(f, "{}\n", " {"));
                for ref item in items.iter() {
                    try!(write!(f, "    {},\n", item));
                }
                write!(f, "{}", "}")
            }
        }
    }
}

pub struct Module {
    pub items: Vec<Item>
}

impl Show for Module {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for item in self.items.iter() {
            for line in format!("{}", item).lines() {
                try!(write!(f, "{}\n", line));
            }
        }
        Ok(())
    }
}
