use lexer::{Token, SourceToken};
use span::{Spanned};
use std::fmt::{Formatter, Result, Show};

// Spanned type decls
macro_rules! spanned {
    ( $( $s:ident => $n:ident ),* ) => ( $( pub type $s = Spanned<$n>; )* )
}

spanned! {
    Type     => TypeNode,
    BinOp    => BinOpNode,
    UnOp     => UnOpNode,
    Lit      => LitNode,
    Expr     => ExprNode,
    FuncArg  => FuncArgNode,
    Block    => BlockNode,
    Stmt     => StmtNode,
    Item     => ItemNode
}

#[deriving(Eq, Clone)]
pub struct IntKind {
    pub signedness: Signedness,
    pub width: Width,
}

impl Show for IntKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f.buf, "{}{}", self.signedness.show_char(), self.width)
    }
}

#[deriving(Eq, Show, Clone)]
pub enum Signedness {
    Signed,
    Unsigned,
}

impl Signedness {
    fn show_char(&self) -> &'static str {
        match *self {
            Signed => "i",
            Unsigned => "u"
        }
    }
}

#[deriving(Eq, Clone)]
pub enum Width {
    Width32,
    Width16,
    Width8
}

impl Show for Width {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f.buf, "{}", match *self {
            Width32 => 32,
            Width16 => 16,
            Width8  => 8,
        })
    }
}

/// Types
#[deriving(Eq)]
pub enum TypeNode {
    BoolType,
    UnitType,
    IntType(IntKind),
    PointerTo(~Type),
    NamedType(~str /* module path */),
    FuncType(~Type, ~Type),
    ArrayType(~Type, u64),
    TupleType(Vec<Type>),
    ParamType(~str /* ident */),
}

impl Show for TypeNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            BoolType                => write!(f.buf, "bool"),
            UnitType                => write!(f.buf, "()"),
            IntType(k)              => write!(f.buf, "{}", k),
            PointerTo(ref t)        => write!(f.buf, "*{}", t),
            NamedType(ref n)        => write!(f.buf, "{}", n),
            FuncType(ref d, ref r)  => write!(f.buf, "{} -> {}", d, r),
            ArrayType(ref t, d)     => write!(f.buf, "{}[{}]", t, d),
            TupleType(ref ts)       => write!(f.buf, "({})", ts),
            ParamType(ref n)        => write!(f.buf, "{}", n),
        }
    }
}

#[deriving(Eq)]
pub enum BinOpNode {
    Plus,
    Minus,
    Times,
    Divide,
    Mod,
    Equals,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    AndAlso,
    OrElse,
    BitAnd,
    BitOr,
    BitXor,
}

impl Show for BinOpNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f.buf, "{}", match *self {
            Plus      => "+",
            Minus     => "-",
            Times     => "*",
            Divide    => "/",
            Mod       => "%",
            Equals    => "==",
            Less      => "<",
            LessEq    => "<=",
            Greater   => ">",
            GreaterEq => ">=",
            AndAlso   => "&&",
            OrElse    => "||",
            BitAnd    => "&",
            BitOr     => "|",
            BitXor    => "^",
        })
    }
}

#[deriving(Eq)]
pub enum UnOpNode {
    Deref,
    AddrOf,
}

impl Show for UnOpNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f.buf, "{}", match *self {
            Deref  => "*",
            AddrOf => "&"
        })
    }
}

#[deriving(Eq)]
pub enum LitNode {
    NumLit(u64, IntKind),
    StringLit(~str),
    BoolLit(bool),
    UnitLit,
    TupleLit(Vec<Lit>)
}

impl Show for LitNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            NumLit(i, nt)     => write!(f.buf, "{}{}", i, nt),
            StringLit(ref s)  => write!(f.buf, "\"{}\"", s),
            BoolLit(b)        => write!(f.buf, "{}", b),
            UnitLit           => write!(f.buf, "()"),
            TupleLit(ref vs)  => write!(f.buf, "({})", vs),
        }
    }
}

pub enum ExprNode {
    LitExpr(Lit),
    IdentExpr(~str /* ident */),
    BinOpExpr(BinOp, ~Expr, ~Expr),
    UnOpExpr(UnOp, ~Expr),
    IndexExpr(~Expr, ~Expr),
    FieldExpr(~Expr, ~str /* ident */),
    AssignExpr(~Expr, ~Expr),
    CallExpr(~Expr, Vec<Expr>),
    CastExpr(~Expr, Type),
    IfExpr(~Expr, ~Block, ~Block),
    BlockExpr(~Block),
    ReturnExpr(~Expr),
}

impl Show for ExprNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            LitExpr(ref l)                  => write!(f.buf, "{}", l),
            IdentExpr(ref id)               => write!(f.buf, "{}", id),
            BinOpExpr(op, ref l, ref r)     => write!(f.buf, "({}{}{})", l, op, r),
            UnOpExpr(op, ref e)             => write!(f.buf, "({}{})", op, e),
            IndexExpr(ref e, ref i)         => write!(f.buf, "{}[{}]", e, i),
            FieldExpr(ref e, ref fld)       => write!(f.buf, "{}.{}", e, fld),
            AssignExpr(ref lv, ref rv)      => write!(f.buf, "({}={})", lv, rv),
            CallExpr(ref id, ref args)      => write!(f.buf, "{}({})", id, args),
            CastExpr(ref e, ref t)          => write!(f.buf, "({} as {})", e, t),
            IfExpr(ref c, ref bt, ref bf)   => write!(f.buf, "if {} \\{\n{}\\} else \\{\n{}\\}", c, bt, bf),
            BlockExpr(ref b)                => write!(f.buf, "{}", b),
            ReturnExpr(ref e)               => write!(f.buf, "return {}", e),
        }
    }
}

pub enum StmtNode {
    LetStmt(~str /* ident now, pattern eventually */, Type, Option<Expr>),
    ExprStmt(Expr), // no trailing semicolon, must have unit type
    SemiStmt(Expr), // trailing semicolon, any type OK
}

impl Show for StmtNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            LetStmt(ref id, ref t, ref expr) => {
                try!(write!(f.buf, "let {}: {}", id, t));
                match *expr {
                    Some(ref e) => { try!(write!(f.buf, " = {}", e)); },
                    None => {}
                }
                try!(write!(f.buf, ";"));
            },
            ExprStmt(ref e) => {
                try!(write!(f.buf, "{}", e));
            },
            SemiStmt(ref e) => {
                try!(write!(f.buf, "{};", e));
            },
        }
        Ok(())
    }
}

pub struct BlockNode {
    items: Vec<Item>,
    stmts: Vec<Stmt>,
    expr:  Option<Expr>,
}

impl Show for BlockNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        try!(write!(f.buf, "{}\n", "{"));
        for item in self.items.iter() {
            for line in format!("{}", item).lines() {
                try!(write!(f.buf, "    {}\n", line));
            }
        }
        for stmt in self.stmts.iter() {
            for line in format!("{}", stmt).lines() {
                try!(write!(f.buf, "    {}\n", line));
            }
        }
        match self.expr {
            Some(ref e) => {
                for line in format!("{}", e).lines() {
                    try!(write!(f.buf, "    {}\n", line));
                }
            },
            None => {}
        }
        write!(f.buf, "{}", "}")
    }
}

pub struct FuncArgNode {
    name: ~str /* ident */,
    argtype: Type
}

impl Show for FuncArgNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f.buf, "{}: {}", self.name, self.argtype)
    }
}

pub enum ItemNode {
    FuncItem(~str /* ident */, Vec<FuncArg>, Type, Block, Vec<~str>),
}

impl Show for ItemNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            FuncItem(ref id, ref args, ref t, ref def, ref tps) => {
                try!(write!(f.buf, "fn {}", id));
                if tps.len() > 0 {
                    try!(write!(f.buf, "<{}>", tps));
                }
                try!(write!(f.buf, "({}) -> {} {}", args, t, def))
            }
        }
        Ok(())
    }
}
