use mc::ast::{LitNode, BinOpNode, UnOpNode};

use std::fmt::{Formatter, Result, Show};
use std::collections::TreeSet;

use util::{Name, Width};

pub mod ast_to_intermediate;
pub mod liveness;
pub mod constant_fold;
pub mod ssa;
pub mod util;
pub mod conflicts;

#[deriving(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Var {
    pub name: Name,
    // If set, stores the generation of the variable. This will be None
    // if we're not yet in SSA form, and must be non-None for SSA.
    pub generation: Option<uint>,
}

impl Show for Var {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self.generation {
            Some(g) => write!(f, "{}<{}>", self.name, g),
            None => write!(f, "{}", self.name),
        }
    }
}

#[deriving(Clone)]
pub enum LValue {
    // Store directly into a variable.
    VarLValue(Var),
    // Store into the location pointed to by Var.
    PtrLValue(Var),
}

impl Show for LValue {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            VarLValue(ref v) => write!(f, "{}", v),
            PtrLValue(ref v) => write!(f, "*{}", v),
        }
    }
}

#[deriving(Eq, PartialEq, Clone)]
// An "element" of an RValue: either a variable or a constant.
pub enum RValueElem {
    Variable(Var),
    Constant(LitNode),
}

impl RValueElem {
    pub fn is_variable(&self) -> bool {
        match *self {
            Variable(..) => true,
            Constant(..) => false,
        }
    }
}

impl Show for RValueElem {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            Variable(ref v) => write!(f, "{}", v),
            Constant(ref l) => write!(f, "{}", l),
        }
    }
}

#[deriving(Clone)]
pub enum Op {
    // Apply a unary operator
    UnOp(Var, UnOpNode, RValueElem),
    // Apply a binary operator
    BinOp(Var, BinOpNode, RValueElem, RValueElem),
    Alloca(Var, u64),
    Call(Var, RValueElem, Vec<RValueElem>),
    // Store to memory address pointed to by first Var.
    Store(Var, Var, Width),
    // Load from the address pointed to by the second Var.
    Load(Var, Var, Width),
    // A label. The set of variables is ones that are active at that point.
    // TODO: make this a map from name -> gen.
    Label(uint, TreeSet<Var>),
    // A goto. The set must specify generations for all variables in the label.
    Goto(uint, TreeSet<Var>),
    // Conditional goto.
    CondGoto(RValueElem, uint, TreeSet<Var>),
    // Return statement.
    Return(RValueElem),
    // Function definition. A special op, that can only appear once, at
    // the beginning of a function.
    Func(Name, Vec<Var>),
    Nop,
}

impl Show for Op {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            UnOp(ref lv, ref op, ref rve) =>
                write!(f, "{: >12} := {}({})\n",
                       format!("{}", lv), op, rve),
            BinOp(ref lv, ref op, ref rve1, ref rve2) =>
                write!(f, "{: >12} := {} {} {}\n",
                       format!("{}", lv), rve1, op, rve2),
            Store(ref lv, ref rv, ref size) =>
                write!(f, "{: >12} :={} {}\n",
                       format!("*{}", lv), size, rv),
            Load(ref lv, ref rv, ref size) =>
                write!(f, "{: >12} :={} *{}\n",
                       format!("{}", lv), size, rv),
            Call(ref lv, ref fname, ref args) =>
                write!(f, "{: >12} := {}({})\n",
                       format!("{}", lv), fname, args),
            Alloca(ref v, ref size) =>
                write!(f, "{: >12} := alloca({})",
                       format!("{}", v), size),
            Label(ref l, ref vars) => write!(f, "{}({}):\n", l, vars),
            Goto(ref l, ref vars) => write!(f, "goto {}({})\n", l, vars),
            CondGoto(ref e, ref l, ref vars) => write!(f, "if {} goto {}({})\n",
                                                       e, l, vars),
            Return(ref v) => write!(f, "return {}\n", v),
            Func(ref name, ref vars) => write!(f, "fn {}({})\n", name, vars),
            Nop => write!(f, "{: >16}\n", "nop"),
        }
    }
}

#[deriving(Show, Clone)]
pub struct OpInfo {
    pub live: TreeSet<Var>, // Which variables are live at this instruction?
    pub used: TreeSet<Var>, // Which variables are used?
    pub def: TreeSet<Var>, // Which variables are defined here?
    pub succ: TreeSet<uint>, // Instructions which can follow this one.
}
