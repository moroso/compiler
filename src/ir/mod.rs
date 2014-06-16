use mc::ast::{LitNode, BinOpNode, UnOpNode};

use std::fmt::{Formatter, Result, Show};
use std::collections::TreeSet;

use util::Name;

pub mod ast_to_intermediate;
pub mod liveness;
pub mod constant_fold;
pub mod ssa;
pub mod util;

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

impl Show for RValueElem {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            Variable(ref v) => write!(f, "{}", v),
            Constant(ref l) => write!(f, "{}", l),
        }
    }
}

#[deriving(Eq, PartialEq, Clone)]
pub enum RValue {
    // A binary operation applied to two RValueElems
    BinOpRValue(BinOpNode, RValueElem, RValueElem),
    // A unary operation applied to an RValueElem
    UnOpRValue(UnOpNode, RValueElem),
    // An RValueElem (variable or constant) itself.
    DirectRValue(RValueElem),
    // Function call
    CallRValue(RValueElem, Vec<RValueElem>),
    // Allocate space on the stack. Used for structures and enums.
    AllocaRValue(u64),
}

impl Show for RValue {
    fn fmt(&self, f: &mut Formatter) -> Result { 
        match *self {
            BinOpRValue(ref op, ref v1, ref v2) =>
                write!(f, 
                       "{: >12} {} {: <12}",
                       format!("{}", v1),
                       op,
                       format!("{}", v2)),
            UnOpRValue(ref op, ref v1) =>
                write!(f,
                       "{} {: >12}",
                       op,
                       format!("{}", v1)),
            DirectRValue(ref d) => write!(f, "{: >12}",
                                         format!("{}", d)),
            CallRValue(ref rve, ref args) => {
                try!(write!(f, "call {}(", rve));
                for arg in args.iter() {
                    try!(write!(f, "{},", arg));
                }
                try!(write!(f, ")"));
                Ok(())
            },
            AllocaRValue(ref size) => {
                write!(f, "alloca {}", size)
            }
        }
    }
}

#[deriving(Clone)]
pub enum Op {
    // A basic assignment.
    Assign(LValue, RValue),
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
            Assign(ref lv, ref rv) =>
                write!(f, "{: >12} := {}\n", format!("{}", lv), rv),
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
