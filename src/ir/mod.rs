use std::fmt::{Formatter, Result, Show};
use ast::{LitNode, BinOpNode};
use collections::TreeSet;

use util::Name;

pub mod ast_to_intermediate;
pub mod liveness;
pub mod constant_fold;
pub mod ssa;

#[deriving(Clone, TotalEq, PartialEq, TotalOrd, PartialOrd)]
pub struct Var {
    pub name: Name,
    pub generation: Option<uint>, // Used for SSA
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
    VarLValue(Var),
    PtrLValue(Var), // Location pointed to by Var.
}

impl Show for LValue {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            VarLValue(ref v) => write!(f, "{}", v),
            PtrLValue(ref v) => write!(f, "*{}", v),
        }
    }
}

#[deriving(TotalEq, PartialEq, Clone)]
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


#[deriving(TotalEq, PartialEq, Clone)]
pub enum RValue {
    BinOpRValue(BinOpNode, RValueElem, RValueElem),
    DirectRValue(RValueElem),
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
            DirectRValue(ref d) => write!(f, "{: >12}",
                                         format!("{}", d)),
        }
    }
}

#[deriving(Clone)]
pub enum Op {
    Assign(LValue, RValue), // t1 := t2 op t3
    Label(uint, TreeSet<Var>),
    Goto(uint, TreeSet<Var>),
    CondGoto(RValueElem, uint, TreeSet<Var>),
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
            Nop => write!(f, "{: >16}\n", "nop"),
        }
    }
}

#[deriving(Show, TotalEq, PartialEq, Clone)]
pub struct OpInfo {
    live: Vec<Var>, // Which variables are live at this instruction?
    used: Vec<Var>, // Which variables are used?
    def: Vec<Var>, // Which variables are defined here?
    succ: Vec<uint>, // Instructions which can follow this one.
}
