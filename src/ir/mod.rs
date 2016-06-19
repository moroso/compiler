use mc::ast::{LitNode, BinOpNode, UnOpNode, Expr};

use std::fmt;
use std::fmt::{Formatter, Result, Display, Debug};
use std::collections::BTreeSet;

use util::{Name, Width};

use mas::ast::InstNode;
use mc::ast::{NodeId, WithIdT};

pub use self::LValue::*;
pub use self::RValueElem::*;
use self::OpNode::*;

pub mod ast_to_intermediate;
pub mod liveness;
pub mod constant_fold;
pub mod ssa;
pub mod util;
pub mod conflicts;
pub mod multiply_optimizer;
pub mod dead_code;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct StaticIRItem {
    pub name: VarName,
    pub label: Option<Name>,
    pub size: usize,
    pub offset: Option<usize>,
    pub is_extern: bool,
    pub is_ref: bool, // Should this variable be a pointer to global space, or
                      // is it actually stored in global space?
    pub is_func: bool, // Is this a function? (If so, size and is_ref are
                       // ignored).
    pub expr: Option<Expr>,
}

allow_string!(StaticIRItem);

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Copy)]
pub enum VarName {
    // Named variable that was present in the source
    NamedVariable(Name, NodeId),
    // Variable referencing a symbol that's builtin/external and has no defid.
    MangledVariable(Name),
    // Temp variable created during conversion to IR
    IRTempVariable(usize),
    // Temp variable created during optimization passes
    OptTempVariable(usize),
}

impl Display for VarName {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            VarName::NamedVariable(name, id) => write!(f, "{}[{}]", name, id),
            VarName::MangledVariable(name) => write!(f, "<BUILTIN {}>", name),
            VarName::IRTempVariable(idx) => write!(f, "<TEMP {}>", idx),
            VarName::OptTempVariable(idx) => write!(f, "<OPT_TEMP {}>", idx)
        }
    }
}

impl VarName {
    pub fn canonical_name(&self) -> String {
        match *self {
            VarName::NamedVariable(name, nodeid) => format!("VAR_{}_{}", name, nodeid.to_uint()),
            VarName::MangledVariable(name) => format!("{}", name),
            VarName::IRTempVariable(idx) => format!("TEMP{}", idx),
            VarName::OptTempVariable(idx) => format!("OPT_TEMP{}", idx),
        }
    }

    pub fn base_name(&self) -> String {
        match *self {
            VarName::NamedVariable(name, _) => format!("{}", name),
            VarName::MangledVariable(name) => format!("{}", name),
            VarName::IRTempVariable(idx) => format!("TEMP{}", idx),
            VarName::OptTempVariable(idx) => format!("OPT_TEMP{}", idx),
        }
    }

}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Copy)]
pub struct Var {
    pub name: VarName,
    // If set, stores the generation of the variable. This will be None
    // if we're not yet in SSA form, and must be non-None for SSA.
    pub generation: Option<usize>,
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self.generation {
            Some(g) => write!(f, "{}<{}>", self.name, g),
            None => write!(f, "{}", self.name),
        }
    }
}

impl Var {
    pub fn canonical_name(&self) -> String {
        match self.generation {
            Some(gen) => format!("{}_{}", self.name.canonical_name(), gen),
            None => format!("{}", self.name.canonical_name()),
        }
    }
}

#[derive(Clone)]
pub enum LValue {
    // Store directly into a variable.
    VarLValue(Var),
    // Store into the location pointed to by Var.
    PtrLValue(Var),
}

impl Display for LValue {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            VarLValue(ref v) => write!(f, "{}", v),
            PtrLValue(ref v) => write!(f, "*{}", v),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
// An "element" of an RValue: either a variable or a constant.
pub enum RValueElem {
    Variable(Var),
    // TODO: this should probably just be a u32 or something.
    // The IR doesn't need to carry type information in constants
    // (it carries signedness in binops, which is all we need).
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

impl Display for RValueElem {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            Variable(ref v) => write!(f, "{}", v),
            Constant(ref l) => write!(f, "{}", l),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Ord, PartialOrd, Debug, Copy)]
pub struct IrNodeId(pub usize);

allow_string!(IrNodeId);

impl IrNodeId {
    pub fn to_uint(&self) -> usize {
        let IrNodeId(did) = *self;
        did
    }
}

pub type Op = WithIdT<IrNodeId, OpNode>;

#[derive(Clone, Debug)]
pub enum OpNode {
    // Apply a unary operator
    UnOp(Var, UnOpNode, RValueElem),
    // Apply a binary operator. We store whether it's signed
    BinOp(Var, BinOpNode, RValueElem, RValueElem, bool),
    Alloca(Var, u64),
    Call(Var, RValueElem, Vec<Var>),
    // Store to memory address pointed to by first Var.
    Store(Var, Var, Width),
    // Load from the address pointed to by the second Var.
    Load(Var, Var, Width),
    // A label. The set of variables is ones that are active at that point.
    // TODO: make this a map from name -> gen.
    Label(usize, BTreeSet<Var>),
    // A goto. The set must specify generations for all variables in the label.
    Goto(usize, BTreeSet<Var>),
    // Conditional goto. Optionally negated.
    CondGoto(bool, RValueElem, usize, BTreeSet<Var>),
    // Return statement.
    Return(RValueElem),
    // Function definition. A special op, that can only appear once, at
    // the beginning of a function. The name option gives the ABI name in
    // the case of externs; None specifies a local function.
    Func(VarName, Vec<Var>, Option<Name>),
    // Inline asm.
    AsmOp(Vec<Vec<InstNode>>),
    Nop,
}

fn write_list<T: Display, U: Iterator<Item=T>>(iter: U) -> String {
    let mut result = "".to_string();
    for item in iter {
        result = result + &format!("{}, ", item);
    }

    result
}

impl Display for OpNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            UnOp(ref lv, ref op, ref rve) =>
                write!(f, "{: >12} := {}({})\n",
                       format!("{}", lv), op, rve),
            BinOp(ref lv, ref op, ref rve1, ref rve2, signed) =>
                write!(f, "{: >12} := {} {}{} {}\n",
                       format!("{}", lv), rve1, op,
                       if signed { "s" } else { "u" }, rve2),
            Store(ref lv, ref rv, ref size) =>
                write!(f, "{: >12} :={} {}\n",
                       format!("*{}", lv), size, rv),
            Load(ref lv, ref rv, ref size) =>
                write!(f, "{: >12} :={} *{}\n",
                       format!("{}", lv), size, rv),
            Call(ref lv, ref fname, ref args) =>
                write!(f, "{: >12} := {}({})\n",
                       format!("{}", lv), fname, write_list(args.iter())),
            Alloca(ref v, ref size) =>
                write!(f, "{: >12} := alloca({})\n",
                       format!("{}", v), size),
            Label(ref l, ref vars) => write!(f, "{}({}):\n", l, write_list(vars.iter())),
            Goto(ref l, ref vars) => write!(f, "goto {}({})\n", l, write_list(vars.iter())),
            CondGoto(ref neg, ref e, ref l, ref vars) =>
                write!(f, "if {}{} goto {}({})\n",
                       if *neg { "!" } else { "" }, e, l, write_list(vars.iter())),
            Return(ref v) => write!(f, "return {}\n", v),
            Func(ref name, ref vars, ref abi_opt) =>
                write!(f, "{}fn {}({})\n",
                       match *abi_opt {
                           Some(ref abi) => format!("extern \"{}\"", abi),
                           None => "".to_string(),
                       }, name, write_list(vars.iter())),
            Nop => write!(f, "{: >16}\n", "nop"),
            AsmOp(ref packets) => {
                try!(write!(f, "Asm("));
                for packet in packets.iter() {
                    try!(write!(f, "[ "));
                    for inst in packet.iter() {
                        try!(write!(f, "{}", inst));
                    }
                    try!(write!(f, "]"));
                }
                write!(f, ")\n;")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct OpInfo {
    pub live: BTreeSet<Var>, // Which variables are live at this instruction?
    pub used: BTreeSet<Var>, // Which variables are used?
    pub def: BTreeSet<Var>, // Which variables are defined here?
    pub succ: BTreeSet<usize>, // Instructions which can follow this one.
}

allow_string!(OpInfo);

impl OpInfo {
    fn new() -> OpInfo {
        OpInfo {
            live: BTreeSet::new(),
            used: BTreeSet::new(),
            def: BTreeSet::new(),
            succ: BTreeSet::new(),
        }
    }
}
