use std::collections::TreeMap;
use util::Name;

use std::fmt;
use std::fmt::{Formatter, Show};

use ast::visit::*;
use ast::*;

/// DefMap maps a NodeId to a Def, where a Def is anything that can be defined
/// by an Ident.  This can be used by the Resolver to map the usages of Idents
/// in types and expressions to the things they define.

#[deriving(Show)]
pub enum Def {
    /// Module definition, with its qualified name and the NodeIds of the child items
    ModDef(Vec<Name>, Vec<NodeId>),

    /// Shorthand type definition
    TypeDef(Type),

    /// Type parameter definition
    GenericDef,

    /// Function definition, with the NodeIds of the args, the return
    /// type, and the NodeIds of any type parameters
    FuncDef(Vec<NodeId>, Type, Vec<NodeId>),

    /// Function argument definition (maybe this should be replaced with PatDef?)
    FuncArgDef(Type),

    /// Struct definition, with its qualified name, a vector of fields names
    /// and their types, and the NodeIds of any type parameters
    StructDef(Vec<Name>, Vec<(Name, Type)>, Vec<NodeId>),

    /// Enum definition, with its qualified name, the NodeIds of the
    /// variants and any type parameters
    EnumDef(Vec<Name>, Vec<NodeId>, Vec<NodeId>),

    /// Enum variant definition, with its qualified name, the NodeId
    /// of the owning enum and the types of the arguments
    VariantDef(Vec<Name>, NodeId, Vec<Type>),

    /// Variable definition bound by a pattern (let statements, match arms)
    PatDef(Option<Type>),
}

pub struct DefMap {
    table: TreeMap<NodeId, Def>,
}

struct DefMapVisitor<'a> {
    defmap: &'a mut DefMap,
    qualifier: Vec<Name>,
}

impl DefMap {
    pub fn new() -> DefMap {
        DefMap {
            table: TreeMap::new(),
        }
    }

    pub fn find<'a>(&'a self, id: &NodeId) -> Option<&'a Def> {
        self.table.find(id)
    }

    pub fn read_module(&mut self, module: &Module) {
        let mut visitor = DefMapVisitor {
            defmap: self,
            qualifier: vec!(),
        };

        visitor.visit_module(module);
    }
}

impl<'a> DefMapVisitor<'a> {
    fn make_qualified_name(&self, name: Name) -> Vec<Name> {
        let mut qn = self.qualifier.clone();
        qn.push(name);
        qn
    }
}

impl<'a> Visitor for DefMapVisitor<'a> {
    fn visit_expr(&mut self, expr: &Expr) {
        match expr.val {
            MatchExpr(ref e, ref arms) => {
                walk_expr(self, *e);
                for arm in arms.iter() {
                    self.visit_pat(&arm.pat);
                    self.visit_expr(&arm.body);
                }
            }
            _ => { walk_expr(self, expr); }
        }
    }

    fn visit_pat(&mut self, pat: &Pat) {
        match pat.val {
            IdentPat(ref ident, ref t) => {
                self.defmap.table.insert(ident.id, PatDef(t.as_ref().map(|t| t.clone())));
            }
            _ => { walk_pat(self, pat); }
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.val {
            LetStmt(ref pat, ref e) => {
                self.visit_pat(pat);
                for e in e.iter() {
                    self.visit_expr(e);
                }
            }
            _ => { walk_stmt(self, stmt); }
        }

    }

    fn visit_item(&mut self, item: &Item) {
        match item.val {
            FuncItem(ref ident, ref args, ref t, ref def, ref tps) => {
                let arg_def_ids = args.iter().map(|arg| {
                    self.defmap.table.insert(arg.ident.id, FuncArgDef(arg.argtype.clone()));
                    arg.ident.id
                }).collect();

                let tp_def_ids = tps.iter().map(|tp| {
                    self.defmap.table.insert(tp.id, GenericDef);
                    tp.id
                }).collect();

                self.defmap.table.insert(ident.id, FuncDef(arg_def_ids, t.clone(), tp_def_ids));

                self.qualifier.push(ident.val.name);
                for def in def.iter() { self.visit_block(def); }
                self.qualifier.pop();
            },
            StructItem(ref ident, ref fields, ref tps) => {
                let mut field_map = vec!();
                for field in fields.iter() {
                    field_map.push((field.name, field.fldtype.clone()));
                }

                let tp_def_ids = tps.iter().map(|tp| {
                    self.defmap.table.insert(tp.id, GenericDef);
                    tp.id
                }).collect();

                let qn = self.make_qualified_name(ident.val.name);
                self.defmap.table.insert(ident.id, StructDef(qn, field_map, tp_def_ids));
            },
            EnumItem(ref ident, ref variants, ref tps) => {
                let variant_def_ids = variants.iter().map(|variant| {
                    let args = variant.args.iter().map(|arg| arg.clone()).collect();
                    let qn = self.make_qualified_name(variant.ident.val.name);
                    self.defmap.table.insert(variant.ident.id, VariantDef(qn, ident.id, args));
                    variant.ident.id
                }).collect();

                let tp_def_ids = tps.iter().map(|tp| {
                    self.defmap.table.insert(tp.id, GenericDef);
                    tp.id
                }).collect();

                let qn = self.make_qualified_name(ident.val.name);
                self.defmap.table.insert(ident.id, EnumDef(qn, variant_def_ids, tp_def_ids));
            }
            ModItem(ref ident, ref module) => {
                let item_ids = module.val.items.iter().map(|item| item.id).collect();
                let qn = self.make_qualified_name(ident.val.name);
                self.defmap.table.insert(ident.id, ModDef(qn, item_ids));

                self.qualifier.push(ident.val.name);
                self.visit_module(module);
                self.qualifier.pop();
            }
            StaticItem(ref ident, ref ty, ref expr) => {
                self.defmap.table.insert(ident.id, PatDef(Some(ty.clone())));

                self.visit_type(ty);

                for e in expr.iter() {
                    self.visit_expr(e);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::DefMap;
    use ast::NodeId;
    use ast::visit::Visitor;
    use parser::ast_from_str;
    use std::collections::TreeMap;

    #[test]
    fn compare_canonicalized() {
        let (_, tree) = ast_from_str("fn wot<T>(t: T) { let u = t; }", |p| p.parse_module());
        let mut defmap = DefMap::new();
        defmap.read_module(&tree);

        assert_eq!(format!("{}", defmap.find(&NodeId(0))).as_slice(),
                   "Some(FuncDef([NodeId(2)], (), [NodeId(1)]))");
        assert_eq!(format!("{}", defmap.find(&NodeId(7))).as_slice(),
                   "Some(PatDef(None))");
    }
}
