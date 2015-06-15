use util::Name;
use mc::session::Session;

use std::fmt;
use std::fmt::{Formatter, Display, Debug};
use std::collections::BTreeMap;

use super::visitor::*;
use super::*;

/// DefMap maps a NodeId to a Def, where a Def is anything that can be defined
/// by an Ident.  This can be used by the Resolver to map the usages of Idents
/// in types and expressions to the things they define.

#[derive(Debug, Clone)]
pub enum Def {
    /// Module definition, with its qualified name and the NodeIds of the child items
    ModDef(Vec<Name>, Vec<NodeId>),

    /// Module definition, with its qualified name and the qualified name of the thing it imports
    UseDef(Vec<Name>, Vec<Name>),

    /// Shorthand type definition
    TypeDef(Type),

    /// Type parameter definition
    GenericDef,

    /// Function definition, with the NodeIds of the args, the return
    /// type, the extern ABI if any, and the NodeIds of any type parameters
    FuncDef(Vec<NodeId>, Type, Option<Name>, Vec<NodeId>),

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

    /// Constant definition
    ConstDef(Type),
}

allow_string!(Def);

pub struct DefMap {
    table: BTreeMap<NodeId, Def>,
}

struct DefMapVisitor<'a, 'b: 'a> {
    session: &'a mut Session<'b>,
    qualifier: Vec<Name>,
}

impl DefMap {
    pub fn new() -> DefMap {
        DefMap {
            table: BTreeMap::new(),
        }
    }

    pub fn find<'a>(&'a self, id: &NodeId) -> Option<&'a Def> {
        self.table.get(id)
    }

    pub fn record(session: &mut Session, module: &Module) {
        let mut visitor = DefMapVisitor {
            session: session,
            qualifier: vec!(),
        };

        visitor.visit_module(module);
    }
}

impl<'a, 'b> DefMapVisitor<'a, 'b> {
    fn make_qualified_name(&self, name: Name) -> Vec<Name> {
        let mut qn = self.qualifier.clone();
        qn.push(name);
        qn
    }
}

impl<'a, 'b> Visitor for DefMapVisitor<'a, 'b> {
    fn visit_expr(&mut self, expr: &Expr) {
        match expr.val {
            MatchExpr(ref e, ref arms) => {
                walk_expr(self, &**e);
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
                self.session.defmap.table.insert(ident.id, Def::PatDef(t.as_ref().map(|t| t.clone())));
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
                    self.session.defmap.table.insert(arg.ident.id, Def::FuncArgDef(arg.argtype.clone()));
                    arg.ident.id
                }).collect();

                let tp_def_ids = tps.iter().map(|tp| {
                    self.session.defmap.table.insert(tp.id, Def::GenericDef);
                    tp.id
                }).collect();

                let abi = match *def {
                    LocalFn(..) => None,
                    ExternFn(abi) => Some(abi),
                };

                self.session.defmap.table.insert(ident.id, Def::FuncDef(arg_def_ids, t.clone(), abi, tp_def_ids));

                self.qualifier.push(ident.val.name);

                match *def {
                    LocalFn(ref block) => self.visit_block(block),
                    ExternFn(..) => {}
                }
                self.qualifier.pop();
            },
            StructItem(ref ident, ref fields, ref tps) => {
                let mut field_map = vec!();
                for field in fields.iter() {
                    field_map.push((field.name, field.fldtype.clone()));
                }

                let tp_def_ids = tps.iter().map(|tp| {
                    self.session.defmap.table.insert(tp.id, Def::GenericDef);
                    tp.id
                }).collect();

                let qn = self.make_qualified_name(ident.val.name);
                self.session.defmap.table.insert(ident.id, Def::StructDef(qn, field_map, tp_def_ids));
            },
            EnumItem(ref ident, ref variants, ref tps) => {
                let variant_def_ids = variants.iter().map(|variant| {
                    let args = variant.args.iter().map(|arg| arg.clone()).collect();
                    let qn = self.make_qualified_name(variant.ident.val.name);
                    self.session.defmap.table.insert(variant.ident.id, Def::VariantDef(qn, ident.id, args));
                    variant.ident.id
                }).collect();

                let tp_def_ids = tps.iter().map(|tp| {
                    self.session.defmap.table.insert(tp.id, Def::GenericDef);
                    tp.id
                }).collect();

                let qn = self.make_qualified_name(ident.val.name);
                self.session.defmap.table.insert(ident.id, Def::EnumDef(qn, variant_def_ids, tp_def_ids));
            }
            TypeItem(ref ident, ref t, _) => {
                self.session.defmap.table.insert(ident.id, Def::TypeDef(t.clone()));
            }
            ModItem(ref ident, ref module) => {
                let item_ids = module.val.items.iter().map(|item| item.id).collect();
                let qn = self.make_qualified_name(ident.val.name);
                self.session.defmap.table.insert(ident.id, Def::ModDef(qn, item_ids));

                self.qualifier.push(ident.val.name);
                self.visit_module(module);
                self.qualifier.pop();
            }
            StaticItem(ref ident, ref ty, ref expr, _) => {
                self.session.defmap.table.insert(ident.id, Def::PatDef(Some(ty.clone())));

                self.visit_type(ty);

                for e in expr.iter() {
                    self.visit_expr(e);
                }
            }
            ConstItem(ref ident, ref ty, ref expr) => {
                self.session.defmap.table.insert(ident.id, Def::ConstDef(ty.clone()));
                self.visit_type(ty);
                self.visit_expr(expr);
            }
            UseItem(ref import) => {
                // Hm. When is this important? We can't do anything good about *.
                match import.val.import {
                    ImportAll => {}
                    ImportNames(ref names) => {
                        for ident in names.iter() {
                            let qn = self.make_qualified_name(ident.val.name);
                            let mut path_qn: Vec<Name> =
                                import.val.elems.iter().map(|e| e.val.name).collect();
                            path_qn.push(ident.val.name);
                            self.session.defmap.table.insert(ident.id, Def::UseDef(qn, path_qn));
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::DefMap;
    use mc::ast::NodeId;
    use mc::ast::visitor::Visitor;
    use mc::parser::ast_from_str;
    use std::collections::BTreeMap;

    #[test]
    fn compare_canonicalized() {
        let (mut session, tree) = ast_from_str("fn wot<T>(t: T) { let u = t; }", |p| p.parse_module());
        DefMap::record(&mut session, &tree);

        assert_eq!(&format!("{:?}", session.defmap.find(&NodeId(0)))[..],
                   "Some(FuncDef([NodeId(2)], UnitType, None, [NodeId(1)]))");
        assert_eq!(&format!("{:?}", session.defmap.find(&NodeId(7)))[..],
                   "Some(PatDef(None))");
    }
}
