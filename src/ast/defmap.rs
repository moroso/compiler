use ast::visit::*;
use ast::*;
use collections::{HashMap,TreeMap};

/// DefMap maps a NodeId to a Def, where a Def is anything that can be defined
/// by an Ident.  This can be used by the Resolver to map the usages of Idents
/// in types and expressions to the things they define.

#[deriving(Show)]
pub enum Def {
    /// Module definition, with the NodeIds of the child items
    ModDef(Vec<NodeId>),

    /// Shorthand type definition
    TypeDef(Type),

    /// Type parameter definition
    GenericDef,

    /// Function definition, with the NodeIds of the args, the return type, and the NodeIds of any type parameters
    FuncDef(Vec<NodeId>, Type, Vec<NodeId>),

    /// Function argument definition (maybe this should be replaced with PatDef?)
    FuncArgDef(Type),

    /// Struct definition, with a map of fields names to their types and the NodeIds of any type parameters
    StructDef(HashMap<AstString, Type>, Vec<NodeId>),

    /// Enum definition, with the NodeIds of the variants and any type parameters
    EnumDef(Vec<NodeId>, Vec<NodeId>),

    /// Enum variant definition, with the NodeId of the owning enum and the types of the arguments
    VariantDef(NodeId, Vec<Type>),

    /// Variable definition bound by a pattern (let statements, match arms)
    PatDef(Option<Type>),
}

pub struct DefMap {
    table: TreeMap<NodeId, Def>,
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
}

/* TODO intern strings so clone is cheaper for NamedTypes */

impl Visitor for DefMap {
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
                self.table.insert(ident.id, PatDef(t.as_ref().map(|t| t.clone())));
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
                    self.table.insert(arg.ident.id, FuncArgDef(arg.argtype.clone()));
                    arg.ident.id
                }).collect();

                let tp_def_ids = tps.iter().map(|tp| {
                    self.table.insert(tp.id, GenericDef);
                    tp.id
                }).collect();

                self.table.insert(ident.id, FuncDef(arg_def_ids, t.clone(), tp_def_ids));

                self.visit_block(def);
            },
            StructItem(ref ident, ref fields, ref tps) => {
                let mut field_map = HashMap::new();
                for field in fields.iter() {
                    field_map.insert(field.name.clone(), field.fldtype.clone());
                }

                let tp_def_ids = tps.iter().map(|tp| {
                    self.table.insert(tp.id, GenericDef);
                    tp.id
                }).collect();

                self.table.insert(ident.id, StructDef(field_map, tp_def_ids));
            },
            EnumItem(ref ident, ref variants, ref tps) => {
                let variant_def_ids = variants.iter().map(|variant| {
                    let args = variant.args.iter().map(|arg| arg.clone()).collect();
                    self.table.insert(variant.ident.id, VariantDef(ident.id, args));
                    variant.ident.id
                }).collect();

                let tp_def_ids = tps.iter().map(|tp| {
                    self.table.insert(tp.id, GenericDef);
                    tp.id
                }).collect();

                self.table.insert(ident.id, EnumDef(variant_def_ids, tp_def_ids));
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
    use collections::TreeMap;

    #[test]
    fn compare_canonicalized() {
        let tree = ast_from_str("fn wot<T>(t: T) { let u = t; }".to_owned(), |p| p.parse_module());
        let mut defmap = DefMap::new();
        defmap.visit_module(&tree);

        assert_eq!(format!("{}", defmap.find(&NodeId(0))),
                   "Some(FuncDef([NodeId(2)], (), [NodeId(1)]))".to_owned());
        assert_eq!(format!("{}", defmap.find(&NodeId(6))),
                   "Some(PatDef(None))".to_owned());
    }
}
