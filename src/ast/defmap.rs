use ast::visit::*;
use ast::*;
use collections::TreeMap;
use Type = ast::TypeNode;

#[deriving(Show)]
pub enum Def {
    TypeDef(Type),
    FuncDef(Type, Vec<DefId>, Vec<DefId>),
    LetDef(Option<Type>),
    FuncArgDef(Type),
    ModDef(Vec<DefId>),
}

pub struct DefMap {
    table: TreeMap<DefId, Def>,
}

impl DefMap {
    pub fn new() -> DefMap {
        DefMap {
            table: TreeMap::new(),
        }
    }

    pub fn find<'a>(&'a self, id: &DefId) -> Option<&'a Def> {
        self.table.find(id)
    }
}

/*
macro_rules! descend {
    ( $id:expr, $b:expr ) => ( { let old_parent = self.parent; self.parent = $id; self.visit_block($b); self.parent = old_parent; } );
}
*/

/* TODO intern strings so clone is cheaper for NamedTypes */

impl Visitor for DefMap {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.val {
            LetStmt(ref ident, ref t, ref e) => {
                self.table.insert(ident.id, LetDef(t.as_ref().map(|t| t.val.clone())));

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
                    self.visit_func_arg(arg);
                    arg.ident.id
                }).collect();

                let tp_def_ids = tps.iter().map(|tp| {
                    self.table.insert(tp.id, TypeDef(UnitType)); // would be ParamType or somesuch if we had things like trait bounds
                    tp.id
                }).collect();

                self.table.insert(ident.id, FuncDef(t.val.clone(), arg_def_ids, tp_def_ids));

                self.visit_block(def);
            },
            StructItem(_, _, _) => {} // TODO: fill this out.
        }
    }
}

#[cfg(test)]
mod tests {
    use super::DefMap;
    use ast::DefId;
    use ast::visit::Visitor;
    use parser::new_from_string;
    use collections::TreeMap;

    #[test]
    fn compare_canonicalized() {
        let mut parser = new_from_string(~"fn wot<T>(t: T) { let u = t; }");
        let tree = parser.parse_module();
        let mut defmap = DefMap::new();
        defmap.visit_module(&tree);

        assert_eq!(format!("{}", defmap.find(&DefId(0))), ~"Some(FuncDef((), [DefId(2)], [DefId(1)]))");
        assert_eq!(format!("{}", defmap.find(&DefId(4))), ~"Some(LetDef(None))");
    }
}
