use ast::*;
use ast::visit::*;
use collections::{SmallIntMap, HashMap};

enum NS {
    TypeNS = 0u,
    ValNS,
    StructNS,
    ModNS,
    NS_COUNT // argh rust why can't you provide num_variants::<NS>()
}

struct Subscope {
    namespaces: SmallIntMap<HashMap<~str, DefId>>,
}

impl Subscope {
    fn new() -> Subscope {
        Subscope {
            namespaces: SmallIntMap::with_capacity(NS_COUNT as uint)
        }
    }

    fn insert(&mut self, ns: NS, ident: &Ident) -> bool {
        let ns = ns as uint;

        if !self.namespaces.contains_key(&ns) {
            self.namespaces.insert(ns, HashMap::new());
        }

        let ns = self.namespaces.find_mut(&ns).take_unwrap();

        ns.insert(ident.name.clone(), ident.id)
    }

    fn insert_item(&mut self, item: &Item) {
        match item.val {
            FuncItem(ref ident, ref args, _, _, ref tps) => {
                self.insert(ValNS, ident);
                for arg in args.iter() {
                    self.insert(ValNS, &arg.ident);
                }
                for tp in tps.iter() {
                    self.insert(TypeNS, tp);
                }
            }
            StructItem(_, _, _) => {} // TODO: fill this out.
            EnumItem(_, _, _) => {} // TODO: fill this out.
        }
    }

    fn find(&self, ns: NS, ident: &Ident) -> Option<DefId> {
        let ns = ns as uint;
        self.namespaces.find(&ns)
            .and_then(|ns| ns.find(&ident.name))
            .map(|id| *id)
    }
}

struct Resolver {
    table: SmallIntMap<DefId>,
    scope: Vec<Subscope>,
}

impl Resolver {
    fn new() -> Resolver {
        Resolver {
            table: SmallIntMap::new(),
            scope: vec!(),
        }
    }

    fn descend(&mut self, items: &Vec<Item>, visit: |&mut Resolver|) {
        let mut subscope = Subscope::new();
        for item in items.iter() {
            subscope.insert_item(item);
        }

        self.scope.push(subscope);
        visit(self);
        self.scope.pop();
    }

    fn resolve(&mut self, ns: NS, ident: &Ident) {
        let DefId(id) = ident.id;
        match self.scope.iter().rev()
                               .map(|subscope| { subscope.find(ns, ident) })
                               .skip_while(|did| did.is_none())
                               .next() {
            Some(Some(did)) => self.table.insert(id as uint, did),
            _ => fail!("Unresolved name {}", ident.name),
        };
    }

    fn add_to_scope(&mut self, ns: NS, ident: &Ident) {
        let subscope = self.scope.mut_last().take_unwrap();
        subscope.insert(ns, ident);
    }
}

impl Visitor for Resolver {
    fn visit_type(&mut self, t: &Type) {
        match t.val {
            NamedType(ref ident) => {
                self.resolve(TypeNS, ident);
            }
            _ => walk_type(self, t)
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr.val {
            IdentExpr(ref ident) => {
                self.resolve(ValNS, ident);
            }
            _ => walk_expr(self, expr)
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.val {
            LetStmt(ref ident, _, ref e) => {
                self.add_to_scope(ValNS, ident);
                for e in e.iter() {
                    self.visit_expr(e);
                }
            }
            _ => walk_stmt(self, stmt)
        }
    }

    fn visit_block(&mut self, block: &Block) {
        self.descend(&block.items, |me| {
            for item in block.items.iter() {
                me.visit_item(item);
            }
            for stmt in block.stmts.iter() {
                me.visit_stmt(stmt);
            }
            for expr in block.expr.iter() {
                me.visit_expr(expr);
            }
        });
    }

    fn visit_module(&mut self, module: &Module) {
        self.descend(&module.items, |me| {
            for item in module.items.iter() {
                me.visit_item(item);
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::Resolver;
    use ast::DefId;
    use ast::visit::Visitor;
    use parser::new_from_string;
    use collections::TreeMap;

    #[test]
    fn basic_resolver_test() {
        let mut parser = new_from_string(~"fn wot<T>(t: T) { let u = t; }");
        let tree = parser.parse_module();
        let mut resolver = Resolver::new();
        resolver.visit_module(&tree);
    }

    #[test]
    #[should_fail]
    fn unresolved_name() {
        let mut parser = new_from_string(~"fn lol<T>(t: T) { let u = wot; }"); // unresolved name wot
        let tree = parser.parse_module();
        let mut resolver = Resolver::new();
        resolver.visit_module(&tree);
    }

    #[test]
    #[should_fail]
    fn unresolved_type() {
        let mut parser = new_from_string(~"fn welp<T>(t: U) { let u = t; }"); // unresolved name U
        let tree = parser.parse_module();
        let mut resolver = Resolver::new();
        resolver.visit_module(&tree);
    }
}
