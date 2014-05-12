use ast::*;
use ast::visit::*;
use collections::{SmallIntMap, HashMap};

//#[allow(non_camel_case_types)] leaving the warning so we remember to patch rust later
pub enum NS {
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

    fn find(&self, ns: NS, ident: &Ident) -> Option<DefId> {
        let ns = ns as uint;
        self.namespaces.find(&ns)
            .and_then(|ns| ns.find(&ident.name))
            .map(|id| *id)
    }
}

pub struct Resolver {
    table: SmallIntMap<DefId>,
    scope: Vec<Subscope>,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            table: SmallIntMap::new(),
            scope: vec!(),
        }
    }

    fn descend(&mut self, items: Option<&Vec<Item>>, visit: |&mut Resolver|) {
        let mut subscope = Subscope::new();

        match items {
            Some(items) => {
                for item in items.iter() {
                    match item.val {
                        FuncItem(ref ident, _, _, _, _) => {
                            subscope.insert(ValNS, ident);
                        }
                        StructItem(ref ident, _, _) => {
                            subscope.insert(TypeNS, ident);
                            subscope.insert(StructNS, ident);
                        }
                        EnumItem(ref ident, ref variants, _) => {
                            subscope.insert(TypeNS, ident);
                            for variant in variants.iter() {
                                subscope.insert(ValNS, &variant.ident);
                            }
                        }
                    }
                }
            }
            None => {}
        }

        self.scope.push(subscope);
        visit(self);
        self.scope.pop();
    }

    fn resolve(&mut self, ns: NS, ident: &Ident) {
        match self.scope.iter().rev()
                               .map(|subscope| { subscope.find(ns, ident) })
                               .skip_while(|did| did.is_none())
                               .next() {
            Some(Some(did)) => self.table.insert(ident.id.to_uint(), did),
            _ => fail!("Unresolved name {}", ident.name),
        };
    }

    fn add_to_scope(&mut self, ns: NS, ident: &Ident) {
        let subscope = self.scope.mut_last().take_unwrap();
        subscope.insert(ns, ident);
    }

    pub fn def_from_ident(&self, ident: &Ident) -> DefId {
        *self.table.find(&ident.id.to_uint()).take_unwrap()
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

    fn visit_pat(&mut self, pat: &Pat) {
        match pat.val {
            IdentPat(ref id, ref t) => {
                for t in t.iter() { self.visit_type(t); }
                self.add_to_scope(ValNS, id);
            }
            _ => walk_pat(self, pat)
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr.val {
            IdentExpr(ref ident) => {
                self.resolve(ValNS, ident);
            }
            MatchExpr(ref e, ref arms) => {
                self.visit_expr(*e);
                for arm in arms.iter() {
                    self.descend(None, |me| {
                        me.visit_pat(&arm.pat);
                        me.visit_expr(&arm.body);
                    });
                }
            }
            _ => walk_expr(self, expr)
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
            _ => walk_stmt(self, stmt)
        }
    }

    fn visit_block(&mut self, block: &Block) {
        self.descend(Some(&block.items), |me| walk_block(me, block));
    }

    fn visit_item(&mut self, item: &Item) {
        match item.val {
            FuncItem(_, ref args, ref t, ref block, ref tps) => {
                self.descend(None, |me| {
                    for tp in tps.iter() {
                        me.add_to_scope(TypeNS, tp);
                    }
                    me.visit_type(t);
                    for arg in args.iter() {
                        me.visit_type(&arg.argtype);
                        me.add_to_scope(ValNS, &arg.ident);
                    }
                    me.visit_block(block);
                });
            }
            StructItem(_, ref fields, ref tps) => {
                self.descend(None, |me| {
                    for tp in tps.iter() {
                        me.add_to_scope(TypeNS, tp);
                    }
                    for field in fields.iter() {
                        me.visit_type(&field.fldtype);
                    }
                });
            }
            EnumItem(ref id, ref variants, ref tps) => {
                self.add_to_scope(TypeNS, id);
                self.descend(None, |me| {
                    for tp in tps.iter() {
                        me.add_to_scope(TypeNS, tp);
                    }
                    for variant in variants.iter() {
                        for arg in variant.args.iter() {
                            me.visit_type(arg);
                        }
                    }
                });
            }
        }
    }

    fn visit_module(&mut self, module: &Module) {
        self.descend(Some(&module.items), |me| walk_module(me, module));
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
        let mut parser = new_from_string("fn wot<T>(t: T) { let u = t; }");
        let tree = parser.parse_module();
        let mut resolver = Resolver::new();
        resolver.visit_module(&tree);
    }

    #[test]
    #[should_fail]
    fn unresolved_name() {
        let mut parser = new_from_string("fn lol<T>(t: T) { let u = wot; }"); // unresolved name wot
        let tree = parser.parse_module();
        let mut resolver = Resolver::new();
        resolver.visit_module(&tree);
    }

    #[test]
    #[should_fail]
    fn unresolved_type() {
        let mut parser = new_from_string("fn welp<T>(t: U) { let u = t; }"); // unresolved name U
        let tree = parser.parse_module();
        let mut resolver = Resolver::new();
        resolver.visit_module(&tree);
    }
}
