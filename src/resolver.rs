use ast::*;
use ast::visit::*;
use collections::{SmallIntMap, TreeMap};

use session::Interner;
use util::Name;

//#[allow(non_camel_case_types)] leaving the warning so we remember to patch rust later
pub enum NS {
    TypeNS = 0u,
    ValNS,
    StructNS,
    ModNS,
    NS_COUNT // argh rust why can't you provide num_variants::<NS>()
}

struct Subscope {
    namespaces: SmallIntMap<TreeMap<Name, NodeId>>,
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
            self.namespaces.insert(ns, TreeMap::new());
        }

        let ns = self.namespaces.find_mut(&ns).take_unwrap();

        ns.insert(ident.val.name.clone(), ident.id)
    }

    fn find(&self, ns: NS, ident: &Ident) -> Option<NodeId> {
        let ns = ns as uint;
        self.namespaces.find(&ns)
            .and_then(|ns| ns.find(&ident.val.name))
            .map(|id| *id)
    }
}

pub struct Resolver {
    table: SmallIntMap<NodeId>,
}

struct ModuleResolver<'a> {
    resolver: &'a mut Resolver,
    interner: &'a Interner,
    scope: Vec<Subscope>,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            table: SmallIntMap::new(),
        }
    }

    /// Get the NodeId of the item that defines the given ident
    pub fn def_from_ident(&self, ident: &Ident) -> NodeId {
        *self.table.find(&ident.id.to_uint()).take_unwrap()
    }

    /// The entry point for the resolver
    pub fn resolve_module(&mut self, interner: &Interner, module: &Module) {
        let mut visitor = ModuleResolver::new(self, interner);
        visitor.visit_module(module)
    }
}

impl<'a> ModuleResolver<'a> {
    fn new(resolver: &'a mut Resolver, interner: &'a Interner) -> ModuleResolver<'a> {
        ModuleResolver {
            resolver: resolver,
            interner: interner,
            scope: vec!(),
        }
    }

    /// Search the current scope stack local-to-global for a matching ident in the requested namespace
    fn resolve_ident(&mut self, ns: NS, ident: &Ident) {
        match self.scope.iter().rev()
                               .filter_map(|subscope| subscope.find(ns, ident))
                               .next() {
            Some(did) => self.resolver.table.insert(ident.id.to_uint(), did),
            None => fail!("Unresolved name {}", self.interner.name_to_str(&ident.val.name)),
        };
    }

    /// Adds the given ident to the given namespace in the current scope
    fn add_to_scope(&mut self, ns: NS, ident: &Ident) {
        let subscope = self.scope.mut_last().take_unwrap();
        subscope.insert(ns, ident);
    }

    /// Descends into a new scope, optionally seeding it with a set of items
    fn descend(&mut self, items: Option<&Vec<Item>>, visit: |&mut ModuleResolver|) {
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
}

impl<'a> Visitor for ModuleResolver<'a> {
    fn visit_type(&mut self, t: &Type) {
        match t.val {
            NamedType(ref ident) => {
                self.resolve_ident(TypeNS, ident);
                match ident.val.tps {
                    Some(ref tps) => {
                        for tp in tps.iter() {
                            self.visit_type(tp);
                        }
                    }
                    None => {}
                }
            }
            _ => walk_type(self, t)
        }
    }

    fn visit_pat(&mut self, pat: &Pat) {
        match pat.val {
            IdentPat(ref ident, ref t) => {
                for t in t.iter() { self.visit_type(t); }
                self.add_to_scope(ValNS, ident);
            }
            VariantPat(ref ident, ref pats) => {
                self.resolve_ident(ValNS, ident);
                for pat in pats.iter() { self.visit_pat(pat); }
            }
            StructPat(ref ident, ref fps) => {
                self.resolve_ident(StructNS, ident);
                for fp in fps.iter() { self.visit_pat(&fp.pat); }
            }
            _ => walk_pat(self, pat)
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr.val {
            IdentExpr(ref ident) => {
                self.resolve_ident(ValNS, ident);
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
        // Seed the new scope with this block's items
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
        // Seed the new scope with this module's items
        self.descend(Some(&module.items), |me| walk_module(me, module));
    }
}

#[cfg(test)]
mod tests {
    use super::Resolver;
    use ast::NodeId;
    use ast::visit::Visitor;
    use parser::ast_from_str;
    use collections::TreeMap;

    #[test]
    fn basic_resolver_test() {
        let (mut interner, tree) = ast_from_str("fn wot<T>(t: T) -> T { let u = t; }", |p| p.parse_module());
        let mut resolver = Resolver::new();
        resolver.resolve_module(&mut interner, &tree);
    }

    #[test]
    #[should_fail]
    fn unresolved_name() {
        let (mut interner, tree) = ast_from_str("fn lol<T>(t: T) { let u = wot; }", |p| p.parse_module()); // unresolved name wot
        let mut resolver = Resolver::new();
        resolver.resolve_module(&mut interner, &tree);
    }

    #[test]
    #[should_fail]
    fn unresolved_type() {
        let (mut interner, tree) = ast_from_str("fn welp<T>(t: U) { let u = t; }", |p| p.parse_module()); // unresolved name U
        let mut resolver = Resolver::new();
        resolver.resolve_module(&mut interner, &tree);
    }
}
