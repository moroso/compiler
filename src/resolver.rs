use ast::*;
use ast::visit::*;
use std::collections::{SmallIntMap, TreeMap};

use session::Interner;
use util::Name;

//#[allow(non_camel_case_types)] leaving the warning so we remember to patch rust later
pub enum NS {
    TypeAndModNS = 0u,
    ValNS,
    StructNS,
    NS_COUNT // argh rust why can't you provide num_variants::<NS>()
}

enum ModuleScope {
    OffBranch(Subscope),
    OnBranch(uint),
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

    fn insert_items(&mut self, items: &Vec<Item>) {
        for item in items.iter() {
            match item.val {
                FuncItem(ref ident, _, _, _, _) => {
                    self.insert(ValNS, ident);
                }
                StructItem(ref ident, _, _) => {
                    self.insert(TypeAndModNS, ident);
                    self.insert(StructNS, ident);
                }
                EnumItem(ref ident, ref variants, _) => {
                    self.insert(TypeAndModNS, ident);
                    for variant in variants.iter() {
                        self.insert(ValNS, &variant.ident);
                    }
                }
                ModItem(ref ident, _) => {
                    self.insert(TypeAndModNS, ident);
                }
                StaticItem(ref ident, _, _) => {
                    self.insert(ValNS, ident);
                }
            }
        }
    }

    fn find(&self, ns: NS, ident: &Ident) -> Option<NodeId> {
        let ns = ns as uint;
        self.namespaces.find(&ns)
            .and_then(|ns| ns.find(&ident.val.name))
            .map(|id| *id)
    }
}

pub struct Resolver {
    table: TreeMap<NodeId, NodeId>,
}

struct ModuleCollector {
    tree: TreeMap<NodeId, ModuleScope>,
}

struct ModuleResolver<'a> {
    resolver: &'a mut Resolver,
    interner: &'a Interner,
    scope: Vec<Subscope>,
    tree: TreeMap<NodeId, ModuleScope>,
    root: ModuleScope,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            table: TreeMap::new(),
        }
    }

    /// Get the NodeId of the item that defines the given path
    pub fn def_from_path(&self, path: &Path) -> NodeId {
        *self.table.find(&path.id).unwrap()
    }

    /// The entry point for the resolver
    pub fn resolve_module(&mut self, interner: &Interner, module: &Module) {
        ModuleResolver::process(self, interner, module);
    }
}

impl ModuleCollector {
    fn collect(module: &Module) -> (Subscope, TreeMap<NodeId, ModuleScope>) {
        let mut collector = ModuleCollector { tree: TreeMap::new() };
        collector.visit_module(module);
        let mut root = Subscope::new();
        root.insert_items(&module.val.items);
        (root, collector.tree)
    }
}

impl<'a> ModuleResolver<'a> {
    fn process(resolver: &'a mut Resolver, interner: &'a Interner, module: &Module) {
        let (root, tree) = ModuleCollector::collect(module);

        let mut modres = ModuleResolver {
            resolver: resolver,
            interner: interner,
            scope: vec!(root),
            tree: tree,
            root: OnBranch(0),
        };

        modres.visit_module(module);
    }

    /// Search the current scope stack local-to-global for a matching ident in the requested namespace
    fn resolve_path(&mut self, ns: NS, path: &Path) {
        use std::slice;

        fn make_scope<'a>(scope: &'a [Subscope], modscope: &'a ModuleScope) -> &'a [Subscope] {
            match *modscope {
                OnBranch(idx) => slice::ref_slice(scope.get(idx).unwrap()),
                OffBranch(ref scope) => slice::ref_slice(scope),
            }
        }

        fn resolve_ident(resolver: &mut Resolver, interner: &Interner, scope: &[Subscope], ns: NS, ident: &Ident) -> NodeId {
            match scope.iter().rev()
                              .filter_map(|subscope| subscope.find(ns, ident))
                              .next() {
                Some(node_id) => {
                    resolver.table.insert(ident.id, node_id);
                    node_id
                }
                None => fail!("Unresolved name {}", interner.name_to_str(&ident.val.name)),
            }
        }

        for elem in path.val.elems.iter() {
            for tp in elem.val.tps.iter().flat_map(|tps| tps.iter()) {
                self.visit_type(tp);
            }
        }

        let mut search_scope = if path.val.global {
            make_scope(self.scope.as_slice(), &self.root)
        } else {
            self.scope.as_slice()
        };

        for elem in path.val.elems.init().iter() {
            let node_id = resolve_ident(self.resolver, self.interner, search_scope, TypeAndModNS, elem);
            search_scope = make_scope(search_scope, self.tree.find(&node_id).unwrap());
        }

        let terminal = path.val.elems.last().unwrap();
        let node_id = resolve_ident(self.resolver, self.interner, search_scope, ns, terminal);
        self.resolver.table.insert(path.id, node_id);
    }

    /// Adds the given ident to the given namespace in the current scope
    fn add_to_scope(&mut self, ns: NS, ident: &Ident) {
        let subscope = self.scope.mut_last().take_unwrap();
        subscope.insert(ns, ident);
    }

    /// Descends into a new scope, optionally seeding it with a set of items
    fn descend(&mut self, items: Option<&Vec<Item>>, visit: |&mut ModuleResolver|) -> Subscope {
        let mut subscope = Subscope::new();

        items.map(|items| subscope.insert_items(items));

        self.scope.push(subscope);
        visit(self);
        self.scope.pop().unwrap()
    }
}

impl Visitor for ModuleCollector {
    fn visit_item(&mut self, item: &Item) {
        match item.val {
            ModItem(ref ident, ref module) => {
                let mut subscope = Subscope::new();
                subscope.insert_items(&module.val.items);
                self.tree.insert(ident.id, OffBranch(subscope));
            }
            _ => {}
        }
    }
}

impl<'a> Visitor for ModuleResolver<'a> {
    fn visit_type(&mut self, t: &Type) {
        match t.val {
            NamedType(ref path) => {
                self.resolve_path(TypeAndModNS, path);
                match path.val.elems.last().unwrap().val.tps {
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
            VariantPat(ref path, ref pats) => {
                self.resolve_path(ValNS, path);
                for pat in pats.iter() { self.visit_pat(pat); }
            }
            StructPat(ref path, ref fps) => {
                self.resolve_path(StructNS, path);
                for fp in fps.iter() { self.visit_pat(&fp.pat); }
            }
            _ => walk_pat(self, pat)
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr.val {
            PathExpr(ref path) => {
                self.resolve_path(ValNS, path);
            }
            StructExpr(ref path, ref flds) => {
                self.resolve_path(StructNS, path);
                for fld in flds.iter() {
                    self.visit_expr(fld.ref1());
                }
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
                        me.add_to_scope(TypeAndModNS, tp);
                    }
                    me.visit_type(t);
                    for arg in args.iter() {
                        me.visit_type(&arg.argtype);
                        me.add_to_scope(ValNS, &arg.ident);
                    }
                    for block in block.iter() { me.visit_block(block); }
                });
            }
            StaticItem(ref ident, ref ty, ref expr) => {
                self.visit_ident(ident);
                self.visit_type(ty);
                for e in expr.iter() {
                    self.visit_expr(e);
                }
            }
            StructItem(_, ref fields, ref tps) => {
                self.descend(None, |me| {
                    for tp in tps.iter() {
                        me.add_to_scope(TypeAndModNS, tp);
                    }
                    for field in fields.iter() {
                        me.visit_type(&field.fldtype);
                    }
                });
            }
            EnumItem(ref id, ref variants, ref tps) => {
                self.add_to_scope(TypeAndModNS, id);
                self.descend(None, |me| {
                    for tp in tps.iter() {
                        me.add_to_scope(TypeAndModNS, tp);
                    }
                    for variant in variants.iter() {
                        for arg in variant.args.iter() {
                            me.visit_type(arg);
                        }
                    }
                });
            }
            ModItem(ref ident, ref module) => {
                use std::mem;

                // Find the subscope we're about to descend into and swap it out of the tree
                let mut scope = match self.tree.swap(ident.id, OnBranch(0)) {
                    Some(OffBranch(scope)) => vec!(scope),
                    _ => fail!()
                };

                // Swap the new scope with our current scope (since scope doesn't leak across modules)
                mem::swap(&mut self.scope, &mut scope);
                
                // Update our root scope pointer
                let old_root_idx = match self.root {
                    OnBranch(idx) => {
                        let mut root = Subscope::new();
                        // Swap in a dummy subscope so we can get the old root subscope out
                        mem::swap(&mut root, scope.get_mut(idx));
                        self.root = OffBranch(root);
                        Some(idx)
                    }
                    _ => None,
                };

                self.visit_module(module);

                // Pop all the context we saved above
                match old_root_idx {
                    Some(idx) => {
                        let mut root = OnBranch(idx);
                        mem::swap(&mut root, &mut self.root);
                            
                        match root {
                            OffBranch(mut root) => {
                                // Swap the old root subscope back into the old scope
                                mem::swap(&mut root, scope.get_mut(idx));
                            }
                            _ => unreachable!(),
                        };
                    }
                    None => {}
                }

                // Swap the old scope back into place
                mem::swap(&mut self.scope, &mut scope);

                // Put the child module's scope back in the tree
                self.tree.swap(ident.id, OffBranch(scope.pop().unwrap()));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Resolver;
    use ast::NodeId;
    use ast::visit::Visitor;
    use parser::ast_from_str;
    use std::collections::TreeMap;

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
