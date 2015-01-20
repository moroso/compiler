use mc::session::Interner;
use mc::session::Session;
use util::Name;

use std::collections::{VecMap, BTreeMap};
use std::slice;

use mc::ast::*;
use mc::ast::visitor::*;

pub use self::NS::*;

use self::ModuleScope::*;

//#[allow(non_camel_case_types)] leaving the warning so we remember to patch rust later
pub enum NS {
    TypeAndModNS = 0i,
    ValNS,
    StructNS,
}

enum ModuleScope {
    OffBranch(Subscope),
    OnBranch(uint),
}

struct Subscope {
    names: VecMap<BTreeMap<uint, NodeId>>,
}

impl Subscope {
    fn new() -> Subscope {
        Subscope {
            names: VecMap::new()
        }
    }

    fn insert(&mut self, ns: NS, name: Name, node_id: NodeId) -> bool {
        let ns = ns as uint;

        if !self.names.contains_key(&name.as_uint()) {
            self.names.insert(name.as_uint(), BTreeMap::new());
        }

        let names = self.names.get_mut(&name.as_uint()).unwrap();

        names.insert(ns, node_id).is_some()
    }

    fn insert_ident(&mut self, ns: NS, ident: &Ident) -> bool {
        self.insert(ns, ident.val.name, ident.id)
    }

    fn insert_items<F>(&mut self, items: &Vec<Item>, get_pairs: F)
        where F: Fn(&[Ident]) -> Vec<(NS, Ident)> {
        for item in items.iter() {
            match item.val {
                UseItem(ref import) => {
                    use std::collections::BTreeSet;
                    use std::iter::FromIterator;

                    let filter = match import.val.import {
                        ImportNames(ref ids) =>
                            FromIterator::from_iter(ids.iter().map(|ref id| id.val.name)),
                        ImportAll =>
                            BTreeSet::new(),
                    };

                    let allow: &Fn(&Ident) -> bool = match import.val.import {
                        ImportNames(..) => &|&: id: &Ident| filter.contains(&id.val.name),
                        ImportAll => &|&: _: &Ident| true,
                    };

                    let pairs = get_pairs(import.val.elems.as_slice());
                    for &(ns, ref ident) in pairs.iter() {
                        if allow(ident) {
                            self.insert_ident(ns, ident);
                        }
                    }
                }
                FuncItem(ref ident, _, _, _, _) => {
                    self.insert_ident(ValNS, ident);
                }
                StructItem(ref ident, _, _) => {
                    self.insert_ident(TypeAndModNS, ident);
                    self.insert_ident(StructNS, ident);
                }
                EnumItem(ref ident, ref variants, _) => {
                    self.insert_ident(TypeAndModNS, ident);
                    for variant in variants.iter() {
                        self.insert_ident(ValNS, &variant.ident);
                    }
                }
                TypeItem(ref ident, _, _) => {
                    self.insert_ident(TypeAndModNS, ident);
                }
                ModItem(ref ident, _) => {
                    self.insert_ident(TypeAndModNS, ident);
                }
                StaticItem(ref ident, _, _, _) => {
                    self.insert_ident(ValNS, ident);
                }
                ConstItem(ref ident, _, _) => {
                    self.insert_ident(ValNS, ident);
                }
                _ => {}
            }
        }
    }

    fn find(&self, ns: NS, ident: &Ident) -> Option<NodeId> {
        let ns = ns as uint;
        self.names.get(&ident.val.name.as_uint())
            .and_then(|names| names.get(&ns))
            .map(|id| *id)
    }
}

pub struct Resolver {
    table: BTreeMap<NodeId, NodeId>,
}

struct ModuleCollector {
    tree: BTreeMap<NodeId, ModuleScope>,
}

struct ModuleResolver<'a> {
    session: &'a mut Session<'a>,
    scope: Vec<Subscope>,
    tree: BTreeMap<NodeId, ModuleScope>,
    root: uint,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            table: BTreeMap::new(),
        }
    }

    /// Get the NodeId of the item that defines the given path
    pub fn def_from_path(&self, path: &Path) -> NodeId {
        *self.table.get(&path.id).unwrap()
    }

    /// Get the NodeId of the item that defines the given path
    pub fn maybe_def_from_path(&self, path: &Path) -> Option<NodeId> {
        match self.table.get(&path.id) {
            Some(ref v) => Some(*v.clone()),
            None => None,
        }
    }

    // The entry point for the resolver
    pub fn resolve<'a>(session: &'a mut Session<'a>,
                       module: &Module) {
        let (root, tree) = ModuleCollector::collect(module);

        let mut modres = ModuleResolver {
            session: session,
            scope: vec!(root),
            tree: tree,
            root: 0,
        };

        modres.visit_module(module);
    }
}

impl ModuleCollector {
    fn collect(module: &Module) -> (Subscope, BTreeMap<NodeId, ModuleScope>) {
        let mut collector = ModuleCollector { tree: BTreeMap::new() };
        collector.visit_module(module);
        let mut root = Subscope::new();
        root.insert_items(&module.val.items, |_| vec!());
        (root, collector.tree)
    }
}

fn try_resolve_ident(scope: &[Subscope], ns: NS, ident: &Ident) -> Option<NodeId> {
    scope.iter().rev().filter_map(|subscope| subscope.find(ns, ident)).next()
}

impl<'a> ModuleResolver<'a> {
    // Takes a module path
    fn try_resolve_subscope(&mut self, global: bool,
                            path: &[Ident]) -> Option<&[Subscope]> {
        let mut search_scope = if global {
            slice::ref_slice(&self.scope[0])
        } else {
            self.scope.slice_from(self.root)
        };

        for elem in path.iter() {
            let maybe_node_id = try_resolve_ident(search_scope, TypeAndModNS, elem);
            let maybe_modscope = match maybe_node_id {
                Some(node_id) => self.tree.get(&node_id),
                None => None,
            };
            match maybe_modscope {
                Some(modscope) => {
                    search_scope = match *modscope {
                        OnBranch(idx) => slice::ref_slice(&self.scope[idx]),
                        OffBranch(ref subscope) => slice::ref_slice(subscope),
                    }
                }
                None => return None,
            }
        }

        Some(search_scope)
    }


    fn try_resolve_path_split(&mut self, ns: NS,
                              global: bool, path: &[Ident], ident: &Ident) -> Option<NodeId> {
        match self.try_resolve_subscope(global, path) {
            None => None,
            Some(search_scope) => {
                try_resolve_ident(search_scope, ns, ident)
            }
        }
    }

    /// Search the current scope stack local-to-global for a matching
    /// ident in the requested namespace
    fn try_resolve_path(&mut self, ns: NS, path: &Path) -> Option<NodeId> {
        self.try_resolve_path_split(ns,
                                    path.val.global,
                                    path.val.elems.init(),
                                    path.val.elems.last().unwrap())
    }

    fn fail_resolve(&mut self, id: NodeId, path: &[Ident]) -> ! {
        let elems: Vec<String> =
            path.iter().map(|e|
                       String::from_str(
                           self.session.interner.name_to_str(&e.val.name))).collect();
        self.session.error(id,
                           format!("Unresolved name `{}`", elems.connect("::")));
    }

    fn resolve_path(&mut self, ns: NS, path: &Path) -> NodeId {
        match self.try_resolve_path(ns, path) {
            Some(node_id) => {
                self.session.resolver.table.insert(path.id, node_id);
                node_id
            }
            None => {
                self.fail_resolve(path.id, path.val.elems.as_slice())
            }
        }
    }

    /// Adds the given ident to the given namespace in the current scope
    fn add_ident_to_scope(&mut self, ns: NS, ident: &Ident) {
        self.add_to_scope(ns, ident.val.name, ident.id);
    }

    /// Adds the given name to the given namespace as the given node_id in the current scope
    fn add_to_scope(&mut self, ns: NS, name: Name, node_id: NodeId) {
        let subscope = self.scope[self.scope.len() - 1];
        subscope.insert(ns, name, node_id);
    }

    /// Descends into a new scope, optionally seeding it with a set of items
    fn descend<F>(&mut self, items: Option<&Vec<Item>>, visit: F) -> Subscope
        where F: Fn(&mut ModuleResolver) {
        let mut subscope = Subscope::new();

        items.map(|items| subscope.insert_items(items, |elems| {
            let scope = self.try_resolve_subscope(true, elems).unwrap();
            let ref names = scope[0].names;

            let mut pairs = vec!();
            for (name, map) in names.iter() {
                for ns in [TypeAndModNS, ValNS, StructNS].iter() {
                    for nid in map.get(&(*ns as uint)).iter() {
                        let ident = IdentNode {
                            tps: None,
                            name: Name(name),
                        };

                        pairs.push((*ns, WithId { id: **nid, val: ident }));
                    }
                }
            }

            pairs
        }));

        self.scope.push(subscope);
        visit(self);
        self.scope.pop().unwrap()
    }

    fn handle_use_names(&mut self, global: bool, elems: &[Ident], idents: &[Ident]) {
        for ident in idents.iter() {

            let mut found = false;
            for ns in [TypeAndModNS, ValNS, StructNS].iter() {
                match self.try_resolve_path_split(*ns, global, elems, ident) {
                    Some(node_id) => {
                        self.add_to_scope(*ns, ident.val.name, node_id);
                        found = true;
                    }
                    None => {}
                }
            }

            if !found {
                let mut path = elems.to_vec();
                path.push(ident.clone());
                self.fail_resolve(ident.id, path.as_slice());
            }
        }
    }

    fn handle_use_all(&mut self, id: NodeId, global: bool, elems: &[Ident]) {
        // I hate you borrow checker.
        // Is there a more idiomatic way to do this?
        let scope = {
            match self.try_resolve_subscope(global, elems) {
                Some(scope) => {
                    assert!(scope.len() == 1);
                    Some(scope[0].names.clone())
                }
                None => None
            }
        };
        let scope = match scope {
            Some(scope) => scope,
            None => self.fail_resolve(id, elems)
        };

        // Ok, now that we have found the stuff, add it all in.
        for (name, map) in scope.iter() {
            for ns in [TypeAndModNS, ValNS, StructNS].iter() {
                for id in map.get(&(*ns as uint)).iter() {
                    self.add_to_scope(*ns, Name(name), **id);
                }
            }
        }
    }

    fn handle_use(&mut self, import: &Import) {
        match import.val.import {
            ImportNames(ref id) => self.handle_use_names(import.val.global,
                                                         import.val.elems.as_slice(),
                                                         id.as_slice()),
            ImportAll => self.handle_use_all(import.id,
                                             import.val.global,
                                             import.val.elems.as_slice())
        };

    }
}

impl Visitor for ModuleCollector {
    fn visit_item(&mut self, item: &Item) {
        match item.val {
            ModItem(ref ident, ref module) => {
                let mut subscope = Subscope::new();
                subscope.insert_items(&module.val.items, |_| vec!());
                self.tree.insert(ident.id, OffBranch(subscope));
                self.visit_module(module);
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
                self.add_ident_to_scope(ValNS, ident);
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
                    self.visit_expr(&fld.1);
                }
            }
            MatchExpr(ref e, ref arms) => {
                self.visit_expr(&**e);
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
        self.descend(Some(&block.val.items), |me| walk_block(me, block));
    }

    fn visit_item(&mut self, item: &Item) {
        match item.val {
            UseItem(ref import) => {
                self.handle_use(import);
            }
            FuncItem(_, ref args, ref t, ref def, ref tps) => {
                self.descend(None, |me| {
                    for tp in tps.iter() {
                        me.add_ident_to_scope(TypeAndModNS, tp);
                    }
                    me.visit_type(t);
                    for arg in args.iter() {
                        me.visit_type(&arg.argtype);
                        me.add_ident_to_scope(ValNS, &arg.ident);
                    }
                    match *def {
                        LocalFn(ref block) => me.visit_block(block),
                        ExternFn(..) => {}
                    }
                });
            }
            StaticItem(ref ident, ref ty, ref expr, _) => {
                self.visit_ident(ident);
                self.visit_type(ty);
                for e in expr.iter() {
                    self.visit_expr(e);
                }
            }
            ConstItem(ref ident, ref ty, ref expr) => {
                self.visit_ident(ident);
                self.visit_type(ty);
                self.visit_expr(expr);
            }
            StructItem(_, ref fields, ref tps) => {
                self.descend(None, |me| {
                    for tp in tps.iter() {
                        me.add_ident_to_scope(TypeAndModNS, tp);
                    }
                    for field in fields.iter() {
                        me.visit_type(&field.fldtype);
                    }
                });
            }
            EnumItem(ref id, ref variants, ref tps) => {
                self.add_ident_to_scope(TypeAndModNS, id);
                self.descend(None, |me| {
                    for tp in tps.iter() {
                        me.add_ident_to_scope(TypeAndModNS, tp);
                    }
                    for variant in variants.iter() {
                        for arg in variant.args.iter() {
                            me.visit_type(arg);
                        }
                    }
                });
            }
            TypeItem(ref id, ref ty, ref tps) => {
                self.add_ident_to_scope(TypeAndModNS, id);
                self.descend(None, |me| {
                    for tp in tps.iter() {
                        me.add_ident_to_scope(TypeAndModNS, tp);
                    }
                    me.visit_type(ty);
                });
            }
            ModItem(ref ident, ref module) => {
                use std::mem;

                let mut idx = self.scope.len();

                // Find the subscope we're about to descend into and swap it out of the tree
                let subscope = match self.tree.get(&ident.id) {
                    Some(&OffBranch(subscope)) => subscope,
                    _ => panic!(),
                };
                self.tree.insert(ident.id, OnBranch(idx));

                // Add the subscope to the scope branch
                self.scope.push(subscope);

                // Now, push a new bogus subscope to prevent polluting
                // the real scope with our imports.
                // FIXME: we want to support reexport with a pub thing.
                self.scope.push(Subscope::new());

                // Update our root scope
                mem::swap(&mut self.root, &mut idx);

                self.visit_module(module);

                // Restore the old root scope
                mem::swap(&mut self.root, &mut idx);

                // Pop the bogus subscope
                self.scope.pop();

                // Pop the subscope
                let subscope = self.scope.pop().unwrap();

                // Put the child module's scope back in the tree
                self.tree.insert(ident.id, OffBranch(subscope));
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Resolver;
    use super::super::session::Session;
    use super::super::ast::NodeId;
    use super::super::ast::visitor::Visitor;
    use super::super::parser::ast_from_str;
    use std::collections::BTreeMap;

    #[test]
    fn basic_resolver_test() {
        let (mut session, tree) = ast_from_str("fn wot<T>(t: T) -> T { let u = t; }", |p| p.parse_module());
        Resolver::resolve(&mut session, &tree);
    }

    #[test]
    #[should_fail]
    fn unresolved_name() {
        let (mut session, tree) = ast_from_str("fn lol<T>(t: T) { let u = wot; }", |p| p.parse_module()); // unresolved name wot
        Resolver::resolve(&mut session, &tree);
    }

    #[test]
    #[should_fail]
    fn unresolved_type() {
        let (mut session, tree) = ast_from_str("fn welp<T>(t: U) { let u = t; }", |p| p.parse_module()); // unresolved name U
        Resolver::resolve(&mut session, &tree);
    }
}
