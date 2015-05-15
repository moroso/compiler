use mc::ast::visitor::{Visitor, walk_item};
use mc::ast::*;
use mc::session::Session;
use std::collections::BTreeMap;

// This is a bit too similar to the mangler. I should get rid of the mangler.

pub struct PathMap {
    table: BTreeMap<NodeId, Vec<String>>,
}

impl PathMap {
    pub fn new() -> PathMap {
        PathMap {
            table: BTreeMap::new(),
        }
    }

    pub fn find<'a>(&'a self, id: &NodeId) -> Option<&'a Vec<String>> {
        self.table.get(id)
    }

    pub fn record(session: &mut Session, module: &Module) {
        let mut visitor = PathMapVisitor {
            session: session,
            path: vec!(),
        };

        visitor.visit_module(module);
    }
}

pub struct PathMapVisitor<'a, 'b: 'a> {
    session: &'a mut Session<'b>,
    path: Vec<String>,
}

impl<'a, 'b> PathMapVisitor<'a, 'b> {

    fn insert(&mut self, id: &Ident, item: &Item) {
        let name = self.session.interner.name_to_str(&id.val.name).to_string();

        let mut path = self.path.clone();
        path.push(name.clone());
        self.session.pathmap.table.insert(id.id, path);

        walk_item(self, item);
    }
}

impl<'a, 'b> Visitor for PathMapVisitor<'a, 'b> {
    fn visit_item(&mut self, item: &Item) {
        match item.val {
            ModItem(ref id, ref body) => {
                self.insert(id, item);

                let name = self.session.interner.name_to_str(&id.val.name).to_string();
                self.path.push(name);
                self.visit_module(body);
                self.path.pop();
            },
            StaticItem(ref id, _, _, false) |
            StructItem(ref id, _, _) |
            EnumItem(ref id, _, _) |
            ConstItem(ref id, _, _) |
            FuncItem(ref id, _, _, LocalFn(..), _) |
            FuncItem(ref id, _, _, ExternFn(..), _) |
            StaticItem(ref id, _, None, true) => {
                self.insert(id, item);
            },
            _ => walk_item(self, item),
        }
    }
}
