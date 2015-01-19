use std::collections::BTreeMap;

use mclib::intern::Name;

use syntax::ast::*;
use syntax::ast::visitor::{Visitor, walk_item};
use syntax::expand::Expansion;

// This is a bit too similar to the mangler. I should get rid of the mangler.

pub struct PathMap {
    table: BTreeMap<NodeId, Vec<Name>>,
}

impl PathMap {
    pub fn new(expn: &Expansion) -> PathMap {
        let mut pathmap = PathMap {
            table: BTreeMap::new(),
        };

        expn.visit(&mut PathMapVisitor {
            pathmap: &mut pathmap,
            path: vec!(),
        });

        pathmap
    }

    pub fn find<'a>(&'a self, id: &NodeId) -> Option<&'a Vec<Name>> {
        self.table.get(id)
    }
}

pub struct PathMapVisitor<'a> {
    pathmap: &'a mut PathMap,
    path: Vec<Name>,
}

impl<'a> PathMapVisitor<'a> {
    fn insert(&mut self, id: &Ident, item: &Item) {
        let mut path = self.path.clone();
        path.push(id.val.name);
        self.pathmap.table.insert(id.id, path);

        walk_item(self, item);
    }
}

impl<'a> Visitor for PathMapVisitor<'a> {
    fn visit_item(&mut self, item: &Item) {
        match item.val {
            ModItem(ref id, ref body) => {
                self.insert(id, item);

                self.path.push(id.val.name);
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
