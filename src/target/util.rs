use mc::ast::visitor::{Visitor, walk_ident, walk_item, walk_path, walk_expr};
use mc::ast::*;
use mc::session::Session;
use std::collections::treemap::TreeMap;

pub struct NameMangler {
    pub names: TreeMap<NodeId, String>,
    pub session: Session,
    path: Vec<String>,
}

impl NameMangler {
    pub fn new(session: Session) -> NameMangler {
        NameMangler { names: TreeMap::new(),
                      path: vec!(),
                      session: session, }
    }
}

impl Visitor for NameMangler {
    fn visit_item(&mut self, item: &Item) {
        match item.val {
            ModItem(ref id, ref body) => {
                let name = String::from_str(
                    self.session.interner.name_to_str(&id.val.name));
                self.path.push(name);
                self.visit_module(body);
                self.path.pop();
            },
            StaticItem(ref id, _, Some(_)) |
            StructItem(ref id, _, _) |
            EnumItem(ref id, _, _) |
            ConstItem(ref id, _, _) |
            FuncItem(ref id, _, _, Some(_), _) => {
                let name = String::from_str(
                    self.session.interner.name_to_str(&id.val.name));
                self.path.push(name.clone());
                let pre_mangled_path = self.path.connect("_");
                // We add "MANGLED" to the beginning of everything for now,
                // to make it really obvious if we forget to mangle something.
                let mangled_name =
                    if pre_mangled_path == String::from_str("main") {
                        // ... except main, which we don't want to mangle!
                        String::from_str("main")
                    } else {
                        format!("MANGLED{}", pre_mangled_path)
                    };
                self.path.pop();
                self.names.insert(id.id, mangled_name);
                walk_item(self, item);
            },
            // Extern things don't get managled.
            StaticItem(ref id, _, None) |
            FuncItem(ref id, _, _, None, _) => {
                let name = String::from_str(
                    self.session.interner.name_to_str(&id.val.name));
                self.names.insert(id.id, name);
            },
            _ => walk_item(self, item),
        }
    }
}

