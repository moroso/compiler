use mc::ast::visitor::{Visitor, walk_ident, walk_item, walk_path, walk_expr};
use mc::ast::*;
use mc::session::Session;
use std::collections::treemap::TreeMap;

pub struct NameMangler {
    pub names: TreeMap<NodeId, String>,
    pub session: Session,
    path: Vec<String>,
    mangle_main: bool,
    mangle_externs: bool,
}

impl NameMangler {
    pub fn new(session: Session, module: &Module,
               mangle_main: bool, mangle_externs: bool) -> NameMangler {
        let mut mangler = NameMangler { names: TreeMap::new(),
                                        path: vec!(),
                                        session: session,
                                        mangle_main: mangle_main,
                                        mangle_externs: mangle_externs };
        mangler.visit_module(module);
        mangler
    }

    fn mangle_id(&mut self, id: &Ident, item: &Item) {
        let name = String::from_str(
            self.session.interner.name_to_str(&id.val.name));
        self.path.push(name.clone());
        let pre_mangled_path = self.path.connect("_");
        // We add "MANGLED" to the beginning of everything for now,
        // to make it really obvious if we forget to mangle something.
        let mangled_name =
            if !self.mangle_main &&
            pre_mangled_path == String::from_str("main") {
                // ... except main, which we don't want to mangle!
                String::from_str("main")
            } else {
                format!("MANGLED{}", pre_mangled_path)
            };
        self.path.pop();
        self.names.insert(id.id, mangled_name);
        walk_item(self, item);
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
            StaticItem(ref id, _, _, false) |
            StructItem(ref id, _, _) |
            EnumItem(ref id, _, _) |
            ConstItem(ref id, _, _) |
            FuncItem(ref id, _, _, Some(_), _) => {
                self.mangle_id(id, item);
            },
            // Extern things don't get managled.
            StaticItem(ref id, _, None, true) |
            FuncItem(ref id, _, _, None, _) => {
                if self.mangle_externs {
                    self.mangle_id(id, item);
                } else {
                    let name = String::from_str(
                        self.session.interner.name_to_str(&id.val.name));
                    self.names.insert(id.id, name);
                }
            },
            _ => walk_item(self, item),
        }
    }
}
