use mc::ast::visitor::{Visitor, walk_ident, walk_item, walk_path, walk_expr};
use mc::ast::*;
use mc::session::Session;
use std::collections::BTreeMap;

pub struct NameMangler<'a> {
    pub names: BTreeMap<NodeId, String>,
    pub session: Session<'a>,
    path: Vec<String>,
    mangle_main: bool,
    mangle_externs: bool,
}

impl<'a> NameMangler<'a> {
    pub fn new(session: Session<'a>, module: &Module,
               mangle_main: bool, mangle_externs: bool) -> NameMangler<'a> {
        let mut mangler = NameMangler { names: BTreeMap::new(),
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
        let pre_mangled_path = self.path.connect("__");
        // We add "__" to the beginning of everything to make it
        // really obvious if we forget to mangle something and to
        // avoid colliding with C.
        let mangled_name =
            if !self.mangle_main &&
            pre_mangled_path == "main".to_string() {
                // ... except main, which we don't want to mangle!
                "main".to_string()
            } else {
                format!("__{}", pre_mangled_path)
            };
        self.path.pop();
        self.names.insert(id.id, mangled_name);
        walk_item(self, item);
    }
}

impl<'a> Visitor for NameMangler<'a> {
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
            FuncItem(ref id, _, _, LocalFn(..), _) => {
                self.mangle_id(id, item);
            },
            // Extern things don't get managled.
            StaticItem(ref id, _, None, true) |
            FuncItem(ref id, _, _, ExternFn(..), _) => {
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
