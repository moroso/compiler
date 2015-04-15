use mc::ast::visitor::Visitor;
use mc::lexer::Lexer;
use mc::parser::Parser;
use mc::resolver::Resolver;
use mc::session::Session;

use package::Package;
use target::Target;
use target::NameMangler;

use util::{IntKind, Name, Width};

use intrinsics::size_of;

use std::collections::{BTreeSet, BTreeMap};

use util;
use std::io::Write;

use mc::ast::*;
use mc::ast::defmap::*;

use typechecker::*;
use values::*;


#[allow(unused_must_use)]
pub fn emit_ccross_prelude(f: &mut Write) {
    // This freestanding stuff is a hack but hey, so is the rest of this?
    writeln!(f, "{}", "#include <stdint.h>");
    writeln!(f, "{}", "#include <stdlib.h>");
    writeln!(f, "{}", "typedef unsigned int uint_t;");
    writeln!(f, "{}", "typedef int int_t;");

    writeln!(f, "{}", "#ifndef MB_FREESTANDING");
    writeln!(f, "{}", "#include <stdio.h>");

    writeln!(f, "{}", "int32_t print_int(int32_t x) { printf(\"%d\\n\", (int)x); return x; }");
    writeln!(f, "{}", "int32_t print_char(int32_t x) { putchar((int)x); return x; }");
    writeln!(f, "{}", "extern void abort();");
    writeln!(f, "{}", "void rt_abort() { abort(); }");
    writeln!(f, "{}", "void *rt_malloc(uint32_t size) { return malloc(size); }");
    writeln!(f, "{}", "#else");
    writeln!(f, "{}", "int32_t print_int(int32_t x) { return x; }");
    writeln!(f, "{}", "int32_t print_char(int32_t x) { return x; }");
    writeln!(f, "{}", "#endif");
}

struct CCrossCompiler<'a> {
    structnames: BTreeSet<NodeId>,
    enumitemnames: BTreeMap<Name, (Ident, Vec<Variant>, uint)>,
    enumnames: BTreeMap<NodeId, Name>,
    session: Session<'a>,
    typemap: Typemap,
    mangle_map: BTreeMap<NodeId, String>,
    indent: uint,
}

fn find_structs(module: &Module) -> BTreeSet<NodeId> {
    let mut struct_set = BTreeSet::new();

    for item in module.val.items.iter() {
        match item.val {
            StructItem(ref id, _, _) => { struct_set.insert(id.id); },
            ModItem(_, ref submod) => {
                let inner_structs = find_structs(submod);
                struct_set.extend(inner_structs.into_iter());
            }
            _ => {},
        }
    }

    struct_set
}

fn find_enum_item_names(module: &Module)
                        -> BTreeMap<Name, (Ident, Vec<Variant>, uint)> {
    let mut enum_map = BTreeMap::new();

    for item in module.val.items.iter() {
        match item.val {
            EnumItem(ref id, ref items, _) => {
                let mut pos = 0;
                for item in items.iter() {
                    enum_map.insert(item.ident.val.name,
                                    (id.clone(), items.clone(), pos));
                    pos += 1;
                }
            },
            ModItem(_, ref submod) => {
                let inner_enums = find_enum_item_names(submod);
                enum_map.extend(inner_enums.into_iter());
            }
            _ => {},
        }
    }

    enum_map
}

fn find_enum_names(module: &Module) -> BTreeMap<NodeId, Name> {
    let mut enum_map = BTreeMap::new();

    for item in module.val.items.iter() {
        match item.val {
            EnumItem(ref id, _, _) => {
                enum_map.insert(item.id, id.val.name);
            },
            ModItem(_, ref submod) => {
                let inner_enums = find_enum_names(submod);
                enum_map.extend(inner_enums.into_iter());
            }
            _ => {}
        }
    }

    enum_map
}

fn is_block_empty(block: &Block) -> bool {
    block.val.items.is_empty() && block.val.stmts.is_empty() &&
        match block.val.expr {
            Some(WithId {val: UnitExpr, ..}) => true,
            _ => false
        }
}


static INDENT_AMT: uint = 4;

impl<'a> CCrossCompiler<'a> {
    fn indent(&mut self) { self.indent += INDENT_AMT; }
    fn unindent(&mut self) { self.indent -= INDENT_AMT; }
    fn ind(&self) -> String {
        let v = (0..self.indent).map(|_|' ' as u8);
        let v: Vec<u8> = v.collect();
        String::from_utf8_lossy(&v[..]).into_owned()
    }

    fn visit_list<T, F>(&self, list: &Vec<T>,
                        mut visit: F,
                        delimiter: &str) -> String
        where F: FnMut(&CCrossCompiler, &T) -> String {
        let list: Vec<String> = list.iter().map(|t| visit(self, t))
            .filter(|x| *x != "".to_string()).collect();
        list.connect(&format!("{}", delimiter)[..])
    }

    fn mut_visit_list<T, F>(&mut self, list: &Vec<T>,
                            mut visit: F,
                            delimiter: &str) -> String
        where F: FnMut(&mut CCrossCompiler, &T) -> String {
        let list: Vec<String> = list.iter().map(|t| visit(self, t))
            .filter(|x| *x != "".to_string()).collect();
        list.connect(delimiter)
    }

    fn visit_binop(&self, op: &BinOp) -> String {
        format!("{}", op)
    }

    fn visit_unop(&self, op: &UnOp) -> String {
        format!("{}", op)
    }

    // A block, as an expression.
    fn visit_block_expr(&mut self, block: &Block) -> String {
        self.visit_block(block, |e| e.map(|e| format!("{};", e)).unwrap_or_default())
    }

    fn visit_id_and_type(&mut self, id: NodeId, t: &Type) -> String {
        let name = self.visit_id(&id);
        self.visit_string_and_type(name, t)
    }

    fn visit_name_and_type(&mut self, name: Name, t: &Type) -> String {
        let name = self.session.interner.name_to_str(&name).to_string();
        self.visit_string_and_type(name, t)
    }

    fn visit_name_and_ty(&self, name: Name, t: &Ty) -> String {
        let name = self.session.interner.name_to_str(&name).to_string();
        self.visit_string_and_ty(name, t)
    }

    fn visit_string_and_type(&mut self, name: String, t: &Type) -> String {
        match t.val {
            // We have to special case this, because of the way things of
            // a function pointer type are declared in C.
            FuncType(ref d, ref r) => {
                let ty = self.visit_type(&**r);
                let list = self.visit_list(d, |me, x| me.visit_type(x), ", ");
                format!("{} (*{})({})", ty, name, list)
            },
            // This might be wrong for nested arrays. Unless our
            // syntax is the reverse of C's?
            ArrayType(ref t, ref size) => {
                let name = format!("{}[{}]", name, self.visit_expr(&**size));
                self.visit_string_and_type(name, &**t)
            },
            PtrType(ref t) => {
                let name = format!("*{}", name);
                self.visit_string_and_type(name, &**t)
            },
            _ => {
                let ty = self.visit_type(t);
                format!("{} {}", ty, name)
            }
        }
    }

    fn visit_string_and_ty(&self, name: String, t: &Ty) -> String {
        match *t {
            // We have to special case this, because of the way things of
            // a function pointer type are declared in C.
            FuncTy(ref d, ref r) => {
                let ty = self.visit_ty(&r.val);
                let list = self.visit_list(d, |me, x| me.visit_ty(&x.val), ", ");
                format!("{} (*{})({})", ty, name, list)
            },
            // This might be wrong for nested arrays. Unless our
            // syntax is the reverse of C's?
            ArrayTy(ref t, size) => {
                let size_str = match size {
                    None => format!(""),
                    Some(n) => format!("{}", n)
                };
                let name = format!("{}[{}]", name, size_str);
                self.visit_string_and_ty(name, &t.val)
            },
            PtrTy(ref t) => {
                let name = format!("*{}", name);
                self.visit_string_and_ty(name, &t.val)
            },
            _ => {
                let ty = self.visit_ty(t);
                format!("{} {}", ty, name)
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> String {
        match stmt.val {
            LetStmt(ref pat, ref e) => {
                let (i, t) = match pat.val {
                    IdentPat(ref i, ref t) => (i, t),
                    _ => panic!("Only IdentPats are supported right now"),
                };

                let ty = match *t {
                    Some(ref ty) => self.visit_name_and_type(i.val.name, ty),
                    None => match *e {
                        Some(ref expr) =>
                            self.visit_name_and_ty(
                                i.val.name,
                                &self.typemap.types[expr.id.to_uint()]),
                        None => panic!("Must specify a type."),
                    }
                };
                let maybe_expr = e.as_ref().map(|e| format!(" = {}", self.visit_expr(e))).unwrap_or_default();
                format!("{}{};", ty, maybe_expr)
            },
            ExprStmt(ref e) | SemiStmt(ref e) => {
                self.visit_expr_stmt(e)
            },
        }
    }

    fn visit_block<F>(&mut self, block: &Block, tail: F) -> String
        where F: Fn(Option<String>) -> String {
        self.indent();
        let delim_s = format!("\n{}", self.ind());
        let delim = &delim_s[..];
        let items = self.mut_visit_list(&block.val.items, |me, t| me.visit_item(t), delim);
        let stmts = self.mut_visit_list(&block.val.stmts, |me, t| me.visit_stmt(t), delim);
        let expr = match block.val.expr {
            Some(ref x) => {
                match x.val {
                    WhileExpr(..) | ForExpr(..) => self.visit_expr(x),
                    ReturnExpr(ref e) => tail(Some(self.visit_expr(&**e))),
                    _ => tail(Some(self.visit_expr(x))),
                }
            }
            None => tail(None),
        };

        //TODO!!!!!!!
        //let out = self.visit_list(&vec!(items, stmts, expr),
            //                          |&:_, t: &String| t.clone(), delim);
            let out = "";
        self.unindent();

        format!("{{{}{}\n{}}}", delim, out, self.ind())
    }

    fn visit_item(&mut self, item: &Item) -> String {
        match item.val {
            UseItem(..) => String::new(),
            ConstItem(ref id, ref t, _) => {
                let ty = self.visit_type(t);
                let name = self.visit_ident(id);
                let lit = self.typemap.consts.get(&id.id)
                    .expect("finding const in map")
                    .clone().ok().expect("getting const value"); // wee
                let lit = WithId { id: id.id, val: lit };
                format!("#define {} (({}){})\n", name, ty, self.visit_lit(&lit))
            }
            FuncItem(ref name, ref args, ref t, ref def, _) => {
                match *def {
                    LocalFn(ref block) => {
                        let ty = self.visit_type(t);
                        let name = self.visit_ident(name);
                        let args = self.mut_visit_list(
                            args, |me, x| me.visit_func_arg(x), ", ");

                        let block = self.visit_block(block, |e| {
                            match e {
                                Some(e) => format!("return {};", e),
                                None => "return;".to_string(),
                            }
                        });

                        format!("{} {}({}) {}", ty, name, args, block)
                    }
                    ExternFn(..) => String::new(),
                }
            }
            StructItem(ref id, ref fields, _) => {
                let name = self.visit_ident(id);
                let fields = self.mut_visit_list(
                    fields,
                    |me, field| format!("{};", me.visit_name_and_type(field.name, &field.fldtype)),
                    "\n    ");
                format!("typedef struct {} {{\n    {}\n}} {};", &name[..], fields, &name[..])
            }
            EnumItem(ref id, ref variants, _) => {
                let name = self.visit_ident(id);
                let variants = self.visit_list(variants, |me, variant| {
                    let mut n: u32 = 0;
                    let fields = me.visit_list(&variant.args,
                                               |me, t| { n += 1; format!("{} field{};", me.visit_type(t), n - 1) },
                                               "\n        ");
                    let name = self.session.interner.name_to_str(&variant.ident.val.name);
                    format!("struct {{ {} }} {};", fields, name)
                }, "\n");
                format!("typedef struct {} {{\n    int tag;\n    union {{\n        {}\n    }} val;\n}} {};",
                        &name[..],
                        variants,
                        &name[..])
            }
            TypeItem(ref id, ref ty, _) => {
                let name = self.visit_ident(id);
                let ty = self.visit_type(ty);
                format!("typedef {} {};\n", &name[..], &ty[..])
            }
            StaticItem(ref id, ref ty, ref expr, is_extern) => {
                let name_and_type = self.visit_id_and_type(id.id, ty);
                match *expr {
                    Some(ref e) => format!("{} = {};",
                                           name_and_type,
                                           self.visit_expr(e)),
                    None => format!("{}{};",
                                    if is_extern { "extern " } else { "" },
                                    name_and_type)
                }
            }
            ModItem(_, ref body) => {
                self.visit_module(body)
            }
            MacroDefItem(..) => unreachable!(),
        }
    }

    fn visit_func_arg(&mut self, arg: &FuncArg) -> String {
        self.visit_name_and_type(arg.ident.val.name, &arg.argtype)
    }

    fn visit_mangled_path(&self, path: &Path) -> String {
        let resolved_node = self.session.resolver.def_from_path(path);
        match self.mangle_map.get(&resolved_node) {
            Some(n) => n.clone(),
            None => self.visit_path(path)
        }
    }

    fn visit_type(&self, t: &Type) -> String {
        match t.val {
            PtrType(ref t) | ArrayType(ref t, _) => {
                format!("{}*", self.visit_type(&**t))
            }
            NamedType(ref path) => {
                let did = self.session.resolver.def_from_path(path);
                let is_param = {
                    // Is this type a type parameter?
                    let d = self.session.defmap.find(&did).take().unwrap();
                    match *d {
                        Def::GenericDef => true,
                        _ => false,
                    }
                };
                if is_param {
                    // Treat all type parameters as void.
                    "void".to_string()
                } else if self.structnames.contains(&did) {
                    format!("struct {}", self.visit_mangled_path(path))
                } else {
                    self.visit_mangled_path(path)
                }
            }
            FuncType(ref d, ref r) => {
                let ty = self.visit_type(&**r);
                let args = self.visit_list(d, |me, x| me.visit_type(x), ", ");
                format!("{}(*)({})", ty, args)
            }
            TupleType(ref ts) => {
                let mut n: u32 = 0;
                let fields = self.visit_list(ts,
                                             |me, t| { n += 1; format!("{} field{};", me.visit_type(t), n - 1) },
                                             "; ");
                format!("struct {{ {} }}", fields)
            }
            BoolType => "int".to_string(),
            UnitType => "void".to_string(),
            DivergingType => "void".to_string(), // this probably is okay
            IntType(IntKind::UnsignedInt(w)) => format!("uint{}_t", w),
            IntType(IntKind::SignedInt(w)) => format!("int{}_t", w),
            IntType(IntKind::GenericInt) => "int".to_string(),
        }
    }

    fn visit_ty(&self, t: &Ty) -> String {
        match *t {
            BoolTy => "int".to_string(),
            UnitTy => "void".to_string(),
            GenericIntTy => "int".to_string(),
            IntTy(w) => format!("int{}_t", w),
            UintTy(w) => format!("uint{}_t", w),
            PtrTy(ref t) | ArrayTy(ref t, _) => {
                format!("{}*", self.visit_ty(&t.val))
            },
            BoundTy(ref bound_id) => {
                match self.typemap.bounds[bound_id.to_uint()] {
                    Concrete(ref ty) => self.visit_ty(ty),
                    ref bounds => panic!("Type is not fully constrained: {}", bounds),
                }
            }
            EnumTy(did, _) |
            StructTy(did, _) => {
                format!("struct {}", self.mangle_map.get(&did).unwrap())
            }
            BottomTy => "void".to_string(),
            FuncTy(ref d, ref r) => {
                let ty = self.visit_ty(&r.val);
                let list = self.visit_list(d, |me, x| me.visit_ty(&x.val), ", ");
                format!("{} (*)({})", ty, list)
            },
            _ => panic!("Not supported yet: {}", t),
        }
    }

    fn visit_path_in_enum_access(&self, path: &Path) -> String {
        let (_, ref variants, ref pos) = *self.enumitemnames.get(&path.val.elems.last().unwrap().val.name).unwrap();
        let variant = &variants[*pos];
        let name = self.session.interner.name_to_str(&variant.ident.val.name);
        name.to_string()
    }

    fn visit_path(&self, path: &Path) -> String {
        // TODO: better mangling.
        match self.enumitemnames.get(&path.val.elems.last().unwrap().val.name) {
            Some(&(_, _, ref pos)) => format!("{{ .tag = {} }}", pos),
            None => {
                let last_component: Vec<String> = path.val.elems.iter()
                    .map(|elem|
                         self.session.interner.name_to_str(&elem.val.name).to_string())
                    .collect();
                last_component.connect("_")
            },
        }
    }

    fn visit_id(&mut self, id: &NodeId) -> String {
        self.mangle_map.get(id).unwrap().clone()
    }

    fn visit_ident(&mut self, ident: &Ident) -> String {
        match self.mangle_map.get(&ident.id) {
            Some(n) => n.clone(),
            _ => self.session.interner.name_to_str(&ident.val.name).to_string()
        }
    }

    fn visit_lit(&self, lit: &Lit) -> String {
        match lit.val {
            NumLit(ref n, ref k) => {
                match *k {
                    IntKind::UnsignedInt(..) => format!("0x{:x}/*{}*/", *n, n),
                    IntKind::GenericInt |
                    IntKind::SignedInt(Width::AnyWidth) |
                    IntKind::SignedInt(Width::Width32) =>
                                 format!("(int32_t)0x{:x}/*{}*/", *n, n),
                    IntKind::SignedInt(Width::Width16) =>
                                 format!("(int16_t)0x{:x}/*{}*/", *n, n),
                    IntKind::SignedInt(Width::Width8) =>
                        format!("(int8_t)0x{:x}/*{}*/", *n, n),
                }
            },
            // I'm sorry about the cast in the following.
            StringLit(ref s) => {
                let parts: Vec<String> = (&s[..]).bytes()
                    .map(|b: u8|format!("\\x{:02x}", b))
                    .collect();
                format!("(uint8_t*)\"{}\"", parts.concat())
            },
            BoolLit(ref b) => format!("{}", if *b { 1u8 } else { 0 }),
            NullLit => "NULL".to_string(),
        }
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> String {
        match expr.val {
            IfExpr(ref e, ref b1, ref b2) => {
                let cond = self.visit_expr(&**e);
                let thenpart = self.visit_block_expr(&**b1);
                if is_block_empty(&**b2) {
                    format!("if ({}) {}", cond, thenpart)
                } else {
                    let elsepart = self.visit_block_expr(&**b2);
                    format!("if ({}) {} else {}", cond, thenpart, elsepart)
                }
            }
            ForExpr(..) | WhileExpr(..) => self.visit_expr(expr),
            BlockExpr(ref b) => self.visit_block_expr(&**b),
            _ => format!("{};", self.visit_expr(expr))
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> String {
        match expr.val {
            UnitExpr => "({})".to_string(),
            LitExpr(ref l) => self.visit_lit(l),
            SizeofExpr(ref t) => format!("sizeof({})", self.visit_type(t)),
            TupleExpr(..) => panic!("Tuples not yet supported."),
            GroupExpr(ref e) => format!("({})", self.visit_expr(&**e)),
            PathExpr(ref p) => {
               self.visit_mangled_path(p)
            }
            StructExpr(_, ref args) => {
                let args =
                    self.mut_visit_list(
                        args,
                        |me, &(ref name, ref e)| {
                            let e = me.visit_expr(e);
                            let name = me.session.interner.name_to_str(name);
                            format!(".{} = {}", name, e)
                        },
                        ", ");
                format!("{{ {} }}",
                        args)
            }
            BinOpExpr(ref op, ref lhs, ref rhs) => {
                let lhs = self.visit_expr(&**lhs);
                let op = self.visit_binop(op);
                let rhs = self.visit_expr(&**rhs);
                let typename = self.visit_ty(&self.typemap.types[expr.id.to_uint()]);
                format!("({})(({}) {} ({}))", typename, lhs, op, rhs)
            }
            UnOpExpr(ref op, ref expr) => {
                let op = self.visit_unop(op);
                let expr = self.visit_expr(&**expr);
                format!("{}({})", op, expr)
            }
            IndexExpr(ref exp, ref idx) => {
                let exp = self.visit_expr(&**exp);
                let idx = self.visit_expr(&**idx);
                format!("({})[{}]", exp, idx)
            }
            DotExpr(ref exp, ref field) => {
                let exp = self.visit_expr(&**exp);
                let field = self.session.interner.name_to_str(field);
                format!("({}).{}", exp, field)
            }
            ArrowExpr(ref exp, ref field) => {
                let exp = self.visit_expr(&**exp);
                let field = self.session.interner.name_to_str(field);
                format!("({})->{}", exp, field)
            }
            AssignExpr(ref op, ref lhs, ref rhs) => {
                let lhs = self.visit_expr(&**lhs);
                let rhs = self.visit_expr(&**rhs);
                let op = op.map_or(String::new(), |op| format!("{}", op));
                format!("({}) {}= ({})", lhs, op, rhs)
            }
            ArrayExpr(ref elems) => {
                format!("{{ {} }}",
                        self.mut_visit_list(
                            elems,
                            |me, e| me.visit_expr(e),
                            ", "))
            }
            CallExpr(ref f, ref args) => {
                let res_type = self.visit_ty(&self.typemap.types[expr.id.to_uint()]);
                match f.val {
                    PathExpr(ref path) => {
                        let name = self.visit_mangled_path(path);

                        match self.enumitemnames.get(&path.val.elems.last().unwrap().val.name) {
                            Some(&(_, _, pos)) => {
                                let mut n: u32 = 0;
                                let args = self.mut_visit_list(args, |me, arg| {
                                    n += 1;
                                    let expr = me.visit_expr(arg);
                                    let actual_name = me.visit_path_in_enum_access(path);
                                    format!(".val.{}.field{} = {}", actual_name, n - 1, expr)
                                }, ", ");
                                format!("{{ .tag = {}, {} }}", pos, args)
                            }
                            None => {
                                let args = self.mut_visit_list(args, |me, x| me.visit_expr(x), ", ");
                                format!("(({}){}({}))", res_type, name, args)
                            }
                        }
                    }
                    _ => {
                        let f = self.visit_expr(&**f);
                        let args = self.mut_visit_list(args, |me, x| me.visit_expr(x), ", ");
                        format!("{}({})", f, args)
                    }
                }
            }
            CastExpr(ref e, ref t) => {
                let ty = self.visit_type(t);
                let expr = self.visit_expr(&**e);
                format!("({})({})", ty, expr)
            }
            IfExpr(ref e, ref b1, ref b2) => {
                let cond = self.visit_expr(&**e);
                let thenpart = self.visit_block_expr(&**b1);
                let elsepart = self.visit_block_expr(&**b2);
                format!("(({}) ? ({}) : ({}))", cond, thenpart, elsepart)
            }
            BlockExpr(ref b) => {
                let expr = self.visit_block_expr(&**b);
                format!("({})", expr)
            }
            ReturnExpr(ref e) => {
                let expr = self.visit_expr(&**e);
                format!("return/*expr*/ {};", expr)
            }
            BreakExpr => format!("break;"),
            ContinueExpr => format!("continue;"),
            WhileExpr(ref e, ref b) => {
                let cond = self.visit_expr(&**e);
                let body = self.visit_block_expr(&**b);
                format!("while ({}) {}", cond, body)
            }
            DoWhileExpr(ref e, ref b) => {
                let cond = self.visit_expr(&**e);
                let body = self.visit_block_expr(&**b);
                format!("do {} while ({})", body, cond)
            }
            ForExpr(ref e1, ref e2, ref e3, ref b) => {
                let e1 = self.visit_expr(&**e1);
                let e2 = self.visit_expr(&**e2);
                let e3 = self.visit_expr(&**e3);
                let body = self.visit_block_expr(&**b);
                format!("for ({}; {}; {}) {}", e1, e2, e3, body)
            }
            MatchExpr(ref e, ref arms) => {
                // TODO: allow types other than ints.
                let (is_void, overall_type_name) = {
                    let ref overall_type = self.typemap.types[expr.id.to_uint()];
                    (*overall_type == UnitTy, self.visit_ty(overall_type))
                };

                let expr = self.visit_expr(&**e);
                let arms = self.mut_visit_list(arms, |me, arm| {
                    let (path, vars): (&Path, &Vec<Pat>) = match arm.pat.val {
                        VariantPat(ref path, ref args) => (path, args),
                        _ => panic!("Only VariantPats are supported in match arms for now")
                    };

                    let body = me.visit_expr(&arm.body);

                    let &(_, ref variants, idx) = me.enumitemnames.get(&path.val.elems.last().unwrap().val.name).unwrap();
                    let this_variant = &variants[idx as uint];

                    let name = me.visit_path_in_enum_access(path);

                    let mut n = 0;
                    //TODO!!!!!!!
                    let vars = "";
                    /*
                    let vars = me.visit_list(vars, |me, var: &Pat| {
                        n += 1;
                        let ty = me.visit_type(&this_variant.args[n - 1]);
                        let varname = match var.val {
                            IdentPat(ref id, _) => me.session.interner.name_to_str(&id.val.name),
                            _ => panic!("Only IdentPats are supported in the arguments of a VariantPat in a match arm for now"),
                        };
                        format!("{} {} = {}.val.{}.field{};",
                                ty, varname, &expr[..], name, n - 1)
                    }, "; ");*/

                    if is_void {
                        format!("case {}: {{\n {}; ({}); break;}}\n",
                                idx, vars, body)
                    } else {
                        format!("case {}: {{\n {} _ = ({}); break;}}\n",
                                idx, vars, body)
                    }
                }, "\n");

                if is_void {
                    format!("({{ switch(({}).tag) {{\n{} \n}} ; }})",
                            expr, arms)
                } else {
                    format!("({{ {} _; switch(({}).tag) {{\n{} \n}} _; }})",
                            overall_type_name, expr, arms)
                }
            }
            MacroExpr(..) => unreachable!(),
        }
    }

    // Visit a module and all of its submodules, applying a worker function.
    // We need this so we can print out all the modules, then all the structs,
    // etc.
    fn visit_module_worker(&mut self,
                           results: &mut Vec<String>,
                           module: &Module,
                           f: &Fn(&mut CCrossCompiler, &mut Vec<String>,&Module)) {

        for item in module.val.items.iter() {
            match item.val {
                ModItem(_, ref module) =>
                    self.visit_module_worker(results, module,
                                             &|me, results, x| f(me,results,x)),
                _ => {}
            }
        }

        f(self, results, module);
    }

    fn visit_module(&mut self, module: &Module) -> String {
        let mut results = vec!();

        // Typedefs
        self.visit_module_worker(&mut results, module, &|me, results, module| {
            results.push(me.mut_visit_list(&module.val.items, |me, item| {
                match item.val {
                    TypeItem(..) => me.visit_item(item),
                    _ => "".to_string(),
                }
            }, "\n"));
        });

        // Constants
        self.visit_module_worker(&mut results, module, &|me, results, module| {
            results.push(me.mut_visit_list(&module.val.items, |me, item| {
                match item.val {
                    ConstItem(..) => me.visit_item(item),
                    _ => "".to_string(),
                }
            }, "\n"));
        });

        // Now print struct prototypes.
        self.visit_module_worker(&mut results, module, &|me, results, module| {
            for item in module.val.items.iter() {
                match item.val {
                    StructItem(ref id, _, _) |
                    EnumItem(ref id, _, _) => {
                        let name = me.visit_ident(id);
                        results.push(format!("struct {};\n", name));
                    },
                    _ => {}
                }
            }
        });

        // Now print function prototypes.
        self.visit_module_worker(&mut results, module, &|me, results, module| {

            for item in module.val.items.iter() {
                match item.val {
                    FuncItem(ref name, ref args, ref t, ref d, _) => {
                        let ty = me.visit_type(t);
                        let name = me.visit_ident(name);
                        // This is a terrible hack.
                        if &name[..] == "malloc" ||
                            &name[..] == "calloc" ||
                            &name[..] == "assert" {
                                continue;
                            }
                        let args = me.mut_visit_list(
                            args,
                            |me, x| me.visit_func_arg(x),
                            ", ");

                        match *d {
                            LocalFn(_) =>
                                results.push(format!("{} {}({});\n",
                                                     ty, name, args)),
                            ExternFn(_) =>
                                results.push(format!("extern {} {}({});\n",
                                                     ty, name, args))
                        }
                    },
                    _ => {},
                }
            }
        });

        // Structs and enums.
        // FIXME: need to topo sort structs
        self.visit_module_worker(&mut results, module, &|me, results, module| {
            results.push(me.mut_visit_list(&module.val.items, |me, item| {
                match item.val {
                    StructItem(..) |
                    EnumItem(..) => me.visit_item(item),
                    _ => "".to_string(),
                }
            }, "\n"));
        });

        // Now globals
        self.visit_module_worker(&mut results, module, &|me, results, module| {
            results.push(me.mut_visit_list(&module.val.items, |me, item| {
                match item.val {
                    StaticItem(..) => me.visit_item(item),
                    _ => "".to_string(),
                }
            }, "\n"));
        });

        // And functions
        self.visit_module_worker(&mut results, module, &|me, results, module| {
            results.push(me.mut_visit_list(&module.val.items, |me, item| {
                match item.val {
                    FuncItem(..) => me.visit_item(item),
                    _ => "".to_string(),
                }
            }, "\n"));
        });

        results.connect("\n")
    }
}

pub struct CTarget {
    opts: (),
}

impl Target for CTarget {
    fn new(_args: Vec<String>) -> Box<CTarget> {
        Box::new(CTarget { opts: () })
    }

    #[allow(unused_must_use)]
    fn compile(&self, p: Package, f: &mut Write) {
        let Package {
            module,
            session,
            typemap,
        } = p;

        let mangler = NameMangler::new(session, &module, false, false);

        let mut cc = CCrossCompiler {
            structnames: find_structs(&module),
            enumitemnames: find_enum_item_names(&module),
            enumnames: find_enum_names(&module),
            session: mangler.session,
            typemap: typemap,
            mangle_map: mangler.names,
            indent: 0,
        };

        emit_ccross_prelude(f);
        writeln!(f, "{}", cc.visit_module(&module));
    }
}
