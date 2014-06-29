use mc::lexer::Lexer;
use mc::parser::Parser;
use mc::ast::visitor::Visitor;
use mc::resolver::Resolver;
use mc::session::Session;

use package::Package;
use target::Target;
use util::Name;
use intrinsics::size_of;

use std::collections::treemap::{TreeSet, TreeMap};

use util;
use std::io::stdio;

use mc::ast::*;
use mc::ast::defmap::*;

use typechecker::*;
use values::*;

struct CCrossCompiler {
    structnames: TreeSet<NodeId>,
    enumitemnames: TreeMap<Name, (Ident, Vec<Variant>, uint)>,
    enumnames: TreeMap<NodeId, Name>,
    session: Session,
    typemap: Typemap,
}

// TODO: this and find_enum_item_names are hacks, and don't actually support
// the module system (they will break in the presence of anything nontrivial
// with paths).
fn find_structs(module: &Module) -> TreeSet<NodeId> {
    let mut struct_set = TreeSet::new();

    for item in module.val.items.iter() {
        match item.val {
            StructItem(ref id, _, _) => { struct_set.insert(id.id); },
            _ => {},
        }
    }

    struct_set
}

fn find_enum_item_names(module: &Module) -> TreeMap<Name,
                                                    (Ident,
                                                     Vec<Variant>,
                                                     uint)> {
    let mut enum_map = TreeMap::new();

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
            _ => {},
        }
    }

    enum_map
}

fn find_enum_names(module: &Module) -> TreeMap<NodeId, Name> {
    let mut enum_map = TreeMap::new();

    for item in module.val.items.iter() {
        match item.val {
            EnumItem(ref id, _, _) => {
                enum_map.insert(item.id, id.val.name);
            },
            _ => {}
        }
    }

    enum_map
}

impl CCrossCompiler {
    fn visit_list<T>(&self, list: &Vec<T>,
                            visit: |&T| -> String,
                            delimiter: &str) -> String {
        let list: Vec<String> = list.iter().map(visit).collect();
        list.connect(format!("{}\n", delimiter).as_slice())
    }

    fn visit_binop(&self, op: &BinOp) -> String {
        format!("{}", op)
    }

    fn visit_unop(&self, op: &UnOp) -> String {
        format!("{}", op)
    }

    // A block, as an expression.
    fn visit_block_expr(&self, block: &Block) -> String {
        self.visit_block(block, |e| e.map(|e| format!("{};", e)).unwrap_or_default())
    }

    fn visit_name_and_type(&self, name: Name, t: &Type) -> String {
        match t.val {
            // We have to special case this, because of the way things of
            // a function pointer type are declared in C.
            FuncType(ref d, ref r) => {
                let ty = self.visit_type(*r);
                let list = self.visit_list(d, |x| self.visit_type(x), ", ");
                let name = self.session.interner.name_to_str(&name);
                format!("{}(*{})({})", ty, name, list)
            },
            ArrayType(ref t, ref size) => {
                let name = self.session.interner.name_to_str(&name);
                format!("{} {}[{}]", self.visit_type(*t), name, *size)
            },
            _ => {
                let ty = self.visit_type(t);
                let name = self.session.interner.name_to_str(&name);
                format!("{} {}", ty, name)
            }
        }
    }

    fn visit_stmt(&self, stmt: &Stmt) -> String {
        match stmt.val {
            LetStmt(ref pat, ref e) => {
                let (i, t) = match pat.val {
                    IdentPat(ref i, ref t) => (i, t),
                    _ => fail!("Only IdentPats are supported right now"),
                };
                let name = self.visit_ident(i);
                let ty = match *t {
                    Some(ref ty) => self.visit_name_and_type(i.val.name, ty),
                    None => match *e {
                        Some(ref expr) => format!("{} {}", self.visit_ty(self.typemap.types.get(&expr.id.to_uint())), name),
                        None => fail!("Must specify a type."),
                    }
                };
                let maybe_expr = e.as_ref().map(|e| format!(" = {}", self.visit_expr(e))).unwrap_or_default();
                format!("{} {};", ty, maybe_expr)
            },
            ExprStmt(ref e) | SemiStmt(ref e) => { format!("{};", self.visit_expr(e)) },
        }
    }

    fn visit_block(&self, block: &Block, tail: |Option<String>| -> String) -> String {
        let items = self.visit_list(&block.val.items, |t| self.visit_item(t), "; ");
        let stmts = self.visit_list(&block.val.stmts, |t| self.visit_stmt(t), "; ");
        let expr = match block.val.expr {
            Some(ref x) => {
                match x.val {
                    WhileExpr(..) | ForExpr(..) => self.visit_expr(x),
                    ReturnExpr(ref e) => tail(Some(self.visit_expr(*e))),
                    _ => tail(Some(self.visit_expr(x))),
                }
            }
            None => tail(None),
        };
        format!("{{ {} {} {} }}", items, stmts, expr)
    }

    fn visit_item(&self, item: &Item) -> String {
        match item.val {
            UseItem(..) => String::new(),
            ConstItem(ref id, ref t, ref expr) => {
                let name_and_type = self.visit_name_and_type(id.val.name, t);
                format!("const {} = {};", name_and_type, self.visit_expr(expr))
            }
            FuncItem(ref name, ref args, ref t, ref block, _) => {
                match *block {
                    Some(ref block) => {
                        let ty = self.visit_type(t);
                        let name = self.visit_ident(name);
                        let args = self.visit_list(args, |x| self.visit_func_arg(x), ", ");

                        let block = self.visit_block(block, |e| {
                            match e {
                                Some(e) => format!("return {};", e),
                                None => String::from_str("return;"),
                            }
                        });

                        format!("{} {}({}) {}", ty, name, args, block)
                    }
                    None => String::new(),
                }
            }
            StructItem(ref id, ref fields, _) => {
                let name = self.visit_ident(id);
                let fields = self.visit_list(fields,
                                             |field| format!("{};", self.visit_name_and_type(field.name, &field.fldtype)),
                                             "\n    ");
                format!("typedef struct {} {{\n    {}\n}} {};", name.as_slice(), fields, name.as_slice())
            }
            EnumItem(ref id, ref variants, _) => {
                let name = self.visit_ident(id);
                let variants = self.visit_list(variants, |variant| {
                    let mut n: u32 = 0;
                    let fields = self.visit_list(&variant.args,
                                                 |t| { n += 1; format!("{} field{};", self.visit_type(t), n - 1) },
                                                 "\n        ");
                    let name = self.session.interner.name_to_str(&variant.ident.val.name);
                    format!("struct {{ {} }} {};", fields, name)
                }, "\n");
                format!("typedef struct {} {{\n    int tag;\n    union {{\n        {}\n    }} val;\n}} {};",
                        name.as_slice(),
                        variants,
                        name.as_slice())
            }
            StaticItem(ref id, ref ty, ref expr) => {
                let name_and_type = self.visit_name_and_type(id.val.name, ty);
                match *expr {
                    Some(ref e) => format!("{} = {};",
                                           name_and_type,
                                           self.visit_expr(e)),
                    None => format!("{};", name_and_type)
                }
            }
            ModItem(..) => fail!("ModItem not supported yet"),
            MacroDefItem(..) => unreachable!(),
        }
    }

    fn visit_func_arg(&self, arg: &FuncArg) -> String {
        self.visit_name_and_type(arg.ident.val.name, &arg.argtype)
    }

    fn visit_type(&self, t: &Type) -> String {
        match t.val {
            PtrType(ref t) | ArrayType(ref t, _) => {
                format!("{}*", self.visit_type(*t))
            }
            NamedType(ref path) => {
                let did = self.session.resolver.def_from_path(path);
                let is_param = {
                    // Is this type a type parameter?
                    let d = self.session.defmap.find(&did).take_unwrap();
                    match *d {
                        GenericDef => true,
                        _ => false,
                    }
                };
                if is_param {
                    // Treat all type parameters as void.
                    String::from_str("void")
                } else if self.structnames.contains(&did) {
                    format!("struct {}", self.visit_path(path))
                } else {
                    self.visit_path(path)
                }
            }
            FuncType(ref d, ref r) => {
                let ty = self.visit_type(*r);
                let args = self.visit_list(d, |x| self.visit_type(x), ", ");
                format!("{}(*)({})", ty, args)
            }
            TupleType(ref ts) => {
                let mut n: u32 = 0;
                let fields = self.visit_list(ts,
                                             |t| { n += 1; format!("{} field{};", self.visit_type(t), n - 1) },
                                             "; ");
                format!("struct {{ {} }}", fields)
            }
            BoolType => String::from_str("int"),
            UnitType => String::from_str("void"),
            DivergingType => String::from_str("void"), // this probably is okay
            IntType(util::UnsignedInt(w)) => format!("uint{}_t", w),
            IntType(util::SignedInt(w)) => format!("int{}_t", w),
            IntType(util::GenericInt) => String::from_str("int"),
        }
    }

    fn visit_ty(&self, t: &Ty) -> String {
        match *t {
            BoolTy => String::from_str("int"),
            UnitTy => String::from_str("void"),
            GenericIntTy => String::from_str("int"),
            IntTy(w) => format!("int{}_t", w),
            UintTy(w) => format!("uint{}_t", w),
            PtrTy(ref t) | ArrayTy(ref t, _) => {
                format!("{}*", self.visit_ty(&t.val))
            },
            BoundTy(ref bound_id) => {
                match *self.typemap.bounds.get(&bound_id.to_uint()) {
                    Concrete(ref ty) => self.visit_ty(ty),
                    ref bounds => fail!("Type is not fully constrained: {}", bounds),
                }
            }
            StructTy(did, _) => {
                match *self.session.defmap.find(&did).take_unwrap() {
                    StructDef(ref qn, _, _) => {
                        let qn: Vec<&str> = qn.iter().map(|n| self.session.interner.name_to_str(n)).collect();
                        qn.connect("_")
                    }
                    _ => unreachable!()
                }
            }
            EnumTy(did, _) => {
                match *self.session.defmap.find(&did).take_unwrap() {
                    EnumDef(ref qn, _, _) => {
                        let qn: Vec<&str> = qn.iter().map(|n| self.session.interner.name_to_str(n)).collect();
                        qn.connect("_")
                    }
                    _ => unreachable!()
                }
            }
            _ => fail!("Not supported yet: {}", t),
        }
    }

    fn visit_path_in_enum_access(&self, path: &Path) -> String {
        let (_, ref variants, ref pos) = *self.enumitemnames.find(&path.val.elems.last().unwrap().val.name).unwrap();
        let variant = variants.get(*pos);
        let name = self.session.interner.name_to_str(&variant.ident.val.name);
        name.to_string()
    }

    fn visit_path(&self, path: &Path) -> String {
        match self.enumitemnames.find(&path.val.elems.last().unwrap().val.name) {
            Some(&(_, _, ref pos)) => format!("{{ .tag = {} }}", pos),
            None => {
                let vec: Vec<&str> = path.val.elems.iter().map(|elem| self.session.interner.name_to_str(&elem.val.name)).collect();
                vec.connect("_")
            },
        }
    }

    fn visit_ident(&self, ident: &Ident) -> String {
        format!("{}", self.session.interner.name_to_str(&ident.val.name))
    }

    fn visit_lit(&self, lit: &Lit) -> String {
        match lit.val {
            NumLit(ref n, _) => format!("{}", n),
            // I'm sorry about the cast in the following.
            StringLit(ref s) => format!("(uint8_t*)\"{}\"", s),
            BoolLit(ref b) => format!("{}", if *b { 1u8 } else { 0 }),
            NullLit => String::from_str("NULL"),
        }
    }

    fn visit_expr(&self, expr: &Expr) -> String {
        match expr.val {
            UnitExpr => String::from_str("({})"),
            LitExpr(ref l) => self.visit_lit(l),
            SizeofExpr(ref t) => format!("sizeof({})", self.visit_type(t)),
            TupleExpr(..) => fail!("Tuples not yet supported."),
            GroupExpr(ref e) => format!("({})", self.visit_expr(*e)),
            PathExpr(ref p) => self.visit_path(p),
            StructExpr(_, ref args) => {
                let args =
                    self.visit_list(
                        args,
                        |&(ref name, ref e)|
                        format!(".{} = {}",
                                self.session.interner.name_to_str(
                                    name),
                                self.visit_expr(e)),
                        ", ");
                format!("{{ {} }}",
                        args)
            }
            BinOpExpr(ref op, ref lhs, ref rhs) => {
                let lhs = self.visit_expr(*lhs);
                let op = self.visit_binop(op);
                let rhs = self.visit_expr(*rhs);
                format!("({}) {} ({})", lhs, op, rhs)
            }
            UnOpExpr(ref op, ref expr) => {
                let op = self.visit_unop(op);
                let expr = self.visit_expr(*expr);
                format!("{}({})", op, expr)
            }
            IndexExpr(ref exp, ref idx) => {
                let exp = self.visit_expr(*exp);
                let idx = self.visit_expr(*idx);
                format!("({})[{}]", exp, idx)
            }
            DotExpr(ref exp, ref field) => {
                let exp = self.visit_expr(*exp);
                let field = self.session.interner.name_to_str(field);
                format!("({}).{}", exp, field)
            }
            ArrowExpr(ref exp, ref field) => {
                let exp = self.visit_expr(*exp);
                let field = self.session.interner.name_to_str(field);
                format!("({})->{}", exp, field)
            }
            AssignExpr(ref op, ref lhs, ref rhs) => {
                let lhs = self.visit_expr(*lhs);
                let rhs = self.visit_expr(*rhs);
                let op = op.map_or(String::new(), |op| format!("{}", op));
                format!("({}) {}= ({})", lhs, op, rhs)
            }
            CallExpr(ref f, ref args) => {
                let res_type = self.visit_ty(self.typemap.types.get(&expr.id.to_uint()));
                match f.val {
                    PathExpr(ref path) => {
                        let name = self.visit_path(path);

                        match self.enumitemnames.find(&path.val.elems.last().unwrap().val.name) {
                            Some(&(_, _, pos)) => {
                                let mut n: u32 = 0;
                                let args = self.visit_list(args, |arg| {
                                    n += 1;
                                    let expr = self.visit_expr(arg);
                                    let actual_name = self.visit_path_in_enum_access(path);
                                    format!(".val.{}.field{} = {}", actual_name, n - 1, expr)
                                }, ", ");
                                format!("{{ .tag = {}, {} }}", pos, args)
                            }
                            None => {
                                let args = self.visit_list(args, |x| self.visit_expr(x), ", ");
                                format!("(({}){}({}))", res_type, name, args)
                            }
                        }
                    }
                    _ => {
                        let f = self.visit_expr(*f);
                        let args = self.visit_list(args, |x| self.visit_expr(x), ", ");
                        format!("{}({})", f, args)
                    }
                }
            }
            CastExpr(ref e, ref t) => {
                let ty = self.visit_type(t);
                let expr = self.visit_expr(*e);
                format!("({})({})", ty, expr)
            }
            IfExpr(ref e, ref b1, ref b2) => {
                let cond = self.visit_expr(*e);
                let thenpart = self.visit_block_expr(*b1);
                let elsepart = self.visit_block_expr(*b2);
                format!("(({})?({}):({}))", cond, thenpart, elsepart)
            }
            BlockExpr(ref b) => self.visit_block_expr(*b),
            ReturnExpr(ref e) => {
                let expr = self.visit_expr(*e);
                format!("return/*expr*/ {};", expr)
            }
            BreakExpr => format!("break;"),
            ContinueExpr => format!("continue;"),
            WhileExpr(ref e, ref b) => {
                let cond = self.visit_expr(*e);
                let body = self.visit_block_expr(*b);
                format!("while({}) {{\n{};}}\n", cond, body)
            }
            ForExpr(ref e1, ref e2, ref e3, ref b) => {
                let e1 = self.visit_expr(*e1);
                let e2 = self.visit_expr(*e2);
                let e3 = self.visit_expr(*e3);
                let body = self.visit_block_expr(*b);
                format!("for({};{};{}) {{\n{};}}\n", e1, e2, e3, body)
            }
            MatchExpr(ref e, ref arms) => {
                // TODO: allow types other than ints.
                let overall_type = self.typemap.types.get(&expr.id.to_uint());
                let is_void = *overall_type == UnitTy;
                let overall_type_name = self.visit_ty(overall_type);

                let expr = self.visit_expr(*e);
                let arms = self.visit_list(arms, |arm| {
                    let (path, vars) = match arm.pat.val {
                        VariantPat(ref path, ref args) => (path, args),
                        _ => fail!("Only VariantPats are supported in match arms for now")
                    };

                    let &(_, ref variants, idx) = self.enumitemnames.find(&path.val.elems.last().unwrap().val.name).unwrap();
                    let this_variant = variants.get(idx as uint);

                    let name = self.visit_path_in_enum_access(path);

                    let mut n = 0;
                    let vars = self.visit_list(vars, |var| {
                        n += 1;
                        let ty = self.visit_type(this_variant.args.get(n - 1));
                        let varname = match var.val {
                            IdentPat(ref id, _) => self.session.interner.name_to_str(&id.val.name),
                            _ => fail!("Only IdentPats are supported in the arguments of a VariantPat in a match arm for now"),
                        };
                        format!("{} {} = {}.val.{}.field{};",
                                ty, varname, expr.as_slice(), name, n - 1)
                    }, "; ");

                    let body = self.visit_expr(&arm.body);
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

    fn visit_module(&self, module: &Module) -> String {
        let mut results = vec!();

        // Start with struct prototypes.
        for item in module.val.items.iter() {
            match item.val {
                StructItem(ref id, _, _) |
                EnumItem(ref id, _, _) => {
                    let name = self.visit_ident(id);
                    results.push(format!("struct {};\n", name));
                },
                _ => {}
            }
        }

        // Now print function prototypes.
        for item in module.val.items.iter() {
            match item.val {
                FuncItem(ref name, ref args, ref t, ref b, _) => {
                    let ty = self.visit_type(t);
                    let name = self.visit_ident(name);
                    // This is a terrible hack.
                    if name.as_slice() == "malloc" ||
                        name.as_slice() == "calloc" ||
                        name.as_slice() == "assert" {
                            continue;
                        }
                    let args = self.visit_list(args, |x| self.visit_func_arg(x), ", ");

                    match *b {
                        Some(_) =>
                            results.push(format!("{} {}({});\n", ty, name, args)),
                        None =>
                            results.push(format!("extern {} {}({});\n", ty, name, args))
                    }
                },
                _ => {},
            }
        }

        results.push(self.visit_list(&module.val.items, |item| {
            match item.val {
                StructItem(..) |
                EnumItem(..) => self.visit_item(item),
                _ => String::from_str(""),
            }
        }, "\n"));

        results.push(self.visit_list(&module.val.items, |item| {
            match item.val {
                StaticItem(..) |
                ConstItem(..) => self.visit_item(item),
                _ => String::from_str(""),
            }
        }, "\n"));

        results.push(self.visit_list(&module.val.items, |item| {
            match item.val {
                FuncItem(..) => self.visit_item(item),
                _ => String::from_str(""),
            }
        }, "\n"));

        results.connect("")
    }
}

pub struct CTarget {
    opts: (),
}

impl Target for CTarget {
    fn new(_args: Vec<String>) -> CTarget {
        CTarget { opts: () }
    }

    fn compile(&self, p: Package) {
        let Package {
            module:  module,
            session: session,
            typemap: typemap,
        } = p;

        let cc = CCrossCompiler {
            structnames: find_structs(&module),
            enumitemnames: find_enum_item_names(&module),
            enumnames: find_enum_names(&module),
            session: session,
            typemap: typemap,
        };

/*
        let mut stderr = stdio::stderr();

        match writeln!(stderr, "{}", module) {
            Err(e) => fail!("{}", e),
            _ => {}
        }
*/
        /* what?
        match writeln!(stderr, "{}", cc.enumitemnames) {
            Err(e) => fail!("{}", e),
            _ => {}
        }
        */

        println!("{}", "#include <stdio.h>");
        println!("{}", "#include <stdlib.h>");
        println!("{}", "#include <stdint.h>");
        println!("{}", "#include <assert.h>");
        println!("{}", "typedef unsigned int uint_t;");
        println!("{}", "typedef int int_t;");

        println!("{}", "int32_t printf0_(uint8_t *s) { return printf(\"%s\", (char *)s); }");
        println!("{}", "int32_t printf1_(uint8_t *s, uint32_t a) { return printf((char *)s, a); }");
        println!("{}", "int32_t printf2_(uint8_t *s, uint32_t a, uint32_t b) { return printf((char *)s, a, b); }");
        println!("{}", "int32_t printf3_(uint8_t *s, uint32_t a, uint32_t b, uint32_t c) { return printf((char *)s, a, b, c); }");
        println!("{}", "int32_t print_int(int32_t x) { printf(\"%d\\n\", (int)x); return x; }");
        println!("{}", "int32_t print_char(int32_t x) { printf(\"%c\", (int)x); return x; }");
        println!("{}", cc.visit_module(&module));
    }
}
