use ast::*;
use ast::defmap::*;
use values::*;
use std::io::stdio;
use lexer::Lexer;
use parser::Parser;
use ast::visit::{Visitor, walk_module};
use collections::treemap::{TreeSet, TreeMap};
use resolver::Resolver;
use session::Session;
use package::Package;
use target::Target;
use typechecker::{Typechecker, Typemap};
use util::Name;

struct CCrossCompiler {
    builtins: TreeSet<Name>,
    structnames: TreeSet<Name>,
    enumitemnames: TreeMap<Name, (Ident, Vec<Variant>, int)>,
    session: Session,
    typemap: Typemap,
}

fn find_structs(module: &Module) -> TreeSet<Name> {
    let mut ht = TreeSet::new();
    for item in module.items.iter() {
        match item.val {
            StructItem(ref id, _, _) => { ht.insert(id.val.name); },
            _ => {}
        }
    }

    ht
}

fn find_enum_item_names(module: &Module) -> TreeMap<Name,
                                                    (Ident,
                                                     Vec<Variant>,
                                                     int)> {
    let mut ht = TreeMap::new();
    for enumitem in module.items.iter() {
        match enumitem.val {
            EnumItem(ref name, ref items, _) => {
                let mut pos = 0;
                for item in items.iter() {
                    ht.insert(item.ident.val.name,
                              (name.clone(), items.clone(), pos));
                    pos += 1;
                }
            },
            _ => {}
        }
    }

    ht
}

impl CCrossCompiler {
    fn visit_list<T>(&self, list: &Vec<T>,
                     visit: |&CCrossCompiler, &T| -> ~str,
                     delimiter: &str) -> ~str {
        let mut res = "".to_owned();
        let mut count = 0;
        for t in list.iter() {
            res = res + visit(self, t);
            count += 1;
            if count < list.len() {
                res = res + format!("{}\n", delimiter);
            }
        }
        res
    }

    fn visit_binop(&self, op: &BinOp) -> ~str {
        format!("{}", op)
    }

    fn visit_unop(&self, op: &UnOp) -> ~str {
        format!("{}", op)
    }

    // A block, as an expression.
    fn visit_expr_block(&self, block: &Block) -> ~str {
        "({ ".to_owned() +
            self.visit_list(&block.items, |s, t| s.visit_item(t), "; ") +
            self.visit_list(&block.stmts, |s, t| s.visit_stmt(t), "; ") +
            match block.expr {
                Some(ref x) => {
                    self.visit_expr(x) +
                    ";"
                },
                None => "".to_owned(),
            } +
            "})"
    }

    fn visit_name_and_type(&self, name: Name, t: &Type) -> ~str {
        match t.val {
            // We have to special case this, because of the way things of
            // a function pointer type are declared in C.
            FuncType(ref d, ref r) => {
                let ty = self.visit_type(*r);
                let list = self.visit_list(d, |s, x| s.visit_type(x), ", ");
                let name = self.session.interner.name_to_str(&name);
                format!("{}(*{})({})", ty, name, list)
            },
            _ => {
                let ty = self.visit_type(t);
                let name = self.session.interner.name_to_str(&name);
                format!("{} {}", ty, name)
            }
        }
    }

    fn visit_stmt(&self, stmt: &Stmt) -> ~str {
        match stmt.val {
            LetStmt(ref pat, ref e) => {
                let (i, t) = match pat.val {
                    IdentPat(ref i, ref t) => (i, t),
                    _ => fail!("Only IdentPats are supported right now"),
                };
                if t.is_none() { fail!("Must specify types now.") };
                self.visit_type(&t.clone().unwrap()) +
                    " " +
                    self.visit_ident(i) +
                    match *e {
                        Some(ref exp) => {
                            " = ".to_owned() +
                                self.visit_expr(exp)
                        },
                        None => "".to_owned(),
                    } +
                    ";"
            },
            ExprStmt(ref e) => { self.visit_expr(e) + ";" },
            SemiStmt(ref e) => { self.visit_expr(e) + ";" },
        }
    }

    fn visit_block(&self, block: &Block) -> ~str {
        "{".to_owned() +
            self.visit_list(&block.items, |s, t| s.visit_item(t), "; ") +
            self.visit_list(&block.stmts, |s, t| s.visit_stmt(t), "; ") +
            match block.expr {
                Some(ref x) => { match x.val {
                                     ReturnExpr(ref e) => self.visit_expr(*e),
                                     WhileExpr(_, _) =>
                                         self.visit_expr(x),
                                     _ => {
                                         "return ".to_owned() +
                                         self.visit_expr(x)
                                     }
                                  }
                }
            None => "return".to_owned(),
        } + ";}"
    }

    fn visit_item(&self, item: &Item) -> ~str {
        match item.val {
            FuncItem(ref name, ref args, ref t, ref block, _) => {
                // Emit nothing for builtin functions.
                if self.builtins.contains(&name.val.name) { "".to_owned() } else {
                    self.visit_type(t) +
                        " " +
                        self.visit_ident(name) +
                        "(" +
                        self.visit_list(args, |s, x| s.visit_func_arg(x), ", ") +
                        ")" +
                        self.visit_block(block)
                }
            }
            StructItem(ref id, ref fields, _) => {
                let mut res = format!("typedef struct {} \\{", self.visit_ident(id));
                for field in fields.iter() {
                    res = res + self.visit_name_and_type(field.name,
                                                         &field.fldtype) + ";\n";
                }
                res + 
                    format!("{} {};", "}", self.session.interner.name_to_str(&id.val.name))
            }
            EnumItem(ref id, ref variants, _) => {
                let mut res = format!("typedef struct {} \\{\n    int tag;\n    union \\{\n",
                                      self.visit_ident(id));
                for variant in variants.iter() {
                    res = res + "        struct {\n";
                    let mut num = 0;
                    for t in variant.args.iter() {
                        res = res + "            " + self.visit_type(t) +
                            " " + format!("field{};\n", num);
                        num += 1;
                    }
                    res = res + format!("        \\} {};\n", self.session.interner.name_to_str(&variant.ident.val.name));
                }
                res + format!("\n    \\} val;\n\\} {};", self.visit_ident(id))
            }
        }
    }

    fn visit_func_arg(&self, arg: &FuncArg) -> ~str {
        self.visit_name_and_type(arg.ident.val.name, &arg.argtype)
    }

    fn visit_type(&self, t: &Type) -> ~str {
        match t.val {
            PtrType(ref p) => {
                self.visit_type(*p) +
                "*"
            }
            NamedType(ref id) => {
                let is_param = {
                    // Is this type a type parameter?
                    let did = self.session.resolver.def_from_ident(id);
                    let d = self.session.defmap.find(&did).take_unwrap();
                    match *d {
                        GenericDef => true,
                        _ => false,
                    }
                };
                if is_param {
                    // Treat all type parameters as void.
                    "void".to_owned()
                } else {
                    if self.structnames.contains(&id.val.name) {
                        "struct "
                    } else {
                        ""
                    }.to_owned() +
                        self.visit_ident(id)
                }
            }
            FuncType(ref d, ref r) => {
                self.visit_type(*r) +
                    "(*)(" +
                    self.visit_list(d, |s, x| s.visit_type(x), ", ") +
                    ")"
            }
            ArrayType(ref a, _) => {
                self.visit_type(*a)
            }
            TupleType(ref ts) => {
                let mut res = "".to_owned();
                for t in ts.iter() {
                    res = res + self.visit_type(t);
                }
                res
            }
            BoolType => "int".to_owned(),
            UnitType => "void".to_owned(),
            IntType(..) => "int".to_owned(),
        }
    }

    fn visit_ident(&self, ident: &Ident) -> ~str {
        match self.enumitemnames.find(&ident.val.name) {
            Some(&(_, _, ref pos)) => {
                format!("\\{ .tag = {} \\}", pos)
            }
            None => format!("{}", self.session.interner.name_to_str(&ident.val.name)),
        }
    }

    fn visit_lit(&self, lit: &Lit) -> ~str {
        match lit.val {
            NumLit(ref n, _) => format!("{}", n),
            StringLit(_) => fail!("TODO"),
            BoolLit(ref b) => format!("{}", if *b { 1 } else { 0 }),
        }
    }

    fn visit_expr(&self, expr: &Expr) -> ~str {
        match expr.val {
            UnitExpr => "({})".to_owned(),
            LitExpr(ref l) => self.visit_lit(l),
            TupleExpr(ref t) => fail!("Tuples not yet supported."),
            GroupExpr(ref e) => format!("({})", self.visit_expr(*e)),
            IdentExpr(ref i) => self.visit_ident(i),
            BinOpExpr(ref op, ref lhs, ref rhs) => {
                "(".to_owned() +
                self.visit_expr(*lhs) +
                ")" +
                self.visit_binop(op) +
                "(" +
                self.visit_expr(*rhs) +
                ")"
            },
            UnOpExpr(ref op, ref expr) => {
                self.visit_unop(op) +
                "(" + 
                self.visit_expr(*expr) +
                ")"
            },
            IndexExpr(ref exp, ref idx) => {
                "(".to_owned() +
                self.visit_expr(*exp) +
                ")[" +
                self.visit_expr(*idx) +
                "]"
            },
            DotExpr(ref exp, ref field) => {
                "(".to_owned() +
                self.visit_expr(*exp) +
                format!(").{}", field)
            },
            ArrowExpr(ref exp, ref field) => {
                "(".to_owned() +
                self.visit_expr(*exp) +
                format!(")->{}", field)
            },
            AssignExpr(ref lhs, ref rhs) => {
                "(".to_owned() +
                self.visit_expr(*lhs) +
                ") = (" +
                self.visit_expr(*rhs) +
                ")"
            }
            CallExpr(ref f, ref args) => {
                match f.val {
                    IdentExpr(ref id) => {
                        let name = self.session.interner.name_to_str(&id.val.name);
                        match self.enumitemnames.find(&id.val.name) {
                            Some(&(_, ref variants, pos)) => {
                                let mut res = format!("\\{ .tag = {}, ", pos);
                                for (i, _) in args.iter().enumerate() {
                                    let expr = self.visit_expr(args.get(i));
                                    res = res + format!(".val.{}.field{} = {}, ", name, i, expr);
                                }
                                res + "}"
                            }
                            None => {
                                let res = format!("{}", name);
                                res +
                                    "(" +
                                    self.visit_list(args, |s, x| s.visit_expr(x), ", ") +
                                    ")"
                            }
                        }
                    },
                    _ => {
                        self.visit_expr(*f) +
                            "(" +
                            self.visit_list(args, |s, x| s.visit_expr(x), ", ") +
                            ")"
                    }

                }
            },
            CastExpr(ref e, ref t) => {
                "(".to_owned() +
                self.visit_type(t) +
                ")(" +
                self.visit_expr(*e) +
                ")"
            },
            IfExpr(ref e, ref b1, ref b2) => {
                "((".to_owned() +
                self.visit_expr(*e) +
                ")?" +
                self.visit_expr_block(*b1) +
                ":" +
                self.visit_expr_block(*b2) +
                ")"
            },
            BlockExpr(ref b) => self.visit_expr_block(*b),
            ReturnExpr(ref e) => {
                "return/*expr*/ ".to_owned() +
                self.visit_expr(*e) +
                ";"
            },
            WhileExpr(ref e, ref b) => {
                "while(".to_owned() +
                self.visit_expr(*e) +
                ") {\n" +
                self.visit_expr_block(*b) +
                ";}\n"
            },
            ForExpr(ref e1, ref e2, ref e3, ref b) => {
                "for(".to_owned() +
                self.visit_expr(*e1) +
                ";" +
                self.visit_expr(*e2) +
                ";" +
                self.visit_expr(*e3) +
                ") {\n" +
                self.visit_expr_block(*b) +
                ";}\n"
            },
            MatchExpr(ref e, ref arms) => {
                // TODO: allow types other than ints.
                let mut res = "({ int _; switch((".to_owned() +
                    self.visit_expr(*e) + ").tag) {\n";
                for arm in arms.iter() {
                    let (name, vars) = match arm.pat.val {
                        VariantPat(ref id, ref args) => (id.val.name, args),
                        _ => fail!("Only VariantPats are supported in match arms for now")
                    };
                    let &(_, ref variants, idx) = self.enumitemnames.find(&name).unwrap();
                    let this_variant = variants.get(idx as uint);
                    res = res + format!("    case {}: \\{", idx);

                    let name = self.session.interner.name_to_str(&name);

                    for (i, var) in this_variant.args.iter().enumerate() {
                        let varname = match vars.get(i as uint).val {
                            IdentPat(ref id, _) => self.session.interner.name_to_str(&id.val.name),
                            _ => fail!("Only IdentPats are supported in the arguments of a VariantPat in a match arm for now"),
                        };
                        let ty = self.visit_type(var);
                        let expr = self.visit_expr(*e);
                        res = res + format!("{} {} = {}.val.{}.field{};", ty, varname, expr, name, i);
                    }
                    res = res + " _ = " +self.visit_expr(&arm.body) + 
                        "; break;}\n";
                }
                res + "\n} _; })"
            }
        }
    }

    fn visit_module(&self, module: &Module) -> ~str {
        let mut res = "".to_owned();
        for item in module.items.iter() {
            res = res + self.visit_item(item);
        }
        res
    }
}

pub struct CTarget {
    opts: (),
}

impl Target for CTarget {
    fn new(_args: Vec<StrBuf>) -> CTarget {
        CTarget { opts: () }
    }

    fn compile(&self, p: Package) {
        let mut stderr = stdio::stderr();

        let Package {
            module:  module,
            session: mut session,
            typemap: typemap,
        } = p;

        let mut builtins = TreeSet::new();
        builtins.insert(session.interner.intern("print_int".to_owned()));

        let cc = CCrossCompiler {
            structnames: find_structs(&module),
            enumitemnames: find_enum_item_names(&module),
            builtins: builtins,
            session: session,
            typemap: typemap,
        };

        match writeln!(stderr, "{}", module) {
            Err(e) => fail!("{}", e),
            _ => {}
        }

        match writeln!(stderr, "{}", cc.enumitemnames) {
            Err(e) => fail!("{}", e),
            _ => {}
        }

        println!("{}",  r#"#include <stdio.h>"# +
                 "\n" + r#"#include <stdlib.h>"# +
                 "\n" + r#"int print_int(int x) { printf("%d\n", x); return x; }"#);
        println!("{}", cc.visit_module(&module));
    }
}
