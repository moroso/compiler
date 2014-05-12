#![feature(globs,phase,macro_rules)]
#![allow(dead_code,unused_imports)]

#[phase(syntax)]
extern crate regex_macros;

extern crate collections;
extern crate regex;

use ast::*;
use ast::defmap::*;
use std::io::stdio::stdin;
use lexer::Lexer;
use parser::Parser;
use ast::visit::{Visitor, walk_module};
use collections::hashmap::{HashSet, HashMap};
use resolve::Resolver;
use parser_context::ParserContext;

mod lexer;
mod parser;
mod span;
mod ast;
mod resolve;
mod parser_context;

struct CCrossCompiler {
    structnames: HashSet<~str>,
    enumitemnames: HashMap<~str, (Ident, Vec<Variant>, int)>,
    context: ~ParserContext,
}

fn find_structs(module: &Module) -> HashSet<~str> {
    let mut ht = HashSet::<~str>::new();
    for item in module.items.iter() {
        match item.val {
            StructItem(ref id, _, _) => { ht.insert(id.val.name.clone()); },
            _ => {}
        }
    }

    ht
}

fn find_enum_item_names(module: &Module) -> HashMap<~str,
                                                    (Ident,
                                                     Vec<Variant>,
                                                     int)> {
    let mut ht = HashMap::<~str, (Ident, Vec<Variant>, int)>::new();
    for enumitem in module.items.iter() {
        match enumitem.val {
            EnumItem(ref name, ref items, _) => {
                let mut pos = 0;
                for item in items.iter() {
                    ht.insert(item.ident.val.name.clone(),
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
    fn visit_list<T>(&mut self, list: &Vec<T>,
                     visit: |&mut CCrossCompiler, &T| -> ~str,
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

    fn visit_binop(&mut self, op: &BinOp) -> ~str {
        format!("{}", op)
    }

    fn visit_unop(&mut self, op: &UnOp) -> ~str {
        format!("{}", op)
    }

    // A block, as an expression.
    fn visit_expr_block(&mut self, block: &Block) -> ~str {
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

    fn visit_name_and_type(&mut self, name: &str, t: &Type) -> ~str {
        match t.val {
            // We have to special case this, because of the way things of
            // a function pointer type are declared in C.
            FuncType(ref d, ref r) => {
                self.visit_type(*r) +
                    format!("(*{})(", name) +
                    self.visit_list(d, |s, x| s.visit_type(x), ", ") +
                    ")"
            },
            _ => {
                self.visit_type(t) +
                    format!(" {}", name)
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> ~str {
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

    fn visit_block(&mut self, block: &Block) -> ~str {
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

    fn visit_item(&mut self, item: &Item) -> ~str {
        match item.val {
            FuncItem(ref name, ref args, ref t, ref block, _) => {
                // Hack for builtin functions.
                if name.val.name == "print_int".to_owned() { "".to_owned() } else {
                    self.visit_type(t) +
                        " " +
                        self.visit_ident(name) +
                        "(" +
                        self.visit_list(args, |s, x| s.visit_func_arg(x), ", ") +
                        ")" +
                        self.visit_block(block)
                }
            }
            StructItem(ref name, ref fields, _) => {
                let mut res = format!("typedef struct {} \\{", name).to_owned();
                for field in fields.iter() {
                    res = res + self.visit_name_and_type(field.name,
                                                         &field.fldtype) + ";\n";
                }
                res + 
                    format!("{} {};", "}", name)
            }
            EnumItem(ref name, ref variants, _) => {
                let mut res = format!("typedef struct {} \\{\n    int tag;\n    union \\{\n",
                                      name).to_owned();
                for variant in variants.iter() {
                    res = res + "        struct {\n";
                    let mut num = 0;
                    for t in variant.args.iter() {
                        res = res + "            " + self.visit_type(t) +
                            " " + format!("field{};\n", num);
                        num += 1;
                    }
                    res = res + format!("        \\} {};\n", variant.ident.val.name);
                }
                res + format!("\n    \\} val;\n\\} {};", name)
            }
        }
    }

    fn visit_func_arg(&mut self, arg: &FuncArg) -> ~str {
        self.visit_name_and_type(arg.ident.val.name, &arg.argtype)
    }

    fn visit_type(&mut self, t: &Type) -> ~str {
        match t.val {
            PtrType(ref p) => {
                self.visit_type(*p) +
                "*"
            }
            NamedType(ref id) => {
                let is_param = {
                    // Is this type a type parameter?
                    let did = self.context.resolver.def_from_ident(id);
                    let d = self.context.defmap.find(&did).take_unwrap();
                    match *d {
                        TypeDef(ref t) if *t == ast::UnitType => true,
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

    fn visit_ident(&mut self, ident: &Ident) -> ~str {
        match self.enumitemnames.find_equiv(&ident.val.name) {
            Some(&(_, _, ref pos)) => {
                format!("\\{ .tag = {} \\}", pos)
            }
            None => format!("{}", ident.val.name),
        }
    }

    fn visit_lit(&mut self, lit: &Lit) -> ~str {
        match lit.val {
            NumLit(ref n, _) => format!("{}", n),
            StringLit(_) => fail!("TODO"),
            BoolLit(ref b) => format!("{}", if *b { 1 } else { 0 }),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> ~str {
        match expr.val {
            UnitExpr => "({})".to_owned(),
            LitExpr(ref l) => self.visit_lit(l),
            TupleExpr(ref t) => fail!("Tuples not yet supported."),
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
                    IdentExpr(ref i) => {
                        // TODO: Why on earth is this needed? (The borrow
                        // checker complains otherwise; why?)
                        let cloned_tab = self.enumitemnames.clone();
                        match cloned_tab.find_equiv(&i.val.name) {
                            Some(&(_, ref variants, ref pos)) => {
                                let mut res = format!("\\{ .tag = {}, ", pos).to_owned();
                                let this_variant = variants.get(*pos as uint).clone();
                                let mut i = 0;
                                for item in this_variant.args.iter() {
                                    res = res + format!(".val.{}.field{} = {}, ",
                                                        this_variant.ident.val.name,
                                                        i,
                                                        self.visit_expr(
                                                            args.get(i)
                                                            )
                                                            );
                                    i += 1;
                                }
                                res + "}"
                            },
                            None => {
                                let res = format!("{}", i.val.name);
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
                    // TODO: Why on earth is this needed? (The borrow
                    // checker complains otherwise; why?)
                    let cloned_tab = self.enumitemnames.clone();

                    let (name, vars) = match arm.pat.val {
                        VariantPat(ref id, ref args) => (&id.val.name, args),
                        _ => fail!("Only VariantPats are supported in match arms for now")
                    };
                    let &(_, ref variants, idx) = cloned_tab.find_equiv( name).unwrap();
                    let this_variant = variants.get(idx as uint).clone();
                    res = res + format!("    case {}: \\{", idx);

                    for (i, var) in this_variant.args.iter().enumerate() {
                        let varname = match vars.get(i as uint).val {
                            IdentPat(ref id, _) => &id.val.name,
                            _ => fail!("Only IdentPats are supported in the arguments of a VariantPat in a match arm for now"),
                        };
                        res = res + format!("{} {} = {}.val.{}.field{};",
                                            self.visit_type(var),
                                            varname,
                                            self.visit_expr(*e),
                                            name,
                                            i
                                            );
                    }
                    res = res + " _ = " +self.visit_expr(&arm.body) + 
                        "; break;}\n";
                }
                res + "\n} _; })"
            }
        }
    }

    fn visit_module(&mut self, module: &Module) -> ~str {
        let mut res = "".to_owned();
        for item in module.items.iter() {
            res = res + self.visit_item(item);
        }
        res
    }
}

fn main() {
    print!("{}", r##"#include <stdio.h>
#include <stdlib.h>
int print_int(int x) { printf("%d\n", x); return x; }
"##);
    let mut stdin = stdin();
    let lexer = Lexer::new(stdin.lines().map(|x| x.unwrap()));
    let mut parser = Parser::new(lexer);

    let ast = parser.parse_module();
    let mut stderr = std::io::stdio::stderr();
    stderr.write_str(format!("{}", ast));
    stderr.write_str(format!("{}\n", find_enum_item_names(&ast)));

    parser.context.defmap.visit_module(&ast);
    parser.context.resolver.visit_module(&ast);

    let mut cc: CCrossCompiler = CCrossCompiler {
        structnames: find_structs(&ast),
        enumitemnames: find_enum_item_names(&ast),
        context: parser.context,
    };

    print!("{}\n", cc.visit_module(&ast));
}
