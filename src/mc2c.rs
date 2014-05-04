#![feature(globs,phase,macro_rules)]
#![allow(dead_code,unused_imports)]

#[phase(syntax)]
extern crate regex_macros;

extern crate collections;
extern crate regex;

use ast::*;
use std::io::stdio::stdin;
use lexer::Lexer;
use parser::Parser;
use ast::visit::{Visitor, walk_module};

mod lexer;
mod parser;
mod span;
mod ast;

struct CCrossCompiler;

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
            match(block.expr) {
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
            LetStmt(ref i, ref t, ref e) => {
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
            DeconstructTupleStmt(ref v, ref e) => "".to_owned(), // TODO
        }
    }

    fn visit_block(&mut self, block: &Block) -> ~str {
        "{".to_owned() +
            self.visit_list(&block.items, |s, t| s.visit_item(t), "; ") +
            self.visit_list(&block.stmts, |s, t| s.visit_stmt(t), "; ") +
            match(block.expr) {
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
                self.visit_type(t) +
                    " " +
                    self.visit_ident(name) +
                    "(" +
                    self.visit_list(args, |s, x| s.visit_func_arg(x), ", ") +
                    ")" +
                    self.visit_block(block)
            }
            StructItem(ref name, ref fields, _) => {
                let mut res = format!("typedef struct {} \\{", name).to_owned();
                for &(ref fieldname, ref fieldtype) in fields.iter() {
                    res = res + self.visit_name_and_type(*fieldname,
                                                         fieldtype) + ";\n";
                }
                res + 
                    format!("{} {};", "}", name)
            }
            EnumItem(_, _, _) => "".to_owned(), // TODO
        }
    }

    fn visit_func_arg(&mut self, arg: &FuncArg) -> ~str {
        self.visit_name_and_type(arg.ident.name, &arg.argtype)
    }

    fn visit_type(&mut self, t: &Type) -> ~str {
        match t.val {
            PtrType(ref p) => {
                self.visit_type(*p) +
                "*"
            }
            NamedType(ref id) => {
                // TODO: this is a hack, and once we have functions that
                // give us better insight into our types, this should
                // be fixed.
                if id.name != "int".to_owned() {
                    "struct "
                } else { "" }.to_owned() +
                    self.visit_ident(id)
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
        format!("{}", ident.name)
    }

    fn visit_lit(&mut self, lit: &Lit) -> ~str {
        match lit.val {
            NumLit(ref n, _) => format!("{}", n),
            StringLit(ref s) => fail!("TODO"),
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
                self.visit_expr(*f) +
                "(" +
                self.visit_list(args, |s, x| s.visit_expr(x), ", ") +
                ")"
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
            MatchExpr(_, _) => "".to_owned() // TODO
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
void print_int(int x) { printf("%d\n", x); }
"##);
    let mut stdin = stdin();
    let lexer = Lexer::new(stdin.lines().map(|x| x.unwrap()));
    let mut parser = Parser::new(lexer);

    let mut cc: CCrossCompiler = CCrossCompiler;

    let ast = parser.parse_module();
    let mut stderr = std::io::stdio::stderr();
    stderr.write_str(format!("{}", ast));

    print!("{}\n", cc.visit_module(&ast));
}
