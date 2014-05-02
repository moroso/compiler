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

    fn print_list<T>(&mut self, list: &Vec<T>,
                     visit: |&mut CCrossCompiler, &T|,
                     delimiter: &str) {
        let mut count = 0;
        for t in list.iter() {
            visit(self, t);
            count += 1;
            if count < list.len() {
                print!("{}", delimiter);
            }
        }
    }

    fn visit_binop(&mut self, op: &BinOp) {
        print!("{}", op);
    }

    fn visit_unop(&mut self, op: &UnOp) {
        print!("{}", op);
    }

    // A block, as an expression.
    fn visit_expr_block(&mut self, block: &Block) {
        print!("{}", "({ ");
        self.print_list(&block.items, |s, t| s.visit_item(t), "; ");
        self.print_list(&block.stmts, |s, t| s.visit_stmt(t), "; ");
        match(block.expr) {
            Some(ref x) => {
                self.visit_expr(x);
                print!(";");
            },
            None => {},
        }
        print!("{}", "})");
    }

}

impl Visitor for CCrossCompiler {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.val {
            LetStmt(ref i, ref t, ref e) => {
                if t.is_none() { fail!("Must specify types now.") };
                self.visit_type(&t.clone().unwrap());
                print!(" ");
                self.visit_ident(i);
                match *e {
                    Some(ref exp) => {
                        print!(" = ");
                        self.visit_expr(exp);
                    },
                    None => {},
                }
                print!(";");
            },
            ExprStmt(ref e) => { self.visit_expr(e); print!(";"); },
            SemiStmt(ref e) => { self.visit_expr(e); print!(";"); },
        }
    }

    fn visit_block(&mut self, block: &Block) {
        print!("{}", "{");
        self.print_list(&block.items, |s, t| s.visit_item(t), "; ");
        self.print_list(&block.stmts, |s, t| s.visit_stmt(t), "; ");
        match(block.expr) {
            Some(ref x) => { print!("return ");
                             match x.val {
                                 ReturnExpr(ref e) => self.visit_expr(*e),
                                 _ => self.visit_expr(x),
                             }
                             print!(";"); }
            None => print!("{}", "return;"),
        }
        print!("{}", "}");
    }

    fn visit_item(&mut self, item: &Item) {
        match item.val {
            FuncItem(ref name, ref args, ref t, ref block, _) => {
                self.visit_type(t);
                print!(" ");
                self.visit_ident(name);
                print!("(");
                let mut count = 0;
                for arg in args.iter().peekable() {
                    self.visit_func_arg(arg);
                    count += 1;
                    if count < args.len() {
                        print!(", ");
                    }
                }
                print!("{}", ")");
                self.visit_block(block);
            }
        }
    }

    fn visit_func_arg(&mut self, arg: &FuncArg) {
        match arg.argtype.val {
            // We have to special case this, because of the way things of
            // a function pointer type are declared in C.
            FuncType(ref d, ref r) => {
                self.visit_type(*r);
                print!("(*");
                self.visit_ident(&arg.ident);
                print!(")(");
                self.print_list(d, |s, x| s.visit_type(x), ", ");
                print!(")");
            },
            _ => {
                self.visit_type(&arg.argtype);
                print!(" ");
                self.visit_ident(&arg.ident);
            }
        }
    }

    fn visit_type(&mut self, t: &Type) {
        match t.val {
            PtrType(ref p) => {
                self.visit_type(*p);
                print!("*");
            }
            NamedType(ref id) => {
                self.visit_ident(id);
            }
            FuncType(ref d, ref r) => {
                self.visit_type(*r);
                print!("(*)(")

                self.print_list(d, |s, x| s.visit_type(x), ", ");
                print!(")");
            }
            ArrayType(ref a, _) => {
                self.visit_type(*a);
            }
            TupleType(ref ts) => {
                ts.iter().map(|t| self.visit_type(t)).all(|_|true);
            }
            BoolType => print!("int"),
            UnitType => print!("void"),
            IntType(..) => print!("int"),
        }
    }

    fn visit_ident(&mut self, ident: &Ident) {
        print!("{}", ident.name);
    }

    fn visit_lit(&mut self, lit: &Lit) {
        match lit.val {
            NumLit(ref n, _) => print!("{}", n),
            StringLit(ref s) => fail!("TODO"),
            BoolLit(ref b) => print!("{}", if *b { 1 } else { 0 }),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr.val {
            UnitExpr => print!("{}", "({})"),
            LitExpr(ref l) => self.visit_lit(l),
            TupleExpr(ref t) => fail!("Tuples not yet supported."),
            IdentExpr(ref i) => self.visit_ident(i),
            BinOpExpr(ref op, ref lhs, ref rhs) => {
                print!("(");
                self.visit_expr(*lhs);
                print!(")");
                self.visit_binop(op);
                print!("(");
                self.visit_expr(*rhs);
                print!(")");
            },
            UnOpExpr(ref op, ref expr) => {
                self.visit_unop(op);
                print!("(");
                self.visit_expr(*expr);
                print!(")");
            },
            IndexExpr(ref exp, ref idx) => {
                print!("(");
                self.visit_expr(*exp);
                print!(")[");
                self.visit_expr(*idx);
                print!("]");
            },
            DotExpr(ref exp, ref field) => {
                print!("(");
                self.visit_expr(*exp);
                print!(").{}", field);
            },
            ArrowExpr(ref exp, ref field) => {
                print!("(");
                self.visit_expr(*exp);
                print!(")->{}", field);
            },
            AssignExpr(ref lhs, ref rhs) => {
                print!("(");
                self.visit_expr(*lhs);
                print!(") = (");
                self.visit_expr(*rhs);
                print!(")");
            }
            CallExpr(ref f, ref args) => {
                print!("(");
                self.visit_expr(*f);
                print!(")(");

                self.print_list(args, |s, x| s.visit_expr(x), ", ");
                print!(")");
            },
            CastExpr(ref e, ref t) => {
                print!("(");
                self.visit_type(t);
                print!(")(");
                self.visit_expr(*e);
                print!(")");
            },
            IfExpr(ref e, ref b1, ref b2) => {
                print!("((");
                self.visit_expr(*e);
                print!("{}", ")?");
                self.visit_expr_block(*b1);
                print!("{}", ":");
                self.visit_expr_block(*b2);
                print!("{}", ")");
            },
            BlockExpr(ref b) => self.visit_expr_block(*b),
            ReturnExpr(ref e) => {
                print!("return/*expr*/ ");
                self.visit_expr(*e);
                print!(";");
            },
            WhileExpr(ref e, ref b) => {},
            ForExpr(ref e1, ref e2, ref e3, ref b) => {},
        }
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

    walk_module(&mut cc, &ast);
    print!("\n");
}
