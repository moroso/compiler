use super::*;

pub trait Visitor: Sized {
    fn visit_item(&mut self, item: &Item) { walk_item(self, item) }
    fn visit_type(&mut self, t: &Type) { walk_type(self, t) }
    fn visit_pat(&mut self, pat: &Pat) { walk_pat(self, pat) }
    fn visit_variant(&mut self, variant: &Variant) { walk_variant(self, variant) }
    fn visit_struct_field(&mut self, field: &Field) { walk_struct_field(self, field) }
    fn visit_func_arg(&mut self, arg: &FuncArg) { walk_func_arg(self, arg) }
    fn visit_block(&mut self, block: &Block) { walk_block(self, block) }
    fn visit_stmt(&mut self, stmt: &Stmt) { walk_stmt(self, stmt) }
    fn visit_expr(&mut self, expr: &Expr) { walk_expr(self, expr) }
    fn visit_match_arm(&mut self, arm: &MatchArm) { walk_match_arm(self, arm) }
    fn visit_lit(&mut self, lit: &Lit) { walk_lit(self, lit) }
    fn visit_ident(&mut self, ident: &Ident) { walk_ident(self, ident) }
    fn visit_path(&mut self, path: &Path) { walk_path(self, path) }
    fn visit_module(&mut self, module: &Module) { walk_module(self, module) }
    fn visit_import(&mut self, import: &Import) { walk_import(self, import) }
}

pub fn walk_item<T: Visitor>(visitor: &mut T, item: &Item) {
    match item.val {
        FuncItem(ref id, ref args, ref t, ref def, ref tps) => {
            visitor.visit_ident(id);
            for arg in args.iter() { visitor.visit_func_arg(arg); }
            visitor.visit_type(t);
            match *def {
                LocalFn(ref block) => visitor.visit_block(block),
                ExternFn(..) => {}
            }
            for id in tps.iter() { visitor.visit_ident(id); }
        },
        StructItem(ref id, ref fields, ref tps) => {
            visitor.visit_ident(id);
            for field in fields.iter() { visitor.visit_struct_field(field); }
            for id in tps.iter() { visitor.visit_ident(id); }
        },
        EnumItem(ref id, ref variants, ref tps) => {
            visitor.visit_ident(id);
            for variant in variants.iter() { visitor.visit_variant(variant); }
            for id in tps.iter() { visitor.visit_ident(id); }
        },
        TypeItem(ref id, ref ty, ref tps) => {
            visitor.visit_ident(id);
            visitor.visit_type(ty);
            for id in tps.iter() { visitor.visit_ident(id); }
        },
        ModItem(ref ident, ref module) => {
            visitor.visit_ident(ident);
            visitor.visit_module(module);
        },
        StaticItem(ref ident, ref ty, ref expr, _) => {
            visitor.visit_ident(ident);
            visitor.visit_type(ty);
            for e in expr.iter() { visitor.visit_expr(e); }
        }
        ConstItem(ref ident, ref ty, ref expr) => {
            visitor.visit_ident(ident);
            visitor.visit_type(ty);
            visitor.visit_expr(expr);
        }
        UseItem(ref import) => {
            visitor.visit_import(import);
        }
        MacroDefItem(..) => {}
    }
}

pub fn walk_type<T: Visitor>(visitor: &mut T, t: &Type) {
    match t.val {
        PtrType(ref p) => {
            visitor.visit_type(&**p);
        }
        NamedType(ref p) => {
            visitor.visit_path(p);
        }
        FuncType(ref d, ref r) => {
            for a in d.iter() { visitor.visit_type(a); }
            visitor.visit_type(&**r);
        }
        ArrayType(ref a, ref d) => {
            visitor.visit_type(&**a);
            visitor.visit_expr(&**d);
        }
        TupleType(ref ts) => {
            for t in ts.iter() { visitor.visit_type(t); }
        }
        BoolType | UnitType | DivergingType | IntType(..) => {}
    }
}

pub fn walk_pat<T: Visitor>(visitor: &mut T, pat: &Pat) {
    match pat.val {
        DiscardPat(ref t) => {
            for t in t.iter() { visitor.visit_type(t); }
        }
        IdentPat(ref id, ref t) => {
            visitor.visit_ident(id);
            for t in t.iter() { visitor.visit_type(t); }
        }
        TuplePat(ref pats) => {
            for pat in pats.iter() {
                visitor.visit_pat(pat);
            }
        }
        VariantPat(ref path, ref pats) => {
            visitor.visit_path(path);
            for pat in pats.iter() {
                visitor.visit_pat(pat);
            }
        }
        StructPat(ref path, ref field_pats) => {
            visitor.visit_path(path);
            for field_pat in field_pats.iter() {
                visitor.visit_pat(&field_pat.pat);
            }
        }
    }
}

pub fn walk_variant<T: Visitor>(visitor: &mut T, variant: &Variant) {
    visitor.visit_ident(&variant.ident);
    for arg in variant.args.iter() {
        visitor.visit_type(arg);
    }
}

pub fn walk_struct_field<T: Visitor>(visitor: &mut T, field: &Field) {
    visitor.visit_type(&field.fldtype);
}

pub fn walk_func_arg<T: Visitor>(visitor: &mut T, arg: &FuncArg) {
    visitor.visit_ident(&arg.ident);
    visitor.visit_type(&arg.argtype);
}

pub fn walk_block<T: Visitor>(visitor: &mut T, block: &Block) {
    for item in block.val.items.iter() { visitor.visit_item(item); }
    for stmt in block.val.stmts.iter() { visitor.visit_stmt(stmt); }
    for expr in block.val.expr.iter()  { visitor.visit_expr(expr); }
}

pub fn walk_stmt<T: Visitor>(visitor: &mut T, stmt: &Stmt) {
    match stmt.val {
        LetStmt(ref pat, ref e) => {
            visitor.visit_pat(pat);
            for e in e.iter() { visitor.visit_expr(e); }
        }
        ExprStmt(ref e) => {
            visitor.visit_expr(e);
        }
        SemiStmt(ref e) => {
            visitor.visit_expr(e);
        }
    }
}

pub fn walk_expr<T: Visitor>(visitor: &mut T, expr: &Expr) {
    match expr.val {
        UnitExpr => {}
        LitExpr(ref l) => {
            visitor.visit_lit(l);
        }
        SizeofExpr(ref t) => {
            visitor.visit_type(t);
        }
        TupleExpr(ref es) => {
            for e in es.iter() { visitor.visit_expr(e); }
        }
        GroupExpr(ref e) => {
            visitor.visit_expr(&**e);
        }
        PathExpr(ref p) => {
            visitor.visit_path(p);
        }
        StructExpr(ref p, ref flds) => {
            visitor.visit_path(p);
            for fld in flds.iter() {
                visitor.visit_expr(&fld.1);
            }
        }
        BinOpExpr(_, ref l, ref r) => {
            visitor.visit_expr(&**l);
            visitor.visit_expr(&**r);
        }
        UnOpExpr(_, ref e) => {
            visitor.visit_expr(&**e);
        }
        IndexExpr(ref a, ref i) => {
            visitor.visit_expr(&**a);
            visitor.visit_expr(&**i);
        }
        DotExpr(ref e, _) => {
            visitor.visit_expr(&**e);
        }
        ArrowExpr(ref e, _) => {
            visitor.visit_expr(&**e);
        }
        AssignExpr(_, ref lv, ref rv) => {
            visitor.visit_expr(&**lv);
            visitor.visit_expr(&**rv);
        }
        ArrayExpr(ref elems) => {
            for elem in elems.iter() { visitor.visit_expr(elem); }
        }
        CallExpr(ref f, ref args) => {
            visitor.visit_expr(&**f);
            for arg in args.iter() { visitor.visit_expr(arg); }
        }
        CastExpr(ref e, ref t) => {
            visitor.visit_expr(&**e);
            visitor.visit_type(t);
        }
        IfExpr(ref c, ref tb, ref fb) => {
            visitor.visit_expr(&**c);
            visitor.visit_block(&**tb);
            visitor.visit_block(&**fb);
        }
        BlockExpr(ref b) => {
            visitor.visit_block(&**b);
        }
        ReturnExpr(ref e) => {
            visitor.visit_expr(&**e);
        }
        BreakExpr => {}
        ContinueExpr => {}
        DoWhileExpr(ref e, ref b) |
        WhileExpr(ref e, ref b) => {
            visitor.visit_expr(&**e);
            visitor.visit_block(&**b);
        }
        ForExpr(ref e1, ref e2, ref e3, ref b) => {
            visitor.visit_expr(&**e1);
            visitor.visit_expr(&**e2);
            visitor.visit_expr(&**e3);
            visitor.visit_block(&**b);
        }
        MatchExpr(ref e, ref arms) => {
            visitor.visit_expr(&**e);
            for arm in arms.iter() {
                visitor.visit_match_arm(arm);
            }
        }
        MacroExpr(..) => {}
    }
}

pub fn walk_match_arm<T: Visitor>(visitor: &mut T, arm: &MatchArm) {
    visitor.visit_pat(&arm.pat);
    visitor.visit_expr(&arm.body);
}

pub fn walk_lit<T: Visitor>(_: &T, lit: &Lit) {
    match lit.val {
        NumLit(..) | StringLit(..) | BoolLit(..) | NullLit => {}
    }
}

pub fn walk_ident<T: Visitor>(visitor: &mut T, ident: &Ident) {
    for tps in ident.val.tps.iter() {
        for tp in tps.iter() { visitor.visit_type(tp); }
    }
}

pub fn walk_path<T: Visitor>(visitor: &mut T, path: &Path) {
    for elem in path.val.elems.iter() {
        visitor.visit_ident(elem);
    }
}

pub fn walk_import<T: Visitor>(visitor: &mut T, path: &Import) {
    for elem in path.val.elems.iter() {
        visitor.visit_ident(elem);
    }
    match path.val.import {
        ImportAll => {}
        ImportNames(ref v) => {
            for elem in v.iter() {
                visitor.visit_ident(elem);
            }
        }
    }
}

pub fn walk_module<T: Visitor>(visitor: &mut T, module: &Module) {
    for item in module.val.items.iter() { visitor.visit_item(item); }
}
