use super::*;

pub trait MutVisitor {
    fn visit_item(&mut self, item: &mut Item) { walk_item(self, item) }
    fn visit_type(&mut self, t: &mut Type) { walk_type(self, t) }
    fn visit_pat(&mut self, pat: &mut Pat) { walk_pat(self, pat) }
    fn visit_variant(&mut self, variant: &mut Variant) { walk_variant(self, variant) }
    fn visit_struct_field(&mut self, field: &mut Field) { walk_struct_field(self, field) }
    fn visit_func_arg(&mut self, arg: &mut FuncArg) { walk_func_arg(self, arg) }
    fn visit_block(&mut self, block: &mut Block) { walk_block(self, block) }
    fn visit_stmt(&mut self, stmt: &mut Stmt) { walk_stmt(self, stmt) }
    fn visit_expr(&mut self, expr: &mut Expr) { walk_expr(self, expr) }
    fn visit_match_arm(&mut self, arm: &mut MatchArm) { walk_match_arm(self, arm) }
    fn visit_lit(&mut self, lit: &mut Lit) { walk_lit(self, lit) }
    fn visit_ident(&mut self, ident: &mut Ident) { walk_ident(self, ident) }
    fn visit_path(&mut self, path: &mut Path) { walk_path(self, path) }
    fn visit_module(&mut self, module: &mut Module) { walk_module(self, module) }
    fn visit_import(&mut self, import: &mut Import) { walk_import(self, import) }
}

pub fn walk_item<T: MutVisitor>(visitor: &mut T, item: &mut Item) {
    match item.val {
        FuncItem(ref mut id, ref mut args, ref mut t, ref mut def, ref mut tps) => {
            visitor.visit_ident(id);
            for arg in args.mut_iter() { visitor.visit_func_arg(arg); }
            visitor.visit_type(t);
            for def in def.mut_iter() { visitor.visit_block(def); }
            for id in tps.mut_iter() { visitor.visit_ident(id); }
        },
        StructItem(ref mut id, ref mut fields, ref mut tps) => {
            visitor.visit_ident(id);
            for field in fields.mut_iter() { visitor.visit_struct_field(field); }
            for id in tps.mut_iter() { visitor.visit_ident(id); }
        },
        EnumItem(ref mut id, ref mut variants, ref mut tps) => {
            visitor.visit_ident(id);
            for variant in variants.mut_iter() { visitor.visit_variant(variant); }
            for id in tps.mut_iter() { visitor.visit_ident(id); }
        },
        TypeItem(ref mut id, ref mut ty, ref mut tps) => {
            visitor.visit_ident(id);
            visitor.visit_type(ty);
            for id in tps.mut_iter() { visitor.visit_ident(id); }
        },
        ModItem(ref mut ident, ref mut module) => {
            visitor.visit_ident(ident);
            visitor.visit_module(module);
        },
        StaticItem(ref mut ident, ref mut ty, ref mut expr, _) => {
            visitor.visit_ident(ident);
            visitor.visit_type(ty);
            for e in expr.mut_iter() { visitor.visit_expr(e); }
        }
        ConstItem(ref mut ident, ref mut ty, ref mut expr) => {
            visitor.visit_ident(ident);
            visitor.visit_type(ty);
            visitor.visit_expr(expr);
        }
        UseItem(ref mut import) => {
            visitor.visit_import(import);
        }
        MacroDefItem(..) => {}
    }
}

pub fn walk_type<T: MutVisitor>(visitor: &mut T, t: &mut Type) {
    match t.val {
        PtrType(ref mut p) => {
            visitor.visit_type(&mut **p);
        }
        NamedType(ref mut p) => {
            visitor.visit_path(p);
        }
        FuncType(ref mut d, ref mut r) => {
            for a in d.mut_iter() { visitor.visit_type(a); }
            visitor.visit_type(&mut **r);
        }
        ArrayType(ref mut a, ref mut d) => {
            visitor.visit_type(&mut **a);
            visitor.visit_expr(&mut **d);
        }
        TupleType(ref mut ts) => {
            for t in ts.mut_iter() { visitor.visit_type(t); }
        }
        BoolType | UnitType | DivergingType | IntType(..) => {}
    }
}

pub fn walk_pat<T: MutVisitor>(visitor: &mut T, pat: &mut Pat) {
    match pat.val {
        DiscardPat(ref mut t) => {
            for t in t.mut_iter() { visitor.visit_type(t); }
        }
        IdentPat(ref mut id, ref mut t) => {
            visitor.visit_ident(id);
            for t in t.mut_iter() { visitor.visit_type(t); }
        }
        TuplePat(ref mut pats) => {
            for pat in pats.mut_iter() {
                visitor.visit_pat(pat);
            }
        }
        VariantPat(ref mut path, ref mut pats) => {
            visitor.visit_path(path);
            for pat in pats.mut_iter() {
                visitor.visit_pat(pat);
            }
        }
        StructPat(ref mut path, ref mut field_pats) => {
            visitor.visit_path(path);
            for field_pat in field_pats.mut_iter() {
                visitor.visit_pat(&mut field_pat.pat);
            }
        }
    }
}

pub fn walk_variant<T: MutVisitor>(visitor: &mut T, variant: &mut Variant) {
    visitor.visit_ident(&mut variant.ident);
    for arg in variant.args.mut_iter() {
        visitor.visit_type(arg);
    }
}

pub fn walk_struct_field<T: MutVisitor>(visitor: &mut T, field: &mut Field) {
    visitor.visit_type(&mut field.fldtype);
}

pub fn walk_func_arg<T: MutVisitor>(visitor: &mut T, arg: &mut FuncArg) {
    visitor.visit_ident(&mut arg.ident);
    visitor.visit_type(&mut arg.argtype);
}

pub fn walk_block<T: MutVisitor>(visitor: &mut T, block: &mut Block) {
    for item in block.val.items.mut_iter() { visitor.visit_item(item); }
    for stmt in block.val.stmts.mut_iter() { visitor.visit_stmt(stmt); }
    for expr in block.val.expr.mut_iter()  { visitor.visit_expr(expr); }
}

pub fn walk_stmt<T: MutVisitor>(visitor: &mut T, stmt: &mut Stmt) {
    match stmt.val {
        LetStmt(ref mut pat, ref mut e) => {
            visitor.visit_pat(pat);
            for e in e.mut_iter() { visitor.visit_expr(e); }
        }
        ExprStmt(ref mut e) => {
            visitor.visit_expr(e);
        }
        SemiStmt(ref mut e) => {
            visitor.visit_expr(e);
        }
    }
}

pub fn walk_expr<T: MutVisitor>(visitor: &mut T, expr: &mut Expr) {
    match expr.val {
        UnitExpr => {}
        LitExpr(ref mut l) => {
            visitor.visit_lit(l);
        }
        SizeofExpr(ref mut t) => {
            visitor.visit_type(t);
        }
        TupleExpr(ref mut es) => {
            for e in es.mut_iter() { visitor.visit_expr(e); }
        }
        GroupExpr(ref mut e) => {
            visitor.visit_expr(&mut **e);
        }
        PathExpr(ref mut p) => {
            visitor.visit_path(p);
        }
        StructExpr(ref mut p, ref mut flds) => {
            visitor.visit_path(p);
            for fld in flds.mut_iter() {
                visitor.visit_expr(fld.mut1());
            }
        }
        BinOpExpr(_, ref mut l, ref mut r) => {
            visitor.visit_expr(&mut **l);
            visitor.visit_expr(&mut **r);
        }
        UnOpExpr(_, ref mut e) => {
            visitor.visit_expr(&mut **e);
        }
        IndexExpr(ref mut a, ref mut i) => {
            visitor.visit_expr(&mut **a);
            visitor.visit_expr(&mut **i);
        }
        DotExpr(ref mut e, _) => {
            visitor.visit_expr(&mut **e);
        }
        ArrowExpr(ref mut e, _) => {
            visitor.visit_expr(&mut **e);
        }
        AssignExpr(_, ref mut lv, ref mut rv) => {
            visitor.visit_expr(&mut **lv);
            visitor.visit_expr(&mut **rv);
        }
        CallExpr(ref mut f, ref mut args) => {
            visitor.visit_expr(&mut **f);
            for arg in args.mut_iter() { visitor.visit_expr(arg); }
        }
        CastExpr(ref mut e, ref mut t) => {
            visitor.visit_expr(&mut **e);
            visitor.visit_type(t);
        }
        IfExpr(ref mut c, ref mut tb, ref mut fb) => {
            visitor.visit_expr(&mut **c);
            visitor.visit_block(&mut **tb);
            visitor.visit_block(&mut **fb);
        }
        BlockExpr(ref mut b) => {
            visitor.visit_block(&mut **b);
        }
        ReturnExpr(ref mut e) => {
            visitor.visit_expr(&mut **e);
        }
        BreakExpr => {}
        ContinueExpr => {}
        WhileExpr(ref mut e, ref mut b) => {
            visitor.visit_expr(&mut **e);
            visitor.visit_block(&mut **b);
        }
        ForExpr(ref mut e1, ref mut e2, ref mut e3, ref mut b) => {
            visitor.visit_expr(&mut **e1);
            visitor.visit_expr(&mut **e2);
            visitor.visit_expr(&mut **e3);
            visitor.visit_block(&mut **b);
        }
        MatchExpr(ref mut e, ref mut arms) => {
            visitor.visit_expr(&mut **e);
            for arm in arms.mut_iter() {
                visitor.visit_match_arm(arm);
            }
        }
        MacroExpr(..) => {}
    }
}

pub fn walk_match_arm<T: MutVisitor>(visitor: &mut T, arm: &mut MatchArm) {
    visitor.visit_pat(&mut arm.pat);
    visitor.visit_expr(&mut arm.body);
}

pub fn walk_lit<T: MutVisitor>(_: &mut T, lit: &mut Lit) {
    match lit.val {
        NumLit(..) | StringLit(..) | BoolLit(..) | NullLit => {}
    }
}

pub fn walk_ident<T: MutVisitor>(visitor: &mut T, ident: &mut Ident) {
    for tps in ident.val.tps.mut_iter() {
        for tp in tps.mut_iter() { visitor.visit_type(tp); }
    }
}

pub fn walk_path<T: MutVisitor>(visitor: &mut T, path: &mut Path) {
    for elem in path.val.elems.mut_iter() {
        visitor.visit_ident(elem);
    }
}

pub fn walk_import<T: MutVisitor>(visitor: &mut T, path: &mut Import) {
    for elem in path.val.elems.mut_iter() {
        visitor.visit_ident(elem);
    }
    match path.val.import {
        ImportAll => {}
        ImportNames(ref mut v) => {
            for elem in v.mut_iter() {
                visitor.visit_ident(elem);
            }
        }
    }
}

pub fn walk_module<T: MutVisitor>(visitor: &mut T, module: &mut Module) {
    for item in module.val.items.mut_iter() { visitor.visit_item(item); }
}
