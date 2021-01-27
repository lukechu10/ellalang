//! Visitor pattern for AST nodes.

use crate::ast::{Expr, ExprKind, Stmt, StmtKind};

/// Visitor trait for AST walking logic.
/// Implement this trait by overriding the hooks (`visit_*` methods).
pub trait Visitor<'ast>: Sized {
    /// Hook called when visiting an [`Expr`].
    /// If this method has a custom implementation, call [`walk_expr`] to use default walking logic.
    fn visit_expr(&mut self, expr: &'ast Expr) {
        walk_expr(self, expr);
    }

    /// Hook called when visiting a [`Stmt`].
    /// If this method has a custom implementation, call [`walk_stmt`] to use default walking logic.
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        walk_stmt(self, stmt);
    }
}

/// Walking logic for visiting all child nodes of an [`Expr`].
pub fn walk_expr<'ast>(visitor: &mut impl Visitor<'ast>, expr: &'ast Expr) {
    match &expr.kind {
        ExprKind::NumberLit(_) => {}
        ExprKind::BoolLit(_) => {}
        ExprKind::StringLit(_) => {}
        ExprKind::Identifier(_) => {}
        ExprKind::FnCall { callee, args } => {
            visitor.visit_expr(callee);
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        ExprKind::Binary { lhs, op: _, rhs } => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        ExprKind::Unary { op: _, arg } => visitor.visit_expr(arg),
        ExprKind::Lambda {
            inner_stmt: _,
            params: _,
            body,
        } => {
            for stmt in body {
                visitor.visit_stmt(stmt);
            }
        }
        ExprKind::Error => {}
    }
}

/// Walking logic for visiting all child nodes of a [`Stmt`].
pub fn walk_stmt<'ast>(visitor: &mut impl Visitor<'ast>, stmt: &'ast Stmt) {
    /// Iteratively visit all statements in a `Vec<Stmt>`.
    macro_rules! visit_stmt_list {
        ($visitor: expr, $body: expr) => {
            for stmt in $body {
                Visitor::visit_stmt($visitor, stmt);
            }
        };
    }

    match &stmt.kind {
        StmtKind::LetDeclaration {
            ident: _,
            initializer,
            ty: _,
        } => visitor.visit_expr(initializer),
        StmtKind::FnParam { ident: _ } => {}
        StmtKind::FnDeclaration {
            ident: _,
            params,
            body,
        } => {
            visit_stmt_list!(visitor, params);
            visit_stmt_list!(visitor, body);
        },
        StmtKind::Block(body) => visit_stmt_list!(visitor, body),
        StmtKind::IfElseStmt {
            condition,
            if_block,
            else_block,
        } => {
            visitor.visit_expr(condition);
            visit_stmt_list!(visitor, if_block);
            if let Some(else_block) = else_block {
                visit_stmt_list!(visitor, else_block);
            }
        }
        StmtKind::WhileStmt { condition, body } => {
            visitor.visit_expr(condition);
            visit_stmt_list!(visitor, body);
        }
        StmtKind::ExprStmt(expr) => visitor.visit_expr(expr),
        StmtKind::ReturnStmt(expr) => visitor.visit_expr(expr),
        StmtKind::Lambda => unreachable!(),
        StmtKind::Error => {}
    }
}
