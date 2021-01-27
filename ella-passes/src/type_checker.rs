//! Type checking and type inference pass.

use std::collections::HashMap;

use ella_parser::ast::{Expr, ExprKind, Stmt, StmtKind};
use ella_parser::visitor::{walk_expr, walk_stmt, Visitor};
use ella_source::{Source, SyntaxError};
use ella_value::BuiltinVars;

use crate::resolve::ResolveResult;

/// Represents a builtin type.
#[derive(Debug, Clone)]
pub enum BuiltinType {
    Bool,
    Number,
    String,
    Function {
        params: Vec<BuiltinType>,
        ret: Box<BuiltinType>,
    },
}

/// Represents an unique type.
#[derive(Debug, Clone)]
pub enum UniqueType {
    Builtin(BuiltinType),
    Unknown,
}

pub type SymbolTypeTable = HashMap<*const Stmt, UniqueType>;
pub type ExprTypeTable = HashMap<*const Expr, UniqueType>;

#[derive(Debug, Clone)]
pub struct TypeCheckResult {
    symbol_type_table: SymbolTypeTable,
    expr_type_table: ExprTypeTable,
}

/// Type checking and type inference pass.
pub struct TypeChecker<'a> {
    resolve_result: &'a ResolveResult,
    symbol_type_table: SymbolTypeTable,
    expr_type_table: ExprTypeTable,
    source: Source<'a>,
}

impl<'a> TypeChecker<'a> {
    /// Create a new empty `TypeChecker`.
    pub fn new(resolve_result: &'a ResolveResult, source: Source<'a>) -> Self {
        Self {
            resolve_result,
            symbol_type_table: SymbolTypeTable::new(),
            expr_type_table: ExprTypeTable::new(),
            source,
        }
    }

    /// Create a new `TypeChecker` with existing type checking results.
    /// This method is used to implement REPL functionality (for restoring global variables).
    /// See [`Self::into_type_check_result`].
    pub fn new_with_type_check_result(
        resolve_result: &'a ResolveResult,
        source: Source<'a>,
        type_check_result: TypeCheckResult,
    ) -> Self {
        Self {
            resolve_result,
            symbol_type_table: type_check_result.symbol_type_table.clone(),
            expr_type_table: type_check_result.expr_type_table.clone(),
            source,
        }
    }

    // pub fn type_check_builtin_vars(&mut self, builtin_vars: &BuiltinVars) {
    //     for (ident, value) in &builtin_vars.values {
    //         self.symbol_type_table
    //     }
    // }

    pub fn into_type_check_result(self) -> TypeCheckResult {
        TypeCheckResult {
            symbol_type_table: self.symbol_type_table,
            expr_type_table: self.expr_type_table,
        }
    }
}

impl<'a> Visitor<'a> for TypeChecker<'a> {
    fn visit_expr(&mut self, expr: &'a Expr) {
        walk_expr(self, expr);

        let ty = match &expr.kind {
            ExprKind::BoolLit(_) => UniqueType::Builtin(BuiltinType::Bool),
            ExprKind::NumberLit(_) => UniqueType::Builtin(BuiltinType::Number),
            ExprKind::StringLit(_) => UniqueType::Builtin(BuiltinType::String),
            ExprKind::Identifier(_) => match self.resolve_result.lookup_identifier(expr) {
                Some(resolved_symbol) => {
                    dbg!(&resolved_symbol.symbol.borrow().stmt);
                    self.symbol_type_table
                        .get(&resolved_symbol.symbol.borrow().stmt)
                        .unwrap()
                        .clone()
                }
                None => UniqueType::Unknown,
            },
            _ => UniqueType::Unknown, // TODO
        };
        assert!(self
            .expr_type_table
            .insert(expr as *const Expr, ty)
            .is_none());
    }

    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        walk_stmt(self, stmt);

        match &stmt.kind {
            StmtKind::LetDeclaration {
                ident,
                initializer,
                ty,
            } => {
                let ty = match ty {
                    Some(ty_path) => match ty_path.ident.as_str() {
                        // TODO: make sure initializer has correct type
                        "bool" => UniqueType::Builtin(BuiltinType::Bool),
                        "number" => UniqueType::Builtin(BuiltinType::Number),
                        "string" => UniqueType::Builtin(BuiltinType::String),
                        _ => {
                            self.source.errors.add_error(SyntaxError::new(
                                format!("unknown type \"{}\"", ty_path.ident),
                                ty_path.span.clone(),
                            ));
                            UniqueType::Unknown
                        }
                    },
                    None => {
                        // TODO: infer type from initializer
                        if let Some(ty) = self.expr_type_table.get(&(initializer as *const Expr)) {
                            ty.clone()
                        } else {
                            // FIXME: should be unreachable
                            self.source.errors.add_error(
                                SyntaxError::new(
                                    "could not infer type for variable",
                                    stmt.span.clone(),
                                )
                                .with_help(format!(
                                    "consider explicitly specifying the type for \"{}\"",
                                    ident
                                )),
                            );
                            UniqueType::Unknown
                        }
                    }
                };
                assert!(self
                    .symbol_type_table
                    .insert(stmt as *const Stmt, ty)
                    .is_none());
            }
            _ => {}
        }
    }
}
