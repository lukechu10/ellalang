//! Type checking and type inference pass.

use std::collections::HashMap;

use ella_parser::ast::{Expr, ExprKind, Stmt, StmtKind};
use ella_parser::visitor::{walk_expr, walk_stmt, Visitor};
use ella_source::{Source, SyntaxError};
use ella_value::BuiltinVars;

use crate::resolve::{ResolveResult, Symbol};

/// Represents a builtin type.
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub enum UniqueType {
    Builtin(BuiltinType),
    /// Runtime type.
    Any,
    /// Error case.
    Unknown,
}

pub type SymbolTypeTable = HashMap<*const Symbol, UniqueType>;
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

    pub fn type_check_builtin_vars(&mut self, builtin_vars: &BuiltinVars) {
        for (ident, _value) in &builtin_vars.values {
            let symbol = self
                .resolve_result
                .lookup_in_accessible_symbols(ident)
                .unwrap(); // get Symbol for builtin value

            assert!(self
                .symbol_type_table
                .insert(symbol.as_ptr() as *const Symbol, UniqueType::Unknown)
                .is_none());
        }
    }

    pub fn type_check_global(&mut self, func: &'a Stmt) {
        match &func.kind {
            StmtKind::FnDeclaration { body, .. } => {
                for stmt in body {
                    self.visit_stmt(stmt);
                }
            }
            _ => panic!("func is not a StmtKind::FnDeclaration"),
        }
    }

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
            ExprKind::Identifier(ident) => match self.resolve_result.lookup_identifier(expr) {
                Some(resolved_symbol) => self
                    .symbol_type_table
                    .get(&(resolved_symbol.symbol.as_ptr() as *const Symbol))
                    .expect(&format!("type of identifier \"{}\"", ident))
                    .clone(),
                None => UniqueType::Unknown,
            },
            _ => UniqueType::Unknown, // TODO
        };
        self.expr_type_table.insert(expr as *const Expr, ty);
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
                        if let Some(ty) = self.expr_type_table.get(&(initializer as *const Expr)) {
                            if ty == &UniqueType::Unknown {
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
                            }
                            ty.clone()
                        } else {
                            // FIXME: declaration without initializer (not yet syntactically possible).
                            unreachable!("declaration without initializer");
                        }
                    }
                };
                let symbol = self.resolve_result.lookup_declaration(stmt).unwrap();
                self.symbol_type_table
                    .insert(symbol.as_ptr() as *const Symbol, ty);
            }
            StmtKind::FnDeclaration {
                ident: _,
                params,
                body: _,
            } => {
                let symbol = self.resolve_result.lookup_declaration(stmt).unwrap();
                self.symbol_type_table
                    .insert(symbol.as_ptr() as *const Symbol, UniqueType::Unknown);

                for param in params {
                    // FIXME: give type to param
                }
                // FIXME give function proper type
            }
            _ => {}
        }
    }
}
