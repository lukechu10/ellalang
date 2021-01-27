//! Type checking and type inference pass.

use std::collections::HashMap;

use ella_parser::ast::{Expr, ExprKind, Stmt, StmtKind};
use ella_parser::lexer::Token;
use ella_parser::visitor::{walk_expr, walk_stmt, Visitor};
use ella_source::{Source, SyntaxError};
use ella_value::{BuiltinType, BuiltinVars, UniqueType};

use crate::resolve::{ResolveResult, Symbol};

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
        for (ident, _value, ty) in &builtin_vars.values {
            let symbol = self
                .resolve_result
                .lookup_in_accessible_symbols(ident)
                .unwrap(); // get Symbol for builtin value

            assert!(self
                .symbol_type_table
                .insert(symbol.as_ptr() as *const Symbol, ty.clone())
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
                    .expect(&format!(
                        "type of identifier \"{}\" at {:?}",
                        ident, expr.span
                    ))
                    .clone(),
                None => UniqueType::Unknown,
            },
            ExprKind::FnCall { callee, args } => {
                let callee_ty = self
                    .expr_type_table
                    .get(&(callee.as_ref() as *const Expr))
                    .unwrap();
                if let UniqueType::Builtin(BuiltinType::Fn { params, ret }) = callee_ty {
                    // check arity
                    if args.len() != params.len() {
                        self.source.errors.add_error(SyntaxError::new(
                            format!(
                                "expected {} argument(s), found {}",
                                params.len(),
                                args.len()
                            ),
                            expr.span.clone(),
                        ));
                    }
                    // check params type
                    for i in 0..params.len() {
                        let param_ty = &params[i];
                        let arg_ty = self
                            .expr_type_table
                            .get(&(&args[i] as *const Expr))
                            .unwrap();

                        if !arg_ty.can_implicit_cast_to(param_ty) {
                            self.source.errors.add_error(SyntaxError::new(
                                format!(
                                    "wrong type in argument position {}",
                                    i + 1, // +1 for 1-based index
                                ),
                                args[i].span.clone(),
                            ));
                        }
                    }
                    ret.as_ref().clone()
                } else if callee_ty == &UniqueType::Any {
                    UniqueType::Any
                } else {
                    self.source.errors.add_error(SyntaxError::new(
                        "value is not a function",
                        expr.span.clone(),
                    ));
                    UniqueType::Unknown
                }
            }
            ExprKind::Binary { lhs, op, rhs } => {
                let lhs_ty = self
                    .expr_type_table
                    .get(&(lhs.as_ref() as *const Expr))
                    .unwrap();
                let rhs_ty = self
                    .expr_type_table
                    .get(&(rhs.as_ref() as *const Expr))
                    .unwrap();

                if lhs_ty == &UniqueType::Any || rhs_ty == &UniqueType::Any {
                    UniqueType::Any // propagate any
                } else {
                    match op {
                        Token::Plus => {
                            if lhs_ty == &UniqueType::Builtin(BuiltinType::Number)
                                && rhs_ty == &UniqueType::Builtin(BuiltinType::Number)
                            {
                                UniqueType::Builtin(BuiltinType::Number)
                            } else if lhs_ty == &UniqueType::Builtin(BuiltinType::String)
                                && rhs_ty == &UniqueType::Builtin(BuiltinType::String)
                            {
                                UniqueType::Builtin(BuiltinType::String)
                            } else {
                                self.source.errors.add_error(SyntaxError::new(
                                    "expected numbers or strings for addition",
                                    expr.span.clone(),
                                ));
                                UniqueType::Unknown
                            }
                        }
                        Token::Minus => {
                            if lhs_ty != &UniqueType::Builtin(BuiltinType::Number)
                                || rhs_ty != &UniqueType::Builtin(BuiltinType::Number)
                            {
                                self.source.errors.add_error(SyntaxError::new(
                                    "expected numbers for subtraction",
                                    expr.span.clone(),
                                ));
                            }
                            UniqueType::Builtin(BuiltinType::Number)
                        }
                        Token::Asterisk => {
                            if lhs_ty != &UniqueType::Builtin(BuiltinType::Number)
                                || rhs_ty != &UniqueType::Builtin(BuiltinType::Number)
                            {
                                self.source.errors.add_error(SyntaxError::new(
                                    "expected numbers for multiplication",
                                    expr.span.clone(),
                                ));
                            }
                            UniqueType::Builtin(BuiltinType::Number)
                        }
                        Token::Slash => {
                            if lhs_ty != &UniqueType::Builtin(BuiltinType::Number)
                                || rhs_ty != &UniqueType::Builtin(BuiltinType::Number)
                            {
                                self.source.errors.add_error(SyntaxError::new(
                                    "expected numbers for division",
                                    expr.span.clone(),
                                ));
                            }
                            UniqueType::Builtin(BuiltinType::Number)
                        }
                        Token::Equals
                        | Token::PlusEquals
                        | Token::MinusEquals
                        | Token::AsteriskEquals
                        | Token::SlashEquals => {
                            // result of assignment is new value
                            if !rhs_ty.can_implicit_cast_to(lhs_ty) {
                                self.source.errors.add_error(SyntaxError::new(
                                    "wrong type in assignment",
                                    rhs.span.clone(),
                                ));
                            }
                            lhs_ty.clone()
                        }
                        Token::EqualsEquals
                        | Token::NotEquals
                        | Token::LessThan
                        | Token::LessThanEquals
                        | Token::GreaterThan
                        | Token::GreaterThanEquals => {
                            if lhs_ty != rhs_ty {
                                self.source.errors.add_error(SyntaxError::new("comparison operators can only be used on two values of the same type", expr.span.clone()));
                            }
                            UniqueType::Builtin(BuiltinType::Bool)
                        }
                        _ => unreachable!(),
                    }
                }
            }
            ExprKind::Unary { op, arg } => {
                let arg_ty = self
                    .expr_type_table
                    .get(&(arg.as_ref() as *const Expr))
                    .unwrap();
                match op {
                    Token::LogicalNot => {
                        if arg_ty != &UniqueType::Builtin(BuiltinType::Bool) {
                            self.source.errors.add_error(SyntaxError::new(
                                "logical not can only be used on a bool",
                                expr.span.clone(),
                            ));
                        }
                        UniqueType::Builtin(BuiltinType::Bool)
                    }
                    Token::Minus => {
                        if arg_ty != &UniqueType::Builtin(BuiltinType::Number) {
                            self.source.errors.add_error(SyntaxError::new(
                                "unary minus can only be used on a number",
                                expr.span.clone(),
                            ));
                        }
                        UniqueType::Builtin(BuiltinType::Number)
                    }
                    _ => unreachable!(),
                }
            }
            ExprKind::Lambda {
                inner_stmt: _,
                params,
                body: _,
            } => {
                let ty = UniqueType::Builtin(BuiltinType::Fn {
                    params: vec![UniqueType::Any; params.len()],
                    ret: Box::new(UniqueType::Any),
                });
                // FIXME give function proper type
                ty
            }
            ExprKind::Error => UniqueType::Unknown,
        };
        self.expr_type_table.insert(expr as *const Expr, ty);
    }

    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        if !matches!(&stmt.kind, StmtKind::FnDeclaration{..}) {
            // function declaration must be type checked first before body to allow for recursion
            walk_stmt(self, stmt);
        }

        match &stmt.kind {
            StmtKind::LetDeclaration {
                ident,
                initializer,
                ty,
            } => {
                let ty = match ty {
                    Some(ty_path) => {
                        let ty = match ty_path.ident.as_str() {
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
                        };
                        // make sure initializer has right type
                        let initializer_ty = self.expr_type_table.get(&(initializer as *const Expr)).unwrap();
                        if !initializer_ty.can_implicit_cast_to(&ty) {
                            self.source.errors.add_error(SyntaxError::new(
                                "initializer has wrong type",
                                initializer.span.clone(),
                            ))
                        }
                        ty
                    }
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
            StmtKind::FnParam { ident: _ } => {
                // FIXME: give type to param
                let symbol = self.resolve_result.lookup_declaration(stmt).unwrap();

                self.symbol_type_table
                    .insert(symbol.as_ptr() as *const Symbol, UniqueType::Any);
            }
            StmtKind::FnDeclaration {
                ident: _,
                params,
                body,
            } => {
                // NOTE: walking is not enabled for this case

                let symbol = self.resolve_result.lookup_declaration(stmt).unwrap();

                let ty = UniqueType::Builtin(BuiltinType::Fn {
                    params: vec![UniqueType::Any; params.len()],
                    ret: Box::new(UniqueType::Any),
                });
                self.symbol_type_table
                    .insert(symbol.as_ptr() as *const Symbol, ty);

                for param in params {
                    self.visit_stmt(param);
                }
                for stmt in body {
                    self.visit_stmt(stmt);
                }
                // FIXME give function proper type
            }
            _ => {}
        }
    }
}
