//! Variable resolution pass.

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Range;
use std::rc::Rc;

use ella_parser::ast::{Expr, ExprKind, Stmt};
use ella_parser::visitor::{walk_expr, Visitor};
use ella_source::{Source, SyntaxError};
use ella_value::BuiltinVars;

/// Result of running [`Resolver`] pass.
/// See [`Resolver::resolve_result`].
#[derive(Debug, Clone, Copy)]
pub struct ResolveResult<'a> {
    symbol_table: &'a SymbolTable,
    resolved_symbol_table: &'a ResolvedSymbolTable,
}

impl<'a> ResolveResult<'a> {
    /// Lookup a [`Stmt`] (by reference) to get variable resolution metadata.
    pub fn lookup_declaration(&self, stmt: &Stmt) -> Option<&'a Rc<RefCell<Symbol>>> {
        self.symbol_table.get(&(stmt as *const Stmt))
    }

    /// Lookup a [`Expr`] (by reference) to get variable resolution metadata.
    pub fn lookup_identifier(&self, expr: &Expr) -> Option<&'a ResolvedSymbol> {
        self.resolved_symbol_table.get(&(expr as *const Expr))
    }
}

/// Represents a symbol (created using `let` or `fn` declaration statement).
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    ident: String,
    scope_depth: u32,
    pub is_captured: bool,
    pub upvalues: Vec<ResolvedUpValue>,
}

/// Represents a resolved upvalue (captured variable).
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedUpValue {
    pub is_local: bool,
    pub index: i32,
}

/// Represents a resolved symbol (identifier or function call expressions).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ResolvedSymbol {
    /// The offset relative to the current function's offset (`current_func_offset`).
    pub offset: i32,
    /// Optimization to emit `ldglobal` and `stglobal` instructions.
    pub is_global: bool,
    pub is_upvalue: bool,
}

/// A [`HashMap`] mapping [`Stmt`]s to [`Symbol`]s.
pub type SymbolTable = HashMap<*const Stmt, Rc<RefCell<Symbol>>>;
/// A [`HashMap`] mapping [`Expr`] to [`ResolvedSymbol`]s.
pub type ResolvedSymbolTable = HashMap<*const Expr, ResolvedSymbol>;

/// Variable resolution pass.
pub struct Resolver<'a> {
    /// A [`HashMap`] mapping all declaration [`Stmt`]s to [`Symbol`]s.
    symbol_table: SymbolTable,
    /// A [`HashMap`] mapping all [`Expr::Identifier`]s to [`ResolvedSymbol`]s.
    resolved_symbol_table: ResolvedSymbolTable,
    /// A [`Vec`] of symbols that are currently in (lexical) scope.
    accessible_symbols: Vec<Rc<RefCell<Symbol>>>,
    /// A stack of current function scope depths. `0` is global scope.
    function_scope_depths: Vec<u32>,
    /// Every time a new function scope is created, `current_func_offset` should be set to `self.resolved_symbols.len()`.
    /// When exiting a function scope, the value should be reverted to previous value.
    current_func_offset: i32,
    /// A stack of current function upvalues.
    function_upvalues: Vec<Vec<ResolvedUpValue>>,
    source: &'a Source<'a>,
}

impl<'a> Resolver<'a> {
    /// Create a new empty `Resolver`.
    pub fn new(source: &'a Source) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            resolved_symbol_table: ResolvedSymbolTable::new(),
            accessible_symbols: Vec::new(),
            function_scope_depths: vec![0],
            current_func_offset: 0,
            function_upvalues: vec![Vec::new()],
            source,
        }
    }

    /// Create a new `Resolver` with existing accessible symbols.
    /// This method is used to implement REPL functionality (for restoring global variables).
    /// See [`Self::accessible_symbols`].
    pub fn new_with_existing_accessible_symbols(
        source: &'a Source,
        resolved_symbols: Vec<Rc<RefCell<Symbol>>>,
    ) -> Self {
        Self {
            accessible_symbols: resolved_symbols,
            ..Self::new(source)
        }
    }

    /// Creates a [`ResolveResult`].
    pub fn resolve_result(&self) -> ResolveResult {
        ResolveResult {
            symbol_table: &self.symbol_table,
            resolved_symbol_table: &self.resolved_symbol_table,
        }
    }

    /// Returns the list of accessible symbols.
    /// This method is used to implement REPL functionality (for restoring global variables).
    /// See [`Self::new_with_existing_accessible_symbols`].
    pub fn accessible_symbols(&self) -> &Vec<Rc<RefCell<Symbol>>> {
        &self.accessible_symbols
    }

    /// Enter a scope.
    fn enter_scope(&mut self) {
        *self.function_scope_depths.last_mut().unwrap() += 1;
    }

    /// Exit a scope. Removes all declarations introduced in previous scope.
    fn exit_scope(&mut self) {
        *self.function_scope_depths.last_mut().unwrap() -= 1;

        // Remove all symbols in current scope.
        self.accessible_symbols = self
            .accessible_symbols
            .iter()
            .filter(|symbol| {
                symbol.borrow().scope_depth <= *self.function_scope_depths.last().unwrap()
            })
            .cloned()
            .collect();
    }

    /// Adds a symbol to `self.accessible_symbols` and `self.symbol_table`.
    fn add_symbol(&mut self, ident: String, stmt: Option<&Stmt>) {
        let symbol = Rc::new(RefCell::new(Symbol {
            ident,
            scope_depth: *self.function_scope_depths.last().unwrap(),
            is_captured: false, // not captured by default
            upvalues: Vec::new(),
        }));
        self.accessible_symbols.push(Rc::clone(&symbol));
        if let Some(stmt) = stmt {
            self.symbol_table.insert(stmt as *const Stmt, symbol);
        }
    }

    /// Returns the function scope depth of the specified `scope_depth`.
    fn find_function_scope_depth(&self, scope_depth: u32) -> usize {
        for (i, function_scope_depth) in self.function_scope_depths.iter().enumerate().rev() {
            if *function_scope_depth < scope_depth {
                return i + 1;
            }
        }
        return 0;
    }

    /// Returns `true` if both scope depths are in the same function (e.g. using block statements). Returns `false` otherwise.
    fn in_same_function_scope(&self, first: u32, second: u32) -> bool {
        self.find_function_scope_depth(first) == self.find_function_scope_depth(second)
    }

    /// Returns a `Some((usize, Rc<RefCell<Symbol>>))` or `None` if cannot be resolved.
    /// The `usize` is the offset of the variable.
    ///
    /// # Params
    /// * `ident` - The identifier to resolve.
    /// * `span` - The span of the expression to resolve. This is used for error reporting in case the variable could not be resolved.
    fn resolve_symbol(
        &mut self,
        ident: &str,
        span: Range<usize>,
    ) -> Option<(usize, Rc<RefCell<Symbol>>)> {
        for (i, symbol) in self.accessible_symbols.iter().enumerate().rev() {
            if symbol.borrow().ident == ident {
                if self.find_function_scope_depth(symbol.borrow().scope_depth) == 0 {
                    return Some((i, symbol.clone()));
                }
                else if self.in_same_function_scope(
                    symbol.borrow().scope_depth,
                    *self.function_scope_depths.last().unwrap(),
                ) {
                    return Some((i - self.current_func_offset as usize, symbol.clone()));
                } else {
                    // capture outer variable
                    symbol.borrow_mut().is_captured = true;

                    // thread upvalue in enclosing functions
                    let mut prev_upvalue_index = 0;
                    for scope_depth in self.find_function_scope_depth(symbol.borrow().scope_depth)
                        + 1
                        ..=self
                            .find_function_scope_depth(*self.function_scope_depths.last().unwrap())
                    {
                        let is_local = scope_depth == symbol.borrow().scope_depth as usize + 1;
                        self.function_upvalues[scope_depth as usize].push(ResolvedUpValue {
                            is_local,
                            index: if is_local {
                                i as i32
                            } else {
                                prev_upvalue_index as i32
                            },
                        });

                        prev_upvalue_index = self.function_upvalues[scope_depth as usize].len() - 1;
                    }

                    return Some((
                        (self.function_upvalues.last().unwrap().len() - 1) as usize,
                        symbol.clone(),
                    ));
                }
            }
        }
        self.source.errors.add_error(SyntaxError::new(
            format!("Cannot resolve symbol {}", ident),
            span,
        ));
        None
    }

    /// Resolve a top-level function [`Stmt`]. This should be used over calling `visit_stmt`.
    pub fn resolve_top_level(&mut self, func: &'a Stmt) {
        match func {
            Stmt::FnDeclaration { body, .. } => {
                for stmt in body {
                    self.visit_stmt(stmt);
                }
            }
            _ => panic!("func is not a Stmt::FnDeclaration"),
        }
    }

    /// Resolve builtin variables.
    pub fn resolve_builtin_vars(&mut self, builtin_vars: &BuiltinVars) {
        for (ident, _value) in &builtin_vars.values {
            self.add_symbol(ident.clone(), None);
        }
    }
}

impl<'a> Visitor<'a> for Resolver<'a> {
    fn visit_expr(&mut self, expr: &'a Expr) {
        walk_expr(self, expr);

        match &expr.kind {
            ExprKind::Identifier(ident) => {
                let symbol = self.resolve_symbol(ident, expr.span.clone());
                if let Some((offset, symbol)) = symbol {
                    self.resolved_symbol_table.insert(
                        expr as *const Expr,
                        ResolvedSymbol {
                            offset: offset as i32,
                            is_global: self.find_function_scope_depth(symbol.borrow().scope_depth)
                                == 0,
                            is_upvalue: self.find_function_scope_depth(
                                *self.function_scope_depths.last().unwrap(),
                            ) > self
                                .find_function_scope_depth(symbol.borrow().scope_depth),
                        },
                    );
                }
            }
            ExprKind::FnCall { callee, args } => {
                self.visit_expr(callee);
                for expr in args {
                    self.visit_expr(expr);
                }
            }
            _ => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        // Do not use default walking logic.

        match stmt {
            Stmt::LetDeclaration { ident, initializer } => {
                self.visit_expr(initializer);
                self.add_symbol(ident.clone(), Some(stmt));
            }
            Stmt::FnDeclaration {
                ident,
                params,
                body,
            } => {
                self.add_symbol(ident.clone(), Some(stmt)); // Add symbol first to allow for recursion.

                let old_func_offset = self.current_func_offset;

                self.current_func_offset = self.accessible_symbols.len() as i32;
                self.function_upvalues.push(Vec::new());
                self.function_scope_depths
                    .push(*self.function_scope_depths.last().unwrap());

                self.enter_scope();
                // add arguments
                for param in params {
                    self.add_symbol(param.clone(), Some(stmt));
                }

                for stmt in body {
                    self.visit_stmt(stmt);
                }
                self.exit_scope();

                // patch self.symbol_table with upvalues
                self.symbol_table
                    .get(&(stmt as *const Stmt))
                    .unwrap()
                    .borrow_mut()
                    .upvalues = self.function_upvalues.pop().unwrap();
                self.function_scope_depths.pop();

                self.current_func_offset = old_func_offset;
            }
            Stmt::Block(body) => {
                self.enter_scope();
                for stmt in body {
                    self.visit_stmt(stmt);
                }
                self.exit_scope();
            }
            Stmt::IfElseStmt {
                condition,
                if_block,
                else_block,
            } => {
                self.visit_expr(condition);
                self.enter_scope();
                for stmt in if_block {
                    self.visit_stmt(stmt);
                }
                self.exit_scope();
                if let Some(else_block) = else_block {
                    self.enter_scope();
                    for stmt in else_block {
                        self.visit_stmt(stmt);
                    }
                    self.exit_scope();
                }
            }
            Stmt::WhileStmt { condition, body } => {
                self.visit_expr(condition);
                self.enter_scope();
                for stmt in body {
                    self.visit_stmt(stmt);
                }
                self.exit_scope();
            }
            Stmt::ExprStmt(expr) => self.visit_expr(expr),
            Stmt::ReturnStmt(expr) => self.visit_expr(expr),
            Stmt::Error => {}
        }
    }
}
