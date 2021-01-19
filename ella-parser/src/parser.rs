//! Parse a [`Source`] into an AST (abstract syntax tree).

use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::lexer::Token;
use ella_source::{Source, SyntaxError};
use logos::{Lexer, Logos};
use std::mem;
use std::ops::Range;

mod expr;
mod stmt;
pub use expr::*;
pub use stmt::*;

/// A parser instance.
pub struct Parser<'a> {
    /// Cached token for peeking.
    current_token: Token,
    previous_span: Range<usize>,
    /// Location of the current token.
    current_span: Range<usize>,
    lexer: Lexer<'a, Token>,
    /// Source code.
    source: &'a Source<'a>,
}

impl<'a> Parser<'a> {
    /// Create a new [`Parser`] from the `source`.
    pub fn new(source: &'a Source<'a>) -> Self {
        let mut lexer = Token::lexer(source.content);
        Self {
            current_token: lexer.next().unwrap_or(Token::Eof),
            previous_span: 0..0,
            current_span: lexer.span(),
            lexer,
            source,
        }
    }
}

impl<'a> Parser<'a> {
    /// Returns an anonymous top level function.
    pub fn parse_program(&mut self) -> Stmt {
        let lo = self.node_start();

        let mut body = Vec::new();
        loop {
            body.push(self.parse_declaration());
            if self.current_token == Token::Eof {
                break;
            }
        }

        StmtKind::FnDeclaration {
            body,
            ident: "<global>".to_string(),
            params: Vec::new(),
        }
        .with_span(lo..self.node_end())
    }

    /// Returns an anonymous top level function.
    /// If the last statement is an [`Stmt::ExprStmt`], it will create a function call to `println()`.
    pub fn parse_repl_input(&mut self) -> Stmt {
        let lo = self.node_start();

        let mut body = Vec::new();
        loop {
            body.push(self.parse_declaration());
            if matches!(self.current_token, Token::Eof | Token::Error) {
                break;
            }
        }

        if let Some(Stmt {
            kind: StmtKind::ExprStmt(expr),
            ..
        }) = body.last_mut()
        {
            *expr = ExprKind::FnCall {
                args: vec![expr.clone()],
                callee: Box::new(ExprKind::Identifier("println".to_string()).with_span(0..0)),
            }
            .with_span(0..0)
        }

        StmtKind::FnDeclaration {
            body,
            ident: "<global>".to_string(),
            params: Vec::new(),
        }
        .with_span(lo..self.node_end())
    }
}

/// Parser utilities.
impl<'a> Parser<'a> {
    fn next(&mut self) -> Token {
        let token = self.lexer.next().unwrap_or(Token::Eof);
        self.current_token = token.clone();

        self.previous_span = self.current_span.clone();
        self.current_span = self.lexer.span();
        token
    }

    /// Predicate that tests whether the next token has the same discriminant and eats the next token if yes as a side effect.
    #[must_use = "to unconditionally eat a token, use Self::next"]
    fn eat(&mut self, tok: Token) -> bool {
        if mem::discriminant(&self.current_token) == mem::discriminant(&tok) {
            self.next(); // eat token
            true
        } else {
            false
        }
    }

    fn expect(&mut self, tok: Token) {
        if !self.eat(tok.clone()) {
            if tok == Token::Semi {
                // do not skip over next token because semicolons is a sync point
                self.source.errors.add_error(
                    SyntaxError::new("expected a `;` character", self.current_span.clone())
                        .with_help("consider adding a `;` character"),
                )
            } else {
                while !self.eat(tok.clone()) && !self.current_token.is_sync_point() {
                    self.next();
                    self.unexpected()
                }
            }
        }
    }

    /// Raises an unexpected token error.
    fn unexpected(&mut self) {
        self.source.errors.add_error(SyntaxError::new(
            "unexpected token",
            self.previous_span.clone(),
        ))
    }

    /// Returns the start of the current token.
    fn node_start(&self) -> usize {
        self.current_span.start
    }

    /// Returns the end of the last token.
    fn node_end(&self) -> usize {
        self.previous_span.end
    }
}
