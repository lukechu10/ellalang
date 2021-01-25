//! AST (abstract syntax tree) data structure.

use std::ops::Range;

use crate::lexer::Token;

/// Represents a type in the source code.
#[derive(Debug, Clone, PartialEq)]
pub struct TypePath {
    /// The identifier of the the type.
    pub ident: String,
    pub span: Range<usize>,
}

/// Wrapper around [`ExprKind`]
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Range<usize>,
}

/// Represents an expression node in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    /// Number literal (represented using floating point `f64`).
    NumberLit(f64),
    /// Boolean literal.
    BoolLit(bool),
    /// String literal.
    StringLit(String),
    /// An identifier (e.g. `foo`).
    Identifier(String),
    /// A function call (e.g. `foo(1, bar, baz())`).
    FnCall { callee: Box<Expr>, args: Vec<Expr> },
    /// A binary expression (e.g. `1+1`).
    Binary {
        lhs: Box<Expr>,
        op: Token,
        rhs: Box<Expr>,
    },
    /// An unary expression (e.g. `-1`).
    Unary { op: Token, arg: Box<Expr> },
    /// A lambda expression.
    Lambda {
        /// Should always be a [`StmtKind::Lambda`]. Note that this field is only a marker and does not store any data.
        inner_stmt: Box<Stmt>,
        params: Vec<String>,
        body: Vec<Stmt>,
    },
    /// Error token. Used for error recovery.
    Error,
}

impl ExprKind {
    /// Construct a [`Expr`] with `self` as `kind` and `span`.
    pub fn with_span(self, span: Range<usize>) -> Expr {
        Expr { kind: self, span }
    }
}

/// Wrapper around [`StmtKind`].
#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Range<usize>,
}

/// Represents a statement node in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    /// Variable declaration.
    LetDeclaration {
        ident: String,
        initializer: Expr,
        /// Optional type annotation.
        ty: Option<TypePath>,
    },
    /// Function declaration.
    FnDeclaration {
        ident: String,
        params: Vec<String>,
        body: Vec<Stmt>,
    },
    /// Block statement.
    Block(Vec<Stmt>),
    /// If/else statement.
    IfElseStmt {
        condition: Expr,
        if_block: Vec<Stmt>,
        /// If `else` clause is not present, this field should be `None`.
        else_block: Option<Vec<Stmt>>,
    },
    /// While statement.
    WhileStmt { condition: Expr, body: Vec<Stmt> },
    /// Expression statement (expression with side effect).
    ExprStmt(Expr),
    /// Return statement.
    ReturnStmt(Expr),
    /// Error token. Used for error recovery.
    Error,
    /// A lambda "statement". There are no fields as this is only a marker, stored inside [`ExprKind::Lambda`] for variable resolution.
    /// This should never be visited in a [`Visitor`].
    Lambda,
}

impl StmtKind {
    /// Construct a [`Stmt`] with `self` as `kind` and `span`.
    pub fn with_span(self, span: Range<usize>) -> Stmt {
        Stmt { kind: self, span }
    }
}
