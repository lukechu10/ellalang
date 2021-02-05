//! Source code lexing (aka scanning, tokenizing).

use logos::Logos;

/// Represents a source code `Token`.
#[derive(Debug, Logos, Clone, PartialEq)]
pub enum Token {
    // literals
    #[regex(r"[0-9.]+", |lex| lex.slice().parse())]
    NumberLit(f64),
    #[regex(r"true|false", |lex| if lex.slice() == "true" { true } else { false } )]
    BoolLit(bool),
    #[regex(r#""[^"]*""#, |lex| lex.slice()[1..lex.slice().len() - 1].to_string())]
    StringLit(String),

    // identifiers
    #[regex("[a-zA-Z_]+[a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // unary operators
    #[token("!")]
    LogicalNot,

    // binary operators
    // - arithmetics
    #[token("+")]
    Plus,
    #[token("-")]
    Minus, // NOTE: can also be unary
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    // - assignment
    #[token("=")]
    Equals,
    #[token("+=")]
    PlusEquals,
    #[token("-=")]
    MinusEquals,
    #[token("*=")]
    AsteriskEquals,
    #[token("/=")]
    SlashEquals,
    // - equality
    #[token("==")]
    EqualsEquals,
    #[token("!=")]
    NotEquals,
    // - ordering
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanEquals,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessThanEquals,

    // punctuation
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,

    // keywords
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("return")]
    Return,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,

    // misc
    #[regex(r"[ \t\n\r\f]+", logos::skip)]
    #[regex(r"//[^\n]*", logos::skip)] // single line comments
    #[error]
    Error,

    /// Only generated in parse phase when `lexer.next()` returns `None`.
    Eof,
}

impl Token {
    /// Returns the binary binding power or `None` if invalid binop token.
    /// Binding power `0` and `1` is reserved for accepting any expression.
    /// Assignment ([`Token::Equals`]) has the lowest precedence with `(3, 2)`.
    /// 
    /// # Example
    /// ```
    /// use ella_parser::lexer::Token;
    /// 
    /// assert_eq!(Token::Plus.binop_bp(), Some((8, 9)));
    /// assert_eq!(Token::Equals.binop_bp(), Some((3, 2)));
    /// assert_eq!(Token::While.binop_bp(), None); // returns None if invalid operator
    /// ```
    pub fn binop_bp(&self) -> Option<(u8, u8)> {
        match self {
            /* Additive */
            Token::Plus | Token::Minus => Some((8, 9)),
            /* Multiplicative */
            Token::Asterisk | Token::Slash => Some((10, 11)),
            /* Assignment */
            Token::Equals
            | Token::PlusEquals
            | Token::MinusEquals
            | Token::AsteriskEquals
            | Token::SlashEquals => Some((3, 2)),
            /* Equality */
            Token::EqualsEquals | Token::NotEquals => Some((4, 5)),
            Token::GreaterThan
            | Token::GreaterThanEquals
            | Token::LessThan
            | Token::LessThanEquals => Some((6, 7)),
            _ => None,
        }
    }

    /// Returns the postfix binding power or `None` if invalid binop token.
    /// Binding power `0` and `1` is reserved for accepting any expression.
    /// 
    /// # Example
    /// ```
    /// use ella_parser::lexer::Token;
    /// 
    /// assert_eq!(Token::OpenParen.postfix_bp(), Some((12, ()))); // function call operator
    /// assert_eq!(Token::While.postfix_bp(), None); // returns None if invalid operator
    /// ```
    pub fn postfix_bp(&self) -> Option<(u8, ())> {
        match self {
            Token::OpenParen => Some((12, ())), // function call
            _ => None,
        }
    }

    /// Returns `true` if the token is synchronization point for error recovery.
    pub fn is_sync_point(&self) -> bool {
        match self {
            Token::Semi
            | Token::OpenBrace
            | Token::CloseBrace
            | Token::OpenParen
            | Token::CloseParen
            | Token::Error
            | Token::Eof => true,
            _ => false,
        }
    }
}
