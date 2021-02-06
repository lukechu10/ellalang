use crate::ast::ExprKind;

use super::*;

impl<'a> Parser<'a> {
    /* Expressions */
    /// Parses any expression.
    /// This is equivalent to calling [`Self::parse_expr_bp`] with `min_bp = 0`.
    pub fn parse_expr(&mut self) -> Expr {
        self.parse_expr_bp(0) // 0 to accept any expression
    }

    /// Parses a primary (atom) expression.
    fn parse_primary_expr(&mut self) -> Expr {
        // NOTE: prefix operators are handled here
        match self.current_token {
            Token::NumberLit(_) | Token::BoolLit(_) | Token::StringLit(_) => {
                self.parse_literal_expr()
            }
            Token::Identifier(_) => self.parse_identifier_expr(),
            Token::LogicalNot => {
                let lo = self.node_start();
                self.next();
                ExprKind::Unary {
                    op: Token::LogicalNot,
                    arg: Box::new(self.parse_expr()),
                }
                .with_span(lo..self.node_end())
            }
            Token::Minus => {
                let lo = self.node_start();
                self.next();
                ExprKind::Unary {
                    op: Token::Minus,
                    arg: Box::new(self.parse_expr()),
                }
                .with_span(lo..self.node_end())
            }
            Token::OpenParen => {
                self.next();
                let expr = self.parse_expr();
                self.expect(Token::CloseParen);
                expr
            }
            Token::Fn => self.parse_lambda_expr(),
            _ => {
                let lo = self.node_start();
                self.next();
                self.unexpected();
                ExprKind::Error.with_span(lo..self.node_end())
            }
        }
    }

    /// Parses an expression with the specified `min_bp`.
    /// To parse any expression use, [`Self::parse_expr`].
    fn parse_expr_bp(&mut self, min_bp: u8) -> Expr {
        let mut lhs = self.parse_primary_expr(); // TODO: move handle prefix into parse_expr_bp

        loop {
            // handle postfix
            if let Some((l_bp, ())) = self.current_token.postfix_bp() {
                if l_bp < min_bp {
                    break;
                }
                let postfix_op = self.current_token.clone();
                self.next();

                match postfix_op {
                    Token::OpenParen => {
                        // parse call expression
                        let mut args = Vec::new();

                        if !self.eat(Token::CloseParen) {
                            loop {
                                args.push(self.parse_expr());

                                if self.eat(Token::CloseParen) {
                                    break;
                                } else if !self.eat(Token::Comma) {
                                    self.next();
                                    self.unexpected();
                                    break;
                                }
                            }
                        }

                        let lo = lhs.span.start;
                        lhs = ExprKind::FnCall {
                            callee: Box::new(lhs),
                            args,
                        }
                        .with_span(lo..self.node_end());
                    }
                    _ => unreachable!(),
                }

                continue;
            }

            // handle infix
            let (l_bp, r_bp) = match self.current_token.binop_bp() {
                Some(bp) => bp,
                None => break, // not a valid binop, stop parsing
            };
            if l_bp < min_bp {
                break; // less than the min_bp, stop parsing
            }

            // self.current_token is a valid binop
            let binop = self.current_token.clone();
            self.next();

            let rhs = self.parse_expr_bp(r_bp);

            let lo = lhs.span.start;
            let hi = rhs.span.end;
            lhs = ExprKind::Binary {
                lhs: Box::new(lhs),
                op: binop,
                rhs: Box::new(rhs),
            }
            .with_span(lo..hi)
        }

        lhs
    }

    /* Expressions.Literals */
    /// Parses a literal expression.
    /// A literal can be either a number literal or a bool literal.
    fn parse_literal_expr(&mut self) -> Expr {
        let lo = self.node_start();

        let val = match self.current_token {
            Token::NumberLit(val) => ExprKind::NumberLit(val),
            Token::BoolLit(val) => ExprKind::BoolLit(val),
            Token::StringLit(ref val) => ExprKind::StringLit(val.clone()),
            _ => {
                self.unexpected();
                ExprKind::Error
            }
        };
        if val != ExprKind::Error {
            self.next(); // eat parsed token if not error
        }

        val.with_span(lo..self.node_end())
    }

    /* Expressions.Identifier */
    /// Parses an identifier or a call expression.
    fn parse_identifier_expr(&mut self) -> Expr {
        let lo = self.node_start();

        let ident = match self.current_token.clone() {
            Token::Identifier(ident) => {
                self.next();
                ident
            }
            _ => {
                self.next();
                self.unexpected();
                return ExprKind::Error.with_span(lo..self.node_end());
            }
        };

        ExprKind::Identifier(ident).with_span(lo..self.node_end())
    }

    /* Expressions.Lambda */
    /// Parses a lambda expression.
    fn parse_lambda_expr(&mut self) -> Expr {
        let lo = self.node_start();

        self.expect(Token::Fn);

        self.expect(Token::OpenParen);
        let mut params = Vec::new();
        if !self.eat(Token::CloseParen) {
            loop {
                params.push(if let Token::Identifier(ref ident) = self.current_token {
                    let ident_lo = self.node_start();
                    let ident = ident.clone();
                    self.next();
                    StmtKind::FnParam { ident }.with_span(ident_lo..self.node_end())
                } else {
                    self.unexpected();
                    return ExprKind::Error.with_span(lo..self.node_end());
                });

                if self.eat(Token::CloseParen) {
                    break;
                } else if !self.eat(Token::Comma) {
                    self.unexpected();
                    break;
                }
            }
        }

        self.expect(Token::OpenBrace);
        let mut body = Vec::new();
        if !self.eat(Token::CloseBrace) {
            loop {
                body.push(self.parse_declaration());

                if self.eat(Token::CloseBrace) {
                    break;
                }
            }
        }

        let hi = self.node_end();
        ExprKind::Lambda {
            inner_stmt: Box::new(StmtKind::Lambda.with_span(lo..hi)),
            params,
            body,
        }
        .with_span(lo..hi)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    fn expr(source: &str) -> Expr {
        let source = source.into();
        let ast = Parser::new(&source).parse_expr();
        assert!(source.has_no_errors());
        ast
    }

    #[test]
    fn test_literal() {
        assert_debug_snapshot!("bool-lit-true", expr("true"));
        assert_debug_snapshot!("bool-lit-false", expr("false"));
        assert_debug_snapshot!("int", expr("1"));
        assert_debug_snapshot!("double-2.0", expr("2.0"));
        assert_debug_snapshot!("double-2.5", expr("2.5"));
    }

    #[test]
    fn test_string_literal() {
        assert_debug_snapshot!("simple-string", expr(r#""abc""#));
        assert_debug_snapshot!("empty-string", expr(r#""""#));
        assert_debug_snapshot!("simple-escape-string", expr(r#""\n\t\\""#));
        assert_debug_snapshot!("all-valid-escape-sequences-string", expr(r#""\n\r\t\b\f\v\0""#));
        assert_debug_snapshot!("redundant-escape-sequences-string", expr(r#""a\aa""#)); // should be "aaa"
    }

    #[test]
    #[should_panic]
    fn bad_string_literal() {
        expr(r#""abc\""#);
    }

    #[test]
    fn test_binary_expr() {
        assert_debug_snapshot!("binary", expr("1 + 1"));
        assert_debug_snapshot!("binary-equality", expr("1 == 2 - 1"));
        assert_debug_snapshot!("binary-associativity", expr("2 * 2 * 2")); // should be (2 * 2) * 2
        assert_debug_snapshot!("binary-associativity-2", expr("a = b = c")); // should be a = (b = c)
        assert_debug_snapshot!("binary-paren", expr("(a + b) * c")); // should be (a + b) * c
    }

    #[test]
    fn test_identifier() {
        assert_debug_snapshot!("identifier", expr("foo"));
    }

    #[test]
    fn test_fn_call() {
        assert_debug_snapshot!("fn-call", expr("foo()"));
        assert_debug_snapshot!("fn-call-with-args", expr("foo(1, bar)"));
        assert_debug_snapshot!("fn-call-with-nested-args", expr("foo(1, bar, baz())"));
        assert_debug_snapshot!("fn-call-chained", expr("foo(1, 2)(3)(4)"));
        assert_debug_snapshot!("fn-call-fib", expr("fib(x - 1) + fib(x - 2)"));
    }

    #[test]
    fn test_lambda() {
        assert_debug_snapshot!("lambda", expr("fn () {}"));
        assert_debug_snapshot!("lambda-with-params", expr("fn (x) {}"));
    }
}
