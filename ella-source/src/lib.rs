//! Source code representation and error management.

use std::{cell::RefCell, fmt, ops::Range};

/// Represents source code.
pub struct Source<'a> {
    /// Original source code.
    pub content: &'a str,
    lines: Vec<usize>,
    /// Accumulated errors.
    pub errors: ErrorReporter,
}

impl<'a> Source<'a> {
    /// Create a new `Source` with the specified `content`.
    pub fn new(content: &'a str) -> Self {
        Self {
            content,
            lines: get_newline_pos(content),
            errors: ErrorReporter::new(),
        }
    }

    /// Returns `true` if `Source` has no accumulated errors. Returns `false` otherwise.
    pub fn has_no_errors(&self) -> bool {
        self.errors.errors.borrow().len() == 0
    }

    /// Returns the line which the `pos` is located at.
    pub fn lookup_line(&self, pos: usize) -> usize {
        if self.lines.is_empty() {
            0
        } else {
            match self.lines.binary_search(&pos) {
                Ok(line) => line,
                Err(line) => line - 1,
            }
        }
    }
}

fn get_newline_pos(src: &str) -> Vec<usize> {
    let mut pos = vec![0]; // 0 is position of first newline char

    for (index, ch) in src.char_indices() {
        if ch == '\n' {
            pos.push(index + 1); // + 1 to get char after newline
        }
    }
    pos
}

impl<'a> Into<Source<'a>> for &'a str {
    fn into(self) -> Source<'a> {
        Source::new(self)
    }
}

/// Represents a syntax error (compile time error).
#[derive(Debug, Clone)]
pub struct SyntaxError {
    message: String,
    span: Range<usize>,
}

impl SyntaxError {
    /// Create a new syntax error with the specified `message` and `span`.
    pub fn new(message: impl ToString, span: Range<usize>) -> Self {
        Self {
            message: message.to_string(),
            span,
        }
    }
}

/// Manages all the errors.
pub struct ErrorReporter {
    errors: RefCell<Vec<SyntaxError>>,
}

impl ErrorReporter {
    /// Create an empty `ErrorReporter`.
    pub fn new() -> Self {
        Self {
            errors: RefCell::new(Vec::new()),
        }
    }

    /// Adds an error to the `ErrorReporter`.
    /// This method uses the interior mutability pattern. This does not require mutability for ergonomics.
    pub fn add_error(&self, error: SyntaxError) {
        // This should be the only place where self.errors is borrowed mutably.
        self.errors.borrow_mut().push(error);
    }
}

impl Default for ErrorReporter {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ErrorReporter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let errors = self.errors.borrow();
        for error in errors.iter() {
            writeln!(
                f,
                "ERROR: {message} at position {position}",
                message = error.message,
                position = error.span.start
            )?;
        }

        Ok(())
    }
}
