//! Source code representation and error management.

use std::{cell::RefCell, fmt, ops::Range};

use console::style;

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

    /// Returns the line and column which the `pos` is located at as a tuple `(line, col)`.
    pub fn lookup_line_col(&self, pos: usize) -> (usize, usize) {
        if self.lines.is_empty() {
            (0, pos)
        } else {
            match self.lines.binary_search(&pos) {
                Ok(line) => (line, 0),
                Err(line) => (line - 1, pos - self.lines[line - 1]),
            }
        }
    }

    /// Gets the slice at `line`.
    pub fn get_line(&self, line: usize) -> &str {
        let start = self.lines[line];
        if line == self.lines.len() - 1 {
            &self.content[start..]
        } else {
            let end = self.lines[line + 1] - 1; // don't get newline character
            &self.content[start..end]
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
    help: Vec<String>,
}

impl SyntaxError {
    /// Create a new syntax error with the specified `message` and `span`.
    pub fn new(message: impl ToString, span: Range<usize>) -> Self {
        Self {
            message: message.to_string(),
            span,
            help: Vec::new(),
        }
    }

    /// Adds a help message to the diagnostic.
    pub fn with_help(mut self, message: impl ToString) -> Self {
        self.help.push(message.to_string());
        self
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

impl<'a> fmt::Display for Source<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let errors = self.errors.errors.borrow();
        for error in errors.iter() {
            let start = self.lookup_line_col(error.span.start);
            let end = self.lookup_line_col(error.span.end);

            writeln!(
                f,
                "{error}{message}\n{filename}",
                error = style("error").red().bright().bold(),
                message = style(format!(": {errMessage}", errMessage = error.message,)).bold(),
                filename = format!(
                    "   {arrow} {filename}",
                    arrow = style("-->").cyan().bright().bold(),
                    filename = format!(
                        "{filename}:{line}:{col}",
                        filename = "unknown", // FIXME
                        line = start.0 + 1,   // +1 for 1-based line position
                        col = start.1 + 1,    // +1 for 1-based column position
                    )
                ),
            )?;
            if start.0 == end.0 {
                let is_0_width = start.1 == end.1;
                let line = if is_0_width {
                    start.0.max(1) - 1 // .max(1) to prevent overflow
                } else {
                    start.0
                };

                writeln!(f, "    {}", style("|").cyan().bright().bold())?;

                write!(
                    f,
                    "{line_content}{}",
                    style("|").cyan().bright().bold(),
                    line_content = style(format!("{:<4}", line + 1)).cyan().bright().bold(),
                )?;
                writeln!(f, " {}", self.get_line(line))?;

                write!(f, "    {}", style("|").cyan().bright().bold())?;
                writeln!(
                    f,
                    "{} {} {}",
                    " ".repeat(if is_0_width { start.1 + 1 } else { start.1 }),
                    style("^".repeat(usize::max(end.1 - start.1, 1))) // at least 1 `^` character (e.g. for missing tokens)
                        .red()
                        .bright()
                        .bold(),
                    style(&error.message).red().bright().bold(),
                )?;
            } else {
                // TODO: multi-line errors
            }
            // print help messages
            for help_msg in &error.help {
                writeln!(f, "{}: {}", style("help").cyan().bright().bold(), help_msg)?;
            }
        }

        Ok(())
    }
}
