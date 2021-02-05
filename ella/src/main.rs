use anyhow::{Context, Result};
use builtin_functions::builtin_initial_state;
use ella::builtin_functions::default_builtin_vars;
use ella_parser::parser::Parser;
use ella_passes::resolve::Resolver;
use ella_passes::type_checker::TypeChecker;
use ella_vm::codegen::Codegen;
use ella_vm::vm::InterpretResult;

use std::io::{self, Write};

mod builtin_functions;

/// Returns `true` if the repl input is finished.
fn repl_input_is_finished(buf: &str) -> bool {
    let mut paren_count = 0;
    let mut brace_count = 0;

    for char in buf.chars() {
        match char {
            '(' => paren_count += 1,
            ')' => paren_count -= 1,
            '{' => brace_count += 1,
            '}' => brace_count -= 1,
            _ => {}
        }

        if paren_count < 0 || brace_count < 0 {
            return true; // paren_count and brace_count should be >= 0. Otherwise is error.
        }
    }

    paren_count == 0 && brace_count == 0
}

/// Reads a line from the repl input. Automatically detects if input is finished (balanced paren and brace operators) and prompts user for second line if not.
fn get_repl_input() -> String {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut buf = String::new();
    // Current indentation level. Incremented when last character is '(' or '{'. Decremented when string contains ')' or '}'.
    let mut indent: i32 = 0;
    match stdin.read_line(&mut buf) {
        Ok(_) => {}
        Err(err) => {
            eprintln!("Error while reading input: {}", err);
        }
    } // initial input

    while !repl_input_is_finished(&buf) {
        let mut tmp = String::new();

        print!("{} ", ".".repeat((5 + (4 * indent)) as usize));
        stdout.flush().unwrap();
        match stdin.read_line(&mut tmp) {
            Ok(_) => {}
            Err(err) => {
                eprintln!("Error while reading input: {}", err);
            }
        }

        if tmp.find(|c| c == '(' || c == '{').is_some() {
            indent += 1;
        }
        if tmp.find(|c| c == ')' || c == '}').is_some() {
            indent -= std::cmp::max(1, 0); // at least 0
        }

        buf += tmp.as_str();
    }

    buf
}

fn repl() {
    let mut stdout = io::stdout();

    let builtin_vars = default_builtin_vars();

    let (mut resolve_result, mut type_check_result, mut vm) = builtin_initial_state(&builtin_vars);

    loop {
        print!("> ");
        stdout.flush().unwrap();
        let input = get_repl_input();

        if input.trim() == ".exit" {
            return; // exit repl
        }

        let source = input.as_str().into();
        let mut parser = Parser::new(&source);
        let ast = parser.parse_repl_input();

        let mut resolver =
            Resolver::new_with_existing_resolve_result(source.clone(), resolve_result.clone());
        resolver.resolve_top_level(&ast);

        let resolve_result_tmp = resolver.into_resolve_result();

        let mut type_checker = TypeChecker::new_with_type_check_result(
            &resolve_result_tmp,
            source.clone(),
            type_check_result,
        );
        type_checker.type_check_global(&ast);
        type_check_result = type_checker.into_type_check_result();

        eprintln!("{}", source);
        if source.has_no_errors() {
            let mut codegen = Codegen::new("<global>".to_string(), &resolve_result_tmp, &source);

            codegen.codegen_function(&ast);

            let chunk = codegen.into_inner_chunk();

            let initial_stack = vm.vm_state().clone();
            let interpret_result = vm.interpret(chunk);
            match &interpret_result {
                InterpretResult::Ok => {
                    // Success, update resolved_symbols with new symbols.
                    resolve_result = resolve_result_tmp;
                }
                InterpretResult::RuntimeError { .. } => {
                    eprintln!("{:?}", interpret_result);
                    // Restore vm stack to previous state to recover from error.
                    vm.restore_vm_state(initial_stack);
                }
            }
        }
    }
}

fn interpret_file_contents(source: &str) {
    let builtin_vars = default_builtin_vars();

    let (mut resolve_result, mut type_check_result, mut vm) = builtin_initial_state(&builtin_vars);

    let source = source.into();
    let mut parser = Parser::new(&source);
    let ast = parser.parse_program();

    let mut resolver = Resolver::new_with_existing_resolve_result(source.clone(), resolve_result);
    resolver.resolve_top_level(&ast);
    resolve_result = resolver.into_resolve_result();

    let mut type_checker =
        TypeChecker::new_with_type_check_result(&resolve_result, source.clone(), type_check_result);
    type_checker.type_check_global(&ast);
    type_check_result = type_checker.into_type_check_result();
    let _ = type_check_result;

    if !source.has_no_errors() {
        eprintln!("{}", source);
    } else {
        let mut codegen = Codegen::new("<global>".to_string(), &resolve_result, &source);

        codegen.codegen_function(&ast);

        let chunk = codegen.into_inner_chunk();
        match vm.interpret(chunk) {
            InterpretResult::Ok => {}
            InterpretResult::RuntimeError { message, line } => {
                eprintln!("runtime error: {}\n   --> unknown:{}", message, line);
            }
        }
    }
}

fn main() -> Result<()> {
    if std::env::args().len() < 2 {
        repl();
    } else {
        let path = std::env::args().nth(1).unwrap();
        let contents = std::fs::read_to_string(path.as_str())
            .context(format!("File {} does not exist.", path))?;
        interpret_file_contents(&contents)
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_repl_input_is_finished() {
        // finished
        assert!(repl_input_is_finished(r#"(({}))"#));
        assert!(repl_input_is_finished(r#"(({123;}))"#));
        assert!(repl_input_is_finished(r#"{(})"#)); // finished even with bad order

        // not finished
        assert!(!repl_input_is_finished(r#"(({123;})"#));
        assert!(!repl_input_is_finished(r#"(({123;"#));
        assert!(!repl_input_is_finished(r#"(({"#));
    }
}
