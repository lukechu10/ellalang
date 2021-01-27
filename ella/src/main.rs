use ella::builtin_functions::default_builtin_vars;
use ella_parser::parser::Parser;
use ella_passes::resolve::Resolver;
use ella_passes::type_checker::TypeChecker;
use ella_source::Source;
use ella_vm::vm::InterpretResult;
use ella_vm::{codegen::Codegen, vm::Vm};

use std::io::{self, Write};

mod builtin_functions;

fn repl() -> ! {
    let mut stdout = io::stdout();
    let stdin = io::stdin();

    let builtin_vars = default_builtin_vars();

    let dummy_source: Source = "".into();
    let mut resolver = Resolver::new(dummy_source.clone());
    resolver.resolve_builtin_vars(&builtin_vars);
    let mut resolve_result = resolver.into_resolve_result();

    let mut type_checker = TypeChecker::new(&resolve_result, dummy_source.clone());
    type_checker.type_check_builtin_vars(&builtin_vars);
    let mut type_check_result = type_checker.into_type_check_result();

    let mut vm = Vm::new(&builtin_vars);
    let mut codegen = Codegen::new("<global>".to_string(), &resolve_result, &dummy_source);
    codegen.codegen_builtin_vars(&builtin_vars);
    vm.interpret(codegen.into_inner_chunk()); // load built in functions into memory

    loop {
        print!("> ");
        stdout.flush().unwrap();

        let mut input = String::new();
        stdin.read_line(&mut input).unwrap();

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

    let dummy_source: Source = "".into();
    let mut resolver = Resolver::new(dummy_source.clone());
    resolver.resolve_builtin_vars(&builtin_vars);
    let mut resolve_result = resolver.into_resolve_result();

    let mut type_checker = TypeChecker::new(&resolve_result, dummy_source.clone());
    type_checker.type_check_builtin_vars(&builtin_vars);
    let mut type_check_result = type_checker.into_type_check_result();

    let mut vm = Vm::new(&builtin_vars);
    let mut codegen = Codegen::new("<global>".to_string(), &resolve_result, &dummy_source);
    codegen.codegen_builtin_vars(&builtin_vars);
    vm.interpret(codegen.into_inner_chunk()); // load built in functions into memory

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
                eprintln!("Runtime Error: {} at line {}", message, line);
            }
        }
    }
}

fn main() {
    if std::env::args().len() < 2 {
        repl();
    } else {
        let path = std::env::args().nth(1).unwrap();
        let contents = std::fs::read_to_string(path);
        match contents {
            Ok(contents) => interpret_file_contents(&contents),
            Err(err) => eprintln!("Error: {}", err),
        }
    }
}
