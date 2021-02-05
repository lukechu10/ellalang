pub mod builtin_functions;

use builtin_functions::{builtin_initial_state, default_builtin_vars};

use ella_parser::parser::Parser;
use ella_passes::resolve::Resolver;
use ella_passes::type_checker::TypeChecker;
use ella_vm::codegen::Codegen;
use ella_vm::vm::InterpretResult;

/// For testing purposes only.
pub fn interpret(source: &str) {
    let source = source.into();

    let builtin_vars = default_builtin_vars();

    let (mut resolve_result, mut type_check_result, mut vm) = builtin_initial_state(&builtin_vars);

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

    eprintln!("{}", source);
    assert!(source.has_no_errors());

    let mut codegen = Codegen::new("<global>".to_string(), &resolve_result, &type_check_result, &source);

    codegen.codegen_function(&ast);

    let chunk = codegen.into_inner_chunk();
    assert_eq!(vm.interpret(chunk), InterpretResult::Ok);
}
