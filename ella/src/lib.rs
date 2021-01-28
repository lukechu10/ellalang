pub mod builtin_functions;

use builtin_functions::default_builtin_vars;

use ella_parser::parser::Parser;
use ella_passes::resolve::Resolver;
use ella_passes::type_checker::TypeChecker;
use ella_source::Source;
use ella_vm::codegen::Codegen;
use ella_vm::vm::{InterpretResult, Vm};

/// For testing purposes only.
pub fn interpret(source: &str) {
    let source = source.into();

    let builtin_vars = default_builtin_vars();

    let dummy_source: Source = "".into();
    let mut resolver = Resolver::new(dummy_source.clone());
    resolver.resolve_builtin_vars(&builtin_vars);
    let mut resolve_result = resolver.into_resolve_result();

    let mut type_checker = TypeChecker::new(&resolve_result, dummy_source.clone());
    type_checker.type_check_builtin_vars(&builtin_vars);
    let mut typecheck_result = type_checker.into_type_check_result();

    let mut vm = Vm::new(&builtin_vars);
    let mut codegen = Codegen::new("<global>".to_string(), &resolve_result, &typecheck_result, &source);
    codegen.codegen_builtin_vars(&builtin_vars);
    vm.interpret(codegen.into_inner_chunk()); // load built in functions into memory

    let mut parser = Parser::new(&source);
    let ast = parser.parse_program();

    let mut resolver = Resolver::new_with_existing_resolve_result(source.clone(), resolve_result);
    resolver.resolve_top_level(&ast);
    resolve_result = resolver.into_resolve_result();

    let mut type_checker =
        TypeChecker::new_with_type_check_result(&resolve_result, source.clone(), typecheck_result);
    type_checker.type_check_global(&ast);
    typecheck_result = type_checker.into_type_check_result();
    let _ = typecheck_result;

    eprintln!("{}", source);
    assert!(source.has_no_errors());

    let mut codegen = Codegen::new("<global>".to_string(), &resolve_result, &typecheck_result, &source);

    codegen.codegen_function(&ast);

    let chunk = codegen.into_inner_chunk();
    assert_eq!(vm.interpret(chunk), InterpretResult::Ok);
}
