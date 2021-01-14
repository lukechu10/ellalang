//! Benchmark VM interpretation

use criterion::{criterion_group, criterion_main, Criterion};
use ella::builtin_functions::default_builtin_vars;
use ella_parser::parser::Parser;
use ella_passes::resolve::Resolver;
use ella_value::chunk::Chunk;
use ella_value::BuiltinVars;
use ella_vm::codegen::Codegen;
use ella_vm::vm::Vm;

fn codegen_str<'a>(source: &str, builtin_vars: &'a BuiltinVars) -> (Chunk, Vm<'a>) {
    let dummy_source = "".into();
    let mut resolver = Resolver::new(&dummy_source);
    resolver.resolve_builtin_vars(&builtin_vars);
    let mut resolve_result = resolver.resolve_result();
    let accessible_symbols = resolver.accessible_symbols();

    let mut vm = Vm::new(&builtin_vars);
    let mut codegen = Codegen::new("<global>".to_string(), resolve_result);
    codegen.codegen_builtin_vars(&builtin_vars);
    vm.interpret(codegen.into_inner_chunk()); // load built in functions into memory

    let source = source.into();
    let mut parser = Parser::new(&source);
    let ast = parser.parse_program();

    let mut resolver =
        Resolver::new_with_existing_accessible_symbols(&source, accessible_symbols.clone());
    resolver.resolve_top_level(&ast);
    resolve_result = resolver.resolve_result();

    eprintln!("{}", source.errors);
    assert!(source.has_no_errors());

    let mut codegen = Codegen::new("<global>".to_string(), resolve_result);

    codegen.codegen_function(&ast);

    (codegen.into_inner_chunk(), vm)
}

macro_rules! benchmark_source {
    ($c: expr, $name: expr, $source: expr) => {{
        let builtin_vars = default_builtin_vars();
        let (chunk, mut vm) = codegen_str($source, &builtin_vars);
        let initial_stack = vm.stack().clone();

        $c.bench_function($name, |b| {
            b.iter(|| {
                // reset stack
                vm.restore_stack(initial_stack.clone());
                vm.interpret(chunk.clone());
            });
        });
    }};
}

fn iteration(c: &mut Criterion) {
    benchmark_source!(
        c,
        "iteration",
        r#"
        let i = 1000000;
        while i > 0 {
            i -= 1;
        }"#
    );
}

fn function_call(c: &mut Criterion) {
    benchmark_source!(
        c,
        "function-call",
        r#"
        fn foo(x) { return x * 2; }
        foo(10);"#
    );
}

criterion_group!(benches, iteration, function_call);
criterion_main!(benches);
