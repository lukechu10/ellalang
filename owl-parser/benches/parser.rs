use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use owl_parser::parser::Parser;

fn expr(source: &str) {
    let source = source.into();
    let _ast = Parser::new(&source).parse_expr();
    assert!(source.has_no_errors());
}

fn long_expr(c: &mut Criterion) {
    let mut group = c.benchmark_group("long-expr");

    let mut source = "1".to_string();
    for _i in 0..1000 {
        source.push_str(" + 1");
    }
    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function("long-expr", |b| b.iter(|| expr(&source)));
}

fn stress_precedence(c: &mut Criterion) {
    let mut group = c.benchmark_group("stress-precedence");

    let mut source = "1".to_string();
    for _i in 0..200 {
        source.push_str(" == 2 < 3 + 5 * 5");
    }
    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function("stress-precedence", |b| b.iter(|| expr(&source)));
}

criterion_group!(benches, long_expr, stress_precedence);
criterion_main!(benches);
