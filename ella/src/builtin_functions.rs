//! Implementations of builtin functions and symbols.

use std::rc::Rc;

use ella_value::object::{Obj, ObjKind};
use ella_value::{BuiltinVars, Value};

#[allow(dead_code)] // This appears to be a bug with rustc. These functions are used in both main.rs and lib.rs

/// Returns the default [`BuiltinVars`] that should be used.
pub fn default_builtin_vars() -> BuiltinVars {
    let mut builtin_vars = BuiltinVars::new();
    builtin_vars.add_native_fn("print", &print, 1);
    builtin_vars.add_native_fn("println", &println, 1);
    builtin_vars.add_native_fn("readln", &readln, 0);
    builtin_vars.add_native_fn("assert", &assert, 1);
    builtin_vars.add_native_fn("assert_eq", &assert_eq, 2);
    builtin_vars.add_native_fn("is_nan", &is_nan, 1);
    builtin_vars.add_native_fn("parse_number", &parse_number, 1);
    builtin_vars.add_native_fn("clock", &clock, 0);
    builtin_vars.add_native_fn("str", &str, 1);
    builtin_vars
}

pub fn print(args: &mut [Value]) -> Value {
    let arg = &args[0];
    print!("{}", arg);

    Value::Bool(true)
}

pub fn println(args: &mut [Value]) -> Value {
    let arg = &args[0];
    println!("{}", arg);

    Value::Bool(true)
}

pub fn readln(_args: &mut [Value]) -> Value {
    let mut input = String::new();
    let stdin = std::io::stdin();
    stdin.read_line(&mut input).expect("cannot read line");
    Value::Object(Rc::new(Obj::new_string(input)))
}

pub fn assert(args: &mut [Value]) -> Value {
    let arg = &args[0];

    match arg {
        Value::Bool(val) => assert!(*val),
        _ => {}
    }
    Value::Bool(true)
}

pub fn assert_eq(args: &mut [Value]) -> Value {
    let left = &args[0];
    let right = &args[1];

    assert_eq!(left, right);
    Value::Bool(true)
}

pub fn is_nan(args: &mut [Value]) -> Value {
    let number = &args[0];

    match number {
        Value::Number(number) if number.is_nan() => Value::Bool(true),
        _ => Value::Bool(false),
    }
}

pub fn parse_number(args: &mut [Value]) -> Value {
    let string = &args[0];

    match string {
        Value::Object(obj) => match &obj.kind {
            ObjKind::Str(string) => Value::Number(string.trim().parse().unwrap_or(f64::NAN)),
            _ => Value::Number(f64::NAN),
        },
        _ => Value::Number(f64::NAN),
    }
}

pub fn clock(_args: &mut [Value]) -> Value {
    let now = std::time::SystemTime::now();
    let since_the_epoch_secs = now
        .duration_since(std::time::UNIX_EPOCH)
        .expect("Time went backwards")
        .as_secs_f64();
    Value::Number(since_the_epoch_secs)
}

pub fn str(args: &mut [Value]) -> Value {
    let arg = &args[0];
    Value::Object(Rc::new(Obj::new_string(format!("{}", arg))))
}
