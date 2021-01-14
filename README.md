# Ella lang

Welcome to Ella lang! Ella lang is a computer programming language implemented in Rust.

## Language features

### Basics

Here is the most basic hello world program:
```
println("Hello World!");
```
Yep! That's it. Don't forget to add the semicolon (`;`) character at the end.

### Variables

One can also store values inside variables:
```
let name = "Ella";
println("Hello " + name + "!");
```
This should print `Hello Ella!` to the console.

Of course, other types exist as well.
```
let number = 1;
let float = 1.5;
let boolean = true; // or false
```

Expressions can also be assigned to variables
```
// same as let computed = 2;
let computed = 10 / 5;
```
or
```
let x = 10;
let y = x / 2; // should evaluate to 2
```

### Functions

Like almost every other programming language, Ella supports defining and calling functions (aka methods in some languages):
```
fn greet() {
    println("Hello World!");
}
greet(); // prints Hello World!
```
Functions can also take parameters:
```
fn greet(name) {
    println("Hello " + name + "!");
}
greet("Luke"); // prints Hello Luke!
```
Functions can return results:
```
fn double(x) {
    return x * 2;
}
```
Results are returned using a `return` statement.

### Expressions

As seen earlier, Ella includes expressions.

Expressions can include arithmetic operators with the appropriate precedence...
```
1 + 2 * 3 // parsed as 1 + (2 * 3)
```
(Note that addition operator, `+`, works both on numbers and on strings. On numbers, `+` is simply math addition; with strings, `+` performs string concatenation.)

reference variables...
```
foo + 10
```
include function calls (with arguments)...
```
double(10) + 2
```
and even reference functions (higher order functions)...
```
fn my_function() { ... }
my_function // reference to a function (not a function call)
```

### Higher order functions and closures

In Ella, functions are also variables. This means we can pass functions to other functions as arguments. Example:
```
// This function applies the function f on g
fn apply(f, x) {
    return f(x);
}
fn double(x) {
    return x * 2;
}
apply(double, 2); // should evaluate to 4
```

Closures are also supported. Example:
```
/// This function combines f and g into a single function
fn compose(f, g) {
    fn inner(x) {
        return f(g(x));
    }
    return inner;
}
fn add_one(x) { return x + 1; }
fn double(x) { return x * 2; }
let func = compose(add_one, double); // func adds one and than doubles the result
func(3); // evaluates to (3 + 1) * 2 = 8
```

### Control flow

Ella supports structured control flow via `if`/`else` and `while` (`for` is still being implemented).

Branching is achieved via `if` and `else`. The `else` block is optional.
```
if condition {
    // do something
} else {
    // do something else
}
```
Note that unlike other languages, the `if` and `else` blocks must be surrounded with `{` and `}` brackets. This prevents the infamous "goto fail" incident as well as not requiring parenthesis around the condition. It is more aesthetic and easier to differentiate between control flow statements and function calls.

Looping is achieved via the `while` statement.
```
while condition {
    // repeat something
}
```
If `condition` is false since the very beginning, the loop will never execute.

### Builtin functions

Ella includes some builtin functions that are defined in Rust:

* `print(x)` - Prints a value `x` to the console.
* `println(x)` - Prints a value `x` to the console followed by a new line (`\n` character).
* `readln()` - Reads a new line from stdin and returns a string.
* `assert(value)` - Asserts a certain condition is `true`. Uses Rust's `assert!` macro under the hood and will panic if fail.
* `assert_eq(value)` - Asserts two values are equal. Uses Rust's `assert_eq!` macro under the hood and will panic if fail.
* `is_nan(num)` - Returns `true` if the number is `NaN`. Returns `false` otherwise. Note that this is the only way to check if a number is `NaN`.
* `parse_number(str)` - Parses a string into a floating point number or `NaN` if invalid.
* `clock()` - Returns a floating point number representing the number of seconds since the Unix epoch. Useful for simple benchmarks.

This list of features is non exhaustive. More features are currently being implemented. Thanks for checking out this project!
