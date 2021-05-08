use ella::interpret;

#[test]
fn empty_program() {
    interpret("");
}

#[test]
#[should_panic]
fn smoke_assert() {
    interpret(
        r#"
        assert(false);"#,
    );
}

#[test]
#[should_panic]
fn smoke_assert_eq() {
    interpret(
        r#"
        assert_eq(1, 2);"#,
    );
}

#[test]
#[should_panic]
fn no_top_level_return() {
    interpret(
        r#"
return 0;"#,
    );
}

#[test]
#[should_panic]
fn no_bad_arity() {
    interpret(
        r#"
fn foo(x) {}
foo(1, 2);"#,
    );
}

#[test]
#[should_panic]
fn no_bad_arity_native() {
    interpret(
        r#"
assert_eq(true, true, true);"#,
    );
}

#[test]
#[should_panic]
fn no_top_level_return_2() {
    interpret(
        r#"
return "a string";"#,
    );
}

#[test]
#[should_panic]
fn no_not_function() {
    interpret(
        r#"
let not_a_function = true;
not_a_function();"#,
    );
}

mod variables {
    use super::*;

    #[test]
    fn variables() {
        interpret(
            r#"
let x = 1;
assert_eq(x, 1);
let y = x + 1;
assert_eq(y, 2);
assert_eq(y, x + 1);
x = 10;
assert_eq(x, 10);"#,
        );
    }

    #[test]
    fn local_variables() {
        interpret(
            r#"
{
    let local = 10;
    local = 20;
    assert_eq(local, 20);
}"#,
        );
    }

    #[test]
    #[should_panic]
    fn do_not_leak_local_from_scope() {
        interpret(
            r#"
{
    let local = 10;
}
local; // not in scope"#,
        );
    }

    #[test]
    fn assign_equality_result() {
        interpret(
            r#"
let x = 1;
let y = 2;
let is_eq = x == 2;
assert(!is_eq);"#,
        );
    }
}

#[test]
fn comments() {
    interpret(
        r#"
        let x = 1; // a comment
        assert_eq(x, 1);"#,
    );
}

#[test]
fn is_nan() {
    interpret(r#"assert(!is_nan(1));"#);
    interpret(r#"assert(is_nan(0/0));"#);
}

#[test]
fn parse_number() {
    interpret(r#"assert(!is_nan(parse_number("1")));"#);
    interpret(r#"assert(!is_nan(parse_number("1.2")));"#);
    interpret(r#"assert(is_nan(parse_number("not a number")));"#);
    interpret(r#"assert(is_nan(parse_number(1)));"#);
}

#[test]
fn op_assign() {
    interpret(
        r#"
        let x = 10;
        assert_eq(x, 10);

        assert_eq(x = 12, 12);// should be new value
        assert_eq(x += 10, 22);
        assert_eq(x -= 20, 2);
        assert_eq(x *= 2, 4);
        assert_eq(x /= 4, 1);
        assert_eq(x, 1);"#,
    );
}

mod strings {
    use super::*;

    #[test]
    fn string_literals() {
        interpret(
            r#"
let str = "Hello";
assert_eq(str, "Hello");"#,
        );
    }

    #[test]
    fn string_concatenation() {
        interpret(
            r#"
assert_eq("Hello " + "world!", "Hello world!");"#,
        );
    }

    #[test]
    #[should_panic]
    fn no_implicit_cast() {
        interpret(r#""a" + 1;"#);
    }
}

mod functions {
    use super::*;

    #[test]
    fn functions() {
        interpret(
            r#"
            fn foo() {
                return 1;
            }
            assert_eq(foo(), 1);"#,
        );
    }

    #[test]
    fn functions_with_params() {
        interpret(
            r#"
            fn double(x) {
                let result = x * 2;
                return result;
            }
            assert_eq(double(10), 20);
            assert_eq(double(-2), -4);"#,
        );
    }

    #[test]
    fn functions_implicit_return() {
        interpret(
            r#"
            fn foo() { }
            assert_eq(foo(), 0);"#,
        );
    }

    #[test]
    fn higher_order_function() {
        interpret(
            r#"
            fn twice(f, v) {
                return f(f(v));
            }
            fn double(x) {
                return x * 2;
            }
            
            assert_eq(twice(double, 10), 40);
            assert_eq(twice(double, -2), -8);"#,
        );
    }

    mod closures {
        use super::*;

        #[test]
        fn close_global_variable() {
            interpret(
                r#"
                fn assert_test() {
                    assert(true); // captures assert fn from global scope
                }
                assert_test();"#,
            );
        }

        #[test]
        fn basic_closures() {
            interpret(
                r#"
                fn closure() {
                    let x = 1;
                    fn inner() {
                        return x + 1;
                    }
                    return inner();
                }
                assert_eq(closure(), 2);"#,
            );
        }

        #[test]
        fn capture_with_block_stmt() {
            interpret(
                r#"
                {
                    assert(true);
                }"#,
            )
        }

        #[test]
        fn close_upvalues() {
            interpret(
                r#"
                fn createAdder(x) {
                    fn adder(y) {
                        return x + y;
                    }
                    return adder;
                }
                let addTwo = createAdder(2);
                assert_eq(addTwo(1), 3);"#,
            );
            interpret(
                r#"
                fn compose(f, g) {
                    fn inner(x) {
                        return f(g(x));
                    }
                    return inner;
                }
                fn addOne(x) { return x + 1; }
                fn addTwo(x) { return x + 2; }
                let addThree = compose(addOne, addTwo);
                assert_eq(addThree(2), 5);"#,
            );
        }

        #[test]
        fn capture_by_ref() {
            interpret(
                r#"
                fn dummy() {}
                let globalSet = dummy; // dummy function to prevent type errors
                let globalGet = dummy;

                fn main() {
                    let a = "initial";

                    fn set() { a = "updated"; }
                    fn get() { return a; }

                    globalSet = set;
                    globalGet = get;
                }

                main();
                assert_eq(globalGet(), "initial");
                globalSet();
                assert_eq(globalGet(), "updated");"#,
            );
        }

        #[test]
        fn nested_upvalues() {
            interpret(
                r#"
                fn outer() {
                    let x = "value";

                    fn middle() {
                        fn inner() {
                            return x;
                        }
                        return inner;
                    }
                    return middle;
                }

                let mid = outer();
                let in = mid();
                assert_eq(in(), "value");"#,
            );
        }
    }

    mod lambda_expr {
        use super::*;

        #[test]
        fn basic_lambda() {
            interpret(
                r#"
                let f = fn() { return 2; };
                assert_eq(f(), 2);"#,
            );
        }

        #[test]
        fn basic_lambda_with_param() {
            interpret(
                r#"
                let square = fn(x) { return x * x; };
                assert_eq(square(10), 100);"#,
            );
        }

        #[test]
        fn lambda_closure() {
            interpret(
                r#"
                fn create_closure(x) {
                    return fn() { return x * x; };
                }
                let f = create_closure(10);
                assert_eq(f(), 100);"#,
            );
        }

        /// IIFE: Immediately invoked function expression
        #[test]
        #[should_panic]
        fn lambda_iife() {
            interpret(
                r#"
                (fn() {
                    assert(false); // should be called and panic
                })();"#,
            );
        }
    }
}

mod control_flow {
    use super::*;

    #[test]
    fn r#if() {
        interpret(
            r#"
let x = 0;
let condition = true;
if condition {
    x = 1;
}
assert_eq(x, 1);

condition = false;
if condition {
    x = 2;
}
assert_eq(x, 1);"#,
        );
    }

    #[test]
    fn if_and_else() {
        interpret(
            r#"
            let x = 0;

            let condition = true;
            if condition {
                x = 1;
            } else {
                x = 2;
            }
            assert_eq(x, 1);

            condition = false;
            if condition {
                x = 1;
            } else {
                x = 2;
            }
            assert_eq(x, 2);"#,
        );
    }

    #[test]
    fn fibonacci() {
        interpret(
            r#"
            fn fib(x) {
                if x <= 1 { return 1; }
                else { return fib(x - 1) + fib(x - 2); }
            }
            
            assert_eq(fib(20), 10946);"#,
        );
    }

    #[test]
    fn accumulate() {
        interpret(
            r#"
            let x = 0;
            let i = 0;
            while i < 20 {
                x = x + i;
                i = i + 1;
            }
            assert_eq(x, 190);"#,
        );
    }
}

/// Test cases designed to trigger to type checker.
mod type_errors {
    use super::*;

    #[test]
    #[should_panic]
    fn wrong_type_in_assignment() {
        interpret(
            r#"
let x = 0;
x = true; // error"#,
        );
    }

    #[test]
    #[should_panic]
    fn value_not_a_function() {
        interpret(
            r#"
let x = 0;
x(); // error"#,
        );
    }

    #[test]
    #[should_panic]
    fn wrong_number_of_arguments() {
        interpret(
            r#"
fn foo(x, y, z) {}
foo(1, 2); // error"#,
        );
    }

    #[test]
    #[should_panic]
    fn wrong_type_in_argument() {
        interpret(
            r#"
assert(1); // error, assert takes a bool"#,
        );
    }

    #[test]
    #[should_panic]
    fn add() {
        interpret(
            r#"
1 + "a"; // error, cannot add string to number"#,
        );
    }

    #[test]
    #[should_panic]
    fn add2() {
        interpret(
            r#"
"a" + 1; // error, cannot add number to string"#,
        );
    }

    #[test]
    #[should_panic]
    fn neg() {
        interpret(
            r#"
"a" - 1; // error"#,
        );
    }

    #[test]
    #[should_panic]
    fn mul() {
        interpret(
            r#"
"a" * 1; // error"#,
        );
    }

    #[test]
    #[should_panic]
    fn div() {
        interpret(
            r#"
"a" / 1; // error"#,
        );
    }

    #[test]
    #[should_panic]
    fn cmp() {
        interpret(
            r#"
"1" == 1; // error"#,
        );
    }

    #[test]
    #[should_panic]
    fn logical_not() {
        interpret(
            r#"
!1; // error, should be bool"#,
        );
    }

    #[test]
    #[should_panic]
    fn unary_minus() {
        interpret(
            r#"
-true; // error, should be number"#,
        );
    }

    #[test]
    #[should_panic]
    fn unknown_type() {
        interpret(
            r#"
let x: Foo = 1; // error, Foo unknown"#,
        );
    }

    #[test]
    #[should_panic]
    fn wrong_type_for_initializer() {
        interpret(
            r#"
let x: bool = 1; // error, expected bool"#,
        );
    }

    #[test]
    #[should_panic]
    fn wrong_type_for_initializer2() {
        interpret(
            r#"
let x = 1; // infer to be number
let y: bool = x; // error, expected bool"#,
        );
    }
}
