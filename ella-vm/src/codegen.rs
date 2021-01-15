//! Lowers AST into a `Chunk` (bytecode).

use ella_parser::ast::ExprKind;
use ella_parser::{
    ast::{Expr, Stmt},
    lexer::Token,
    visitor::Visitor,
};
use ella_passes::resolve::{ResolveResult, Symbol};
use ella_value::chunk::{Chunk, OpCode};
use ella_value::object::{Function, Obj, ObjKind};
use ella_value::{BuiltinVars, Value};
use std::cell::RefCell;
use std::{collections::HashMap, rc::Rc};

const DUMP_CHUNK: bool = true;

/// Generate bytecode from an abstract syntax tree.
pub struct Codegen<'a> {
    chunk: Chunk,
    constant_strings: HashMap<String, Rc<Obj>>,
    resolve_result: ResolveResult<'a>,
    /// Every time a new scope is created, a new value is pushed onto the stack.
    /// This is to keep track of how many `pop` instructions to emit when exiting the scope.
    scope_stack: Vec<Vec<Rc<RefCell<Symbol>>>>,
}

impl<'a> Codegen<'a> {
    pub fn new(name: String, resolve_result: ResolveResult<'a>) -> Self {
        Self {
            chunk: Chunk::new(name),
            constant_strings: HashMap::new(),
            resolve_result,
            scope_stack: vec![Vec::new()],
        }
    }

    /// Consumes `self` and returns the generated [`Chunk`].
    #[must_use]
    pub fn into_inner_chunk(self) -> Chunk {
        self.chunk
    }

    /// Returns the chunk for the top-level function.
    /// Do not use [`Visitor::visit_stmt`] to codegen a function as it will create a separate [`Chunk`].
    /// To get the generated [`Chunk`], call [`Codegen::into_inner_chunk`].
    /// # Params
    /// * `func` - The function to codegen for.
    pub fn codegen_function(&mut self, func: &'a Stmt) {
        match func {
            Stmt::FnDeclaration { body, .. } => {
                for stmt in body {
                    self.visit_stmt(stmt);
                }
            }
            _ => panic!("func is not a Stmt::FnDeclaration"),
        }

        if DUMP_CHUNK {
            eprintln!("{}", self.chunk);
        }
    }

    pub fn codegen_builtin_vars(&mut self, builtin_vars: &BuiltinVars) {
        for (_ident, value) in &builtin_vars.values {
            let constant = self.chunk.add_constant(value.clone());
            self.chunk.write_chunk(OpCode::Ldc, 0);
            self.chunk.write_chunk(constant, 0);
        }
    }

    fn enter_scope(&mut self) {
        self.scope_stack.push(Vec::new());
    }

    fn add_symbol(&mut self, stmt: &Stmt) {
        let symbol = self.resolve_result.lookup_declaration(stmt).unwrap();
        self.scope_stack.last_mut().unwrap().push(Rc::clone(symbol));
    }

    fn exit_scope(&mut self) {
        let scope = self.scope_stack.pop().unwrap();
        for symbol in scope {
            match symbol.borrow().is_captured {
                true => {
                    self.chunk.write_chunk(OpCode::CloseUpVal, 0);
                }
                false => {
                    self.chunk.write_chunk(OpCode::Pop, 0);
                    self.chunk
                        .add_debug_annotation_at_last(format!("cleanup local variable"));
                }
            };
        }
    }

    /// Emits a placeholder jump.
    /// Returns the index of the start of the jump offset. This should be later patched using [`Chunk::patch_jump`].
    fn emit_jump(&mut self, instr: OpCode, line: usize) -> usize {
        self.chunk.write_chunk(instr, line);
        self.chunk.write_chunk(0xff, line); // placeholder
        self.chunk.write_chunk(0xff, line); // placeholder
        self.chunk.code.len() - 2
    }

    /// Emits a `loop` instruction.
    fn emit_loop(&mut self, instr: OpCode, loop_start: usize, line: usize) {
        let offset = self.chunk.code.len() - loop_start + 3;

        self.chunk.write_chunk(instr, line);
        self.chunk.write_chunk(((offset >> 8) & 0xff) as u8, line);
        self.chunk.write_chunk((offset & 0xff) as u8, line);
    }
}

impl<'a> Visitor<'a> for Codegen<'a> {
    fn visit_expr(&mut self, expr: &'a Expr) {
        // Do not use default walking logic.

        /// Generate codegen for shorthand assignments (e.g. `+=`).
        macro_rules! gen_op_assign {
            ($instr: expr, $lhs: expr, $rhs: expr, $line: expr) => {{
                let resolved_symbol = *self.resolve_result.lookup_identifier($lhs).unwrap();

                // load value
                if resolved_symbol.is_global {
                    self.chunk.write_chunk(OpCode::LdGlobal, $line);
                    self.chunk.write_chunk(resolved_symbol.offset as u8, $line);
                } else if resolved_symbol.is_upvalue {
                    self.chunk.write_chunk(OpCode::LdUpVal, $line);
                    self.chunk.write_chunk(resolved_symbol.offset as u8, $line);
                } else {
                    self.chunk.write_chunk(OpCode::LdLoc, $line);
                    self.chunk.write_chunk(resolved_symbol.offset as u8, $line);
                }

                self.visit_expr($rhs);
                self.chunk.write_chunk($instr, $line);

                // store value
                if resolved_symbol.is_global {
                    self.chunk.write_chunk(OpCode::StGlobal, $line);
                    self.chunk.write_chunk(resolved_symbol.offset as u8, $line);
                } else if resolved_symbol.is_upvalue {
                    self.chunk.write_chunk(OpCode::StUpVal, $line);
                    self.chunk.write_chunk(resolved_symbol.offset as u8, $line);
                } else {
                    self.chunk.write_chunk(OpCode::StLoc, $line);
                    self.chunk.write_chunk(resolved_symbol.offset as u8, $line);
                }

                self.chunk.write_chunk(OpCode::Pop, $line); // remove rhs
                self.chunk.write_chunk(OpCode::Pop, $line); // intentional 2nd pop

                // load value, result of op assign is new value
                if resolved_symbol.is_global {
                    self.chunk.write_chunk(OpCode::LdGlobal, $line);
                    self.chunk.write_chunk(resolved_symbol.offset as u8, $line);
                } else if resolved_symbol.is_upvalue {
                    self.chunk.write_chunk(OpCode::LdUpVal, $line);
                    self.chunk.write_chunk(resolved_symbol.offset as u8, $line);
                } else {
                    self.chunk.write_chunk(OpCode::LdLoc, $line);
                    self.chunk.write_chunk(resolved_symbol.offset as u8, $line);
                }
            }};
        }

        match &expr.kind {
            ExprKind::NumberLit(val) => {
                if *val == 0.0 {
                    self.chunk.write_chunk(OpCode::Ld0, 0);
                } else if *val == 1.0 {
                    self.chunk.write_chunk(OpCode::Ld1, 0);
                } else {
                    self.chunk.emit_ldf64(*val, 0);
                }
            }
            ExprKind::BoolLit(val) => {
                match val {
                    true => self.chunk.write_chunk(OpCode::LdTrue, 0),
                    false => self.chunk.write_chunk(OpCode::LdFalse, 0),
                };
            }
            ExprKind::StringLit(val) => {
                let obj = if let Some(obj) = self.constant_strings.get(val) {
                    // reuse same String
                    obj.clone()
                } else {
                    let obj = Rc::new(Obj::new_string(val.clone()));
                    self.constant_strings.insert(val.clone(), obj.clone());
                    obj
                };
                let constant = self.chunk.add_constant(Value::Object(obj));
                self.chunk.write_chunk(OpCode::Ldc, 0);
                self.chunk.write_chunk(constant, 0);
            }
            ExprKind::Identifier(ident) => {
                let resolved_symbol = *self.resolve_result.lookup_identifier(expr).unwrap();

                if resolved_symbol.is_global {
                    self.chunk.write_chunk(OpCode::LdGlobal, 0);
                    self.chunk
                        .add_debug_annotation_at_last(format!("load global variable {}", ident));
                    self.chunk.write_chunk(resolved_symbol.offset as u8, 0);
                } else if resolved_symbol.is_upvalue {
                    self.chunk.write_chunk(OpCode::LdUpVal, 0);
                    self.chunk
                        .add_debug_annotation_at_last(format!("load upvalue {}", ident));
                    self.chunk.write_chunk(resolved_symbol.offset as u8, 0);
                } else {
                    self.chunk.write_chunk(OpCode::LdLoc, 0);
                    self.chunk
                        .add_debug_annotation_at_last(format!("load local variable {}", ident));
                    self.chunk.write_chunk(resolved_symbol.offset as u8, 0);
                }
            }
            ExprKind::FnCall { callee, args } => {
                let arity = args.len() as u8;
                for arg in args {
                    self.visit_expr(arg);
                }
                self.visit_expr(callee);
                self.chunk.write_chunk(OpCode::Calli, 0);
                self.chunk.write_chunk(arity, 0);
            }
            ExprKind::Binary { lhs, op, rhs } => {
                match op {
                    Token::Equals
                    | Token::PlusEquals
                    | Token::MinusEquals
                    | Token::AsteriskEquals
                    | Token::SlashEquals => {
                        self.visit_expr(rhs); // do not codegen lhs
                    }
                    _ => {
                        self.visit_expr(lhs);
                        self.visit_expr(rhs);
                    }
                }
                match op {
                    Token::Plus => {
                        self.chunk.write_chunk(OpCode::Add, 0);
                    }
                    Token::Minus => {
                        self.chunk.write_chunk(OpCode::Sub, 0);
                    }
                    Token::Asterisk => {
                        self.chunk.write_chunk(OpCode::Mul, 0);
                    }
                    Token::Slash => {
                        self.chunk.write_chunk(OpCode::Div, 0);
                    }
                    Token::Equals => {
                        let resolved_symbol =
                            *self.resolve_result.lookup_identifier(lhs.as_ref()).unwrap();

                        if resolved_symbol.is_global {
                            self.chunk.write_chunk(OpCode::StGlobal, 0);
                            self.chunk.write_chunk(resolved_symbol.offset as u8, 0);
                        } else if resolved_symbol.is_upvalue {
                            self.chunk.write_chunk(OpCode::StUpVal, 0);
                            self.chunk.write_chunk(resolved_symbol.offset as u8, 0);
                        } else {
                            self.chunk.write_chunk(OpCode::StLoc, 0);
                            self.chunk.write_chunk(resolved_symbol.offset as u8, 0);
                        }
                    }
                    Token::PlusEquals => gen_op_assign!(OpCode::Add, lhs, rhs, 0),
                    Token::MinusEquals => gen_op_assign!(OpCode::Sub, lhs, rhs, 0),
                    Token::AsteriskEquals => gen_op_assign!(OpCode::Mul, lhs, rhs, 0),
                    Token::SlashEquals => gen_op_assign!(OpCode::Div, lhs, rhs, 0),
                    Token::EqualsEquals => {
                        self.chunk.write_chunk(OpCode::Eq, 0);
                    }
                    Token::NotEquals => {
                        self.chunk.write_chunk(OpCode::Eq, 0);
                        self.chunk.write_chunk(OpCode::Not, 0);
                    }
                    Token::LessThan => {
                        self.chunk.write_chunk(OpCode::Less, 0);
                    }
                    Token::LessThanEquals => {
                        // a <= b equivalent to !(a > b)
                        self.chunk.write_chunk(OpCode::Greater, 0);
                        self.chunk.write_chunk(OpCode::Not, 0);
                    }
                    Token::GreaterThan => {
                        self.chunk.write_chunk(OpCode::Greater, 0);
                    }
                    Token::GreaterThanEquals => {
                        // a >= b equivalent to !(a < b)
                        self.chunk.write_chunk(OpCode::Less, 0);
                        self.chunk.write_chunk(OpCode::Not, 0);
                    }
                    _ => unreachable!(),
                };
            }
            ExprKind::Unary { op, arg } => {
                self.visit_expr(arg);
                match op {
                    Token::LogicalNot => self.chunk.write_chunk(OpCode::Not, 0),
                    Token::Minus => self.chunk.write_chunk(OpCode::Neg, 0),
                    _ => unreachable!(),
                };
            }
            ExprKind::Error => unreachable!(),
        }
    }

    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        // Do not use default walking logic.

        match stmt {
            Stmt::LetDeclaration {
                ident: _,
                initializer,
            } => {
                self.visit_expr(initializer); // Push value of expression onto top of stack.
                self.add_symbol(stmt);
            }
            Stmt::FnDeclaration {
                ident,
                params,
                body: _, // Body is codegen in a new `Codegen` instance.
            } => {
                // NOTE: we don't need to create a new scope here because the VM automatically cleans up the created local variables.
                let ident = ident.clone();
                let arity = params.len() as u32;

                // Create a new `Codegen` instance, codegen the function, and add the chunk to the `ObjKind::Fn`.
                let fn_chunk = {
                    let mut cg = Codegen::new(ident.clone(), self.resolve_result);
                    cg.codegen_function(stmt);
                    cg.chunk
                };

                let symbol = self.resolve_result.lookup_declaration(stmt).unwrap();

                let func = Rc::new(Obj {
                    kind: ObjKind::Fn(Function {
                        ident,
                        arity,
                        chunk: fn_chunk,
                        upvalues_count: symbol.borrow().upvalues.len(),
                    }),
                });
                let constant = self.chunk.add_constant(Value::Object(func));
                self.chunk.write_chunk(OpCode::Closure, 0);
                self.chunk.write_chunk(constant, 0);

                for symbol in &symbol.borrow().upvalues {
                    self.chunk.write_chunk(symbol.is_local as u8, 0);
                    self.chunk.write_chunk(symbol.index as u8, 0);
                }
            }
            Stmt::Block(body) => {
                self.enter_scope();
                for stmt in body {
                    self.visit_stmt(stmt);
                }
                self.exit_scope();
            }
            Stmt::IfElseStmt {
                condition,
                if_block,
                else_block,
            } => {
                self.visit_expr(condition);
                self.chunk.add_debug_annotation_at_last("if condition");

                let then_jump = self.emit_jump(OpCode::JmpIfFalse, 0);
                self.chunk.write_chunk(OpCode::Pop, 0);

                self.enter_scope();
                for stmt in if_block {
                    self.visit_stmt(stmt);
                }
                self.exit_scope();

                if let Some(else_block) = else_block {
                    let else_jump = self.emit_jump(OpCode::Jmp, 0);

                    self.chunk.patch_jump(then_jump);
                    self.chunk.write_chunk(OpCode::Pop, 0);

                    self.enter_scope();
                    for stmt in else_block {
                        self.visit_stmt(stmt);
                    }
                    self.exit_scope();

                    self.chunk.patch_jump(else_jump);
                } else {
                    self.chunk.patch_jump(then_jump);
                    self.chunk.write_chunk(OpCode::Pop, 0);
                }
            }
            Stmt::WhileStmt { condition, body } => {
                let loop_start = self.chunk.code.len();
                self.visit_expr(condition);

                let exit_jump = self.emit_jump(OpCode::JmpIfFalse, 0);
                self.chunk.write_chunk(OpCode::Pop, 0);

                for stmt in body {
                    self.visit_stmt(stmt);
                }

                self.emit_loop(OpCode::Loop, loop_start, 0);

                self.chunk.patch_jump(exit_jump);
                self.chunk.write_chunk(OpCode::Pop, 0);
            }
            Stmt::ExprStmt(expr) => {
                self.visit_expr(expr);
                self.chunk.write_chunk(OpCode::Pop, 0);
            }
            Stmt::ReturnStmt(expr) => {
                if let Expr {
                    kind: ExprKind::NumberLit(number),
                    ..
                } = expr
                {
                    if *number == 0.0 {
                        self.chunk.write_chunk(OpCode::Ret0, 0);
                    } else if *number == 1.0 {
                        self.chunk.write_chunk(OpCode::Ret1, 0);
                    } else {
                        self.chunk.emit_ldf64(*number, 0);
                        self.chunk.write_chunk(OpCode::Ret, 0);
                    }
                } else {
                    self.visit_expr(expr);
                    self.chunk.write_chunk(OpCode::Ret, 0);
                }
            }
            Stmt::Error => unreachable!(),
        }
    }
}
