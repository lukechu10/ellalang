use ella_value::chunk::{Chunk, OpCode};
use ella_value::object::{Closure, Function, NativeFn, Obj, ObjKind, UpValue};
use ella_value::{BuiltinVars, Value, ValueArray};
use num_traits::FromPrimitive;

use std::cell::RefCell;
use std::rc::Rc;

const INSPECT_VM_STACK: bool = false;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InterpretResult {
    Ok,
    RuntimeError { message: String, line: usize },
}

#[derive(Clone)]
struct CallFrame {
    /// Instruction pointer.
    ip: usize,
    /// NOTE: not actually a pointer but rather an index to the start of the `CallFrame`.
    frame_pointer: usize,
    closure: Rc<Closure>,
}

pub struct Vm<'a> {
    /// VM stack.
    stack: ValueArray,
    call_stack: Vec<CallFrame>,
    builtin_vars: &'a BuiltinVars,
    upvalues: Vec<Rc<RefCell<UpValue>>>,
}

impl<'a> Vm<'a> {
    fn chunk(&self) -> &Chunk {
        &self.call_stack.last().unwrap().closure.func.chunk
    }

    fn code(&self) -> &[u8] {
        &self.call_stack.last().unwrap().closure.func.chunk.code
    }

    fn ip_mut(&mut self) -> &mut usize {
        &mut self.call_stack.last_mut().unwrap().ip
    }

    fn ip(&self) -> usize {
        self.call_stack.last().unwrap().ip
    }

    fn frame(&self) -> &CallFrame {
        self.call_stack.last().unwrap()
    }

    fn read_u8(&mut self) -> u8 {
        let byte: u8 = self.code()[self.ip()];
        *self.ip_mut() += 1;
        byte
    }

    fn read_u16(&mut self) -> u16 {
        let short: u16 = (self.code()[self.ip()] as u16) << 8 | self.code()[self.ip() + 1] as u16;
        *self.ip_mut() += 2;
        short
    }

    fn read_f64(&mut self) -> f64 {
        let mut bytes: [u8; 8] = [0; 8];
        bytes.copy_from_slice(&self.code()[self.ip()..self.ip() + 8]);

        let value = f64::from_le_bytes(bytes);
        *self.ip_mut() += 8;
        value
    }

    fn read_constant(&mut self) -> Value {
        let constant: Value = self.chunk().constants[self.code()[self.ip()] as usize].clone();
        *self.ip_mut() += 1;
        constant
    }

    fn cleanup_function(&mut self) {
        let return_value = self.stack.pop().unwrap();
        let frame = self.call_stack.pop().unwrap(); // remove a `CallFrame` from the call stack.

        for i in frame.frame_pointer..self.stack.len() {
            self.close_upvalues(i);
        }
        // cleanup local variables created in function
        self.stack.truncate(frame.frame_pointer);

        self.stack.push(return_value);
    }

    /// If inside a function, cleans up and returns `true`. Else returns `false` and does nothing.
    fn try_implicit_ret(&mut self) -> bool {
        if self.call_stack.len() > 1 {
            // inside a function
            self.stack.push(Value::Number(0.0)); // FIXME: returns 0.0 by default
            self.cleanup_function();
            true
        } else {
            self.call_stack.pop().unwrap();
            false
        }
    }

    fn resolve_upvalue_into_value(&self, upvalue: &UpValue) -> Value {
        match upvalue {
            UpValue::Open(index) => self.stack[*index].clone(),
            UpValue::Closed(value) => value.clone(),
        }
    }

    fn set_upvalue(&mut self, upvalue: Rc<RefCell<UpValue>>, new_value: Value) {
        let old_upvalue = upvalue.borrow().clone();
        match old_upvalue {
            UpValue::Open(index) => self.stack[index] = new_value,
            UpValue::Closed(ref _value) => {
                upvalue.replace(UpValue::Closed(new_value));
            }
        }
    }

    /// Uses the last value on the stack as the return value and cleans up the local variables created inside the function.
    fn close_upvalues(&mut self, index: usize) {
        let value = self.stack[index].clone();
        for upvalue in &self.upvalues {
            if upvalue.borrow().is_open_with_index(index) {
                upvalue.replace(UpValue::Closed(value.clone()));
            }
        }
    }

    fn find_open_upvalue_with_index(&self, index: usize) -> Option<Rc<RefCell<UpValue>>> {
        for upvalue in &self.upvalues {
            if upvalue.borrow().is_open_with_index(index) {
                return Some(upvalue.clone());
            }
        }
        None
    }

    fn runtime_error(&self, message: impl ToString) -> InterpretResult {
        InterpretResult::RuntimeError {
            message: message.to_string(),
            line: self.chunk().lines[self.ip() - 1], // -1 to get the last instruction
        }
    }

    fn run(&mut self) -> InterpretResult {
        /// Generate vm for binary operator.
        macro_rules! gen_num_binary_op {
            ($op: tt, $result: path) => {{
                let b: Value = self.stack.pop().unwrap();
                let a: Value = self.stack.pop().unwrap();

                let a = match a {
                    Value::Number(val) => val,
                    _ => return self.runtime_error("Operands must be numbers."),
                };

                let b = match b {
                    Value::Number(val) => val,
                    _ => return self.runtime_error("Operands must be numbers."),
                };

                self.stack.push($result(a $op b));
            }};

            ($op: tt) => {
                gen_num_binary_op!($op, Value::Number)
            }
        }

        macro_rules! gen_unchecked_num_binary_op {
            ($op: tt, $result: path) => {{
                let b: Value = self.stack.pop().unwrap();
                let a: Value = self.stack.pop().unwrap();

                let a = match a {
                    Value::Number(val) => val,
                    _ => unsafe { std::hint::unreachable_unchecked() },
                };

                let b = match b {
                    Value::Number(val) => val,
                    _ => unsafe {std::hint::unreachable_unchecked() },
                };

                self.stack.push($result(a $op b));
            }};

            ($op: tt) => {
                gen_unchecked_num_binary_op!($op, Value::Number)
            }
        }

        while self.ip() < self.code().len() || self.try_implicit_ret() {
            let opcode = self.read_u8();
            let opcode = OpCode::from_u8(opcode).expect("invalid opcode");
            match opcode {
                OpCode::Ldc => {
                    let constant = self.read_constant();
                    self.stack.push(constant);
                }
                OpCode::Ldf64 => {
                    let value = self.read_f64();
                    self.stack.push(Value::Number(value));
                }
                OpCode::Ld0 => self.stack.push(Value::Number(0.0)),
                OpCode::Ld1 => self.stack.push(Value::Number(1.0)),
                OpCode::LdLoc => {
                    let local_index = self.read_u8() as usize + self.frame().frame_pointer;
                    let local = self.stack[local_index].clone();
                    self.stack.push(local);
                }
                OpCode::StLoc => {
                    let local_index = self.read_u8() as usize + self.frame().frame_pointer;
                    let value = self.stack.last().unwrap().clone();
                    self.stack[local_index] = value;
                }
                OpCode::LdGlobal => {
                    let index = self.read_u8();
                    let local = self.stack[index as usize].clone();
                    self.stack.push(local);
                }
                OpCode::StGlobal => {
                    let index = self.read_u8();
                    let value = self.stack.last().unwrap().clone();
                    self.stack[index as usize] = value;
                }
                OpCode::LdUpVal => {
                    let index = self.read_u8();
                    let upvalue = self.call_stack.last().unwrap().closure.upvalues.borrow()
                        [index as usize]
                        .clone();
                    let value = self.resolve_upvalue_into_value(&upvalue.borrow());
                    self.stack.push(value);
                }
                OpCode::StUpVal => {
                    let index = self.read_u8();
                    let value = self.stack.last().unwrap().clone();
                    let upvalue = self.call_stack.last().unwrap().closure.upvalues.borrow()
                        [index as usize]
                        .clone();
                    self.set_upvalue(upvalue, value);
                }
                OpCode::CloseUpVal => {
                    let index = self.stack.len() - 1;
                    self.close_upvalues(index);
                    self.stack.pop().unwrap();
                }
                OpCode::Neg => {
                    let val = self.stack.pop().unwrap();
                    match val {
                        Value::Number(val) => self.stack.push(Value::Number(-val)),
                        _ => return self.runtime_error("Operand must be a number."),
                    }
                }
                OpCode::Not => {
                    let val = self.stack.pop().unwrap();
                    match val {
                        Value::Bool(val) => self.stack.push(Value::Bool(!val)),
                        _ => return self.runtime_error("Operand must be a boolean."),
                    }
                }
                OpCode::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    let a_num = a.cast_to_number();
                    let b_num = b.cast_to_number();
                    if let (Some(a_num), Some(b_num)) = (a_num, b_num) {
                        self.stack.push(Value::Number(a_num + b_num));
                    } else {
                        let a_str = a.cast_to_str();
                        let b_str = b.cast_to_str();
                        if a_str.is_some() && b_str.is_some() {
                            // handle string concatenation
                            let obj = Rc::new(Obj::new_string(format!(
                                "{}{}",
                                a_str.unwrap(),
                                b_str.unwrap()
                            )));
                            self.stack.push(Value::Object(obj));
                        } else {
                            return self.runtime_error(format!(
                                "Operands must be numbers or strings. Received {} and {}",
                                a, b
                            ));
                        }
                    }
                }
                OpCode::Sub => gen_num_binary_op!(-),
                OpCode::Mul => gen_num_binary_op!(*),
                OpCode::Div => gen_num_binary_op!(/),
                OpCode::AddF64 => gen_unchecked_num_binary_op!(+),
                OpCode::SubF64 => gen_unchecked_num_binary_op!(-),
                OpCode::MulF64 => gen_unchecked_num_binary_op!(*),
                OpCode::DivF64 => gen_unchecked_num_binary_op!(/),
                OpCode::Ret => {
                    if self.call_stack.len() <= 1 {
                        return self.runtime_error("Can only use return in a function.");
                    }
                    self.cleanup_function();
                }
                OpCode::Ret0 => {
                    self.stack.push(Value::Number(0.0));
                    if self.call_stack.len() <= 1 {
                        return self.runtime_error("Can only use return in a function.");
                    }
                    self.cleanup_function();
                }
                OpCode::Ret1 => {
                    self.stack.push(Value::Number(1.0));
                    if self.call_stack.len() <= 1 {
                        return self.runtime_error("Can only use return in a function.");
                    }
                    self.cleanup_function();
                }
                OpCode::LdTrue => self.stack.push(Value::Bool(true)),
                OpCode::LdFalse => self.stack.push(Value::Bool(false)),
                OpCode::Eq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(a == b));
                }
                OpCode::Greater => gen_num_binary_op!(>, Value::Bool),
                OpCode::Less => gen_num_binary_op!(<, Value::Bool),
                OpCode::Pop => {
                    self.stack.pop().unwrap(); // throw away result
                }
                OpCode::Calli => {
                    match self.stack.pop().unwrap() {
                        Value::Object(obj) => match &obj.kind {
                            ObjKind::Fn(_) => {
                                unreachable!("can not call ObjKind::Fn, wrap function in a ObjKind::Closure instead");
                            }
                            ObjKind::Closure(closure) => {
                                let calli_arity = self.read_u8();

                                if closure.func.arity != calli_arity as u32 {
                                    return self.runtime_error(format!(
                                        "Expected {} argument(s), received {}.",
                                        closure.func.arity, calli_arity
                                    ));
                                }

                                // add new `CallFrame` to call stack
                                self.call_stack.push(CallFrame {
                                    ip: 0,
                                    frame_pointer: self.stack.len() - closure.func.arity as usize,
                                    closure: Rc::new(closure.clone()),
                                });
                            }
                            ObjKind::NativeFn(NativeFn {
                                ident: _,
                                arity,
                                func,
                            }) => {
                                let calli_arity = self.read_u8();

                                if *arity != calli_arity as u32 {
                                    return self.runtime_error(format!(
                                        "Expected {} argument(s), received {}.",
                                        arity, calli_arity
                                    ));
                                }

                                let stack_len = self.stack.len();
                                let args = &mut self.stack[stack_len - *arity as usize..stack_len];
                                debug_assert_eq!(args.len(), *arity as usize);

                                let result = func(args);
                                // remove arguments from stack
                                for _i in 0..*arity {
                                    self.stack.pop().unwrap();
                                }
                                self.stack.push(result);
                            }
                            _ => return self.runtime_error("Value is not a function."),
                        },
                        _ => return self.runtime_error("Value is not a function."),
                    }
                }
                OpCode::Closure => {
                    let func = match self.read_constant() {
                        Value::Object(obj) => match &obj.kind {
                            ObjKind::Fn(function) => function.clone(),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    let upvalues_count = func.upvalues_count;
                    let closure = Closure {
                        func,
                        upvalues: Rc::new(RefCell::new(Vec::with_capacity(upvalues_count))),
                    };

                    for _i in 0..upvalues_count {
                        let is_local = self.read_u8() != 0;
                        let upvalue_index = self.read_u8();

                        let upvalue = if is_local {
                            match self.find_open_upvalue_with_index(upvalue_index as usize) {
                                Some(upvalue) => upvalue,
                                None => {
                                    let upvalue = Rc::new(RefCell::new(UpValue::Open(
                                        upvalue_index as usize,
                                    )));
                                    self.upvalues.push(upvalue.clone());
                                    upvalue
                                }
                            }
                        } else {
                            self.frame().closure.upvalues.borrow()[upvalue_index as usize].clone()
                        };

                        closure.upvalues.borrow_mut().push(upvalue);
                    }
                    debug_assert_eq!(closure.upvalues.borrow().len(), upvalues_count);

                    self.stack.push(Value::Object(Rc::new(Obj {
                        kind: ObjKind::Closure(closure),
                    })));
                }
                OpCode::Jmp => {
                    let offset = self.read_u16();
                    *self.ip_mut() += offset as usize;
                }
                OpCode::JmpIfFalse => {
                    let offset = self.read_u16();
                    if matches!(self.stack.last().unwrap(), Value::Bool(false)) {
                        *self.ip_mut() += offset as usize;
                    }
                }
                OpCode::Loop => {
                    let offset = self.read_u16();
                    *self.ip_mut() -= offset as usize;
                }
            }

            if INSPECT_VM_STACK {
                eprintln!(
                    "IP: {ip}, Chunk: {chunk}, VM stack: {stack:?}",
                    ip = self.ip(),
                    chunk = self.chunk().name,
                    stack = &self.stack[(self.builtin_vars.values.len()).min(self.stack.len())..] // do not show builtin vars in stack
                );
            }
        }

        InterpretResult::Ok
    }

    pub fn new(builtin_vars: &'a BuiltinVars) -> Self {
        Self {
            stack: Vec::new(),
            call_stack: Vec::new(),
            builtin_vars,
            upvalues: Vec::new(),
        }
    }

    /// Executes the chunk
    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        let func = Function {
            arity: 0,
            chunk: Rc::new(chunk),
            ident: "top".to_string(),
            upvalues_count: 0, // cannot have any upvalues for top-level function
        };
        let closure = Closure {
            func,
            upvalues: Rc::new(RefCell::new(Vec::new())),
        };
        self.call_stack = vec![CallFrame {
            ip: 0,            // start interpreting at first opcode
            frame_pointer: 0, // global frame_pointer points to start of stack
            closure: Rc::new(closure),
        }];

        self.run()
    }

    pub fn vm_state(&self) -> &ValueArray {
        &self.stack
    }

    pub fn restore_vm_state(&mut self, stack: ValueArray) {
        self.stack = stack;
    }
}
