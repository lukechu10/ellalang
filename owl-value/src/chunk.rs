//! Definitions for [`Chunk`] and [`OpCode`].

use std::collections::HashMap;

use crate::{Value, ValueArray};
use enum_primitive_derive::Primitive;

/// Represents an opcode. Internally represented using 1 byte (`u8`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Primitive)]
#[repr(u8)]
pub enum OpCode {
    /// Load a constant onto the stack.
    /// *2 bytes (1 operand)*
    Ldc = 0,
    /// Load a f64 onto the stack.
    /// *9 bytes (1 f64 le operand)*
    Ldf64 = 26,
    /// Load the constant 0 onto the stack.
    /// *1 byte*
    Ld0 = 27,
    /// Load the constant 1 onto the stack.
    /// *1 byte*
    Ld1 = 28,
    /// Load a local variable onto the stack.
    /// *2 bytes (1 operand)*
    LdLoc = 15,
    /// Stores the top value on the stack into a local variable.
    /// *2 bytes (1 operand)*
    StLoc = 16,
    /// Loads a global variable onto the stack.
    /// The operand is the absolute position of the variable on the stack.
    /// *2 bytes (1 operand)*
    LdGlobal = 24,
    /// Sets a global variable onto the stack.
    /// The operand is the absolute position of the variable on the stack.
    /// *2 bytes (1 operand)*
    StGlobal = 25,
    /// Loads an upvalue onto the stack.
    /// *2 bytes (1 operand)*
    LdUpVal = 17,
    /// Stores the top value on the stack into an upvalue.
    /// *2 bytes (1 operand)*
    StUpVal = 18,
    /// Closes an upvalue.
    /// *1 byte*
    CloseUpVal = 20,
    /// Negate the last value on the stack.
    /// *1 byte*
    Neg = 1,
    /// Logical not on a boolean value.
    /// *1 byte*
    Not = 2,
    Add = 3,
    Sub = 4,
    Mul = 5,
    Div = 6,
    /// Unchecked version of `add`.
    AddF64 = 100,
    /// Unchecked version of `sub`.
    SubF64 = 101,
    /// Unchecked version of `mul`.
    MulF64 = 102,
    /// Unchecked version of `div`.
    DivF64 = 103,
    /// Returns the last value on the stack.
    /// *1 byte*
    Ret = 7,
    /// Return the constant 0.
    /// *1 byte*
    Ret0 = 29,
    /// Return the constant 1.
    /// *1 byte*
    Ret1 = 30,
    /// Loads `true` onto the stack.
    /// *1 byte*
    LdTrue = 8,
    /// Loads `false` onto the stack.
    /// *1 byte*
    LdFalse = 9,
    Eq = 10,
    Greater = 11,
    Less = 12,
    /// Pops and disposes the last value on the stack.
    /// *1 byte*
    Pop = 13,
    /// Calls the function on the top of the stack.
    /// To load the function, use `ldc` to load a function object.
    /// Arity is the operand.
    /// *2 bytes (1 operand)*
    Calli = 14,
    /// Creates a closure with a constant function and pushes it onto the stack.
    /// *Variable number of operands*
    Closure = 19,
    /// Jump with the specified offset.
    /// **NOTE**: `jmp` cannot jump backwards. To jump backwards, use [`OpCode::Loop`].
    /// *2 bytes (1 u16 operand)*
    Jmp = 21,
    /// Jump with the specified offset if the last value on the stack is `true`.
    /// **NOTE**: This instruction does not pop the stack.
    /// *2 bytes (1 u16 operand)*
    JmpIfFalse = 22,
    /// Jump backwards with the specified offset.
    /// *2 bytes (1 u16 operand)*
    Loop = 23,
}

/// Represents a chunk of bytecode.
#[derive(Debug, Clone)]
pub struct Chunk {
    /// A [`Vec`] of [`OpCode`]s and operands.
    pub code: Vec<u8>, // a byte array
    /// Source code positions for each byte in `code`.
    pub lines: Vec<usize>,
    /// Constant table for this [`Chunk`].
    pub constants: ValueArray,
    /// The name of the chunk.
    /// For most cases, should be the name of the function.
    /// If the [`Chunk`] is the top-level chunk, the name should `<global>`.
    pub name: String,
    /// Per line debug annotations.
    pub(crate) debug_annotations: HashMap<usize, String>,
}

/// `u8` and `OpCode` should implement this trait.
pub trait ToByteCode {
    /// Transforms `self` into an `u8`.
    fn to_byte_code(&self) -> u8;
}

impl ToByteCode for OpCode {
    fn to_byte_code(&self) -> u8 {
        *self as u8
    }
}

impl ToByteCode for u8 {
    fn to_byte_code(&self) -> u8 {
        *self
    }
}

impl Chunk {
    /// Create an empty chunk with the specified `name`.
    ///
    /// # Example
    /// ```
    /// use owl_value::chunk::Chunk;
    /// let chunk = Chunk::new("my_chunk".to_string());
    /// assert_eq!(chunk.name, "my_chunk");
    /// ```
    pub fn new(name: String) -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: ValueArray::new(),
            name,
            debug_annotations: HashMap::new(),
        }
    }

    /// Write data to the [`Chunk`]. This can be an [`OpCode`] or an operand (`u8`).
    /// Returns the index of the added byte.
    ///
    /// # Params
    /// * `opcode` - The data to write to the chunk.
    /// * `line` - The original source line. This is used for runtime error messages and debugging.
    ///
    /// # Example
    /// ```
    /// use owl_value::chunk::{Chunk, OpCode};
    /// let mut chunk = Chunk::new("my_chunk".to_string());
    /// let index = chunk.write_chunk(OpCode::Ldc, 0);
    /// assert_eq!(index, 0);
    /// let index = chunk.write_chunk(1, 0);
    /// assert_eq!(index, 1);
    /// assert_eq!(chunk.code, vec![0, 1]);
    /// assert_eq!(chunk.lines, vec![0, 0]);
    /// ```
    pub fn write_chunk(&mut self, opcode: impl ToByteCode, line: usize) -> usize {
        debug_assert_eq!(self.code.len(), self.lines.len());
        self.code.push(opcode.to_byte_code());
        self.lines.push(line);
        debug_assert_eq!(self.code.len(), self.lines.len());
        self.code.len() - 1 // -1 to include the effect of adding the byte to self.code
    }

    /// Patches a `jmp` or `jmp_if_false` instruction to jump to current position.
    pub fn patch_jump(&mut self, offset: usize) {
        // -2 to adjust for the bytecode for the jump itself.
        let jump = self.code.len() - offset - 2;

        if jump > std::u16::MAX as usize {
            panic!("cannot jump more than std::u16::MAX");
        }

        self.code[offset] = ((jump >> 8) & 0xff) as u8;
        self.code[offset + 1] = (jump & 0xff) as u8;
    }

    /// Creates a `ldf64` instruction with the specified value.
    pub fn emit_ldf64(&mut self, value: f64, line: usize) {
        self.write_chunk(OpCode::Ldf64, line);
        let bytes = value.to_le_bytes();
        for byte in bytes.iter() {
            self.write_chunk(*byte, line);
        }
    }

    /// Add a constant to the constant table.
    /// Returns the index of the added constant.
    ///
    /// # Example
    /// ```
    /// use owl_value::chunk::Chunk;
    /// use owl_value::Value;
    /// let mut chunk = Chunk::new("my_chunk".to_string());
    /// let index = chunk.add_constant(Value::Bool(true));
    /// assert_eq!(index, 0);
    /// assert_eq!(chunk.constants, vec![Value::Bool(true)]);
    /// let index = chunk.add_constant(Value::Number(2.0));
    /// assert_eq!(index, 1);
    /// assert_eq!(chunk.constants, vec![Value::Bool(true), Value::Number(2.0)]);
    /// ```
    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        let loc = self.constants.len() - 1;
        if loc as u8 as usize != loc {
            todo!("load constant wide");
        }
        loc as u8
    }

    /// Adds a debug annotation (shown when disassembling) to the last byte in the chunk.
    /// This method should be called right after writing the [`OpCode`] and before writing any operands.
    ///
    /// **NOTE**: overrides any existing debug annotation.
    pub fn add_debug_annotation_at_last(&mut self, message: impl ToString) {
        self.debug_annotations
            .insert(self.code.len() - 1, message.to_string());
    }
}
