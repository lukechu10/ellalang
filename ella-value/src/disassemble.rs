//! [`Chunk`] disassembling support.

use crate::chunk::{Chunk, OpCode};
use crate::object::ObjKind;
use crate::Value;
use console::style;
use num_traits::FromPrimitive;
use std::fmt;

impl Chunk {
    /// Disassemble simple (1 byte) instruction.
    fn simple_instr(
        &self,
        f: &mut fmt::Formatter<'_>,
        name: &str,
        offset: usize,
        msg: &str,
    ) -> Result<usize, fmt::Error> {
        writeln!(f, "{} {}", name, msg)?;
        Ok(offset + 1)
    }

    /// Disassemble `ldc` (2 bytes) instruction.
    fn constant_instr(
        &self,
        f: &mut fmt::Formatter<'_>,
        name: &str,
        offset: usize,
        msg: &str,
    ) -> Result<usize, fmt::Error> {
        let constant_index = self.code[offset + 1];
        let constant = self.constants[constant_index as usize].clone();
        writeln!(
            f,
            "{:<10} {:<3} (value = {}) {}",
            name, constant_index, constant, msg
        )?;
        Ok(offset + 2)
    }

    /// Disassemble `ldloc`, `stloc`, `ldglobal`, `stglobal`, `ldupval` and `stupval` (2 bytes) instruction.
    fn ld_or_st_instr(
        &self,
        f: &mut fmt::Formatter<'_>,
        name: &str,
        offset: usize,
        msg: &str,
    ) -> Result<usize, fmt::Error> {
        let var_offset = self.code[offset + 1];
        writeln!(f, "{:<10} {} {}", name, var_offset, msg)?;
        Ok(offset + 2)
    }

    /// Disassemble `ldf64` (9 bytes) instruction.
    fn ldf64_instr(
        &self,
        f: &mut fmt::Formatter<'_>,
        name: &str,
        offset: usize,
        msg: &str,
    ) -> Result<usize, fmt::Error> {
        let mut bytes: [u8; 8] = [0; 8];
        bytes.copy_from_slice(&self.code[offset + 1..offset + 9]);
        let number = f64::from_le_bytes(bytes);

        writeln!(f, "{:<10} {} {}", name, number, msg)?;
        Ok(offset + 9)
    }

    /// Disassemble `calli` (2 bytes) instruction.
    fn calli_instr(
        &self,
        f: &mut fmt::Formatter<'_>,
        name: &str,
        offset: usize,
        msg: &str,
    ) -> Result<usize, fmt::Error> {
        let arity = self.code[offset + 1];
        writeln!(f, "{:<10} {} {}", name, arity, msg)?;
        Ok(offset + 2)
    }

    /// Disassemble `closure` (variable operands) instruction.
    fn closure_instr(
        &self,
        f: &mut fmt::Formatter<'_>,
        name: &str,
        mut offset: usize,
        msg: &str,
    ) -> Result<usize, fmt::Error> {
        let constant_index = self.code[offset + 1];
        let constant = self.constants[constant_index as usize].clone();
        writeln!(
            f,
            "{:<10} {:<3} (value = {}) {}",
            name, constant_index, constant, msg
        )?;
        offset += 2;

        if let Value::Object(obj) = constant {
            if let ObjKind::Fn(func) = &obj.kind {
                for _i in 0..func.upvalues_count {
                    let is_local = self.code[offset];
                    let index = self.code[offset + 1];
                    writeln!(
                        f,
                        "{:04} {:>4} `--{:<7}{:>2}",
                        style(offset).black().bright(),
                        "|",
                        if is_local != 0 { "local" } else { "upvalue" },
                        index
                    )?;
                    offset += 2;
                }
            } else {
                unreachable!();
            }
        } else {
            unreachable!()
        }

        Ok(offset)
    }

    /// Disassembles `jmp` and `jmp_if_false` and `loop` (3 bytes) instruction.
    fn jmp_instr(
        &self,
        f: &mut fmt::Formatter<'_>,
        name: &str,
        offset: usize,
        msg: &str,
    ) -> Result<usize, fmt::Error> {
        let jump_offset: u16 = (self.code[offset + 1] as u16) << 8 | self.code[offset + 2] as u16;
        writeln!(f, "{:<10} {} {}", name, jump_offset, msg)?;
        Ok(offset + 3)
    }

    /// Disassembles the instruction at the given `offset`.
    fn disassemble_instr(
        &self,
        f: &mut fmt::Formatter<'_>,
        offset: usize,
    ) -> Result<usize, fmt::Error> {
        write!(f, "{:04} ", style(offset).black().bright())?;

        let instr = self.code[offset];

        // Print source line number.
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            write!(f, "{:>4} ", "|")?;
        } else {
            write!(f, "{:>4} ", self.lines[offset] + 1)?; // +1 for 1-based line position
        }

        let blank_msg = String::new();
        let msg = &format!(
            "{}",
            style(
                self.debug_annotations
                    .get(&offset)
                    .map(|string| format!("// {}", string))
                    .unwrap_or(blank_msg),
            )
            .color256(29) // dark green
        );

        match OpCode::from_u8(instr) {
            Some(OpCode::Ldc) => self.constant_instr(f, "ldc", offset, msg),
            Some(OpCode::Ldf64) => self.ldf64_instr(f, "ldf64", offset, msg),
            Some(OpCode::Ld0) => self.simple_instr(f, "ld0", offset, msg),
            Some(OpCode::Ld1) => self.simple_instr(f, "ld1", offset, msg),
            Some(OpCode::LdLoc) => self.ld_or_st_instr(f, "ldloc", offset, msg),
            Some(OpCode::StLoc) => self.ld_or_st_instr(f, "stloc", offset, msg),
            Some(OpCode::LdGlobal) => self.ld_or_st_instr(f, "ldglobal", offset, msg),
            Some(OpCode::StGlobal) => self.ld_or_st_instr(f, "stglobal", offset, msg),
            Some(OpCode::LdUpVal) => self.ld_or_st_instr(f, "ldupval", offset, msg),
            Some(OpCode::StUpVal) => self.ld_or_st_instr(f, "stupval", offset, msg),
            Some(OpCode::CloseUpVal) => self.simple_instr(f, "closeupval", offset, msg),
            Some(OpCode::Neg) => self.simple_instr(f, "neg", offset, msg),
            Some(OpCode::Not) => self.simple_instr(f, "not", offset, msg),
            Some(OpCode::Add) => self.simple_instr(f, "add", offset, msg),
            Some(OpCode::Sub) => self.simple_instr(f, "sub", offset, msg),
            Some(OpCode::Mul) => self.simple_instr(f, "mul", offset, msg),
            Some(OpCode::Div) => self.simple_instr(f, "div", offset, msg),
            Some(OpCode::Ret) => self.simple_instr(f, "ret", offset, msg),
            Some(OpCode::Ret0) => self.simple_instr(f, "ret0", offset, msg),
            Some(OpCode::Ret1) => self.simple_instr(f, "ret1", offset, msg),
            Some(OpCode::LdTrue) => self.simple_instr(f, "ld_true", offset, msg),
            Some(OpCode::LdFalse) => self.simple_instr(f, "ld_false", offset, msg),
            Some(OpCode::Eq) => self.simple_instr(f, "eq", offset, msg),
            Some(OpCode::Greater) => self.simple_instr(f, "greater", offset, msg),
            Some(OpCode::Less) => self.simple_instr(f, "less", offset, msg),
            Some(OpCode::Pop) => self.simple_instr(f, "pop", offset, msg),
            Some(OpCode::Calli) => self.calli_instr(f, "calli", offset, msg),
            Some(OpCode::Closure) => self.closure_instr(f, "closure", offset, msg),
            Some(OpCode::Jmp) => self.jmp_instr(f, "jmp", offset, msg),
            Some(OpCode::JmpIfFalse) => self.jmp_instr(f, "jmp_if_false", offset, msg),
            Some(OpCode::Loop) => self.jmp_instr(f, "loop", offset, msg),
            None => self.simple_instr(f, "invalid", offset, msg), // skip bad instruction
        } // returns the next ip
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "== {} ==", self.name)?;

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instr(f, offset)?;
        }

        Ok(())
    }
}
