//! Runtime value representation.
//!
//! Important types in this crate are:
//! * [`Value`] - Represents a variable value at runtime.
//! * [`Obj`] and [`ObjKind`] - Represents a heap allocated value at runtime.

pub mod chunk;
pub mod disassemble;
pub mod object;

use std::fmt;
use std::rc::Rc;

use object::{Closure, Function, NativeFn, Obj, ObjKind};

/// Symbols that are available globally.
#[derive(Default)]
pub struct BuiltinVars {
    pub values: Vec<(String, Value, UniqueType)>,
}

impl BuiltinVars {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a builtin symbol.
    pub fn add_value(&mut self, ident: String, value: Value, ty: UniqueType) {
        self.values.push((ident, value, ty));
    }

    /// Add a builtin native function. Alias for [`Self::add_value`] for simplifying [`ObjKind::NativeFn`] creation.
    pub fn add_native_fn(
        &mut self,
        ident: impl ToString,
        func: &'static dyn Fn(&mut [Value]) -> Value,
        arity: u32,
        ty: UniqueType,
    ) {
        let obj = Value::Object(Rc::new(Obj {
            kind: ObjKind::NativeFn(NativeFn {
                arity,
                func,
                ident: ident.to_string(),
            }),
        }));
        self.add_value(ident.to_string(), obj, ty);
    }
}

/// Represents a builtin type.
#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinType {
    Bool,
    Number,
    String,
    Fn {
        params: Vec<UniqueType>,
        ret: Box<UniqueType>,
    },
}

/// Represents an unique type.
#[derive(Debug, Clone, PartialEq)]
pub enum UniqueType {
    Builtin(BuiltinType),
    /// Runtime type.
    Any,
    /// Error case.
    /// Behaves like `Any` case during type checking to prevent cascading errors.
    Unknown,
}

impl UniqueType {
    /// # Example
    /// ```
    /// use ella_value::{BuiltinType, UniqueType};
    /// // Can always cast to same type (string -> string).
    /// assert!(UniqueType::from(BuiltinType::String).can_implicit_cast_to(&BuiltinType::String.into()));
    ///
    /// // Any and unknown can accept any type.
    /// assert!(UniqueType::from(BuiltinType::Number).can_implicit_cast_to(&UniqueType::Any));
    /// assert!(UniqueType::from(BuiltinType::Number).can_implicit_cast_to(&UniqueType::Unknown));
    /// ```
    pub fn can_implicit_cast_to(&self, other: &Self) -> bool {
        match other {
            Self::Builtin(_) => self == other,
            Self::Any => true,
            Self::Unknown => true,
        }
    }
}

impl From<BuiltinType> for UniqueType {
    fn from(ty: BuiltinType) -> Self {
        Self::Builtin(ty)
    }
}

/// Represents a runtime value. The [`Value::Object`] variant holds a [`Rc`] to the heap.
#[derive(Clone, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Object(Rc<object::Obj>),
}

impl Value {
    /// Attempts to cast the [`Value`] into a `&str` or `None` if wrong type.
    pub fn cast_to_str(&self) -> Option<&str> {
        match self {
            Self::Object(obj) => match &obj.kind {
                object::ObjKind::Str(string) => Some(&string),
                _ => None,
            },
            _ => None,
        }
    }

    /// Attempts to cast the [`Value`] into a `f64` or `None` if wrong type.
    pub fn cast_to_number(&self) -> Option<f64> {
        match self {
            Self::Number(val) => Some(*val),
            _ => None,
        }
    }

    /// Prints the object with the specified [`fmt::Formatter`].
    fn print_obj(f: &mut fmt::Formatter<'_>, obj: &object::Obj) -> fmt::Result {
        match &obj.kind {
            ObjKind::Str(str) => write!(f, "{}", str),
            ObjKind::Fn(Function { ident, .. }) => write!(f, "<fn {}>", ident),
            ObjKind::Closure(Closure { func, .. }) => write!(f, "<fn closure {}>", func.ident),
            ObjKind::NativeFn(object::NativeFn { ident, .. }) => write!(f, "<native fn {}>", ident),
        }
    }

    /// Dumps the object with the specified [`fmt::Formatter`]. Prints the string's escaped sequences (e.g. `'\n'`).
    fn print_dbg_obj(f: &mut fmt::Formatter<'_>, obj: &object::Obj) -> fmt::Result {
        match &obj.kind {
            ObjKind::Str(str) => write!(f, "{:?}", str),
            ObjKind::Fn(Function { ident, .. }) => write!(f, "<fn {}>", ident),
            ObjKind::Closure(Closure { func, .. }) => write!(f, "<fn closure {}>", func.ident),
            ObjKind::NativeFn(object::NativeFn { ident, .. }) => write!(f, "<native fn {}>", ident),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Object(val) => Self::print_obj(f, val),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Object(val) => Self::print_dbg_obj(f, val),
        }
    }
}

/// Type alias for `Vec<Value>`.
pub type ValueArray = Vec<Value>;
