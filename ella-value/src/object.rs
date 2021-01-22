//! Object (heap allocated) values.

use crate::chunk::Chunk;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::rc::Rc;

use super::Value;

/// Represents a native function (implemented in Rust).
#[derive(Clone)]
pub struct NativeFn {
    /// The identifier of the native function.
    pub ident: String,
    /// The number of arguments the native function accepts.
    pub arity: u32,
    /// A function pointer to the Rust implementation.
    /// The function accepts a `&mut [Value]` which is a slice into the VM's stack where the function arguments are stored.
    /// The function returns a [`Value`] which is the return value for the function.
    pub func: &'static dyn Fn(&mut [Value]) -> Value,
}

/// Represents a function. Functions are usually created at compile time and stored in the constant table.
#[derive(Clone)]
pub struct Function {
    /// The identifier of the function.
    pub ident: String,
    /// The number of arguments the function accepts.
    pub arity: u32,
    /// The chunk of the function.
    pub chunk: Rc<Chunk>,
    /// The number of upvalues this function captures.
    /// If the function does not capture any variable, this should be `0`.
    pub upvalues_count: usize,
}

/// Represents a closure. It is equivalent to a [`Function`] with additional captured variables.
/// Closures are always created at runtime via the `closure` instruction.
#[derive(Clone)]
pub struct Closure {
    /// The wrapped function.
    pub func: Function,
    /// Captured variables.
    pub upvalues: Rc<RefCell<Vec<Rc<RefCell<UpValue>>>>>,
}

/// Represents a captured variable.
/// The [`UpValue::Open`] state is used when the variable still lives on the stack.
/// The [`UpValue::Closed`] state is used when the scope is exited and the value is moved onto the heap.
#[derive(Debug, Clone)]
pub enum UpValue {
    Open(usize),
    Closed(Value),
}

impl UpValue {
    /// Returns `true` if the `UpValue` is in an open state ([`UpValue::Open`]).
    /// # Example
    /// ```
    /// use ella_value::object::UpValue;
    /// use ella_value::Value;
    ///
    /// let upvalue = UpValue::Open(10);
    /// assert!(upvalue.is_open());
    /// let upvalue = UpValue::Closed(Value::Bool(false));
    /// assert!(!upvalue.is_open());
    /// ```
    pub fn is_open(&self) -> bool {
        matches!(self, Self::Open(_))
    }

    /// Returns `true` if the `UpValue` is in an open state ([`UpValue::Open`]) and has the given `index`.
    /// # Example
    /// ```
    /// use ella_value::object::UpValue;
    /// use ella_value::Value;
    ///
    /// let upvalue = UpValue::Open(10);
    /// assert!(upvalue.is_open_with_index(10));
    /// assert!(!upvalue.is_open_with_index(11));
    /// let upvalue = UpValue::Closed(Value::Bool(false));
    /// assert!(!upvalue.is_open_with_index(10));
    /// ```
    pub fn is_open_with_index(&self, index: usize) -> bool {
        matches!(self, Self::Open(x) if *x == index)
    }
}

/// Inner representation for [`Obj`].
#[derive(Clone)]
pub enum ObjKind {
    Str(String),
    Fn(Function),
    Closure(Closure),
    NativeFn(NativeFn),
}

impl PartialEq for ObjKind {
    fn eq(&self, other: &ObjKind) -> bool {
        match self {
            Self::Str(l) => match other {
                Self::Str(r) => l == r,
                _ => false,
            },
            _ => false,
        }
    }
}

/// Represents a (heap allocated) object.
#[derive(Clone, PartialEq)]
pub struct Obj {
    /// Inner representation.
    pub kind: ObjKind,
}

impl Obj {
    /// Create a new heap allocated string ([`ObjKind::Str`]).
    pub fn new_string(str: String) -> Self {
        Self {
            kind: ObjKind::Str(str),
        }
    }
}

impl PartialOrd for Obj {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        None // object ordering is always false
    }
}

/// Set to `true` to print a debug message when objects (heap allocated) are dropped.
const LOG_OBJECT_DROP: bool = false;

/// `Drop` is implemented for `Obj` merely to ease gc debugging.
impl Drop for Obj {
    fn drop(&mut self) {
        if LOG_OBJECT_DROP {
            match &self.kind {
                ObjKind::Str(string) => eprintln!("Collecting object {:?}", string),
                ObjKind::Fn(Function { ident, .. }) => {
                    eprintln!("Collecting function object {:?}", ident)
                }
                ObjKind::Closure(Closure { func, .. }) => {
                    eprintln!("Collecting closure object {:?}", func.ident)
                }
                ObjKind::NativeFn(NativeFn { ident, .. }) => {
                    eprintln!("Collecting native function object {:?}", ident)
                }
            }
        }
    }
}
