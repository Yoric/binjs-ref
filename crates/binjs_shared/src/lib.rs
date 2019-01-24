#[macro_use]
extern crate downcast_rs;
extern crate itertools;
extern crate json;
#[macro_use]
extern crate log;
extern crate serde;
#[macro_use]
extern crate serde_derive;

mod json_conversion;
pub use json_conversion::*;

pub mod ast;
pub use ast::{ ReflectEnum, ReflectInterface, Node, ReflectType };

pub mod mru;
mod shared_string;
pub use shared_string::SharedString;

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Offset(pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VisitMe<T> {
    /// Visit the children of this node.
    ///
    /// The value is a guard
    HoldThis(T),

    /// Skip the children of this node, skip the `exit_` method, return immediately.
    DoneHere,
}

/// An identifier, inside the grammar.
shared_string!(pub IdentifierName);
pub type Identifier = IdentifierName;

/// A property, inside the grammar.
shared_string!(pub PropertyKey);

/// An interface *of* the grammar.
shared_string!(pub InterfaceName);

/// A field name *of* the grammar.
shared_string!(pub FieldName);

/// A container for f64 values that implements an *arbitrary*
/// total order, equality relation, hash.
#[derive(Clone, Debug, Copy, Deserialize, Serialize)]
pub struct F64(f64);
impl From<f64> for F64 {
    fn from(value: f64) -> F64 {
        F64(value)
    }
}
impl Into<f64> for F64 {
    fn into(self) -> f64 {
        self.0
    }
}
impl PartialOrd for F64 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.to_bits().cmp(&other.0.to_bits()))
    }
}
impl Ord for F64 {
    // An arbitrary total order on F64.
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl PartialEq for F64 {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}
impl Eq for F64 {} // Bitwise equality on F64.
impl std::hash::Hash for F64 {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.0.to_bits().hash(state)
    }
}
