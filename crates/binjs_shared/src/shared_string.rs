use std;
use std::hash::Hash;
use std::ops::Deref;
use std::rc::Rc;

use serde::de::{ Deserialize, Deserializer };
use serde::ser::{ Serialize, Serializer };

/// An implementation of strings that may easily be shared without copies.
///
/// Static strings may be imported without copy, while dynamic strings
/// are converted into `Rc`.
#[derive(Clone, Debug, Eq, Ord)]
pub enum SharedString {
    Dynamic(Rc<String>),
    Static(&'static str)
}
impl Deref for SharedString {
    type Target = str;
    fn deref(&self) -> &str {
        match *self {
            SharedString::Static(ref s) => *s,
            SharedString::Dynamic(ref rc) => rc.deref()
        }
    }
}
impl PartialEq for SharedString {
    fn eq(&self, other: &SharedString) -> bool {
        self.deref() == other.deref()
    }
}
impl PartialEq<str> for SharedString {
    fn eq(&self, other: &str) -> bool {
        self.deref().eq(other)
    }
}
impl<'a> PartialEq<&'a str> for SharedString {
    fn eq(&self, other: &&'a str) -> bool {
        self.deref().eq(*other)
    }
}
impl PartialOrd for SharedString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.deref().partial_cmp(other.deref())
    }
}
impl Hash for SharedString {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        self.deref().hash(hasher)
    }
}
impl std::fmt::Display for SharedString {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        self.deref().fmt(formatter)
    }
}
impl Default for SharedString {
    fn default() -> Self {
        SharedString::Static("<uninitialized SharedString>")
    }
}
/// Shared strings are serialized as strings. This loses any sharing :/
impl Serialize for SharedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer
    {
        self.deref().serialize(serializer)
    }
}
/// Shared strings are deserialized as Dynamic strings.
impl<'de> Deserialize<'de> for SharedString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>
    {
        let dynamic = String::deserialize(deserializer)?;
        Ok(SharedString::Dynamic(Rc::new(dynamic)))
    }
}
impl SharedString {
    pub fn as_str(&self) -> &str {
        self.deref()
    }
    pub fn from_str(value: &'static str) -> Self {
        SharedString::Static(value)
    }
    pub fn from_rc_string(value: Rc<String>) -> Self {
        SharedString::Dynamic(value)
    }
    pub fn from_string(value: String) -> Self {
        SharedString::Dynamic(Rc::new(value))
    }
}
impl std::borrow::Borrow<str> for SharedString
{
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

#[macro_export]
macro_rules! shared_string {
    (pub $name: ident) => {
        #[derive(Clone, Eq, PartialOrd, Ord, Debug, Hash, Serialize, Deserialize)]
        pub struct $name(pub shared_string::SharedString);
        impl $name {
            pub fn from_str(value: &'static str) -> Self {
                $name(shared_string::SharedString::from_str(value))
            }
            pub fn from_string(value: String) -> Self {
                $name(shared_string::SharedString::from_string(value))
            }
            pub fn from_rc_string(value: std::rc::Rc<String>) -> Self {
                $name(shared_string::SharedString::from_rc_string(value))
            }
            pub fn as_str(&self) -> &str {
                self.0.as_str()
            }
            pub fn as_shared_string(&self) -> &shared_string::SharedString {
                &self.0
            }
        }
        impl PartialEq for $name {
            fn eq(&self, other: &$name) -> bool {
                self.0.eq(&other.0)
            }
        }
        impl<'a> PartialEq<&'a str> for $name {
            fn eq(&self, other: &&'a str) -> bool {
                self.0.eq(other)
            }
        }
        impl Default for $name {
            fn default() -> Self {
                Self::from_str("<uninitialized SharedString>")
            }
        }
    }
}