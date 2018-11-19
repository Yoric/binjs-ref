use std::fmt::*;

/// The path followed when walking an AST.
///
/// Designed to be used both to quickly find out how to contextually handle
/// a specific node and for error-reporting.
///
/// ```
/// use binjs_shared::ast::Path;
///
/// let mut path = Path::new();
/// assert!(path.get(0).is_none());
///
/// // Once we have entered both an interface and a field, `path.get(0)` will be `Some`.
/// path.enter_interface("Interface 1");
/// assert!(path.get(0).is_none());
///
/// path.enter_field("Field 1");
///
/// {
///   let item = path.get(0).unwrap();
///   assert_eq!(item.field, "Field 1");
///   assert_eq!(item.interface, "Interface 1");
/// }
///
/// path.enter_interface("Interface 2");
/// path.enter_field("Field 2");
///
/// {
///   let item = path.get(0).unwrap();
///   assert_eq!(item.field, "Field 2");
///   assert_eq!(item.interface, "Interface 2");
/// }
/// {
///   let item = path.get(1).unwrap();
///   assert_eq!(item.field, "Field 1");
///   assert_eq!(item.interface, "Interface 1");
/// }
///
/// // We need to exit the field before exiting the interface.
/// path.exit_field("Field 2"); // Exiting the wrong field would panic.
/// path.exit_interface("Interface 2"); // Exiting the wrong interface would panic.
/// path.exit_field("Field 1"); // Exiting the wrong field would panic.
/// path.exit_interface("Interface 1"); // Exiting the wrong interface would panic.
/// ```

#[derive(Clone, Default, Deserialize, Serialize)]
pub struct Path<I, F> where I: Debug, F: Debug {
    /// Some(foo) if we have entered interface foo but no field yet.
    /// Otherwise, None.
    interface: Option<I>,
    items: Vec<PathItem<I, F>>,
}
impl<I, F> From<Vec<PathItem<I, F>>> for Path<I, F> where I: Debug, F: Debug {
    fn from(items: Vec<PathItem<I, F>>) -> Self {
        Path {
            interface: None,
            items
        }
    }
}
impl<I, F> std::hash::Hash for Path<I, F> where I: Debug + std::hash::Hash, F: Debug + std::hash::Hash {
    /// As we implement Borrow<[PathItem<...>] for Path, we must ensure that `Hash`
    /// gives the same result for a `Path` and its `[PathItem]` representation.
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        self.items.as_slice()
            .hash(hasher)
    }
}
impl<I, F> std::cmp::PartialEq for Path<I, F> where I: Debug + PartialEq, F: Debug + PartialEq {
    /// As we implement Borrow<[PathItem<...>] for Path, we must ensure that `Eq`
    /// gives the same result for a `Eq` and its `[PathItem]` representation.
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
    }
}
impl<I, F> std::cmp::Eq for Path<I, F> where I: Debug + Eq, F: Debug + Eq {
    // Nothing to do.
}


#[derive(Debug, PartialEq, Eq, Hash, Clone, Deserialize, Serialize)]
pub struct PathItem<I, F> where I: Debug, F: Debug {
    pub interface: I,
    pub field: F,
}
impl<I, F> PathItem<I, F> where I: Debug, F: Debug {
    pub fn interface(&self) -> &I {
        &self.interface
    }
    pub fn field(&self) -> &F {
        &self.field
    }
}


impl<I, F> Debug for Path<I, F> where I: Debug, F: Debug {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use itertools::Itertools;
        write!(f, "[{items}{more}]",
            items = self.items.iter()
                .map(|item| format!("{:?}.{:?}", item.interface, item.field))
                .format(" > "),
            more = if let Some(ref interface) = self.interface {
                format!(" > {:?}", interface)
            } else {
                "".to_string()
            }
        )
    }
}
impl<I, F> Path<I, F> where I: Debug + PartialEq, F: Debug + PartialEq {
    /// Create an empty `Path`.
    pub fn new() -> Self {
        Self {
            interface: None,
            items: vec![],
        }
    }

    pub fn extend_from_slice(&mut self, slice: &[PathItem<I, F>]) where I: Clone, F: Clone {
        self.items
            .extend_from_slice(slice)
    }

    /// Create an empty `Path`, initialized to hold up
    /// to `capacity` elements without resize.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            interface: None,
            items: Vec::with_capacity(capacity)
        }
    }

    /// Enter an interface.
    ///
    /// All calls to `enter_interface` MUST be balanced with calls
    /// to `exit_interface`.
    pub fn enter_interface(&mut self, node: I) {
        debug!(target: "path", "enter_interface: {:?}", node);
        debug_assert!(self.interface.is_none(), "We shouldn't have a pending interface, got {:?}", self.interface);
        self.interface = Some(node);
    }
    pub fn exit_interface(&mut self, node: I) {
        debug!(target: "path", "exit_interface: {:?}", node);
        let interface = self.interface.take()
            .expect("Could not exit_interface if we're not in an interface");
        debug_assert!(node == interface);
    }
    pub fn enter_field(&mut self, field: F) {
        debug!(target: "path", "enter_field: {:?} at {:?}", field, self.interface);
        let interface = self.interface.take()
            .unwrap();
        self.items.push(PathItem {
            interface,
            field,
        });
    }
    pub fn exit_field(&mut self, field: F) {
        debug!(target: "path", "exit_field: {:?}", field);
        debug_assert!(self.interface.is_none(), "We shouldn't have a pending interface, got {:?}", self.interface);
        let PathItem {
            interface,
            field: prev
        } = self.items.pop()
            .expect("Could not exit_field from an empty ASTath");
        debug_assert!(prev == field);
        self.interface = Some(interface);
    }
    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn get(&self, index: usize) -> Option<&PathItem<I, F>> {
        if index >= self.len() {
            return None;
        }
        Some(&self.items[self.len() - index - 1])
    }

    /// Return the last `len` elements of the path, in
    /// the order in which they appear in the path
    /// (current element is last).
    ///
    /// If there are fewer than `len` elements, return
    /// as many elements as possible.
    pub fn tail(&self, len: usize) -> &[PathItem<I, F>] {
        if len < self.len() {
            &self.items[self.len() - len..]
        } else {
            &self.items
        }
    }

    /// Iter through the path, from the root to the current position.
    pub fn iter(&self) -> impl Iterator<Item = &PathItem<I, F>> {
        self.items.iter()
    }
}

impl<I, F> std::borrow::Borrow<[PathItem<I, F>]> for Path<I, F> where I: Debug + PartialEq, F: Debug + PartialEq {
    fn borrow(&self) -> &[PathItem<I, F>] {
        &self.items
    }
}