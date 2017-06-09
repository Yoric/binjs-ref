//! Grammars for specifying an AST that this tool can manipulate.
//!
//! This abstracts away the concepts of [ESTree](https://github.com/estree/estree).

#![allow(dead_code, unused)]

use std;
use std::cell::*;
use std::collections::{ HashMap, HashSet };
use std::hash::*;
use std::ops::Deref;
use std::rc::*;


#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct NodeName(Rc<String>);
impl NodeName {
    pub fn to_string(&self) -> &String {
        self.0.as_ref()
    }
    pub fn to_str(&self) -> &str {
        &self.0
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Kind(Rc<String>);
impl Kind {
    pub fn to_string(&self) -> &String {
        self.0.as_ref()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct FieldName(Rc<String>);
impl FieldName {
    pub fn to_string(&self) -> &String {
        self.0.as_ref()
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Field {
    name: FieldName,
    type_: Type,
}
impl Hash for Field {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.name.hash(state)
    }
}
impl Field {
    pub fn new(name: FieldName, type_: Type) -> Self {
        Field {
            name,
            type_
        }
    }
    pub fn name(&self) -> &FieldName {
        &self.name
    }
    pub fn type_(&self) -> &Type {
        &self.type_
    }
}

/// A type, typically that of a field.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /// An array of values of the same type.
    Array(Box<Type>),

    /// An object.
    Obj(Obj),

    /// A choice between several literals, e.g. `"get" | "set"`.
    Enum(NodeName),

    /// A value that may belong to one or more interfaces.
    ///
    /// Note that sum types between primitive types MUST be upgraded
    /// to sum types between their corresponding interfaces, e.g.
    /// `boolean | string | null` will be represented as
    /// `Boolean | String | Null`
    Interfaces {
        names: Vec<NodeName>,
        or_null: bool
    },

    // Primitive types
    Boolean,
    String,
    Number,
}

impl Type {
    /// Shorthand constructor.
    pub fn interface(name: &NodeName) -> Self {
        Type::Interfaces {
            names: vec![name.clone()],
            or_null: false
        }
    }
    pub fn enumeration(name: &NodeName) -> Self {
        Type::Enum(name.clone())
    }
    pub fn interfaces(names: &[&NodeName]) -> Self {
        Type::Interfaces {
            names: names.iter().cloned().cloned().collect(),
            or_null: false
        }
    }
    pub fn array(self) -> Self {
        Type::Array(Box::new(self))
    }
    pub fn or_null(self) -> Option<Self> {
        match self {
            Type::Interfaces {
                names,
                or_null
            } => {
                Some(Type::Interfaces {
                    names,
                    or_null: true
                })
            },
            _ => None
        }
    }
}

/// Obj of an object-like value.
#[derive(Clone, Debug)]
pub struct Obj {
    fields: Vec<Field>,
}
impl PartialEq for Obj {
    fn eq(&self, other: &Self) -> bool {
        let me : HashSet<_> = self.fields.iter().collect();
        let other : HashSet<_> = other.fields.iter().collect();
        me == other
    }
}
impl Eq for Obj {}

impl Obj {
    /// Create a new empty structure
    pub fn new() -> Self {
        Obj {
            fields: Vec::new()
        }
    }
    /// A list of the fields in the structure.
    pub fn fields<'a>(&'a self) -> &'a [Field] {
        &self.fields
    }
    /// Fetch a specific field in the structure
    pub fn field<'a>(&'a self, name: &FieldName) -> Option<&'a Field> {
        self.fields.iter().find(|field| &field.name == name)
    }

    pub fn with_own_field(self, field: Field) -> Self {
        if self.field(field.name()).is_some() {
            println!("Field: attempting to overwrite {:?}", field.name());
            panic!();
            return self
        }
        let mut fields = self.fields;
        fields.push(field);
        Obj {
            fields
        }
    }

    /// Extend a structure with a field.
    pub fn with_field(self, name: &FieldName, type_: Type) -> Self {
        if self.field(name).is_some() {
            println!("Field: attempting to overwrite {:?}", name);
            panic!();
            return self
        }
        let mut fields = self.fields;
        fields.push(Field {
            name: name.clone(),
            type_
        });
        Obj {
            fields
        }
    }
}

/// Structure of an enum of strings.
#[derive(Clone, Debug)]
pub struct Enum {
    /// Unordered list of strings, without duplicates.
    strings: Vec<String>,
}
impl Default for Enum {
    fn default() -> Self {
        Enum {
            strings: Vec::new(),
        }
    }
}
impl Enum {
    pub fn strings(&self) -> &[String] {
        &self.strings
    }

    /// Add a string to the enum. Idempotent.
    pub fn with_string(&mut self, string: &str) -> &mut Self {
        let string = string.to_string();
        if self.strings.iter().find(|x| **x == string).is_none() {
            self.strings.push(string.to_string())
        }
        self
    }
    /// Add several enums to the list. Idempotent.
    pub fn with_strings(&mut self, strings: &[&str]) -> &mut Self {
        for string in strings {
            self.with_string(string);
        }
        self
    }
}

#[derive(Clone, Debug)]
pub struct Interface {
    /// The name of the interface, e.g. `Node`.
    name: NodeName,

    /// The kind used to differentiate node that inhabit this
    /// interface from nodes inhabiting other interfaces.
    ///
    /// May be `None` for interfaces such as `Node` or `Expression`
    /// that serve only as a common ancestor for a sum of refined sub-interfaces
    /// and have no inhabitants of their own.
    kind: Option<Kind>,

    /// The parents of this interface.
    parent_interfaces: Vec<NodeName>,

    /// The contents of this interface, excluding the contents of parent interfaces.
    own_contents: Obj,
}

impl Interface {
    pub fn with_field(&mut self, name: &FieldName, type_: Type) -> &mut Self {
        // FIXME: There must be a better way to do this.
        let mut contents = Obj::new();
        std::mem::swap(&mut self.own_contents, &mut contents);
        self.own_contents = contents.with_field(name, type_);
        self
    }
    pub fn with_own_field(&mut self, field: Field) -> &mut Self {
        // FIXME: There must be a better way to do this.
        let mut contents = Obj::new();
        std::mem::swap(&mut self.own_contents, &mut contents);
        self.own_contents = contents.with_own_field(field);
        self
    }
    pub fn with_parent(&mut self, parent: &NodeName) -> &mut Self {
        if self.parent_interfaces.iter().find(|x| *x == parent).is_none() {
            self.parent_interfaces.push(parent.clone())
        }
        self
    }
}

/// A data structure used to progressively construct the `Syntax`.
pub struct SyntaxBuilder {
    /// All the interfaces entered so far.
    interfaces: HashMap<NodeName, RefCell<Interface>>,

    /// All the enums entered so far.
    enums: HashMap<NodeName, RefCell<Enum>>,

    names: HashMap<String, Rc<String>>,
}

impl SyntaxBuilder {
    pub fn new() -> Self {
        SyntaxBuilder {
            interfaces: HashMap::new(),
            enums: HashMap::new(),
            names: HashMap::new()
        }
    }

    /// Return an `NodeName` for a name. Equality comparison
    /// on `NodeName` can be performed by checking physical
    /// equality.
    pub fn node_name(&mut self, name: &str) -> NodeName {
        if let Some(result) = self.names.get(name) {
            return NodeName(result.clone())
        }
        let shared = Rc::new(name.to_string());
        let result = NodeName(shared.clone());
        self.names.insert(name.to_string(), shared);
        result
    }

    pub fn field_name(&mut self, name: &str) -> FieldName {
        if let Some(result) = self.names.get(name) {
            return FieldName(result.clone());
        }
        let shared = Rc::new(name.to_string());
        let result = FieldName(shared.clone());
        self.names.insert(name.to_string(), shared);
        result
    }

    /// Add an interface with a `kind` identical to its name.
    pub fn add_kinded_interface(&mut self, name: &NodeName) -> Option<RefMut<Interface>> {
        let kind = Kind(name.0.clone());
        let result = self.add_virtual_interface(name)
            .map(|mut result| {result.kind = Some(kind); result});
        result
    }

    /// Add a virtual interface, i.e. one that doesn't have a `kind`,
    /// i.e. one that does not have immediate inhabitants. Super-interfaces
    /// or sub-interfaces with a `kind` may have inhabitants.
    pub fn add_virtual_interface(&mut self, name: &NodeName) -> Option<RefMut<Interface>> {
        if self.interfaces.get(name).is_some() {
            return None;
        }
        let interface = Interface {
            name: name.clone(),
            kind: None,
            own_contents: Obj::new(),
            parent_interfaces: Vec::new(),
        };
        self.interfaces.insert(name.clone(), RefCell::new(interface));
        self.interfaces.get(name).map(RefCell::borrow_mut)
    }

    /// Add a named enumeration.
    pub fn add_enum(&mut self, name: &NodeName) -> Option<RefMut<Enum>> {
        if self.enums.get(name).is_some() {
            return None;
        }
        let e = RefCell::new(Enum::default());
        self.enums.insert(name.clone(), e);
        self.enums.get(name).map(RefCell::borrow_mut)
    }

    /// Generate the graph.
    pub fn into_syntax(self) -> Syntax {
        let mut interfaces_by_name = HashMap::new();
        let mut interfaces_by_kind = HashMap::new();
        let mut names = HashMap::new();
        let mut kinds = HashMap::new();
        let mut field_names : HashMap<String, FieldName> = HashMap::new();


        for (name, interface) in &self.interfaces {
            //println!("\nCompiling interface {:?}", name);
            {
                let string = name.to_str().to_string();
                assert!(names.insert(string.clone(), Rc::new(string)).is_none());
            }

            if let Some(ref kind) = interface.borrow().kind {
                assert!(kinds.insert(kind.to_string().clone(), kind.clone()).is_none());
            }


            // Compute the fields and ancestors of `interface`.
            let mut ancestors_met = HashSet::new();
            let mut my_fields = HashMap::new();

            // To do so, walk the ancestors of `interface`. Algorithmically,
            // this could explode, but in practice, I haven't seen a depth higher than 4.
            let mut roots = vec![name.clone()];
            let mut all_my_ancestors = HashSet::new();
            while let Some(root) = roots.pop() {
                if ancestors_met.contains(&root) {
                    // With mutual inheritance, let's not copy stuff more than
                    // once. Should also prevent (but not detect) infinite loops.
                    continue;
                }

                all_my_ancestors.insert(root.clone());
                ancestors_met.insert(root.clone());
                let node = self.interfaces.get(&root).unwrap();
                debug_assert_eq!(node.borrow().name, root);

                for parent_names in &node.borrow().parent_interfaces {
                    roots.push(parent_names.clone());
                }
                for field in &node.borrow().own_contents.fields {
                    let name = field_names.entry(field.name.to_string().clone())
                        .or_insert_with(|| field.name().clone())
                        .clone();
                    if let Some(prev) = my_fields.get(&name) {
                        if prev != field.type_() {
                            println!("Conflict: attempting to insert {:?}", name);
                            println!("Previous: {:?}", prev);
                            println!("Overwrite: {:?}", field.type_());
                            println!("While treating {:?}", root);
                            assert!(false, "CHECK THIS OVERWRITE");
                        }
                        println!("Skipping");
                        // FIXME: We should make more efforts to ensure that
                        // we always end up with the bottom-most version
                        continue;
                    }
                    my_fields.insert(name, field.type_.clone());
                    // FIXME: We should handle the case in which a field is updated,
                    // e.g. `VariableDeclaration.kind` is extended from `"var"` to
                    // `"var" | "let" | "const"`.
                    // I believe that we need to make sure that we never overwrite
                    // a child with a parent.
                }
            }

            let fields = my_fields.drain()
                .map(|(name, type_)| Field { name, type_ })
                .collect();
            let node = Rc::new(InterfaceNode {
                ancestors: all_my_ancestors.drain().collect(),
                interface: interface.borrow().clone(),
                full_contents: Obj { fields }
            });

            if let Some(ref kind) = node.interface.kind {
                assert!(interfaces_by_kind.insert(kind.clone(), node.clone()).is_none());
            }

            assert!(interfaces_by_name.insert(name.clone(), node).is_none());
        }
        // FIXME: What about RegexpLiteral? & co

        // Now handle `enums`.
        for key in self.enums.keys() {
            let string = key.to_str().to_string();
            assert!(names.insert(string.clone(), Rc::new(string)).is_none());
        }
        let enums_by_name = self.enums;
        Syntax {
            interfaces_by_name,
            interfaces_by_kind,
            enums_by_name,
            names,
            kinds,
            fields: field_names,
        }
    }
}

/// An interface, with additional data computed during the call to
/// `SyntaxBuilder::as_syntax`.
pub struct InterfaceNode {
    interface: Interface,

    /// All the ancestors of this interface.
    ancestors: Vec<NodeName>,

    full_contents: Obj,
}

impl InterfaceNode {
    /// Returns the full list of fields for this structure.
    /// This method is in charge of:
    /// - ensuring that the fields of parent structures are properly accounted for;
    /// - disregarding ignored fields (i.e. `position`, `type`);
    /// - disregarding fields with a single possible value.
    pub fn contents(&self) -> &Obj {
        &self.full_contents
    }

    pub fn name(&self) -> &NodeName {
        &self.interface.name
    }

    pub fn kind(&self) -> Option<Kind> {
        match self.interface.kind {
            None => None,
            Some(ref x) => Some(x.clone())
        }
    }
}

/// Immutable representation of the syntax.
pub struct Syntax {
    interfaces_by_name: HashMap<NodeName, Rc<InterfaceNode>>,
    interfaces_by_kind: HashMap<Kind, Rc<InterfaceNode>>,
    enums_by_name: HashMap<NodeName, RefCell<Enum>>,
    names: HashMap<String, Rc<String>>,
    kinds: HashMap<String, Kind>,
    fields: HashMap<String, FieldName>,
}

impl Syntax {
    /// Return all the ancestors of an interface, including itself.
    pub fn get_ancestors_by_name_including_self(&self, name: &NodeName) -> Option<&[NodeName]> {
        self.interfaces_by_name
            .get(name)
            .map(|node| node.ancestors.as_slice())
    }
    pub fn get_interface_by_kind(&self, kind: &Kind) -> Option<&InterfaceNode> {
        self.interfaces_by_kind
            .get(kind)
            .map(Rc::deref)
    }
    pub fn get_interface_by_name(&self, name: &NodeName) -> Option<&InterfaceNode> {
        self.interfaces_by_name
            .get(name)
            .map(Rc::deref)
    }
    pub fn get_enum_by_name(&self, name: &NodeName) -> Option<Ref<Enum>> {
        self.enums_by_name
            .get(name)
            .map(RefCell::borrow)
    }
    pub fn get_kind(&self, name: &str) -> Option<Kind> {
        self.kinds
            .get(name)
            .cloned()
    }
    pub fn get_field_name(&self, name: &str) -> Option<FieldName> {
        self.fields
            .get(name)
            .cloned()
    }
}