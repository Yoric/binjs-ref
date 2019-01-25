//! A mechanism used to build a baseline dictionary.
//!
//! The baseline dictionary is a simple dictionary extracted mechanically from the grammar
//! specifications.

use ::entropy::Dictionary;
use io::statistics::Instances;

use binjs_meta::spec::{ self, Spec };
use binjs_shared::{ FieldName, InterfaceName, SharedString };

use std::borrow::Borrow;
use std::collections::{ HashMap, HashSet };
use std::rc::Rc;

use itertools::Itertools;

type IOPath = binjs_shared::ast::Path<
    InterfaceName,
    (
        /* child index */ usize,
        /* field name */ FieldName,
    ),
>;

pub fn build(depth: usize, spec: &Spec) -> Dictionary<Instances> {
    let mut builder = BaselineDictionaryBuilder::new(depth, spec);
    builder.start();
    builder.done()
}

struct BaselineDictionaryBuilder<'a> {
    dictionary: Dictionary<Instances>,
    spec: &'a Spec,
    null_name: InterfaceName,
    depth: usize,
    path_by_interface_name: HashMap<InterfaceName, HashSet<IOPath>>,
}
impl<'a> BaselineDictionaryBuilder<'a> {
    pub fn new(depth: usize, spec: &'a Spec) -> Self {
        let null_name = InterfaceName::from_rc_string(spec.get_null_name().to_rc_string().clone());
        debug!(target: "baseline", "Starting baseline with depth {depth}, null = \"{null}\", root = \"{root}\"",
            null = null_name.as_str(),
            root = spec.get_root_name().to_str(),
            depth = depth);

        debug!(target: "baseline", "Roots: [{:?}]\n",
            spec.resolved_sums_of_interfaces_by_name()
                .get(spec.get_root_name())
                .unwrap()
                .iter()
                .map(|node_name| InterfaceName::from_rc_string(node_name.to_rc_string().clone()))
                .format(", "));

        BaselineDictionaryBuilder {
            spec,
            null_name,
            depth,
            dictionary: Dictionary::new(depth, 0),
            path_by_interface_name: HashMap::new(),
        }
    }

    fn collect_paths_to_prefix<I, NodeValue: 'a>(&self, path: &IOPath, interface_name: &InterfaceName, iter: I)
        -> Vec<(IOPath, NodeValue)>
    where
        I: Iterator<Item = (&'a IOPath, &'a NodeValue, &'a Instances)>,
        NodeValue: Clone,
    {
        let mut result = vec![];
        for (sub_path, value, statistics) in iter {
            assert_eq!(Into::<usize>::into(*statistics), 1);
            match sub_path.get(0) {
                Some(item) if item.interface() == interface_name => {
                    result.push((path.with_suffix(sub_path.borrow()), (*value).clone()));
                }
                _ => {}
            }
        }
        result
    }

    fn extend_content(&mut self, path: &IOPath, type_spec: &spec::TypeSpec) {
        use self::spec::TypeSpec;
        use self::spec::NamedType;
        match type_spec {
            TypeSpec::Array { ref contents, .. } => {
                // Entering an array doesn't affect the path.
                self.extend_content(path, contents.spec());
            }
            TypeSpec::TypeSum(ref sum) => {
                for spec in sum.types() {
                    self.extend_content(path, spec);
                }
            }
            TypeSpec::NamedType(ref name) => {
                let named_type = self.spec.get_type_by_name(name).unwrap(); // Grammar has been verified already.
                match named_type {
                    NamedType::Interface(ref interface) => {
                        let interface_name = InterfaceName::from_rc_string(interface.name().to_rc_string().clone());

                        // Now, take all the paths that start with `interface_name` and prefix their path with `path`.

                        let bools_to_add = self.collect_paths_to_prefix(path, &interface_name, self.dictionary.bool_by_path.iter());
                        for (path, value) in bools_to_add.into_iter() {
                            self.dictionary.bool_by_path.add_if_absent(path.borrow(), value.clone());
                        }

                        let string_enums_to_add = self.collect_paths_to_prefix(path, &interface_name, self.dictionary.string_enum_by_path.iter());
                        for (path, value) in string_enums_to_add.into_iter() {
                            self.dictionary.string_enum_by_path.add_if_absent(path.borrow(), value.clone());
                        }

                        let interface_names_to_add = self.collect_paths_to_prefix(path, &interface_name, self.dictionary.interface_name_by_path.iter());
                        for (path, value) in interface_names_to_add.into_iter() {
                            self.dictionary.interface_name_by_path.add_if_absent(path.borrow(), value.clone());
                        }

                        // FIXME: Handle path_by_interface_name?
                    }
                    NamedType::Typedef(ref def) => self.extend_content(path, def.spec()),
                    NamedType::StringEnum(_) => {
                        // String enums have been seeded as part of `seed_content`.
                        // At this stage, we only need to alter their path.
                    }
                }
            }
            TypeSpec::Boolean => {
                // Booleans have been seeded as part of `seed_content`.
                // At this stage, we only need to alter their path.
            }
            TypeSpec::String | TypeSpec::Number | TypeSpec::UnsignedLong | TypeSpec::Offset | TypeSpec::Void
                | TypeSpec::IdentifierName | TypeSpec::PropertyKey => {
                    // User-extensible values don't get in the baseline dictionary.
            }
        }
    }

    fn seed_content(&mut self, path: &IOPath, type_spec: &spec::TypeSpec, or_null: bool) {
        use self::spec::TypeSpec;
        use self::spec::NamedType;
        match type_spec {
            TypeSpec::Array { ref contents, .. } => {
                // Entering an array doesn't affect the path.
                self.seed_content(path, contents.spec(), contents.is_optional());
            }
            TypeSpec::TypeSum(ref sum) => {
                for spec in sum.types() {
                    self.seed_content(path, spec, or_null);
                }
            }
            TypeSpec::NamedType(ref name) => {
                let named_type = self.spec.get_type_by_name(name).unwrap(); // Grammar has been verified already.
                match named_type {
                    NamedType::Interface(ref interface) => {
                        let interface_name = InterfaceName::from_rc_string(interface.name().to_rc_string().clone());

                        // Store path => interface and interface => path.
                        self.dictionary.interface_name_by_path.add_if_absent(path.borrow(), interface_name.clone());
                        self.path_by_interface_name.entry(interface_name.clone())
                            .or_insert_with(|| HashSet::new())
                            .insert(path.clone());
                        if or_null {
                            self.dictionary.interface_name_by_path.add_if_absent(path.borrow(), self.null_name.clone());
                            self.path_by_interface_name.entry(self.null_name.clone())
                                .or_insert_with(|| HashSet::new())
                                .insert(path.clone());
                        }
                    }
                    NamedType::Typedef(ref def) => self.seed_content(path, def.spec(), or_null || def.is_optional()),
                    NamedType::StringEnum(ref string_enum) => {
                        for value in string_enum.strings() {
                            let shared_string = SharedString::from_rc_string(Rc::new(value.clone()));
                            self.dictionary.string_enum_by_path.add_if_absent(path.borrow(), shared_string);
                        }
                        if or_null {
                            panic!()
                            // The byte-level format supports this, but as the specs don't require it,
                            // our internal APIs don't for the moment.
                        }
                    }
                }
            }
            TypeSpec::Boolean => {
                self.dictionary.bool_by_path.add_if_absent(path.borrow(), Some(true));
                self.dictionary.bool_by_path.add_if_absent(path.borrow(), Some(false));
                if or_null {
                    self.dictionary.bool_by_path.add_if_absent(path.borrow(), None);
                }
            }
            TypeSpec::String | TypeSpec::Number | TypeSpec::UnsignedLong | TypeSpec::Offset | TypeSpec::Void
                | TypeSpec::IdentifierName | TypeSpec::PropertyKey => {
                    // User-extensible values don't get in the baseline dictionary.
            }
        }
    }

    pub fn start(&mut self) {
        // Seed the dictionary with depth 1 data.
        debug!(target: "baseline", "Seeding dictionary to depth 1");
        let mut path = IOPath::new();
        for (_, interface) in self.spec.interfaces_by_name() {
            let interface_name = InterfaceName::from_rc_string(interface.name().to_rc_string().clone());
            path.enter_interface(interface_name.clone());
            for (position, field) in interface.contents().fields().iter().enumerate() {
                let field_name = FieldName::from_rc_string(field.name().to_rc_string().clone());
                path.enter_field((position, field_name.clone()));
                self.seed_content(&path, field.type_().spec(), field.type_().is_optional());
                path.exit_field((position, field_name));
            }
            path.exit_interface(interface_name.clone());
        }
        debug!(target: "baseline", "After seeding, dictionary has {} states", self.dictionary.len());

        for i in 1..self.depth {
            debug!(target: "baseline", "Extending dictionary to depth {}", i + 1);
            // Expand path depth by 1 level.
            let mut path = IOPath::new();
            for (_, interface) in self.spec.interfaces_by_name() {
                let interface_name = InterfaceName::from_rc_string(interface.name().to_rc_string().clone());
                path.enter_interface(interface_name.clone());
                for (position, field) in interface.contents().fields().iter().enumerate() {
                    let field_name = FieldName::from_rc_string(field.name().to_rc_string().clone());
                    path.enter_field((position, field_name.clone()));
                    self.extend_content(&path, field.type_().spec());
                    path.exit_field((position, field_name));
                }
                path.exit_interface(interface_name.clone());
            }
            debug!(target: "baseline", "After extending, dictionary has {} states", self.dictionary.len());
        }

        // Garbage-collect paths that have nothing to do here.
        let depth = self.depth;
        let roots: HashSet<_> = self.spec.resolved_sums_of_interfaces_by_name()
            .get(self.spec.get_root_name())
            .unwrap_or_else(|| panic!("Cannot get grammar roots {:?}", self.spec.get_root_name()))
            .iter()
            .map(|node_name| InterfaceName::from_rc_string(node_name.to_rc_string().clone()))
            .collect();

        {
            let retain = |path: &IOPath| {
                // Retain paths that have the expected depth.
                if path.len() == depth {
                    return true;
                }
                // Also retain shorter paths that start from the root.
                match path.get(0) {
                    Some(item) if roots.contains(item.interface()) => true,
                    _ => false
                }
            };
            self.dictionary.bool_by_path.retain(|path, _, _| retain(path));
            self.dictionary.interface_name_by_path.retain(|path, _, _| retain(path));
            self.dictionary.string_enum_by_path.retain(|path, _, _| retain(path));
        }

        // Finally, introduce the only 0-length paths.
        let path = IOPath::new();
        for root in roots {
            self.dictionary.interface_name_by_path.add_if_absent(path.borrow(), root);
        }
    }

    pub fn done(self) -> Dictionary<Instances> {
        debug!(target: "baseline", "Final dictionary has {} state", self.dictionary.len());
        self.dictionary
    }
}

/*
struct BaselineDictionaryBuilder<'a> {
    dictionary: Dictionary<Instances>,
    spec: &'a Spec,
    path: IOPath,
    null_name: InterfaceName,
}
impl<'a> BaselineDictionaryBuilder<'a> {
    pub fn new(depth: usize, spec: &'a Spec) -> Self {
        let null_name = InterfaceName::from_rc_string(spec.get_null_name().to_rc_string().clone());

        BaselineDictionaryBuilder {
            dictionary: Dictionary::new(depth, /* width */0),
            spec,
            null_name,
            path: IOPath::new(),
        }
    }

    pub fn start(&mut self) {
        self.visit_named_type(&self.spec.get_root(), false);
    }

    pub fn done(self) -> Dictionary<Instances> {
        assert_eq!(self.path.len(), 0);
        self.dictionary
    }

    fn visit_named_type(&mut self, node: &binjs_meta::spec::NamedType, or_null: bool) {
        use binjs_meta::spec::NamedType::*;
        match node {
            Interface(ref interface) =>
                self.visit_interface(interface, or_null),
            Typedef(ref typedef) =>
                self.visit_type(typedef, or_null),
            StringEnum(ref string_enum) =>
                self.visit_string_enum(string_enum, or_null)
        }
    }

    fn visit_type(&mut self, type_: &spec::Type, or_null: bool) {
        use self::spec::TypeSpec;
        match type_.spec {
            TypeSpec::Array {
                ref contents,
                ..
            } => self.visit_type(contents, false),
            TypeSpec::NamedType(ref name) => {
                let named_type = self.spec.get_type_by_name(name)
                    .unwrap_or_else(|| panic!("Undefined type {}", name.to_str()));
                self.visit_named_type(&named_type, or_null || type_.is_optional())
            }
            TypeSpec::TypeSum(ref sum) => {
                for spec in sum.types() {
                    // At this stage, we have a guarantee that `spec` is an interface.
                    if let TypeSpec::NamedType(ref name) = *spec {
                        let named_type = self.spec.get_type_by_name(name)
                            .unwrap_or_else(|| panic!("Undefined type {}", name.to_str()));
                        self.visit_named_type(&named_type, or_null || type_.is_optional())
                    }
                }
            }
            TypeSpec::Boolean => {
                if self.dictionary.bool_by_path.contains_path(self.path.borrow()) {
                    // Already visited in this context, avoid looping.
                }
                self.dictionary.bool_by_path.add_if_absent(self.path.borrow(), Some(false));
                self.dictionary.bool_by_path.add_if_absent(self.path.borrow(),Some(true));
                if or_null || type_.is_optional() {
                    self.dictionary.bool_by_path.add_if_absent(self.path.borrow(), None);
                }
            }
            TypeSpec::String | TypeSpec::Number | TypeSpec::UnsignedLong | TypeSpec::Offset | TypeSpec::Void
            | TypeSpec::IdentifierName | TypeSpec::PropertyKey => {
                // User-extensible values, nothing to do here.
            }
        }
    }

    fn visit_interface(&mut self, interface: &spec::Interface, or_null: bool) {
        debug!(target: "baseline", "Visiting interface {} in path {:?}", interface.name().to_str(), self.path);
        let interface_name = InterfaceName::from_rc_string(interface.name().to_rc_string().clone());
        if self.dictionary.interface_name_by_path.contains_value(self.path.borrow(), interface_name.clone()) {
            // Already visited in this context, avoid looping.
            debug!(target: "baseline", "Context already visited, avoid looping");
            return;
        }
        // Register interface.
        self.dictionary.interface_name_by_path.add_if_absent(self.path.borrow(), interface_name.clone());
        if or_null {
            // The null interface also works here.
            self.dictionary.interface_name_by_path.add_if_absent(self.path.borrow(), self.null_name.clone());
        }

        // Visit fields.
        self.path.enter_interface(interface_name.clone());
        // FIXME: Find a way to assert/unit test that the fields are in the same order as in ast.rs.
        // FIXME: Otherwise, the `position` index will be completely broken.

        for (position, field) in interface.contents().fields().iter().enumerate() {
            let field_name = FieldName::from_rc_string(field.name().to_rc_string().clone());
            self.path.enter_field((position, field_name.clone()));
            self.visit_type(field.type_(), false);
            self.path.exit_field((position, field_name));
        }

        self.path.exit_interface(interface_name.clone());
    }

    fn visit_string_enum(&mut self, string_enum: &Rc<spec::StringEnum>, or_null: bool) {
        if self.dictionary.string_enum_by_path.contains_path(self.path.borrow()) {
            // Already visited in this context, avoid looping.
            return;
        }
        if or_null {
            unimplemented!();
            // The null case is supported by the byte-level format, but not by our internal APIs.
            // If the spec evolves to require this, we'll need to evolve the APIs.
        }
        for value in string_enum.strings() {
            let shared_string = SharedString::from_rc_string(Rc::new(value.clone()));
            self.dictionary.string_enum_by_path.add_if_absent(self.path.borrow(), shared_string);
        }
    }
}

*/