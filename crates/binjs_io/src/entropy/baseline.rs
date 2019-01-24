//! A mechanism used to build a baseline dictionary.

use ::entropy::Dictionary;
use io::statistics::Instances;

use binjs_meta::spec::{ self, Spec };
use binjs_shared::{ FieldName, InterfaceName, SharedString };

use std::borrow::Borrow;
use std::collections::{ HashMap, HashSet };
use std::rc::Rc;

type IOPath = binjs_shared::ast::Path<
    InterfaceName,
    (
        /* child index */ usize,
        /* field name */ FieldName,
    ),
>;

pub struct BaselineDictionaryBuilder<'a> {
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
        self.visit_named_type(&self.spec.get_root(), false)
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
        let interface_name = InterfaceName::from_rc_string(interface.name().to_rc_string().clone());
        if self.dictionary.interface_name_by_path.contains_value(self.path.borrow(), interface_name.clone()) {
            // Already visited in this context, avoid looping.
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

