//! A mechanism used to build a baseline entropy dictionary.
//!
//! A baseline dictionary is a dictionary built without samples, only from the grammar
//! webidl specifications, by assuming that, at every node, every construction that is apparently
//! grammatically correct has the same probability. Note that this produces some false positives,
//! e.g. constructions that appear valid in the webidl but that are actually invalid,
//! (such as a `continue` without a loop) are assumed to be valid by the baseline dictionary.
//! Also note that baseline dictionary only deal with nodes whose value is taken from a finite
//! set of possible values specified by the library (booleans, string enums, interfaces/interface
//! sums), by opposition to user-extensible values (strings, arbitrary numbers, ...).
//!
//! The main use of the baseline dictionary is as an escape mechanism for entropy coding.
//! By combining a dictionary created by sampling and a baseline dictionary, we produce
//! a dictionary heavily biased towards constructions that are likely to appear, but
//! that also assigns probability 𝛆>0 to every other construction that is possible in
//! the grammar.
//!
//! For proper use, a compressed file must refer to BOTH a sampling dictionary and a
//! baseline dictionary. It is possible to use only a baseline dictionary, but experiments
//! shows a ~50% file size increase (see https://github.com/binast/binjs-ref/issues/249).
//! It is also possible to use only a sampling dictionary, at the cost of extensibility,
//! but experiments do not indicate any advantage to doing so, except perhaps faster
//! initialization.

use entropy::dictionary;
use entropy::Dictionary;
use io::statistics::Instances;

use binjs_meta::spec::{self, Spec};
use binjs_shared::{FieldName, IOPath, InterfaceName, SharedString};

use std::borrow::Borrow;
use std::collections::HashSet;
use std::rc::Rc;

use itertools::Itertools;

/// Build a baseline dictionary from a spec.
///
/// The resulting baseline dictionary always has a depth of 1.
pub fn build(spec: &Spec) -> Dictionary<Instances> {
    let mut builder = BaselineDictionaryBuilder::new(spec);
    builder.start();
    builder.done()
}

/// A structure dedicated to building a baseline dictionary.
struct BaselineDictionaryBuilder<'a> {
    /// The dictionary currently being constructed.
    dictionary: Dictionary<Instances>,

    /// The specs from which to extract the dictionary.
    spec: &'a Spec,

    /// The name of the special `null` interface, used to represent the `null` case of `attribute Foo?`.
    null_name: InterfaceName,
}
impl<'a> BaselineDictionaryBuilder<'a> {
    /// Create a new baseline dictionary builder for a given depth and spec.
    pub fn new(spec: &'a Spec) -> Self {
        let null_name = InterfaceName::from_rc_string(spec.get_null_name().to_rc_string().clone());
        debug!(target: "baseline", "Starting baseline with null = \"{null}\", root = \"{root}\"",
            null = null_name.as_str(),
            root = spec.get_root_name().to_str());

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
            dictionary: Dictionary::new(dictionary::Options::default().with_depth(1).with_width(0)),
        }
    }

    /// Extract all possible values for a field accessible at `path` and with type `type_spec` and
    /// add them to the dictionary.
    ///
    /// - `path` must be the length 1 path used to access the field.
    /// - `type_spec` is the type of the field.
    /// - if `or_null` is `true`, the field is optional, i.e. the value may be `null`, otherwise the
    ///    value is guaranteed to be non-null.
    ///
    /// This method takes into account:
    /// - fields with bool or bool? values;
    /// - fields with string enum values;
    /// - fields with interface or interface sum values.
    ///
    /// This method will recursively follow typedefs but will not walk the children of the field.
    fn seed_one_field(&mut self, path: &IOPath, type_spec: &spec::TypeSpec, or_null: bool) {
        use self::spec::NamedType;
        use self::spec::TypeSpec;

        debug_assert_eq!(path.len(), 1);

        match type_spec {
            TypeSpec::Array { ref contents, .. } => {
                // In the current implementation of Entropy, arrays are simply ignored.
                // Entering an array doesn't affect the path.
                self.seed_one_field(path, contents.spec(), contents.is_optional());
            }
            TypeSpec::TypeSum(ref sum) => {
                for spec in sum.types() {
                    self.seed_one_field(path, spec, or_null);
                }
            }
            TypeSpec::NamedType(ref name) => {
                let named_type = self.spec.get_type_by_name(name).unwrap(); // Grammar has been verified already.
                match named_type {
                    NamedType::Interface(ref interface) => {
                        let interface_name =
                            InterfaceName::from_rc_string(interface.name().to_rc_string().clone());

                        self.dictionary
                            .interface_name_by_path_mut()
                            .add_if_absent(path.borrow(), Some(interface_name.clone()));
                        if or_null {
                            self.dictionary
                                .interface_name_by_path_mut()
                                .add_if_absent(path.borrow(), Some(self.null_name.clone()));
                        }
                    }
                    NamedType::Typedef(ref def) => {
                        self.seed_one_field(path, def.spec(), or_null || def.is_optional())
                    }
                    NamedType::StringEnum(ref string_enum) => {
                        for value in string_enum.strings() {
                            let shared_string =
                                SharedString::from_rc_string(Rc::new(value.clone()));
                            self.dictionary
                                .string_enum_by_path_mut()
                                .add_if_absent(path.borrow(), Some(shared_string));
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
                self.dictionary
                    .bool_by_path_mut()
                    .add_if_absent(path.borrow(), Some(true));
                self.dictionary
                    .bool_by_path_mut()
                    .add_if_absent(path.borrow(), Some(false));
                if or_null {
                    self.dictionary
                        .bool_by_path_mut()
                        .add_if_absent(path.borrow(), None);
                }
            }
            TypeSpec::String
            | TypeSpec::Number
            | TypeSpec::UnsignedLong
            | TypeSpec::Offset
            | TypeSpec::Void
            | TypeSpec::IdentifierName
            | TypeSpec::PropertyKey => {
                // User-extensible values don't get in the baseline dictionary.
            }
        }
    }

    pub fn start(&mut self) {
        // Seed the dictionary with depth 1 data.
        debug!(target: "baseline", "Seeding dictionary to depth 1");
        let mut path = IOPath::new();
        for (_, interface) in self.spec.interfaces_by_name() {
            let interface_name =
                InterfaceName::from_rc_string(interface.name().to_rc_string().clone());
            path.enter_interface(interface_name.clone());
            for (position, field) in interface.contents().fields().iter().enumerate() {
                let field_name = FieldName::from_rc_string(field.name().to_rc_string().clone());
                path.enter_field((position, field_name.clone()));
                self.seed_one_field(&path, field.type_().spec(), field.type_().is_optional());
                path.exit_field((position, field_name));
            }
            path.exit_interface(interface_name.clone());
        }
        debug!(target: "baseline", "After seeding, dictionary has {} states", self.dictionary.len());

        // Finally, introduce the only 0-length paths.
        let roots: HashSet<_> = self
            .spec
            .resolved_sums_of_interfaces_by_name()
            .get(self.spec.get_root_name())
            .unwrap_or_else(|| panic!("Cannot get grammar roots {:?}", self.spec.get_root_name()))
            .iter()
            .map(|node_name| InterfaceName::from_rc_string(node_name.to_rc_string().clone()))
            .collect();
        let empty_path = IOPath::new();
        for root in roots {
            self.dictionary
                .interface_name_by_path_mut()
                .add_if_absent(empty_path.borrow(), Some(root));
        }
    }

    pub fn done(self) -> Dictionary<Instances> {
        debug!(target: "baseline", "Final dictionary has {} state", self.dictionary.len());
        self.dictionary
    }
}
