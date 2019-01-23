extern crate binjs_meta;
extern crate env_logger;
extern crate itertools;
extern crate log;

use binjs_meta::export::{TypeDeanonymizer, TypeName};
use binjs_meta::spec::*;
use binjs_meta::util::*;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use itertools::Itertools;

/// Source code produced by exporting a spec to Rust.
pub struct ExportedSource {
    /// Source code for a strongly-typed data structure implementing the specification.
    pub typed: String,

    /// Source code for a weakly-typed (JSON-based) data structure implementing the specification.
    pub generic: String,
}

/// Generate Rust source
pub struct RustExporter {
    /// The original specifications, without deanonymization.
    ///
    /// Used to generate code that will create dynamically
    /// an instance of `Spec`.
    spec: Spec,
}
impl RustExporter {
    /// Create a Rust exporter from the original specifications.
    pub fn new(spec: Spec) -> Self {
        RustExporter { spec }
    }

    /// Generate the dynamic version of a `Type`.
    fn type_(type_: &Type, prefix: &str) -> String {
        let spec = Self::type_spec(type_.spec(), prefix);
        let ref spec = spec.trim_start();
        format!(
            "{prefix}{type_}{close}",
            type_ = spec,
            prefix = prefix,
            close = if type_.is_optional() {
                ".optional().unwrap()"
            } else {
                ".required()"
            }
        )
    }

    /// Generate the dynamic version of a `TypeSpec`.
    fn type_spec(spec: &TypeSpec, prefix: &str) -> String {
        match *spec {
            TypeSpec::Array {
                ref contents,
                supports_empty: false,
            } => format!(
                "{prefix}{contents}.non_empty_array()",
                contents = Self::type_(contents, prefix),
                prefix = prefix
            ),
            TypeSpec::Array {
                ref contents,
                supports_empty: true,
            } => format!(
                "{prefix}{contents}.array()",
                contents = Self::type_(contents, prefix),
                prefix = prefix
            ),
            TypeSpec::Boolean => format!("{prefix}Type::bool()", prefix = prefix),
            TypeSpec::Offset => format!("{prefix}Type::offset()", prefix = prefix),
            TypeSpec::String => format!("{prefix}Type::string()", prefix = prefix),
            TypeSpec::Number => format!("{prefix}Type::number()", prefix = prefix),
            TypeSpec::UnsignedLong => format!("{prefix}Type::unsigned_long()", prefix = prefix),
            TypeSpec::IdentifierName => format!("{prefix}Type::identifier_name()", prefix = prefix),
            TypeSpec::PropertyKey => format!("{prefix}Type::property_key()", prefix = prefix),
            TypeSpec::NamedType(ref name) => format!(
                "{prefix}Type::named(&names.{name})",
                name = name.to_rust_identifier_case(),
                prefix = prefix
            ),
            TypeSpec::TypeSum(ref types) => {
                let indent = format!("{prefix}    ", prefix = prefix);
                format!(
                    "{prefix}Type::sum(&[\n{sum}\n{prefix}])",
                    prefix = prefix,
                    sum = types
                        .types()
                        .iter()
                        .map(|t| Self::type_spec(t, &indent))
                        .format(",\n")
                )
            }
            TypeSpec::Void => "void".to_string(),
        }
    }

    pub fn to_rust_source(&self) -> ExportedSource {
        let deanonymizer = TypeDeanonymizer::new(&self.spec);
        let supersums_of: HashMap<_, _> = deanonymizer
            .supersums()
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        let deanonymized = deanonymizer.into_spec(SpecOptions {
            root: self.spec.get_root_name(),
            null: self.spec.get_null_name(),
        });

        // Buffer used to generate the strongly-typed data structure.
        let mut ast_buffer = String::new();
        ast_buffer.push_str("
use binjs_shared;
use binjs_shared::{ FieldName, FromJSON, FromJSONError, IdentifierName, InterfaceName, Offset, PropertyKey, SharedString, ToJSON, VisitMe };
use binjs_io::{ Deserialization, InnerDeserialization, Serialization, TokenReader, TokenReaderError, TokenWriter, TokenWriterError };

use io::*;

use std;
use std::convert::{ From };

use json;
use json::JsonValue as JSON;

");

        // Buffer used to generate the generic data structure (struct declaration).
        let mut struct_buffer = String::new();
        struct_buffer.push_str("pub struct Library {\n");

        // Buffer used to generate the generic data structure (impl declaration).
        let mut impl_buffer = String::new();
        impl_buffer.push_str("impl Library {\n    pub fn new(builder: &mut SpecBuilder) -> Self {\n        let names = Library {\n");

        // Export the name definitions.
        fn print_struct_names<'a, T>(buffer: &mut String, source: T)
        where
            T: Iterator<Item = &'a NodeName>,
        {
            let mut names: Vec<_> = source.map(|x| x.to_string()).collect();
            names.sort();
            for name in names {
                let source = format!(
                    "    pub {snake}: NodeName,\n",
                    snake = name.to_rust_identifier_case()
                );
                buffer.push_str(&source);
            }
        }
        fn print_impl_names<'a, T>(buffer: &mut String, source: T)
        where
            T: Iterator<Item = &'a NodeName>,
        {
            let mut names: Vec<_> = source.map(|x| x.to_string()).collect();
            names.sort();
            for name in names {
                let source = format!(
                    "            {snake}: builder.node_name(\"{original}\"),\n",
                    snake = name.to_rust_identifier_case(),
                    original = name
                );
                buffer.push_str(&source);
            }
        }
        fn print_ast_string_enums(buffer: &mut String, source: &HashMap<NodeName, Rc<StringEnum>>) {
            let names = source.keys().sorted();
            for name in names {
                let string_enum = source.get(&name).unwrap();
                let rust_name = name.to_class_cases();
                let definition = format!(
                    "
/// Implementation of string enum {name}
#[derive(PartialEq, Debug, Clone)]\npub enum {rust_name} {{\n{values}\n}}\n",
                    name = name,
                    rust_name = rust_name,
                    values = string_enum
                        .strings()
                        .iter()
                        .map(|s| format!(
                            "
    /// Implementation of variant \"{spec_variant_name}\"
    {rust_variant_name}",
                            spec_variant_name = s,
                            rust_variant_name = ToCases::to_cpp_enum_case(s)
                        ))
                        .format(",\n")
                );
                let default = format!(
                    "
type ViewMut{name}<'a> = ViewMutNothing<{name}>;
impl<'a> From<&'a mut {name}> for ViewMut{name}<'a> {{
    fn from(_: &'a mut {name}) -> Self {{
        ViewMutNothing::default()
    }}
}}
impl Default for {name} {{
    fn default() -> Self {{
        {name}::{default}
    }}
}}
",
                    name = name,
                    default = string_enum.strings()[0].to_cpp_enum_case()
                );

                let to_json = format!(
                    "
impl ToJSON for {name} {{
    fn export(&self) -> JSON {{
        json::from(match *self {{
{cases}
        }})
    }}
}}\n\n",
                    cases = string_enum
                        .strings()
                        .iter()
                        .map(|s| format!(
                            "           {name}::{typed} => \"{string}\"",
                            name = name,
                            typed = s.to_cpp_enum_case(),
                            string = s,
                        ))
                        .format(",\n"),
                    name = name
                );

                let from_reader = format!("
impl<R> Deserializer<R> where R: TokenReader {{
    fn deserialize_variant_{lowercase_name}_aux(&mut self, path: &mut IOPath) -> Result<{name}, TokenReaderError> where R: TokenReader {{
        let key = self.reader.string_enum_at(path)?;
        match key.as_str() {{
{variants}
            _ => Err(From::from(TokenReaderError::invalid_value(&\"{lowercase_name}\"))),
        }}
    }}
    fn deserialize_variant_{lowercase_name}(&mut self, path: &mut IOPath) -> Result<{name}, TokenReaderError> where R: TokenReader {{
        let result = self.deserialize_variant_{lowercase_name}_aux(path);
        if result.is_err() {{
            self.reader.poison();
        }}
        result
    }}
}}

impl<R> Deserialization<R, {name}> for Deserializer<R> where R: TokenReader {{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<{name}, TokenReaderError> {{
        debug!(target: \"deserialize_es6\", \"Deserializing variant {name}\");
        self.deserialize_variant_{lowercase_name}(path)
    }}
}}
",
                    name = name,
                    lowercase_name = name.to_rust_identifier_case(),
                    variants = string_enum.strings()
                        .iter()
                        .map(|s| format!("            \"{string}\" => Ok({name}::{typed}),",
                            name = name,
                            typed = s.to_cpp_enum_case(),
                            string = s))
                        .format("\n")
                    );

                let from_json = format!(
                    "
impl FromJSON for {name} {{
    fn import(source: &JSON) -> Result<Self, FromJSONError > {{
        match source.as_str() {{
{cases},
            _ => Err(FromJSONError {{
                expected: \"Instance of {name}\".to_string(),
                got: source.dump(),
            }})
        }}
    }}
}}\n\n",
                    cases = string_enum
                        .strings()
                        .iter()
                        .map(|s| format!(
                            "           Some(\"{string}\") => Ok({name}::{typed})",
                            name = name,
                            typed = s.to_cpp_enum_case(),
                            string = s,
                        ))
                        .format(",\n"),
                    name = name
                );

                let to_writer = format!(
                    "
impl<'a, W> Serialization<W, &'a {name}> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, value: &'a {name}, path: &mut IOPath) -> Result<(), TokenWriterError> {{
        debug!(target: \"serialize_es6\", \"Serializing string enum {name}\");
        let str = match *value {{
{variants}
        }};
        self.writer.string_enum_at(&SharedString::from_str(str), path)
    }}
}}
",
                    name = name,
                    variants = string_enum
                        .strings()
                        .iter()
                        .map(|s| format!(
                            "            {name}::{typed} => \"{string}\"",
                            name = name,
                            typed = s.to_cpp_enum_case(),
                            string = s,
                        ))
                        .format(",\n")
                );

                let walker = format!("
impl<'a> Walker<'a> for {name} where Self: 'a {{
    type Output = {name};
    fn walk<V, E, G: Default>(&'a mut self, _: &mut WalkPath, _: &mut V) -> Result<Option<Self::Output>, E> where V: Visitor<E, G> {{
        Ok(None)
    }}
}}\n",
                    name = name);

                buffer.push_str(&definition);
                buffer.push_str(&default);
                buffer.push_str(&from_json);
                buffer.push_str(&to_json);
                buffer.push_str(&from_reader);
                buffer.push_str(&to_writer);
                buffer.push_str(&walker);
                buffer.push_str("\n\n\n");
            }
        }
        fn print_ast_typedefs(
            buffer: &mut String,
            source: &HashMap<NodeName, Rc<Type>>,
            supersums: &HashMap<NodeName, HashSet<NodeName>>,
            null_name: &str,
        ) {
            // Compute reverse supersums.
            let mut rev_supersums = HashMap::new();
            for (k, set) in supersums {
                for v in set {
                    let entry = rev_supersums.entry(v).or_insert_with(|| HashSet::new());
                    entry.insert(k.clone());
                }
            }
            let rev_supersums = rev_supersums;

            let mut enums = vec![];
            let mut options = vec![];
            let mut lists = vec![];
            let mut primitives = vec![];
            let names: Vec<_> = source.keys().sorted();

            for name in names.into_iter() {
                // Since the source is deanonymized, all type definitions are just one layer deep.
                let typedef = source.get(name).unwrap();
                if typedef.is_optional() {
                    options.push(name);
                } else {
                    match *typedef.spec() {
                        TypeSpec::TypeSum(_) => {
                            enums.push(name);
                        }
                        TypeSpec::Array { .. } => {
                            lists.push(name);
                        }
                        TypeSpec::Boolean
                        | TypeSpec::Number
                        | TypeSpec::UnsignedLong
                        | TypeSpec::String
                        | TypeSpec::Void => {
                            primitives.push(name);
                        }
                        _ => {
                            buffer.push_str(&format!("// UNIMPLEMENTED: {}\n", name));
                        }
                    }
                }
            }
            buffer.push_str("\n\n// Type sums (by lexicographical order)\n");
            let typesums: HashMap<_, _> = enums
                .iter()
                .filter_map(|name| {
                    let typedef = source.get(name).unwrap();
                    if let TypeSpec::TypeSum(ref sum) = *typedef.spec() {
                        let types: Vec<_> = sum
                            .types()
                            .iter()
                            .map(|t| {
                                if let TypeSpec::NamedType(ref case) = *t {
                                    case
                                } else {
                                    panic!();
                                }
                            })
                            .collect();
                        Some((name.clone(), types))
                    } else {
                        None
                    }
                })
                .collect();
            for name in enums.drain(..) {
                let typedef = source.get(name).unwrap();
                let node_name = name;
                let name = name.to_class_cases();
                if let TypeSpec::TypeSum(_) = *typedef.spec() {
                    let types = typesums.get(&node_name).unwrap(); // Just built above.

                    let definition = format!(
                        "
/// Implementation of interface sum {node_name}
#[derive(PartialEq, Debug, Clone)]
pub enum {name} {{\n{contents}\n}}\n

/// A mechanism to view value as an instance of interface sum {node_name}
///
/// Used to perform shallow cast between larger sums and smaller sums.
pub enum ViewMut{name}<'a> {{\n{ref_mut_contents}\n}}\n",
                        name = name,
                        node_name = node_name,
                        contents = types
                            .iter()
                            .map(|case| format!(
                                "    {name}(Box<{name}>)",
                                name = case.to_class_cases()
                            ))
                            .format(",\n"),
                        ref_mut_contents = types
                            .iter()
                            .map(|case| format!(
                                "    {name}(&'a mut {name})",
                                name = case.to_class_cases()
                            ))
                            .format(",\n"),
                    );

                    let single_variant_from = format!(
                        "
{}
",
                        types
                            .iter()
                            .map(|case| format!(
                                "
impl From<{variant_name}> for {name} {{
    fn from(value: {variant_name}) -> Self {{
        {name}::{variant_name}(Box::new(value))
    }}
}}
",
                                name = name,
                                variant_name = case.to_class_cases()
                            ))
                            .format("\n")
                    );
                    let subsum_from = match rev_supersums.get(&node_name) {
                        None => "".to_string(),
                        Some(subsums) => {
                            format!("{}", subsums.iter()
                                .map(|subsum_name| format!("
/// Convert an instance of an enum into an instance of a larger enum.
impl From<{subsum_name}> for {name} {{
    fn from(value: {subsum_name}) -> Self {{
        match value {{
{cases}
        }}
    }}
}}
",
                                        subsum_name = subsum_name.to_class_cases(),
                                        name = name,
                                        cases = typesums.get(subsum_name).unwrap()
                                            .iter()
                                            .map(|variant| {
                                                format!("           {subsum_name}::{variant_name}(x) => {name}::{variant_name}(x)",
                                                    subsum_name = subsum_name.to_class_cases(),
                                                    name = name,
                                                    variant_name = variant.to_class_cases())
                                            })
                                            .format(",\n")
                                    ))
                                    .format("\n")
                            )
                        }
                    };

                    let default = format!(
                        "
impl Default for {name} {{
    fn default() -> Self {{
        {name}::{default}
    }}
}}

",
                        name = name,
                        default = format!(
                            "{variant}(Box::new(Default::default()))",
                            variant = types[0].to_class_cases()
                        ),
                    );

                    let from_reader = format!("
impl<R> Deserialization<R, {name}> for Deserializer<R> where R: TokenReader {{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<{name}, TokenReaderError> {{
        debug!(target: \"deserialize_es6\", \"Deserializing sum {name}\");
        let (kind, _) = self.reader.enter_tagged_tuple_at(path)?;
        debug!(target: \"deserialize_es6\", \"Deserializing sum {name}, found {{}}\", kind.as_str());
        let path_interface = kind.clone();
        let result = match kind.as_str() {{
{variants}
            _ => {{
                error!(target: \"deserialize_es6\", \"Deserializing sum {name}, found invalid {{}}\", kind.as_str());
                Err(From::from(TokenReaderError::BadEnumVariant))
            }}
        }};
        if result.is_err() {{
            self.reader.poison();
        }}
        self.reader.exit_tagged_tuple_at(path)?;
        result
    }}
}}
impl<R> Deserialization<R, Option<{name}>> for Deserializer<R> where R: TokenReader {{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<{name}>, TokenReaderError> {{
        debug!(target: \"deserialize_es6\", \"Deserializing optional sum {name}\");
        let (kind, _) = self.reader.enter_tagged_tuple_at(path)?;
        let path_interface = kind.clone();
        let result = match kind.as_str() {{
{variants_some}
            \"{null}\" => Ok(None),
            _ => {{
                error!(target: \"deserialize_es6\", \"Deserializing sum Option<{name}>, found invalid {{}}\", kind.as_str());
                Err(From::from(TokenReaderError::BadEnumVariant))
            }}
        }};
        if result.is_err() {{
            self.reader.poison();
        }}
        self.reader.exit_tagged_tuple_at(path)?;
        result
    }}
}}
",
                                name = name,
                                variants = types
                                    .iter()
                                    .map(|case| {
                                        format!("           \"{case}\" => {{
                    path.enter_interface(path_interface.clone());
                    let result = self.deserialize_inner(path)
                        .map(|r| {name}::{constructor}(Box::new(r)));
                    path.exit_interface(path_interface);
                    result
                }}",
                                            name = name,
                                            case = case,
                                            constructor = case.to_class_cases())
                                    })
                                    .format("\n"),
                                variants_some = types
                                    .iter()
                                    .map(|case| {
                                        format!("           \"{case}\" => {{
            path.enter_interface(path_interface.clone());
            let result = self.deserialize_inner(path)
                .map(|r| Some({name}::{constructor}(Box::new(r))));
            path.exit_interface(path_interface);
            result
        }}",
                                            name = name,
                                            case = case,
                                            constructor = case.to_class_cases())
                                    })
                                    .format("\n"),
                                    null = null_name,
                                );

                    let from_json = format!("
impl FromJSON for {name} {{
    fn import(value: &JSON) -> Result<Self, FromJSONError> {{
        match value[\"type\"].as_str() {{
{cases},
            _ => Err(FromJSONError {{
                expected: \"Instance of {kind}\".to_string(),
                got: value.dump()
            }})
        }}
    }}
}}\n\n",
                                name = name,
                                kind = name,
                                cases = types.iter()
                                    .map(|case| {
                                        format!("           Some(\"{case}\") => Ok({name}::{constructor}(Box::new(FromJSON::import(value)?)))",
                                            name = name,
                                            case = case,
                                            constructor = case.to_class_cases())
                                    })
                                    .format(",\n")
                                );

                    let to_json = format!(
                        "
impl ToJSON for {name} {{
    fn export(&self) -> JSON {{
        match *self {{
{cases}
        }}
    }}
}}\n\n",
                        name = name,
                        cases = types
                            .iter()
                            .map(|case| format!(
                                "           {name}::{constructor}(ref value) => value.export()",
                                name = name,
                                constructor = case.to_class_cases()
                            ))
                            .format(",\n")
                    );

                    let to_writer = format!("
impl<'a, W> Serialization<W, &'a Option<{rust_name}>> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, value: &'a Option<{rust_name}>, path: &mut IOPath) -> Result<(), TokenWriterError> {{
        debug!(target: \"serialize_es6\", \"Serializing optional sum {rust_name}\");
        match *value {{
            None => {{
                let interface_name = InterfaceName::from_str(\"{null}\");
                self.writer.enter_tagged_tuple_at(&{null_interface}{{}}, &interface_name, &[], path)?;
                self.writer.exit_tagged_tuple_at(&{null_interface}{{}}, &interface_name, &[], path)?;
                Ok(())
            }}
            Some(ref sum) => (self as &mut Serialization<W, &'a {rust_name}>).serialize(sum, path)
        }}
    }}
}}
impl<'a, W> Serialization<W, &'a {rust_name}> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, value: &'a {rust_name}, path: &mut IOPath) -> Result<(), TokenWriterError> {{
        debug!(target: \"serialize_es6\", \"Serializing sum {rust_name}\");
        match *value {{
{variants}
        }}
    }}
}}
",
                        null = null_name,
                        null_interface = null_name.to_class_cases(),
                        rust_name = name.to_class_cases(),
                        variants = types
                            .iter()
                            .map(|case| {
                                format!(
"           {name}::{constructor}(ref value) => {{
                // Path will be updated by the serializer for this tagged tuple.
                (self as &mut Serialization<W, &'a {constructor}>).serialize(value, path)
            }}",
                                    name = name,
                                    constructor = case.to_class_cases())
                            })
                            .format(",\n")
                        );

                    let walk = format!("
impl<'a> Walker<'a> for {name} {{
    type Output = {name};
    fn walk<V, E, G: Default>(&'a mut self, path: &mut WalkPath, visitor: &mut V) -> Result<Option<{name}>, E> where V: Visitor<E, G> {{
        let mut walker : ViewMut{name} = self.into();
        walker.walk(path, visitor)
    }}
}}
impl<'a> Walker<'a> for ViewMut{name}<'a> where Self: 'a {{
    type Output = {name};
    fn walk<V, E, G: Default>(&'a mut self, path: &mut WalkPath, visitor: &mut V) -> Result<Option<{name}>, E> where V: Visitor<E, G> {{
        let me = self;
{supers}
        match visitor.enter_{snake}(path, me)? {{
            VisitMe::DoneHere => Ok(None),
            VisitMe::HoldThis(_guard) => {{
                let mut rewrite = match *me {{
{cases}
                }};
                match rewrite {{
                    None => visitor.exit_{snake}(path, me),
                    Some(ref mut r) => visitor.exit_{snake}(path, &mut r.into())
                }}
                // guard is now dropped
            }}
        }}
    }}
}}
",
                        supers = match rev_supersums.get(&node_name) {
                            None => "".to_string(),
                            Some(supers) => {
                                format!("{}", supers.iter()
                                    .map(|super_name| {
                                        format!("
        // Attempt to cast to `ViewMut{specialized}`.
        let me = match From::from(me) {{
            Ok(mut specialized) => {{
                let _ : ViewMut{specialized} = specialized;
                return specialized.walk(path, visitor)
                    .map(|ok| ok.map(|some| some.into()));
            }}
            Err(me) => me
        }};",
                                            specialized = super_name.to_class_cases())
                                    })
                                    .format("\n")
                                )
                            }
                        },
                        snake = name.to_rust_identifier_case(),
                        name = name,
                        cases = types
                            .iter()
                            .map(|case| {
                                format!("                   ViewMut{name}::{constructor}(ref mut value) =>
                        {{
                            let mut visit_mut : ViewMut{constructor} = (*value).into();
                            visit_mut.walk(path, visitor)?
                                .map(|rewrite| {name}::{constructor}(Box::new(rewrite)))
                        }}",
                                    name = name,
                                    constructor = case.to_class_cases())
                            })
                            .format(",\n")
                        );

                    buffer.push_str(&definition);
                    buffer.push_str(&default);
                    buffer.push_str(&single_variant_from);
                    buffer.push_str(&subsum_from);
                    buffer.push_str(&from_reader);
                    buffer.push_str(&to_writer);
                    buffer.push_str(&from_json);
                    buffer.push_str(&to_json);
                    // buffer.push_str(&into);
                    buffer.push_str(&walk);

                    buffer.push_str(&format!("
impl<'a> From<&'a mut {name}> for ViewMut{name}<'a> {{
    fn from(value: &'a mut {name}) -> ViewMut{name}<'a> {{
        match *value {{
{variants}
        }}
    }}
}}
",
                        name = name.to_class_cases(),
                        variants = types.iter()
                            .map(|variant| {
                                format!("            {name}::{variant}(ref mut x) => ViewMut{name}::{variant}(x),",
                                    name = name.to_class_cases(),
                                    variant = variant.to_class_cases(),
                                )
                            })
                            .format("\n")
                        )
                    );

                    if let Some(sub) = supersums.get(node_name) {
                        // Implement a `From` to convert `self` into `sub`.
                        let name = name.to_class_cases();
                        buffer.push_str(&format!("
{each}
",
                            each = sub.iter()
                                .map(|super_name| {
                                    let super_name = super_name.to_class_cases();
                                    format!("
impl<'a, 'b> From<&'a mut ViewMut{super_name}<'a>> for Result<ViewMut{name}<'b>, &'a mut ViewMut{super_name}<'a>> where 'a: 'b {{
    fn from(value: &'a mut ViewMut{super_name}<'a>) -> Result<ViewMut{name}<'b>, &'a mut ViewMut{super_name}<'a>> {{
        match *value {{
{variants}
            _ => Err(value)
        }}
    }}
}}
",
                                        name = name,
                                        super_name = super_name,
                                        variants = types.iter()
                                            .map(|variant| {
                                                format!("            ViewMut{super_name}::{variant}(ref mut x) => Ok(ViewMut{name}::{variant}(*x)),",
                                                    name = name,
                                                    super_name = super_name,
                                                    variant = variant.to_class_cases(),
                                                )
                                            })
                                            .format("\n")
                                        )
                                })
                                .format("\n")
                        ));
                    }

                    buffer.push_str("\n\n");
                } else {
                    panic!()
                }
            }

            buffer.push_str("\n\n// Aliases to primitive types (by lexicographical order)\n");
            // FromJSON/ToJSON are already implemented in `binjs::utils`
            for name in primitives.drain(..) {
                let typedef = source.get(name).unwrap();
                let source = format!(
"/// Alias to primitive type.
pub type {name} = {contents};

/// Shallow casting mechanism for {name}.
pub struct ViewMut{name}<'a>(&'a mut {name});
impl<'a> From<&'a mut {name}> for ViewMut{name}<'a> {{
    fn from(value: &'a mut {name}) -> Self {{
        ViewMut{name}(value)
    }}
}}
impl<'a> Walker<'a> for ViewMut{name}<'a> {{
    type Output = {name};
    fn walk<V, E, G: Default>(&'a mut self, _: &mut WalkPath, _: &mut V) -> Result<Option<Self::Output>, E> where V: Visitor<E, G> {{
        // Do not inspect the contents of a primitive.
        Ok(None)
    }}
}}
",
                    name = name.to_class_cases(),
                    contents = match *typedef.spec() {
                        TypeSpec::Boolean => "bool",
                        TypeSpec::Number => "f64",
                        TypeSpec::UnsignedLong => "u32",
                        TypeSpec::String => "binjs_shared::SharedString",
                        TypeSpec::Offset => "Offset",
                        TypeSpec::Void => "()",
                        _ => panic!("Unexpected type in alias to a primitive type: {name}",
                            name = name)
                    });
                buffer.push_str(&source);
            }
            buffer.push_str("\n\n// Aliases to list types (by lexicographical order)\n");
            // FromJSON/ToJSON are already implemented in `binjs::utils`
            for name in lists.drain(..) {
                let typedef = source.get(name).unwrap();
                if let TypeSpec::Array {
                    ref contents,
                    ref supports_empty,
                } = *typedef.spec()
                {
                    if let TypeSpec::NamedType(ref contents) = *contents.spec() {
                        // FunctionBody's implementation is already
                        // emitted as ListOfStatement.
                        // Emit only ViewMut* to avoid duplicate definitions.
                        // FIXMEarai: Use more generic way to detect such case.
                        if name.to_string() == "FunctionBody" {
                            let source = format!(
                                "{empty_check}pub type {name} = Vec<{contents}>;
pub type ViewMut{name}<'a> = ViewMutListOfStatement<'a>;
",
                                empty_check = if *supports_empty {
                                    ""
                                } else {
                                    "// FIXME: Should discard empty vectors.\n"
                                },
                                name = name.to_class_cases(),
                                contents = contents.to_class_cases()
                            );
                            buffer.push_str(&source);
                            continue;
                        }

                        let source = format!("
/// Implementation of list type {name}.
{empty_check}pub type {name} = Vec<{contents}>;

/// Shallow casting mechanism.
pub struct ViewMut{name}<'a>(&'a mut {name});
impl<'a> From<&'a mut {name}> for ViewMut{name}<'a> {{
    fn from(value: &'a mut {name}) -> Self {{
        ViewMut{name}(value)
    }}
}}
impl<'a> Walker<'a> for {name} {{
    type Output = {name};
    fn walk<V, E, G: Default>(&'a mut self, path: &mut WalkPath, visitor: &mut V) -> Result<Option<{name}>, E> where V: Visitor<E, G> {{
        let mut walker : ViewMut{name} = self.into();
        walker.walk(path, visitor)
    }}
}}
impl<'a> Walker<'a> for ViewMut{name}<'a> {{
    type Output = {name};
    fn walk<V, E, G: Default>(&'a mut self, path: &mut WalkPath, visitor: &mut V) -> Result<Option<Self::Output>, E> where V: Visitor<E, G> {{
        // Do not callback on the `Vec<>` itself, just on its contents.
        for iter in self.0.iter_mut() {{
            let rewrite = {{
                let mut specialized : ViewMut{contents} = iter.into();
                specialized.walk(path, visitor)?
            }};
            if let Some(rewrite) = rewrite {{
                *iter = rewrite;
            }}
        }}
        Ok(None)
    }}
}}


impl<'a, W> Serialization<W, &'a {name}> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, value: &'a {name}, path: &mut IOPath) -> Result<(), TokenWriterError> {{
        debug!(target: \"serialize_es6\", \"Serializing list {name}\");
        self.writer.enter_list_at(value.len(), path)?;
        for child in value {{
            // All the children of the list share the same path.
            self.serialize(child, path)?;
        }}
        self.writer.exit_list_at(path)?;
        Ok(())
    }}
}}
",
                            empty_check = if *supports_empty { "" } else { "// FIXME: Should discard empty vectors.\n" },
                            name = name.to_class_cases(),
                            contents = contents.to_class_cases());
                        buffer.push_str(&source);
                        continue;
                    }
                }
                panic!(
                    "Could not implement alias to list type {name}: {contents:?}",
                    contents = typedef.spec(),
                    name = name
                );
            }
            buffer.push_str("\n\n// Aliases to optional types (by lexicographical order)\n");
            // FromJSON/ToJSON are already implemented in `binjs::utils`
            for name in options.drain(..) {
                let typedef = source.get(name).unwrap();
                if let TypeSpec::NamedType(ref contents) = *typedef.spec() {
                    let source = format!(
"/// Alias to optional type type.
pub type {name} = Option<{contents}>;\n

/// Shallow casting mechanism.
pub struct ViewMut{name}<'a>(&'a mut {name});
impl<'a> From<&'a mut {name}> for ViewMut{name}<'a> {{
    fn from(value: &'a mut {name}) -> Self {{
        ViewMut{name}(value)
    }}
}}
impl<'a> Walker<'a> for ViewMut{name}<'a> {{
    type Output = {name};
    fn walk<V, E, G: Default>(&'a mut self, path: &mut WalkPath, visitor: &mut V) -> Result<Option<Self::Output>, E> where V: Visitor<E, G> {{
        // Do not callback on the `Option<>` itself, just on its contents.
        if let Some(ref mut contents) = self.0 {{
            let result =
            {{
                let mut walker : ViewMut{contents} = contents.into();
                walker.walk(path, visitor)?
            }};
            if let Some(replacement) = result {{
                *contents = replacement;
            }}
        }}
        Ok(None)
    }}
}}

",
                        name = name.to_class_cases(),
                        contents = contents.to_class_cases());
                    buffer.push_str(&source);
                } else {
                    panic!();
                }
            }
        }
        fn print_ast_interfaces(
            buffer: &mut String,
            source: &HashMap<NodeName, Rc<Interface>>,
            null_name: &str,
        ) {
            let names: Vec<_> = source.keys().sorted();
            for name in &names {
                let interface = source.get(name).unwrap();
                let rust_name = name.to_class_cases();
                let field_specs: Vec<_> = interface
                    .contents()
                    .fields()
                    .iter()
                    .map(|field| {
                        let spec = if field.type_().is_optional() {
                            TypeName::type_(field.type_())
                        } else {
                            match *field.type_().spec() {
                                TypeSpec::NamedType(ref contents) => contents.to_class_cases(),
                                TypeSpec::Boolean => "bool".to_string(),
                                TypeSpec::Number => "f64".to_string(),
                                TypeSpec::UnsignedLong => "u32".to_string(),
                                TypeSpec::String => "String".to_string(),
                                TypeSpec::Void => "()".to_string(),
                                TypeSpec::Offset => "Offset".to_string(),
                                _ => TypeName::type_(field.type_()),
                            }
                        };
                        (field.name(), spec)
                    })
                    .collect();
                let field_specs_map: HashMap<_, _> =
                    field_specs.iter().map(|(a, b)| (a.clone(), b)).collect();
                let definition = format!(
                    "
/// Implementation of interface {spec_name}.
#[derive(Default, PartialEq, Debug, Clone)]
pub struct {rust_name} {{
{fields}
}}

/// Make it possible to downcast/upcast between {rust_name} and `binjs_shared::ast::Node`.
impl binjs_shared::Node for {rust_name} {{

    /// The name of this node. Used mostly for debugging purposes.
    fn name(&self) -> &'static str {{
        \"{rust_name}\"
    }}
}}
",
                    fields = field_specs
                        .iter()
                        .map(|(field_name, spec)| format!(
                            "    /// Implementation of field {spec_name}
    pub {rust_name}: {contents}",
                            rust_name = field_name.to_rust_identifier_case(),
                            spec_name = field_name.to_str(),
                            contents = spec
                        ))
                        .format(",\n"),
                    rust_name = rust_name,
                    spec_name = name
                );

                let from_reader = format!("
impl<R> Deserializer<R> where R: TokenReader {{
    fn deserialize_tuple_{lowercase_name}(&mut self, path: &mut IOPath) -> Result<{rust_name}, TokenReaderError> where R: TokenReader {{
        let (interface_name, _) = self.reader.enter_tagged_tuple_at(path)?;
        let result =
            if let \"{name}\" = interface_name.as_str() {{
                debug!(target: \"deserialize_es6\", \"Deserializing tagged tuple {name}: present\");
                path.enter_interface(interface_name.clone());
                let result = self.deserialize_inner(path);
                path.exit_interface(interface_name);
                result
            }} else {{
                debug!(target: \"deserialize_es6\", \"Deserializing tagged tuple {name}: found invalid {{}}\", interface_name.as_str());
                Err(From::from(TokenReaderError::BadEnumVariant))
            }};
        if result.is_err() {{
            debug!(target: \"deserialize_es6\", \"Deserializing tagged tuple {name}: failed\");
            self.reader.poison();
        }}
        debug!(target: \"deserialize_es6\", \"Deserializing tagged tuple {name}: finalizing\");
        self.reader.exit_tagged_tuple_at(path)?;
        debug!(target: \"deserialize_es6\", \"Deserializing tagged tuple {name}: done\");
        result
    }}
}}

impl<R> InnerDeserialization<R, {rust_name}> for Deserializer<R> where R: TokenReader {{
    fn deserialize_inner(&mut self, path: &mut IOPath) -> Result<{rust_name}, TokenReaderError> where R: TokenReader {{
        let _ = path; // Deactivate warnings if there are no fields.
        print_file_structure!(self.reader, \"{name} {{{{\");
{fields_def}
        print_file_structure!(self.reader, \"}}}}\");
        Ok({rust_name} {{
{fields_use}
        }})
    }}
}}

impl<R> Deserialization<R, {rust_name}> for Deserializer<R> where R: TokenReader {{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<{rust_name}, TokenReaderError> {{
        debug!(target: \"deserialize_es6\", \"Deserializing tagged tuple {rust_name}\");
        self.deserialize_tuple_{lowercase_name}(path)
    }}
}}
impl<R> Deserialization<R, Option<{rust_name}>> for Deserializer<R> where R: TokenReader {{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<{rust_name}>, TokenReaderError> {{
        debug!(target: \"deserialize_es6\", \"Deserializing optional tuple {rust_name}\");
        let (kind, _) = self.reader.enter_tagged_tuple_at(path)?;
        let result = match kind.as_str() {{
            \"{rust_name}\" => {{
                let path_interface = kind.clone();
                debug!(target: \"deserialize_es6\", \"Deserializing optional tuple {rust_name}: present\");
                path.enter_interface(path_interface.clone());
                let result = self.deserialize_inner(path).map(Some);
                path.exit_interface(path_interface);
                result
            }}
            \"{null}\" => {{
                debug!(target: \"deserialize_es6\", \"Deserializing optional tuple {rust_name}: absent\");
                print_file_structure!(self.reader, \"{name} : None\");
                Ok(None)
            }},
            _ => {{
                error!(target: \"deserialize_es6\", \"Deserializing optional tuple {rust_name}, found invalid {{}}\", kind.as_str());
                Err(From::from(TokenReaderError::BadEnumVariant))
            }}
        }};
        if result.is_err() {{
            self.reader.poison();
        }}
        self.reader.exit_tagged_tuple_at(path)?;
        result
    }}
}}
",
                    name = name,
                    null = null_name,
                    rust_name = rust_name,
                    lowercase_name = name.to_rust_identifier_case()
                        .trim_end_matches('_'),
                    fields_def = interface.contents()
                        .fields()
                        .iter()
                        .enumerate()
                        .map(|(index, field)| format!("
        print_file_structure!(self.reader, \".{field_name}\");
        let path_field = ({index}, FieldName::from_str(\"{field_name}\")); // String is shared
        path.enter_field(path_field.clone());
        let data_{rust_field_name} = self.deserialize(path) as Result<{spec}, TokenReaderError>;
        path.exit_field(path_field);
        let data_{rust_field_name} = data_{rust_field_name}?;
",
                            rust_field_name = field.name().to_rust_identifier_case(),
                            field_name = field.name().to_str(),
                            spec = field_specs_map.get(field.name()).unwrap(),
                            index = index))
                        .format("\n"),
                    fields_use = interface.contents()
                        .fields()
                        .iter()
                        .map(|field| format!("            {name}: data_{name},",
                            name = field.name().to_rust_identifier_case()))
                        .format("\n")
                    );
                let to_writer = format!("
impl<'a, W> Serialization<W, &'a Option<{rust_name}>> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, value: &'a Option<{rust_name}>, path: &mut IOPath) -> Result<(), TokenWriterError> {{
        debug!(target: \"serialize_es6\", \"Serializing optional tagged tuple {name}\");
        match *value {{
            None => {{
                let interface_name = InterfaceName::from_str(\"{null}\");
                self.writer.enter_tagged_tuple_at(&{null_interface}{{}}, &interface_name, &[], path)?;
                self.writer.exit_tagged_tuple_at(&{null_interface}{{}}, &interface_name, &[], path)?;
                Ok(())
            }}
            Some(ref sum) => (self as &mut Serialization<W, &'a {rust_name}>).serialize(sum, path)
        }}
    }}
}}
impl<'a, W> Serialization<W, &'a {rust_name}> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, value: &'a {rust_name}, path: &mut IOPath) -> Result<(), TokenWriterError> {{
        debug!(target: \"serialize_es6\", \"Serializing tagged tuple {name}\");
        let interface_name = InterfaceName::from_str(\"{name}\"); // String is shared
        let field_names = [{field_names}];

        self.writer.enter_tagged_tuple_at(value, &interface_name, &field_names, path)?;
        path.enter_interface(interface_name.clone());
        let result = loop {{ // Fake loop, used only to be able to `break` early.
{fields}
            break Ok(());
        }};
        path.exit_interface(interface_name.clone());
        result?;
        self.writer.exit_tagged_tuple_at(value, &interface_name, &field_names, path)?;

        Ok(())
    }}
}}
",
                        null = null_name,
                        null_interface = null_name.to_class_cases(),
                        name = name,
                        rust_name = rust_name,
                        field_names = interface.contents()
                            .fields()
                            .iter()
                            .map(|field| format!("&FieldName::from_str(\"{field_name}\")",
                                field_name = field.name().to_str()))
                            .format(", "),
                        fields = interface.contents()
                            .fields()
                            .iter()
                            .enumerate()
                            .map(|(index, field)| format!(
"
            let field_name = FieldName::from_str(\"{field_name}\");
            let path_item = ({index}, field_name.clone()); // String is shared
            path.enter_field(path_item.clone());
            let result = (self as &mut Serialization<W, &'a _>).serialize(&value.{rust_field_name}, path);
            path.exit_field(path_item);
            if let Err(err) = result {{
                break Err(err); // Break with error
            }}",
                                index = index,
                                field_name = field.name().to_str(),
                                rust_field_name = field.name().to_rust_identifier_case()))
                            .format("\n")
                    );

                let from_json = format!(
                    "
impl FromJSON for {rust_name} {{
    fn import(value: &JSON) -> Result<Self, FromJSONError> {{
        match value[\"type\"].as_str() {{
            Some(\"{kind}\") => {{ /* Good */ }},
            _ => return Err(FromJSONError {{
                expected: \"Instance of {kind}\".to_string(),
                got: value.dump()
            }})
        }}
        Ok({rust_name} {{
{fields}
        }})
    }}
}}\n\n",
                    kind = name,
                    rust_name = rust_name,
                    fields = interface
                        .contents()
                        .fields()
                        .iter()
                        .map(|field| format!(
                            "            {name}: FromJSON::import(&value[\"{key}\"])?,\n",
                            key = field.name().to_str(),
                            name = field.name().to_rust_identifier_case()
                        ))
                        .format("")
                );

                let to_json = format!(
                    "
impl ToJSON for {rust_name} {{
    fn export(&self) -> JSON {{
        object!{{
            \"type\" => json::from(\"{kind}\"),
{fields}
        }}
    }}
}}\n\n",
                    kind = name,
                    rust_name = rust_name,
                    fields = interface
                        .contents()
                        .fields()
                        .iter()
                        .map(|field| format!(
                            "             \"{key}\" => self.{name}.export()",
                            key = field.name().to_str(),
                            name = field.name().to_rust_identifier_case()
                        ))
                        .format(",\n")
                );

                let walk = format!("
/// Shallow casting mechanism.
pub struct ViewMut{rust_name}<'a>(&'a mut {rust_name});
impl<'a> From<&'a mut {rust_name}> for ViewMut{rust_name}<'a> {{
    fn from(value: &'a mut {rust_name}) -> Self {{
        ViewMut{rust_name}(value)
    }}
}}
impl<'a> Walker<'a> for {rust_name} {{
    type Output = {rust_name};
    fn walk<V, E, G: Default>(&'a mut self, path: &mut WalkPath, visitor: &mut V) -> Result<Option<{rust_name}>, E> where V: Visitor<E, G> {{
        let mut walker : ViewMut{rust_name} = self.into();
        walker.walk(path, visitor)
    }}
}}
impl<'a> Walker<'a> for ViewMut{rust_name}<'a> where Self: 'a {{
    type Output = {rust_name};
    fn walk<V, E, G: Default>(&'a mut self, path: &mut WalkPath, visitor: &mut V) -> Result<Option<Self::Output>, E> where V: Visitor<E, G> {{
        path.enter_interface(ASTNode::{rust_name});
        match visitor.enter_{snake}(path, self.0)? {{
            VisitMe::DoneHere => Ok(None),
            VisitMe::HoldThis(_guard) => {{
{fields}
                let result = visitor.exit_{snake}(path, self.0)?;
                path.exit_interface(ASTNode::{rust_name});
                Ok(result)
                // guard is now dropped
            }}
        }}
    }}
}}
",
                    rust_name = rust_name,
                    snake = name.to_rust_identifier_case(),
                    fields = field_specs
                        .iter()
                        .map(|(field_name, field_spec)| {
                            format!("                 {{
                    path.enter_field(ASTField::{variant});
                    let result = {{
                        let mut walker : ViewMut{spec} = (&mut self.0.{name}).into();
                        walker.walk(path, visitor)?
                    }};
                    if let Some(replacement) = result {{
                        self.0.{name} = replacement;
                    }}
                    path.exit_field(ASTField::{variant});
                }}",
                                name = field_name.to_rust_identifier_case(),
                                variant = field_name.to_class_cases(),
                                spec = field_spec.to_class_cases()
                            )
                        })
                        .format("\n")
                    );
                buffer.push_str(&definition);
                buffer.push_str(&from_reader);
                buffer.push_str(&to_writer);
                buffer.push_str(&from_json);
                buffer.push_str(&to_json);
                buffer.push_str(&walk);
                buffer.push_str("\n\n\n");
            }

            let interfaces_enum = format!(
                "
/// All the interfaces.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]\npub enum ASTNode {{
{interfaces}
}}\n\n\n",
                interfaces = names
                    .iter()
                    .map(|name| format!(
                        "    /// {name}
    {rust_name}",
                        name = name,
                        rust_name = name.to_class_cases()
                    ))
                    .format(",\n")
            );

            buffer.push_str(&interfaces_enum);
        }

        fn print_visitor(
            buffer: &mut String,
            interfaces: &HashMap<NodeName, Rc<Interface>>,
            typedefs: &HashMap<NodeName, Rc<Type>>,
        ) {
            let path = "
/// A PathItem, used when walking the tree with the strongly-typed `Walker` API.
pub type WalkPathItem = binjs_shared::ast::PathItem<ASTNode, ASTField>;
/// A Path, used when walking the tree with the strongly-typed `Walker` API.
pub type WalkPath = binjs_shared::ast::Path<ASTNode, ASTField>;

/// A Path, used when walking the tree with more weakly-typed APIs, e.g. TokenReader/TokenWriter.
pub type IOPath = binjs_shared::ast::Path<binjs_shared::InterfaceName, ( /* child index */ usize, /* field name */ binjs_shared::FieldName)>;
";
            let mut interface_names = interfaces.keys().sorted();
            let mut sum_names = typedefs
                .iter()
                .filter_map(|(name, typedef)| {
                    if typedef.is_optional() {
                        return None;
                    }
                    if let TypeSpec::TypeSum(_) = typedef.spec() {
                        return Some(name);
                    }
                    return None;
                })
                .sorted();

            let visitor = format!("
/// A set of callbacks used to inspect the contents of an AST in a strongly-typed
/// manner. For each node `Foo`, `enter_foo()` will be called before visiting the
/// children, giving the opportunity to alter the node, and `enter_foo()` will be
/// called after visiting the children, giving the opportunity to alter it further.
///
/// Each of the nodes of this AST implements `Walker` and may be visited recursively
/// using `Visitor`.
///
/// Type argument `G` is a type of guards.
pub trait Visitor<E, G=()> where G: Default {{
{interfaces}
{sums}
   fn visit_offset(&mut self, _path: &WalkPath, _node: &mut Offset) -> Result<(), E> {{
        Ok(())
    }}
}}\n
pub trait Walker<'a>: Sized {{
    type Output;
    fn walk<V, E, G: Default>(&'a mut self, path: &mut WalkPath, visitor: &mut V) -> Result<Option<Self::Output>, E> where V: Visitor<E, G>;
}}\n

/// A structure that cannot be visited.
#[derive(Default)]
struct ViewMutNothing<T> {{
    nothing: std::marker::PhantomData<T>
}}
impl<'a, T> Walker<'a> for ViewMutNothing<T> {{
    type Output = T;
    fn walk<V, E, G: Default>(&'a mut self, _: &mut WalkPath, _: &mut V) -> Result<Option<Self::Output>, E> where V: Visitor<E, G> {{
        // Do not inspect the contents of a nothing.
        Ok(None)
    }}
}}

type ViewMutBool = ViewMutNothing<bool>;
impl<'a> From<&'a mut bool> for ViewMutNothing<bool> {{
    fn from(_: &'a mut bool) -> Self {{
        ViewMutNothing::default()
    }}
}}
impl<'a> Walker<'a> for bool {{
    type Output = Self;
    fn walk<V, E, G: Default>(&'a mut self, _: &mut WalkPath, _: &mut V) -> Result<Option<Self>, E> where V: Visitor<E, G> {{
        // Do not inspect the contents of a bool.
        Ok(None)
    }}
}}
type ViewMutF64 = ViewMutNothing<f64>;
impl<'a> From<&'a mut f64> for ViewMutNothing<f64> {{
    fn from(_: &'a mut f64) -> Self {{
        ViewMutNothing::default()
    }}
}}
impl<'a> Walker<'a> for f64 {{
    type Output = Self;
    fn walk<V, E, G: Default>(&'a mut self, _: &mut WalkPath, _: &mut V) -> Result<Option<Self>, E> where V: Visitor<E, G> {{
        // Do not inspect the contents of a f64.
        Ok(None)
    }}
}}
type ViewMutU32 = ViewMutNothing<u32>;
impl<'a> From<&'a mut u32> for ViewMutNothing<u32> {{
    fn from(_: &'a mut u32) -> Self {{
        ViewMutNothing::default()
    }}
}}
impl<'a> Walker<'a> for u32 {{
    type Output = Self;
    fn walk<V, E, G: Default>(&'a mut self, _: &mut WalkPath, _: &mut V) -> Result<Option<Self>, E> where V: Visitor<E, G> {{
        // Do not inspect the contents of a u32.
        Ok(None)
    }}
}}

// We actually use ViewMutOffset to reset offsets to 0 during tests.
pub struct ViewMutOffset<'a>(&'a mut Offset);
impl<'a> From<&'a mut Offset> for ViewMutOffset<'a> {{
    fn from(value: &'a mut Offset) -> Self {{
        ViewMutOffset(value)
    }}
}}
impl<'a> Walker<'a> for ViewMutOffset<'a> {{
    type Output = Offset;
    fn walk<V, E, G: Default>(&'a mut self, path: &mut WalkPath, visitor: &mut V) -> Result<Option<Self::Output>, E> where V: Visitor<E, G> {{
        visitor.visit_offset(path, &mut self.0)?;
        Ok(None)
    }}
}}

type ViewMutIdentifierName = ViewMutNothing<IdentifierName>;
impl<'a> From<&'a mut IdentifierName> for ViewMutNothing<IdentifierName> {{
    fn from(_: &'a mut IdentifierName) -> Self {{
        ViewMutNothing::default()
    }}
}}


type ViewMutPropertyKey = ViewMutNothing<PropertyKey>;
impl<'a> From<&'a mut PropertyKey> for ViewMutNothing<PropertyKey> {{
    fn from(_: &'a mut PropertyKey) -> Self {{
        ViewMutNothing::default()
    }}
}}
\n\n\n",
                interfaces = interface_names
                    .drain(..)
                    .map(|name| {
                        let interface = interfaces.get(&name).unwrap();
                        format!("
    fn enter_{name}(&mut self, _path: &WalkPath, _node: &mut {node_name}) -> Result<VisitMe<G>, E> {{
        Ok(VisitMe::HoldThis(G::default()))
    }}
    fn exit_{name}(&mut self, _path: &WalkPath, _node: &mut {node_name}) -> Result<Option<{node_name}>, E> {{
        Ok(None)
    }}
",
                            name = name.to_rust_identifier_case(),
                            node_name = interface.name().to_class_cases())
                    })
                    .format("\n"),
                sums = sum_names
                    .drain(..)
                    .map(|name| {
                        format!("
    fn enter_{name}<'a>(&mut self, _path: &WalkPath, _node: &mut ViewMut{node_name}<'a>) -> Result<VisitMe<G>, E> {{
        Ok(VisitMe::HoldThis(G::default()))
    }}
    fn exit_{name}<'a>(&mut self, _path: &WalkPath, _node: &mut ViewMut{node_name}<'a>) -> Result<Option<{node_name}>, E> {{
        Ok(None)
    }}
",
                            name = name.to_rust_identifier_case(),
                            node_name = name.to_class_cases())
                    })
                    .format("\n"),
            );

            buffer.push_str(&path);
            buffer.push_str(&visitor);
        }

        struct_buffer.push_str("    // String enum names (by lexicographical order)\n");
        impl_buffer.push_str("            // String enum names (by lexicographical order)\n");
        ast_buffer.push_str("// String enums (by lexicographical order)\n");
        print_struct_names(&mut struct_buffer, self.spec.string_enums_by_name().keys());
        print_impl_names(&mut impl_buffer, self.spec.string_enums_by_name().keys());
        print_ast_string_enums(&mut ast_buffer, deanonymized.string_enums_by_name());

        struct_buffer.push_str("\n\n    // Typedef names (by lexicographical order)\n");
        impl_buffer.push_str("\n\n            // Typedef names (by lexicographical order)\n");
        print_struct_names(&mut struct_buffer, self.spec.typedefs_by_name().keys());
        print_impl_names(&mut impl_buffer, self.spec.typedefs_by_name().keys());
        print_ast_typedefs(
            &mut ast_buffer,
            deanonymized.typedefs_by_name(),
            &supersums_of,
            self.spec.get_null_name().to_str(),
        );

        struct_buffer.push_str("\n\n    // Interface names (by lexicographical order)\n");
        impl_buffer.push_str("\n\n            // Interface names (by lexicographical order)\n");
        ast_buffer.push_str("\n\n// Interfaces and interface names (by lexicographical order)\n");
        print_struct_names(&mut struct_buffer, self.spec.interfaces_by_name().keys());
        print_impl_names(&mut impl_buffer, self.spec.interfaces_by_name().keys());
        print_ast_interfaces(
            &mut ast_buffer,
            deanonymized.interfaces_by_name(),
            self.spec.get_null_name().to_str(),
        );
        print_visitor(
            &mut ast_buffer,
            deanonymized.interfaces_by_name(),
            deanonymized.typedefs_by_name(),
        );

        struct_buffer.push_str("\n\n\n    // Field names (by lexicographical order)\n");
        impl_buffer.push_str("\n\n\n            // Field names (by lexicographical order)\n");
        ast_buffer.push_str(
            "\n\n\n// Field names (by lexicographical order)
/// All the fields.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]\npub enum ASTField {\n",
        );
        let mut fields = HashSet::new();
        for interface in self.spec.interfaces_by_name().values() {
            for field in interface.contents().fields() {
                fields.insert(field.name().to_string().clone());
                if field.is_lazy() {
                    fields.insert(format!("{}_skip", field.name().to_string().clone()));
                }
            }
        }
        let fields: Vec<_> = fields.drain().sorted();
        for name in fields {
            let snake = name.to_rust_identifier_case();
            let struct_source = format!("    pub field_{snake}: FieldName,\n", snake = snake);
            struct_buffer.push_str(&struct_source);

            let impl_source = format!(
                "            field_{snake}: builder.field_name(\"{original}\"),\n",
                snake = snake,
                original = name
            );
            impl_buffer.push_str(&impl_source);

            let ast_source = format!("    {variant},\n", variant = name.to_class_cases());
            ast_buffer.push_str(&ast_source);
        }

        impl_buffer.push_str("        };\n");
        struct_buffer.push_str("}");
        ast_buffer.push_str("}\n");

        impl_buffer.push_str("\n\n\n        // Enumerations\n");
        for (name, def) in self.spec.string_enums_by_name() {
            let strings = format!(
                "{strings}",
                strings = def
                    .strings()
                    .iter()
                    .map(|s| format!("                \"{s}\"", s = s))
                    .format(",\n")
            );
            let impl_source = format!(
                "        builder.add_string_enum(&names.{name}).unwrap()
            .with_strings(&[\n{strings}\n           ]);\n\n",
                name = name.to_rust_identifier_case(),
                strings = strings
            );
            impl_buffer.push_str(&impl_source);
        }
        for (name, def) in self.spec.typedefs_by_name() {
            let impl_source = format!(
                "        builder.add_typedef(&names.{name}).unwrap()
            .with_type(\n{spec});\n\n",
                name = name.to_rust_identifier_case(),
                spec = Self::type_(def, "                    ")
            );
            impl_buffer.push_str(&impl_source);
        }
        for (name, def) in self.spec.interfaces_by_name() {
            let fields = format!("{fields}",
                fields = def.contents()
                    .fields()
                    .iter()
                    .map(|field| format!("            .{method}(\n                 &names.field_{name},\n{type_}\n            )",
                        method = if field.is_lazy() {
                            "with_field_lazy"
                        } else {
                            "with_field"
                        },
                        name = field.name().to_rust_identifier_case(),
                        type_= Self::type_(field.type_(), "                 "),
                    ))
                    .format("\n"));
            let impl_source = format!(
                "        builder.add_interface(&names.{name}).unwrap()\n{fields};\n\n",
                name = name.to_rust_identifier_case(),
                fields = fields
            );
            impl_buffer.push_str(&impl_source);
        }

        impl_buffer.push_str("        names\n    }\n");
        impl_buffer.push_str(
            "
}

impl Library {
    /// Insert scope annotations in the scope.
    pub fn annotate(&self, ast: &mut JSON) {
        use binjs_es6;
        let mut visitor = binjs_es6::scopes::AnnotationVisitor::new();
        visitor.annotate(ast);
    }
    pub fn lazify(&self, level: u32, ast: &mut JSON) {
        use binjs_es6;
        let mut visitor = binjs_es6::lazy::LazifierVisitor::new(level);
        visitor.annotate(ast);
    }
}

impl Annotator for Library {
    fn annotate(&self, ast: &mut JSON) {
        Library::annotate(self, ast)
    }
}
",
        );
        ExportedSource {
            typed: format!("// This file was generated by binjs_meta generate_library.\n{ast_}\n",
                ast_ = ast_buffer),
            generic: format!("// This file was generated by binjs_meta generate_library.\npub use annotate::Annotator;\nuse binjs_meta::spec::*;\nuse json::JsonValue as JSON;\n\n\n{struct_}\n{impl_}",
                struct_ = struct_buffer,
                impl_ = impl_buffer)
        }
    }
}
