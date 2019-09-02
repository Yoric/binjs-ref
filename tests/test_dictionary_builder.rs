extern crate binjs;
extern crate itertools;

use binjs::generic::{IdentifierName, InterfaceName, Offset, PropertyKey, SharedString};
use binjs::io::entropy;
use binjs::io::entropy::dictionary::{
    DictionaryBuilder, FilesContaining, LinearTable, Options as DictionaryOptions,
};
use binjs::io::entropy::rw::TableRefStreamState;
use binjs::io::{Deserialization, Serialization, TokenSerializer};
use binjs::source::{Shift, SourceParser};
use binjs::specialized::es6::ast::{Script, Visitor, WalkPath, Walker};
use binjs::specialized::es6::io::IOPath;

use std::collections::HashMap;

use itertools::Itertools;

const DEPTH: usize = 2;
const WIDTH: usize = 32;

#[macro_use]
extern crate test_logger;

/// A visitor designed to reset offsets to 0.
struct OffsetCleanerVisitor;
impl Visitor<()> for OffsetCleanerVisitor {
    fn visit_offset(&mut self, _path: &WalkPath, node: &mut Offset) -> Result<(), ()> {
        *node = binjs::generic::Offset(0);
        Ok(())
    }
}

test!(test_entropy_roundtrip, {
    let parser = Shift::try_new().expect("Could not launch Shift");

    let dict_sources = [
        "var x = y",
        "let x = y",
        "'use strict'",
        "function foo(x, y) { var i; for (i = 0; i < 100; ++i) { console.log('Some text', x, y + i, x + y + i, x + y + i + 1); } }",
        "function foo() { if (Math.PI != 3) console.log(\"That's alright\"); }",
    ];

    let mut builder = DictionaryBuilder::new(
        DictionaryOptions::default()
            .with_depth(DEPTH)
            .with_width(WIDTH),
    );
    for source in &dict_sources {
        println!("Parsing");
        let mut ast = parser.parse_str(source).expect("Could not parse source");

        println!("Annotating");
        let enricher = binjs::specialized::es6::Enrich::default();
        enricher.enrich(&mut ast).expect("Could not enrich AST");

        println!("Extracting dictionary");
        let mut serializer = binjs::specialized::es6::io::Serializer::new(&mut builder);
        let mut path = IOPath::new();
        serializer
            .serialize(&ast, &mut path)
            .expect("Could not walk");
        let _ = serializer.done().expect("Could not walk");
    }

    println!("* Checking identifiers per file");
    check_strings(
        &builder.files_containing().identifier_name_instances,
        vec![
            ("Math", 1),
            ("console", 2),
            ("foo", 2),
            ("i", 1),
            ("x", 3),
            ("y", 3),
        ],
        |name| Some(IdentifierName::from_string(name.to_string())),
    );

    println!("* Checking property keys per file");
    check_strings(
        &builder.files_containing().property_key_instances,
        vec![("PI", 1), ("log", 2)],
        |name| Some(PropertyKey::from_string(name.to_string())),
    );

    println!("* Checking interface names per file");
    check_strings(
        &builder.files_containing().interface_name_instances,
        vec![
            ("", 2), // FIXME: Where is this `null`?
            ("AssertedBlockScope", 1),
            ("AssertedDeclaredName", 4),
            ("AssertedParameterScope", 2),
            ("AssertedPositionalParameterName", 1), // FIXME: Where is this?
            ("AssertedScriptGlobalScope", 5),
            ("AssertedVarScope", 2),
            ("AssignmentExpression", 1),
            ("AssignmentTargetIdentifier", 1),
            ("BinaryExpression", 2),
            ("BindingIdentifier", 4),
            ("Block", 1),
            ("CallExpression", 2),
            ("Directive", 1),
            ("EagerFunctionDeclaration", 2),
            ("ExpressionStatement", 2),
            ("ForStatement", 1),
            ("FormalParameters", 2),
            ("FunctionOrMethodContents", 2),
            ("IdentifierExpression", 4),
            ("IfStatement", 1),
            ("LiteralNumericExpression", 2),
            ("LiteralStringExpression", 2),
            ("Script", 5),
            ("StaticMemberExpression", 2),
            ("UpdateExpression", 1),
            ("VariableDeclaration", 3),
            ("VariableDeclarator", 3),
        ],
        |name| InterfaceName::from_string(name.to_string()),
    );

    println!("* String literals per file");
    check_strings(
        &builder.files_containing().string_literal_instances,
        vec![("Some text", 1), ("That\'s alright", 1), ("use strict", 1)],
        |value| Some(SharedString::from_string(value.to_string())),
    );

    // We may now access data.
    let dictionary = builder.done(0.into() /* Keep all user-extensible data */);
    println!("Built a dictionary with {} states", dictionary.len());

    // Spec to generate the fallback dictionary.
    let spec = binjs::generic::es6::Library::spec();

    // Also create an empty dictionary family.
    let empty_dictionary = {
        let builder = DictionaryBuilder::new(
            DictionaryOptions::default()
                .with_depth(DEPTH)
                .with_width(WIDTH),
        );
        builder.done(0.into())
    };

    for (name, maybe_dictionary) in
        vec![("sampled", dictionary), ("empty", empty_dictionary)].into_iter()
    {
        println!("** Testing with {} dictionary", name);
        let options = entropy::Options::new(&spec, maybe_dictionary);

        println!("Starting roundtrip with the sources that were used to build thhe dictionary");
        for source in &dict_sources {
            test_with_options(&parser, source, &options);
        }

        println!(
            "Starting roundtrip with user-extensible values that do not show up in the dictionary"
        );
        let out_of_dictionary = [
            "var z = x + y;",
            "'use asm';",
            "function not_in_the_dictionary() {}",
            "function foo() { if (Math.E != 4) console.log(\"That's also normal.\"); }",
        ];
        for source in &out_of_dictionary {
            test_with_options(&parser, source, &options);
        }

        println!("Starting roundtrip with grammar constructions that have a probability of 0 in the dictionary");
        let out_of_dictionary = [
            "let z = y",
            "let foo = function () { }",
            "(function() { while (false) { console.log('What am I doing here?')} })",
        ];
        for source in &out_of_dictionary {
            test_with_options(&parser, source, &options);
        }
    }
});

fn test_with_options<S>(parser: &S, source: &str, options: &entropy::Options)
where
    S: SourceParser<Script>,
{
    println!("Parsing with dictionary: {}", source);
    let mut reference = parser.parse_str(source).expect("Could not parse source");

    println!("Reannotating");
    let enricher = binjs::specialized::es6::Enrich::default();
    enricher
        .enrich(&mut reference)
        .expect("Could not enrich AST");

    let mut path = IOPath::new();

    println!("Serializing with entropy");
    let encoder = entropy::write::Encoder::new(None, options.clone());
    let mut serializer = binjs::specialized::es6::io::Serializer::new(encoder);
    serializer
        .serialize(&reference, &mut path)
        .expect("Could not walk");
    let data = serializer.done().expect("Could not walk");
    assert_eq!(path.len(), 0);

    println!("Deserializing {} bytes with entropy", data.len());
    let decoder = entropy::read::Decoder::new(&options.clone(), std::io::Cursor::new(data))
        .expect("Could not create decoder");
    let mut deserializer = binjs::specialized::es6::io::Deserializer::new(decoder);
    let mut extracted: Script = deserializer
        .deserialize(&mut path)
        .expect("Could not deserialize");

    println!("Checking equality between ASTs");
    println!("{}", source);
    // At this stage, we have a problem: offsets are 0 in `ast`, but not necessarily
    // in `decoded`.
    extracted
        .walk(&mut WalkPath::new(), &mut OffsetCleanerVisitor)
        .expect("Could not cleanup offsets");
    assert_eq!(extracted, reference);
}

fn check_strings<T, F>(found: &HashMap<T, FilesContaining>, expected: Vec<(&str, usize)>, f: F)
where
    F: Fn(&str) -> T,
    T: Eq + std::hash::Hash + Ord + Clone + std::fmt::Debug,
{
    let found = found
        .iter()
        .map(|(name, instances)| ((*name).clone(), *instances))
        .sorted()
        .collect::<Vec<_>>();
    let expected = expected
        .into_iter()
        .map(|(name, instances)| (f(name), FilesContaining(instances)))
        .sorted()
        .collect::<Vec<_>>();
    assert_eq!(found, expected);
}

test!(test_linear_table, {
    // Initialize a linear table from a shared dictionary.
    let mut value_to_instances = HashMap::new();
    for i in 0..5 {
        value_to_instances.insert(i, i.into());
    }

    let mut linear_table = LinearTable::new(value_to_instances, /* threshold */ 1.into());

    assert_eq!(
        linear_table.shared_len(),
        3,
        "3 values should have passed the threshold"
    );
    assert_eq!(linear_table.prelude_len(), 0);
    assert_eq!(linear_table.len(), 3);

    // Add a prelude dictionary.
    for i in 5..20 {
        assert!(!linear_table.fetch_index(&i).is_hit());
    }

    assert_eq!(linear_table.shared_len(), 3);
    assert_eq!(linear_table.prelude_len(), 15);
    assert_eq!(linear_table.len(), 18);

    // An arbitrary list of fetch requests that attempts to ensure we hit
    // all the interesting cases.
    let requests = [
        2, 4, 6, 8, 10, 12, 14, 16, // Regular access
        3, 5, 7, 9, 11, 13, 15,
        17, // More regular access, just make sure that it's not monotonic
        10, 11, 4,  // Access old values
        18, // A fresh value
        10, 11, 4, 18, // Recent values
    ];

    // Generate an arbitrary list of indices.
    let indices = requests
        .iter()
        .map(|i| *linear_table.fetch_index(&i).slot())
        .collect_vec();

    for window_len in 0..10 {
        let mut serializer: TableRefStreamState<usize> =
            TableRefStreamState::new(window_len, &linear_table);
        let serialized = indices
            .iter()
            .map(|index| serializer.into_u32(*index))
            .collect_vec();

        eprintln!("Serialized: {:?}", serialized);

        let mut deserializer: TableRefStreamState<usize> =
            TableRefStreamState::new(window_len, &linear_table);
        let deserialized = serialized
            .iter()
            .map(|as_u32| deserializer.from_u32(*as_u32).unwrap())
            .collect_vec();

        assert_eq!(
            indices, deserialized,
            "Checking that indices == deserialized with a window_len of {}",
            window_len
        )
    }
});
