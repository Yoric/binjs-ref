extern crate binjs;
extern crate itertools;

use binjs::generic::{FromJSON, IdentifierName, InterfaceName, Offset, PropertyKey, SharedString};
use binjs::io::entropy;
use binjs::io::entropy::dictionary::{DictionaryBuilder, FilesContaining};
use binjs::io::{Deserialization, TokenSerializer};
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

    let mut builder = DictionaryBuilder::new(DEPTH, WIDTH);
    for source in &dict_sources {
        println!("Parsing");
        let ast = parser.parse_str(source).expect("Could not parse source");
        let mut ast =
            binjs::specialized::es6::ast::Script::import(&ast).expect("Could not import AST");

        println!("Annotating");
        binjs::specialized::es6::scopes::AnnotationVisitor::new().annotate_script(&mut ast);

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
    let mut builder = binjs::meta::spec::SpecBuilder::new();
    let library = binjs::generic::es6::Library::new(&mut builder);
    let spec = builder.into_spec(binjs::meta::spec::SpecOptions {
        root: &library.program,
        null: &library.null,
    });

    for maybe_dictionary in vec![Some(dictionary), None].into_iter() {
        if maybe_dictionary.is_some() {
            println!("** Testing with a dictionary");
        } else {
            println!("** Testing WITHOUT a dictionary");
        }
        let options = entropy::Options::new(&spec, maybe_dictionary);

        println!("Starting roundtrip with the sources that were used to build thhe dictionary");
        for source in &dict_sources {
            test_with_options(&parser, source, &options);
        }

        println!("Starting roundtrip with user-extensible values that do not show up in the dictionary");
        let out_of_dictionary = [
            "var z = y",
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
            "(function() { while (false) { console.log('What am I doing here?')} })"
        ];
        for source in &out_of_dictionary {
            test_with_options(&parser, source, &options);
        }
    }
});

fn test_with_options<S>(parser: &S, source: &str, options: &entropy::Options)
where
    S: SourceParser,
{
    println!("Parsing with dictionary: {}", source);
    let ast = parser.parse_str(source).expect("Could not parse source");
    let mut ast = binjs::specialized::es6::ast::Script::import(&ast).expect("Could not import AST");

    println!("Reannotating");
    binjs::specialized::es6::scopes::AnnotationVisitor::new().annotate_script(&mut ast);

    let mut path = IOPath::new();

    println!("Serializing with entropy");
    let encoder = entropy::write::Encoder::new(None, options.clone());
    let mut serializer = binjs::specialized::es6::io::Serializer::new(encoder);
    serializer
        .serialize(&ast, &mut path)
        .expect("Could not walk");
    let data = serializer.done().expect("Could not walk");
    assert_eq!(path.len(), 0);

    println!("Deserializing {} bytes with entropy", data.len());
    let decoder = entropy::read::Decoder::new(&options, std::io::Cursor::new(data))
        .expect("Could not create decoder");
    let mut deserializer = binjs::specialized::es6::io::Deserializer::new(decoder);
    let mut script: Script = deserializer
        .deserialize(&mut path)
        .expect("Could not deserialize");

    println!("Checking equality between ASTs");
    println!("{}", source);
    // At this stage, we have a problem: offsets are 0 in `ast`, but not necessarily
    // in `decoded`.
    script
        .walk(&mut WalkPath::new(), &mut OffsetCleanerVisitor)
        .expect("Could not cleanup offsets");
    assert_eq!(ast, script);
}

fn check_strings<T, F>(found: &HashMap<T, FilesContaining>, expected: Vec<(&str, usize)>, f: F)
where
    F: Fn(&str) -> T,
    T: Eq + std::hash::Hash + Ord + Clone + std::fmt::Debug,
{
    let found = found
        .iter()
        .map(|(name, instances)| ((*name).clone(), *instances))
        .sorted();
    let expected = expected
        .into_iter()
        .map(|(name, instances)| (f(name), FilesContaining(instances)))
        .sorted();
    assert_eq!(found, expected);
}
