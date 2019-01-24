//! Generate a believed-to-be-correct JS file and its encoding.
//!
//! Note that the JS file is only correct insofar as the AST matches the grammar.

extern crate binjs;

extern crate clap;
extern crate env_logger;
extern crate rand;

const DEFAULT_TREE_SIZE: isize = 5;

use binjs::generic::pick::{Pick, Picker};
use binjs::generic::FromJSON;
use binjs::source::Shift;
use binjs::specialized::es6::ast::Walker;
use binjs::specialized::es6::io::Encoder;

use clap::*;
use std::io::Write;

fn main() {
    env_logger::init();

    let matches = App::new("BinJS source file generator")
        .author("David Teller <dteller@mozilla.com>")
        .about(
r#"Generate large numbers of believed-to-be-correct-files, both in text source code and binjs.
Note that this tool does not attempt to make sure that the files are entirely correct, only that they match BinJS's AST."#)
        .args(&[
            Arg::with_name("PREFIX")
                .required(true)
                .help("A prefix for generated files."),
            Arg::with_name("number")
                .long("number")
                .short("n")
                .takes_value(true)
                .required(true)
                .help("Number of files to generate."),
            Arg::with_name("size")
                .long("size")
                .takes_value(true)
                .help("Expected file size (in AST depth). Default: 5."),
            Arg::with_name("random-ast-metadata")
                .long("random-metadata")
                .help("If specified, generate random ast metadata (declared variables, etc.)."),
            Arg::with_name("lazify")
                .long("lazify")
                .takes_value(true)
                .default_value("0")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("Number of layers of functions to lazify. 0 = no lazification, 1 = functions at toplevel, 2 = also functions in functions at toplevel, etc."),
        ])
        .subcommand(binjs::io::Format::subcommand())
        .get_matches();

    let mut rng = rand::thread_rng();

    // Prepare grammar (used for entropy).
    let mut builder = binjs::meta::spec::SpecBuilder::new();
    let _ = binjs::generic::es6::Library::new(&mut builder);
    let spec_options = binjs::meta::spec::SpecOptions {
        null: &builder.node_name(""),
        root: &builder.node_name("Script"),
    };
    let spec = builder.into_spec(spec_options);

    let mut format = binjs::io::Format::from_matches(&spec, &matches)
        .expect("Could not determine encoding format")
        .randomize_options(&mut rng);

    let prefix = matches
        .value_of("PREFIX")
        .expect("Missing argument `PREFIX`");

    let number = matches
        .value_of("number")
        .expect("Missing argument `number`");
    let number: usize = number.parse().expect("Invalid number");

    let lazification =
        str::parse(matches.value_of("lazify").expect("Missing lazify")).expect("Invalid number");

    let size: isize = match matches.value_of("size") {
        None => DEFAULT_TREE_SIZE,
        Some(size) => size.parse().expect("Invalid size"),
    };

    let mut builder = binjs::meta::spec::SpecBuilder::new();
    let library = binjs::generic::es6::Library::new(&mut builder);
    let spec = builder.into_spec(binjs::meta::spec::SpecOptions {
        root: &library.program,
        null: &library.null,
    });

    let random_metadata = matches.is_present("random-metadata");

    let parser = Shift::try_new().expect("Could not launch Shift");

    let mut i = 0;
    loop {
        if i >= number {
            break;
        }
        let json = Picker.random(&spec, &mut rng, size);

        let mut ast =
            binjs::specialized::es6::ast::Script::import(&json).expect("Could not import AST");

        if lazification > 0 {
            let mut path = binjs::specialized::es6::ast::WalkPath::new();
            let mut visitor = binjs::specialized::es6::lazy::LazifierVisitor::new(lazification);
            ast.walk(&mut path, &mut visitor)
                .expect("Could not introduce laziness");
        }

        if !random_metadata {
            // Overwrite random annotations.
            binjs::specialized::es6::scopes::AnnotationVisitor::new().annotate_script(&mut ast);
        }

        if let Ok(source) = parser.to_source(&spec, json) {
            i += 1;

            println!("Generated sample {}/{}", i, number);
            {
                let mut source_file = std::fs::File::create(format!("{}-{}.js", prefix, i))
                    .expect("Could not create js file");
                source_file
                    .write_all(source.as_bytes())
                    .expect("Could not write js file");
            }

            let encoder = Encoder::new();
            let encoded = encoder
                .encode(None, &mut format, &ast)
                .expect("Could not encode AST");

            {
                let mut encoded_file = std::fs::File::create(format!("{}-{}.binjs", prefix, i))
                    .expect("Could not create binjs file");
                encoded_file
                    .write_all(encoded.as_ref().as_ref())
                    .expect("Could not write binjs file");
            }
        } else {
            println!("...could not pretty print this sample");
            continue;
        }
    }
}
