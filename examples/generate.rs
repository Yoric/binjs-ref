//! Generate a believed-to-be-correct JS file and its encoding.
//!
//! Note that the JS file is only correct insofar as the AST matches the grammar.


extern crate binjs;

extern crate clap;
extern crate env_logger;
extern crate rand;

const DEFAULT_TREE_SIZE: isize = 5;

use binjs::source::Shift;
use binjs::generic::io::Encoder;
use binjs::generic::pick::{ Pick, Picker };

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
                .help("If specified, generate random ast metadata (declared variables, etc.).")
        ])
        .subcommand(binjs::io::Format::subcommand())
        .get_matches();

    let mut rng = rand::thread_rng();

    let mut format = binjs::io::Format::from_matches(&matches)
        .expect("Could not determine encoding format")
        .randomize_options(&mut rng);

    let prefix = matches.value_of("PREFIX")
        .expect("Missing argument `PREFIX`");

    let number = matches.value_of("number")
        .expect("Missing argument `number`");
    let number : usize = number.parse()
        .expect("Invalid number");

    let size : isize = match matches.value_of("size") {
        None => DEFAULT_TREE_SIZE,
        Some(size) => size.parse()
            .expect("Invalid size")
    };

    let mut builder = binjs::meta::spec::SpecBuilder::new();
    let library = binjs::generic::es6::Library::new(&mut builder);
    let spec = builder.into_spec(binjs::meta::spec::SpecOptions {
        root: &library.program,
        null: &library.null
    });

    let random_metadata = matches.is_present("random-metadata");

    let parser = Shift::new();

    let mut i = 0;
    loop {
        if i >= number {
            break;
        }
        let mut ast = Picker.random(&spec, &mut rng, size);

        if !random_metadata {
            // Overwrite random annotations.
            library.annotate(&mut ast);
        }

        if let Ok(source) = parser.to_source(&spec, &ast) {
            i += 1;

            println!("Generated sample {}/{}", i, number);
            {
                let mut source_file = std::fs::File::create(format!("{}-{}.js", prefix, i))
                    .expect("Could not create js file");
                source_file.write_all(source.as_bytes())
                    .expect("Could not write js file");
            }

            let encoder = Encoder::new();
            let encoded = encoder.encode(None, &spec, &mut format, &ast)
                .expect("Could not encode AST");

            {
                let mut encoded_file = std::fs::File::create(format!("{}-{}.binjs", prefix, i))
                    .expect("Could not create binjs file");
                encoded_file.write_all(encoded.as_ref().as_ref())
                    .expect("Could not write binjs file");
            }
        } else {
            println!("...could not pretty print this sample");
            continue;
        }
    }
}
