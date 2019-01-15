//! Encode a BinJS, then decode it, ensure that we obtain the same AST.

extern crate binjs;
extern crate clap;
extern crate env_logger;

use clap::*;

use binjs::meta::spec::*;
use binjs::source::*;

use std::io::*;

fn main() {
    env_logger::init();

    let matches = App::new("BinJS roundtrip tester")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Check that encoding + decoding a file yields the same AST.")
        .args(&[
            Arg::with_name("in")
                .long("in")
                .multiple(true)
                .takes_value(true)
                .help("Input files to use. Must be JS source file."),
            Arg::with_name("numbering")
                .long("numbering")
                .takes_value(true)
                .possible_values(&["mru", "frequency"])
                .help("Numbering strategy for the tree. Defaults to frequency."),
            Arg::with_name("statistics")
                .long("stat")
                .help("Show statistics."),
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

    let mut format = binjs::io::Format::from_matches(&matches)
        .expect("Could not determine encoding format");

    let lazification = str::parse(matches.value_of("lazify").expect("Missing lazify"))
        .expect("Invalid number");

    let files : Vec<_> = matches.values_of("in")
        .unwrap()
        .collect();
    println!("List of files: {:?}", files);

    let parser = Shift::new();
    let mut spec_builder = SpecBuilder::new();
    let library = binjs::generic::es6::Library::new(&mut spec_builder);
    let spec = spec_builder.into_spec(SpecOptions {
        null: &library.null,
        root: &library.program,
    });

    for source_path in files {
        println!("Parsing {}.", source_path);

        let mut ast = parser.parse_file(source_path)
            .expect("Could not parse source");

        println!("Annotating.");
        library.annotate(&mut ast);


        if lazification > 0 {
            println!("Introducing laziness.");
            library.lazify(lazification, &mut ast);
        }

        let encoder = binjs::generic::io::Encoder::new();
        let encoded = encoder.encode(None, &spec, &mut format, &ast)
            .expect("Could not encode AST");

        let data = encoded.as_ref();

        let decoder = binjs::generic::io::Decoder::new();
        let decoded = decoder.decode(&spec, &mut format, Cursor::new(data))
            .expect("Could not decode AST");

        println!("Checking.");
        let equal = binjs::generic::syntax::Comparator::compare(&spec, &ast, &decoded)
            .expect("Could not compare ASTs");
        assert!(equal);

        println!("Roundtrip success!");
    }
}
