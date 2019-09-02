//! Encode a BinJS, then decode it, ensure that we obtain the same AST.

extern crate binjs;
extern crate env_logger;
extern crate glob;
#[macro_use]
extern crate log;

use binjs::generic::*;
use binjs::io::bytes::compress::*;
use binjs::io::multipart::*;
use binjs::io::*;
use binjs::source::*;
use binjs::specialized::es6::ast::{IOPath, Script, Visitor, WalkPath, Walker};

use std::io::Cursor;
use std::thread;

const PATHS: [&'static str; 5] = [
    // Use only subset of the dataset to reduce the time taken by this test on
    // automation.  Those files are chosen almost-randomly.
    "tests/data/facebook/single/0FL4yAxztNI.js",
    "tests/data/facebook/single/1vzDGthwISY.js",
    "tests/data/frameworks/jquery.3.3.1.min.js",
    "tests/data/frameworks/react-dom.production.16.3.2.min.js",
    // Files below are for testing specific case. Run all of them.
    "tests/data/misc/*.js",
];

fn progress() {
    // Make sure that we see progress in the logs, without spamming these logs.
    eprint!(".");
}

/// A visitor designed to reset offsets to 0.
struct OffsetCleanerVisitor;
impl Visitor<()> for OffsetCleanerVisitor {
    fn visit_offset(&mut self, _path: &WalkPath, node: &mut Offset) -> Result<(), ()> {
        *node = binjs::generic::Offset(0);
        Ok(())
    }
}

#[test]
fn test_roundtrip() {
    thread::Builder::new()
        .name("test_roundtrip large stack dedicated thread".to_string())
        .stack_size(20 * 1024 * 1024)
        .spawn(|| {
            main();
        })
        .expect("Could not launch dedicated thread")
        .join()
        .expect("Error in dedicated thread");
}

fn main() {
    env_logger::init();

    let parser = Shift::try_new().expect("Could not launch Shift");

    // All combinations of options for compression.
    let mut all_options = {
        use self::Compression::*;
        let mut vec = vec![];
        let compressions = [
            Identity, Gzip,
            /*Deflate seems broken upstream,*/ Brotli, /*Lzw doesn't work yet*/
        ]
        .into_iter()
        .cloned()
        .collect::<Vec<_>>();
        for grammar_table in &compressions {
            for strings_table in &compressions {
                for tree in &compressions {
                    vec.push(Targets {
                        grammar_table: CompressionTarget::new(grammar_table.clone()),
                        strings_table: CompressionTarget::new(strings_table.clone()),
                        tree: CompressionTarget::new(tree.clone()),
                    });
                }
            }
        }
        vec
    };

    eprint!("\nTesting roundtrip with laziness");
    for path_suffix in &PATHS {
        let path = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), path_suffix);
        debug!(target: "test_roundtrip", "Starting laziness test_roundtrip from {}", path);

        'laziness_per_entry: for entry in glob::glob(&path).expect("Invalid glob pattern") {
            let mut path = binjs::specialized::es6::ast::WalkPath::new();
            let entry = entry.expect("Invalid entry");
            eprint!("\n{:?}.", entry);

            // Immutable copy.
            let reference_ast = parser
                .parse_file(entry.clone())
                .expect("Could not parse source");

            'per_level: for level in &[0, 1, 2, 3, 4, 5] {
                debug!(target: "test_roundtrip", "Testing {:?} with laziness level {}", entry, level);
                let mut ast = reference_ast.clone();
                let enricher = binjs::specialized::es6::Enrich {
                    lazy_threshold: *level,
                    ..Default::default()
                };
                enricher.enrich(&mut ast).expect("Could not enrich AST");
                progress();

                let options = Targets {
                    grammar_table: CompressionTarget::new(Compression::Identity),
                    strings_table: CompressionTarget::new(Compression::Identity),
                    tree: CompressionTarget::new(Compression::Identity),
                };
                debug!(target: "test_roundtrip", "Encoding.");
                let writer = binjs::io::TokenWriterTreeAdapter::new(
                    binjs::io::multipart::TreeTokenWriter::new(options.clone()),
                );
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer
                    .serialize(&ast, &mut IOPath::new())
                    .expect("Could not encode AST");
                let data = serializer.done().expect("Could not finalize AST encoding");
                progress();

                debug!(target: "test_roundtrip", "Decoding.");
                let source = Cursor::new(data);
                let reader = binjs::io::multipart::TreeTokenReader::new(source)
                    .expect("Could not decode AST container");
                let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);

                let mut decoded: Script = deserializer
                    .deserialize(&mut IOPath::new())
                    .expect("Could not decode");
                progress();

                debug!(target: "test_roundtrip", "Checking.");
                // At this stage, we have a problem: offsets are 0 in `ast`, but not 0
                // in `decoded`.
                decoded
                    .walk(&mut path, &mut OffsetCleanerVisitor)
                    .expect("Could not cleanup offsets");
                progress();
                assert_eq!(ast, decoded);
            }
        }
    }

    eprint!("\nCompression tests");
    for path_suffix in &PATHS {
        let path = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), path_suffix);
        debug!(target: "test_roundtrip", "Starting test_roundtrip from {}", path);

        'compression_per_entry: for entry in glob::glob(&path).expect("Invalid glob pattern") {
            let entry = entry.expect("Invalid entry");

            // Parse and preprocess file.

            eprint!("\n{:?}.", entry);
            let ast = parser
                .parse_file(entry.clone())
                .expect("Could not parse source");
            {
                progress();
                debug!(target: "test_roundtrip", "Starting simple round trip for {:?}", entry);

                // Roundtrip `simple`
                debug!(target: "test_roundtrip", "Encoding");
                let writer = binjs::io::TokenWriterTreeAdapter::new(
                    binjs::io::simple::TreeTokenWriter::new(),
                );
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer
                    .serialize(&ast, &mut IOPath::new())
                    .expect("Could not encode AST");
                let data = serializer.done().expect("Could not finalize AST encoding");

                progress();
                debug!(target: "test_roundtrip", "Decoding.");
                let source = Cursor::new(data);
                let reader = binjs::io::simple::TreeTokenReader::new(source);
                let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);

                let decoded = deserializer
                    .deserialize(&mut IOPath::new())
                    .expect("Could not decode");
                progress();

                debug!(target: "test_roundtrip", "Checking.");
                assert_eq!(ast, decoded);

                debug!(target: "test_roundtrip", "Completed simple round trip for {:?}", entry);
            }

            // Roundtrip `multipart`

            'per_option: for options in &mut all_options {
                progress();
                debug!(target: "test_roundtrip", "Starting multipart round trip for {:?} with options {:?}", entry, options);
                debug!(target: "test_roundtrip", "Encoding.");
                options.reset();
                let writer = binjs::io::TokenWriterTreeAdapter::new(
                    binjs::io::multipart::TreeTokenWriter::new(options.clone()),
                );
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer
                    .serialize(&ast, &mut IOPath::new())
                    .expect("Could not encode AST");
                let data = serializer.done().expect("Could not finalize AST encoding");

                progress();

                debug!(target: "test_roundtrip", "Decoding.");
                let source = Cursor::new(data);
                let reader = binjs::io::multipart::TreeTokenReader::new(source)
                    .expect("Could not decode AST container");
                let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);

                let decoded = deserializer
                    .deserialize(&mut IOPath::new())
                    .expect("Could not decode");
                progress();

                debug!(target: "test_roundtrip", "Checking.");
                assert_eq!(ast, decoded);

                debug!(target: "test_roundtrip", "Completed multipart round trip for {:?} with options {:?}", entry, options);
                progress();
            }
        }
    }
}
