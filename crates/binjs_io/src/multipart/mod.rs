//! A multipart format, in which each part can be compressed independently.
//!
//! # Overview
//!
//! The file is divided in sections. Each section is prefixed by its bytelength, so as to permit
//! skipping a section and/or reading sections concurrently. Each section may be compressed
//! independently, possibly with different compression formats, with the expectation that this
//! will let compressors take best advantage of the distinct structures of each section.
//!
//! (future versions may allow file-wide compression, too)
//!
//! The sections are:
//!
//! 1. the grammar table;
//! 2. the strings table (which contains both strings and identifiers);
//! 3. the representation of the tree.
//!
//! The grammar table lists the AST nodes used in the file. Its primary role is to serve as a lightweight
//! versioning mechanism - for instance, older versions of JS may define a node `Function` with three fields
//! (`body`, `arguments` and optional `name`), while more recent versions of JS may define the same node
//! with *five* fields (`body`, `arguments`, `async`, `generator` and optional `name`). A BinAST file
//! may contain *either* variants of `Function`, depending on when it was created. The grammar table lets recent
//! parsers determine that some fields are omitted and should be replaced by their default value. In fact, a
//! BinAST file could even contain *both* variants of `Function`, for compression purposes. Also, when a
//! parser encounters a grammar table with nodes that either have an unknown name or contain unknown
//! fields, it may decide to reject the file immediately (it doesn't have to, mind you).
//!
//! The strings table lists all strings (including identifiers) in the file. Its primary role is to speed
//! up parsing by making sure that each string only needs to be parsed/checked/atomized once during parsing.
//! Its secondary role is compression.
//!
//! In the current version, the tree is a sequence of tokens. All these tokens are ambiguous and a stream may
//! only be tokenized by a client that knows both the grammar and the grammar table. Specific tokens (lists)
//! contain their byte length, so as to allow skipping them for purposes of lazy parsing and/or concurrent
//! parsing.
//!
//! # Format
//!
//! The entire file is formatted as:
//!
//! - the characters `"BINJS"`;
//! - a container version number (`varnum`, currently `0`);
//! - the compressed grammar table (see below);
//! - the compressed strings table (see below);
//! - the compressed tree (see below).
//!
//! ## Grammar table
//!
//! The grammar table serves to map tagged tuple indices to actual constructions in the JS grammar.
//!
//! - the characters `"[GRAMMAR]"`;
//! - a `prefix` identifying the compression format used for the grammar (one of "identity;", "br;", "gzip;", "compress;", "deflate;").
//! - the number of compressed bytes (`varnum`);
//! - compressed in the format identified by `prefix`:
//!    - the number of entries (`varnum`);
//!    - for each entry,
//!      - byte length of entry (`varnum`);
//!      - one of
//!        - the invalid strings [255, 0] (representing the null interface, only valid if byte length is 2);
//!        - a utf-8 encoded string (utf-8 encoded, `bytelen` bytes, no terminator).
//!
//! ## Strings table
//!
//! The grammar table serves to map tagged tuple indices to strings.
//!
//! - the characters `"[STRINGS]"`;
//! - a `prefix` identifying the compression format used for the grammar (one of "identity;", "br;", "gzip;", "compress;", "deflate;").
//! - the number of compressed bytes (`varnum`);
//! - compressed in the format identified by `prefix`;
//!    - the number of entries (`varnum`);
//!    - for each entry,
//!      - byte length of string (`varnum`);
//!      - one of
//!        - the invalid strings [255, 0] (representing the null string, only valid if byte length is 2);
//!        - a utf-8 encoded string (utf-8 encoded, `bytelen` bytes, no terminator).
//!
//! ## The tree
//!
//! This contains the actual tree for a specific grammar. The file does not contain all the information
//! to determine the nature of next token. Rather, this must be led by the grammar.
//!
//! - the characters `"[TREE]"`;
//! - a `prefix` identifying the compression format used for the grammar (one of "identity;", "br;", "gzip;", "compress;", "deflate;").
//! - the number of compressed bytes (`varnum`);
//! - compressed in the format identified by `prefix`:
//!   - one tree token.
//!
//! ### Tree token
//!
//!  A tree token is defined as one of
//!
//!   - a number of bytes (aka Offset), represented as:
//!     - a `varnum`;
//!   - a null float, represented as:
//!     - a low-endian IEEE764 64-bit floating point value signalling NaN (8 bytes),
//!   - a non-null float, represented as:
//!     - a low-endian IEEE764 64-bit floating point value non-signalling NaN (8 bytes),
//!   - a null boolean, represented as:
//!     -  a single byte with value `2` (one byte);
//!   - a non-null boolean, represented as:
//!     -  a single byte with value `0` (false) or `1` (true) (one byte);
//!   - a string, representing as
//!     - an entry in the table of strings (`varnum`);
//!   - a list, represented as
//!       - number of items (`varnum`);
//!       - for each item
//!          - the token;
//!   - a tagged tuple, represented as
//!     - an entry in the grammar table (`varnum`);
//!     - for each field
//!       - the token

use binjs_shared::SharedString;

use clap;

/// Implementation of the token reader.
mod read;

/// Implementation of the token writer.
mod write;

/// The header of the strings table section.
const HEADER_STRINGS_TABLE: &str = "[STRINGS]";

/// The header of the grammars table section.
const HEADER_GRAMMAR_TABLE: &str = "[GRAMMAR]";

/// The header of the tree section.
const HEADER_TREE: &str = "[TREE]";

/// A trait specifying whether a piece of data needs the addition of a length index.
trait FormatInTable {
    const HAS_LENGTH_INDEX: bool;
}

impl FormatInTable for Option<SharedString> {
    const HAS_LENGTH_INDEX: bool = false;
}

pub use self::read::TreeTokenReader;
pub use self::write::{Statistics, Targets, TreeTokenWriter};

/// Command-line management.
pub struct FormatProvider;
impl ::FormatProvider for FormatProvider {
    fn subcommand<'a, 'b>(&self) -> clap::App<'a, 'b> {
        use clap::*;
        SubCommand::with_name("multipart")
            .about("Use the multipart format (default)")
            .arg(Arg::with_name("x-inner-compression")
                .help("(EXPERIMENTAL) Apply a secondary compression *inside* the file. Used only when compressing.")
                .long("x-inner-compression")
                .takes_value(true)
                .possible_values(&["identity", "gzip", "deflate", "br", "lzw"])
            )
            .arg(Arg::with_name("x-dump-sections")
                .help("(EXPERIMENTAL) Export sections to individual files. Used only when compressing.")
                .long("x-dump-sections")
            )
    }

    fn handle_subcommand(
        &self,
        _spec: &binjs_meta::spec::Spec,
        matches: Option<&clap::ArgMatches>,
    ) -> Result<::Format, ::std::io::Error> {
        use bytes::compress::Compression;
        use multipart::{Statistics, Targets};

        use std::cell::RefCell;
        use std::rc::Rc;
        let stats = Rc::new(RefCell::new(Statistics::default().with_source_bytes(0)));
        let compression = matches
            .map(|matches| {
                Compression::parse(matches.value_of("x-inner-compression"))
                    .expect("Could not parse x-inner-compression")
            })
            .unwrap_or(Compression::Identity);
        Ok(::Format::Multipart {
            targets: Targets {
                strings_table: ::CompressionTarget::new(compression.clone()),
                grammar_table: ::CompressionTarget::new(compression.clone()),
                tree: ::CompressionTarget::new(compression.clone()),
            },
            stats,
        })
    }
}

#[test]
fn test_multipart_io() {
    println!("Multipart (de)tokenizer test starting");
    extern crate env_logger;
    env_logger::init();

    use binjs_shared::ast::Path;
    use binjs_shared::{FieldName, InterfaceName, SharedString};

    use io::{TokenReader, TokenWriterWithTree};
    use multipart::*;
    use CompressionTarget;

    use std::fs::*;
    use std::io::{Cursor, Write};

    // All combinations of options for compression.
    let all_options = {
        use bytes::compress::Compression::*;
        let mut vec = vec![];
        let compressions = [Identity, Gzip, Deflate /*Lzw, Brotli don't work yet*/];
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

    for mut options in all_options {
        println!("Options {:?}", options);
        let suffix = format!(
            "{:?}-{:?}-{:?}",
            options.grammar_table, options.strings_table, options.tree
        );
        let mut path = Path::new();

        {
            options.reset();
            let mut writer = TreeTokenWriter::new(options.clone());
            writer
                .string(Some(&SharedString::from_str("simple string")))
                .expect("Writing simple string");

            let output = writer.done().expect("Finalizing data");
            File::create(format!("/tmp/test-simple-string-{}.binjs", suffix))
                .expect("Could not create file")
                .write_all(&output)
                .unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).expect("Creating reader");
            let simple_string = reader
                .string_at(&path)
                .expect("Reading simple string")
                .expect("Non-null string");
            assert_eq!(&simple_string, "simple string");
        }

        {
            options.reset();
            let data = SharedString::from_str("string with escapes \u{0}\u{1}\u{0}");
            let mut writer = TreeTokenWriter::new(options.clone());
            writer
                .string(Some(&data))
                .expect("Writing string with escapes");

            let output = writer.done().expect("Finalizing data");
            File::create(format!("/tmp/test-string-with-escapes-{}.binjs", suffix))
                .unwrap()
                .write_all(&output)
                .unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let escapes_string = reader
                .string_at(&path)
                .expect("Reading string with escapes")
                .expect("Non-null string");
            assert_eq!(escapes_string, data);
        }

        println!("Testing tagged tuple I/O");

        {
            options.reset();
            let mut writer = TreeTokenWriter::new(options.clone());
            let item_0 = writer.string(Some(&SharedString::from_str("foo"))).unwrap();
            let item_1 = writer.string(Some(&SharedString::from_str("bar"))).unwrap();
            let item_2 = writer.float(Some(3.1415)).unwrap();
            writer
                .tagged_tuple(
                    &InterfaceName::from_str("some tuple"),
                    &[
                        (&FieldName::from_str("abc"), item_0),
                        (&FieldName::from_str("def"), item_1),
                        (&FieldName::from_str("value"), item_2),
                    ],
                )
                .expect("Writing trivial tagged tuple");

            let output = writer.done().expect("Finalizing data");
            File::create(format!("/tmp/test-simple-tagged-tuple-{}.binjs", suffix))
                .unwrap()
                .write_all(&output)
                .unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let (name, fields) = reader
                .enter_tagged_tuple_at(&path)
                .expect("Reading trivial tagged tuple");
            assert_eq!(name, "some tuple");

            assert_eq!(fields, None);
            let simple_string_1 = reader
                .string_at(&path)
                .expect("Reading trivial tagged tuple[0]")
                .expect("Reading a non-null string");
            let simple_string_2 = reader
                .string_at(&path)
                .expect("Reading trivial tagged tuple[1]")
                .expect("Reading a non-null string");
            let simple_float = reader
                .float_at(&path)
                .expect("Reading trivial tagged tuple[2]")
                .expect("Reading a non-null float");

            reader
                .exit_tagged_tuple_at(&path)
                .expect("Trivial tagged tuple read properly");

            assert_eq!(&simple_string_1, "foo");
            assert_eq!(&simple_string_2, "bar");
            assert_eq!(simple_float, 3.1415);
        }

        println!("Testing list I/O");

        {
            options.reset();
            let mut writer = TreeTokenWriter::new(options.clone());
            writer.list(vec![]).expect("Writing empty list");

            let output = writer.done().expect("Finalizing data");
            File::create(format!("/tmp/test-empty-list-{}.binjs", suffix))
                .unwrap()
                .write_all(&output)
                .unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let len = reader.enter_list_at(&path).expect("Reading empty list");
            assert_eq!(len, 0);

            reader
                .exit_list_at(&path)
                .expect("Empty list read properly");
        }

        {
            options.reset();
            let mut writer = TreeTokenWriter::new(options.clone());
            let item_0 = writer.string(Some(&SharedString::from_str("foo"))).unwrap();
            let item_1 = writer.string(Some(&SharedString::from_str("bar"))).unwrap();
            writer
                .list(vec![item_0, item_1])
                .expect("Writing trivial list");

            let output = writer.done().expect("Finalizing data");
            File::create(format!("/tmp/test-trivial-list-{}.binjs", suffix))
                .unwrap()
                .write_all(&output)
                .unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let len = reader.enter_list_at(&path).expect("Reading trivial list");
            assert_eq!(len, 2);

            let simple_string = reader
                .string_at(&path)
                .expect("Reading trivial list[0]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "foo");
            let simple_string = reader
                .string_at(&path)
                .expect("Reading trivial list[1]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "bar");

            reader
                .exit_list_at(&path)
                .expect("Trivial list read properly");
        }

        {
            options.reset();
            let mut writer = TreeTokenWriter::new(options.clone());
            let item_0 = writer.string(Some(&SharedString::from_str("foo"))).unwrap();
            let item_1 = writer.string(Some(&SharedString::from_str("bar"))).unwrap();
            let list = writer
                .list(vec![item_0, item_1])
                .expect("Writing inner list");
            writer.list(vec![list]).expect("Writing outer list");

            let output = writer.done().expect("Finalizing data");
            File::create(format!("/tmp/test-nested-lists-{}.binjs", suffix))
                .unwrap()
                .write_all(&output)
                .unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let len = reader.enter_list_at(&path).expect("Reading outer list");
            assert_eq!(len, 1);

            let len = reader.enter_list_at(&path).expect("Reading inner list");
            assert_eq!(len, 2);

            let simple_string = reader
                .string_at(&path)
                .expect("Reading trivial list[0]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "foo");
            let simple_string = reader
                .string_at(&path)
                .expect("Reading trivial list[1]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "bar");

            reader
                .exit_list_at(&path)
                .expect("Inner list read properly");
            reader
                .exit_list_at(&path)
                .expect("Inner list read properly");
        }
    }
}
