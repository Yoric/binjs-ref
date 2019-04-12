//! Generate a static prediction table from a sample of JS source files.

extern crate binjs;

extern crate bincode;
extern crate clap;
extern crate env_logger;
#[macro_use]
extern crate log;

use binjs::io::entropy::dictionary::{DictionaryBuilder, Options as DictionaryOptions};
use binjs::io::{Path as IOPath, Serialization, TokenSerializer};
use binjs::source::{Shift, SourceParser};
use binjs::specialized::es6::Enrich;

use std::fs::{self, File};
use std::path::Path;
use std::thread;

use clap::*;

struct Options<'a> {
    parser: &'a Shift,
    enricher: Enrich,
    quiet: bool,
    show_ast: bool,
}

macro_rules! progress {
    ($quiet:expr, $($args:tt)*) => {
        if !$quiet {
            println!($($args)*);
        }
    }
}

fn handle_path<'a>(
    options: &mut Options<'a>,
    shared_builder: &mut DictionaryBuilder,
    shared_number_of_files: &mut usize,
    source_path: &Path,
    sub_dir: &Path,
) {
    progress!(options.quiet, "Treating {:?} ({:?})", source_path, sub_dir);
    let is_dir = fs::metadata(source_path).unwrap().is_dir();
    if is_dir {
        let file_name = source_path
            .file_name()
            .unwrap_or_else(|| panic!("Invalid source path {:?}", source_path));
        let sub_dir = sub_dir.join(file_name);
        for entry in fs::read_dir(source_path)
            .expect("Could not open directory")
            .map(|dir| dir.unwrap())
        {
            handle_path(
                options,
                shared_builder,
                shared_number_of_files,
                &entry.path().as_path(),
                &sub_dir,
            );
        }
        return;
    }
    if let Some(Some("js")) = source_path.extension().map(std::ffi::OsStr::to_str) {
        // Proceed
    } else {
        progress!(options.quiet, "Skipping {:?}", source_path);
        return;
    }

    progress!(options.quiet, "Parsing.");

    handle_path_or_text(options, shared_builder, shared_number_of_files, source_path);
}

fn handle_path_or_text<'a>(
    options: &mut Options<'a>,
    dictionary_builder: &mut DictionaryBuilder,
    shared_number_of_files: &mut usize,
    source: &Path,
) {
    let mut ast = options
        .parser
        .parse_file(source)
        .expect("Could not parse source");

    options
        .enricher
        .enrich(&mut ast)
        .expect("Could not enrich AST");

    if options.show_ast {
        serde_json::to_writer_pretty(std::io::stdout(), &ast).unwrap();
        println!();
    }

    progress!(options.quiet, "Building dictionary.");
    {
        let mut serializer = binjs::specialized::es6::io::Serializer::new(dictionary_builder);
        serializer
            .serialize(&ast, &mut IOPath::new())
            .expect("Could not generate dictionary");
        serializer.done().expect("Could not finalize dictionary");
    }

    *shared_number_of_files += 1;
}

fn main() {
    thread::Builder::new()
        .name("large stack dedicated thread".to_string())
        .stack_size(20 * 1024 * 1024)
        .spawn(|| {
            main_aux();
        })
        .expect("Could not launch dedicated thread")
        .join()
        .expect("Error in dedicated thread");
}

fn main_aux() {
    env_logger::init();

    let matches = App::new("BinJS encoder")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Generate a static prediction table from a bunch of JS source files.")
        .args(&[
            Arg::with_name("in")
                .long("in")
                .short("i")
                .multiple(true)
                .takes_value(true)
                .required(true)
                .help("Input files to use. Must be JS source file. May be specified multiple times."),
            Arg::with_name("out")
                .required(true)
                .long("out")
                .short("o")
                .takes_value(true)
                .help("Output directory to use for writing the dictionaries. May be overwritten."),
            Arg::with_name("show-ast")
                .long("show-ast")
                .help("Show the AST of each source file before extracting the dictionary"),
            Arg::with_name("quiet")
                .long("quiet")
                .short("q")
                .help("Do not print progress"),
            Arg::with_name("depth")
                .long("depth")
                .takes_value(true)
                .default_value("2")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("Maximal path length to store in the dictionary."),
            Arg::with_name("window-width")
                .long("window-width")
                .takes_value(true)
                .default_value("32")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("String window width."),
            Arg::with_name("threshold")
                .long("threshold")
                .takes_value(true)
                .default_value("2")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("Prune from the dictionary all user-extensible values that appear in at most [threshold] files"),
        ])
        .args(Enrich {
            scopes: true,
            ..Default::default()
        }.args().as_slice())
        .get_matches();

    // Common options.
    let sources: Vec<_> = matches
        .values_of("in")
        .map_or_else(|| Vec::new(), |input| input.map(Path::new).collect());

    let dest = Path::new(matches.value_of("out").unwrap());

    let quiet = matches.is_present("quiet");

    let depth = str::parse(matches.value_of("depth").unwrap()).expect("Invalid number");

    let width = str::parse(matches.value_of("window-width").unwrap()).expect("Invalid number");

    let threshold: usize =
        str::parse(matches.value_of("threshold").unwrap()).expect("Invalid number");

    let enricher = Enrich::from_matches(&matches);

    // Setup.
    let parser = Shift::try_new().expect("Could not launch Shift");
    let mut builder = DictionaryBuilder::new(
        DictionaryOptions::default()
            .with_depth(depth)
            .with_width(width),
    );
    let mut number_of_files = 0;

    let mut options = Options {
        parser: &parser,
        enricher,
        quiet,
        show_ast: matches.is_present("show-ast"),
    };

    // Process files.
    for source_path in sources {
        handle_path(
            &mut options,
            &mut builder,
            &mut number_of_files,
            source_path,
            /* local root */ Path::new(""),
        );
    }

    progress!(
        quiet,
        "Successfully generated dictionary from {} files",
        number_of_files
    );

    // FIXME: Remove strings that appear in a single file.

    // Write dictionaries.
    fs::DirBuilder::new()
        .recursive(true)
        .create(dest)
        .expect("Could not create directory");

    // Write the entire probability table.
    //
    // As of this writing:
    // - the format is really, really bad;
    // - much of the information inside the table will never be used.
    //
    // To be improved, iteratively.
    let dest_dictionary = dest.join("dict.entropy");
    progress!(quiet, "Writing probabilities to {:?}", dest_dictionary);
    let file_dictionary =
        File::create(dest_dictionary).unwrap_or_else(|e| panic!("Could not create file: {:?}", e));
    let dictionary = builder.done(threshold.into());
    debug!(target: "generate_dictionary", "Current dictionary is '{}'", dictionary.name());
    debug!(target: "generate_dictionary", "Created dictionary with {} interface names", dictionary.current().interface_names().len());
    bincode::serialize_into(file_dictionary, &dictionary)
        .expect("Could not serialize entropy dictionary");
}
