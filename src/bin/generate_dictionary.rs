//! Generate a static prediction table from a sample of JS source files.

extern crate binjs;

extern crate bincode;
extern crate clap;
extern crate env_logger;
extern crate log;

use binjs::source::{ Shift, SourceParser };
use binjs::generic::FromJSON;
use binjs::specialized::es6::ast::Walker;
use binjs::io::{ Path as IOPath, TokenSerializer };
use binjs::io::entropy::model::{ Dictionary, DictionaryBuilder, KindedStringMap, FilesContaining, Instances };

use std::fs::*;
use std::thread;
use std::path::{ Path };

use clap::*;

struct Options<'a> {
    parser: &'a Shift,
    lazification: u32,
    quiet: bool,
}

macro_rules! progress {
    ($quiet:expr, $($args:tt)*) => {
        if !$quiet {
            println!($($args)*);
        }
    }
}



fn handle_path<'a>(options: &mut Options<'a>,
    shared_dictionary: &mut Dictionary<Instances>,
    shared_files_containing_string: &mut KindedStringMap<FilesContaining>,
    shared_number_of_files: &mut usize,
    source_path: &Path,
    sub_dir: &Path)
{
    progress!(options.quiet, "Treating {:?} ({:?})", source_path, sub_dir);
    let is_dir = std::fs::metadata(source_path)
        .unwrap()
        .is_dir();
    if is_dir {
        let file_name = source_path.file_name()
            .unwrap_or_else(|| panic!("Invalid source path {:?}", source_path));
        let sub_dir = sub_dir.join(file_name);
        for entry in std::fs::read_dir(source_path)
            .expect("Could not open directory")
            .map(|dir| dir.unwrap())
        {
            handle_path(options, shared_dictionary, shared_files_containing_string, shared_number_of_files,
                &entry.path().as_path(), &sub_dir);
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

    handle_path_or_text(options, shared_dictionary, shared_files_containing_string, shared_number_of_files, source_path);
}

fn handle_path_or_text<'a>(options: &mut Options<'a>,
    shared_dictionary: &mut Dictionary<Instances>,
    shared_files_containing_string: &mut KindedStringMap<FilesContaining>,
    shared_number_of_files: &mut usize,
    source: &Path)
{
    let json = options.parser.parse_file(source)
        .expect("Could not parse source");

    let mut ast = binjs::specialized::es6::ast::Script::import(&json)
        .expect("Could not import AST");
    binjs::specialized::es6::scopes::AnnotationVisitor::new()
        .annotate_script(&mut ast);

    if options.lazification > 0 {
        progress!(options.quiet, "Introducing laziness.");
        let mut path = binjs::specialized::es6::ast::WalkPath::new();
        let mut visitor = binjs::specialized::es6::lazy::LazifierVisitor::new(options.lazification);
        ast.walk(&mut path, &mut visitor)
            .expect("Could not introduce laziness");
    }

    progress!(options.quiet, "Building dictionary.");
    let old_state_len = shared_dictionary.len();
    let old_string_len = shared_files_containing_string.len();

    {
        let builder = DictionaryBuilder::new(shared_dictionary, shared_files_containing_string);
        let mut serializer = binjs::specialized::es6::io::Serializer::new(builder);
        serializer.serialize(&ast, &mut IOPath::new())
            .expect("Could not generate dictionary");
        serializer.done()
            .expect("Could not finalize dictionary");
    }

    progress!(options.quiet, "Successfully built dictionary with {old_state_len} => {new_state_len} states, {old_string_len} => {new_string_len} strings",
        new_state_len = shared_dictionary.len(),
        old_state_len = old_state_len,
        new_string_len = shared_files_containing_string.len(),
        old_string_len = old_string_len);

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
            Arg::with_name("lazify")
                .long("lazify")
                .takes_value(true)
                .default_value("0")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("Number of layers of functions to lazify. 0 = no lazification, 1 = functions at toplevel, 2 = also functions in functions at toplevel, etc."),
            Arg::with_name("quiet")
                .long("quiet")
                .short("q")
                .help("Do not print progress"),
            Arg::with_name("depth")
                .long("depth")
                .takes_value(true)
                .default_value("3")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("Maximal path length to store in the dictionary."),
        ])
        .get_matches();

    // Common options.
    let sources : Vec<_> = matches.values_of("in")
        .map_or_else(|| Vec::new(),
                     |input| input
                     .map(Path::new)
                     .collect());

    let dest = Path::new(matches.value_of("out")
        .unwrap());

    let quiet = matches.is_present("quiet");

    let lazification = str::parse(matches.value_of("lazify").expect("Missing lazify"))
        .expect("Invalid number");

    let depth = str::parse(matches.value_of("depth").unwrap())
        .expect("Invalid number");

    progress!(quiet, "Generating dictionary with lazification {lazification}, depth {depth}",
        lazification = lazification,
        depth = depth);

    // Setup.
    let parser = Shift::new();
    let mut dictionary = Dictionary::new(depth);
    let mut files_containing_string = KindedStringMap::default();
    let mut number_of_files = 0;

    let mut options = Options {
        parser: &parser,
        lazification,
        quiet,
    };

    // Process files.
    for source_path in sources {
        handle_path(&mut options, &mut dictionary, &mut files_containing_string, &mut number_of_files,
            source_path, /* local root */ Path::new(""));
    }

    progress!(quiet, "Successfully generated dictionary from {} files", number_of_files);

    // FIXME: Remove strings that appear in a single file.

    // Write dictionaries.
    DirBuilder::new()
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
    let file_dictionary = File::create(dest_dictionary)
        .unwrap_or_else(|e| panic!("Could not create file: {:?}", e));
    bincode::serialize_into(file_dictionary, &dictionary)
        .expect("Could not serialize entropy dictionary");

    let dest_strings = dest.join("dict.strings");
    progress!(quiet, "Writing strings to {:?}", dest_strings);
    let file_strings = File::create(dest_strings)
        .unwrap_or_else(|e| panic!("Could not create file: {:?}", e));
    bincode::serialize_into(file_strings, &files_containing_string)
        .expect("Could not serialize strings dictionary");
}


