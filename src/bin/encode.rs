//! Encode a text source to a BinJS.

extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate log;

use binjs::io::{ CompressionTarget, Format };
use binjs::source::{ Shift, SourceParser };
use binjs::generic::FromJSON;
use binjs::specialized::es6::io::Encoder;
use binjs::specialized::es6::ast::Walker;

use std::fs::*;
use std::io::*;
use std::thread;
use std::path::{ Path, PathBuf };

use clap::*;

fn export_section(dest_bin_path: &Option<PathBuf>, target: &mut CompressionTarget, extension: &str) {
    let path = dest_bin_path
        .clone()
        .expect("Cannot write partial file without a destination")
        .with_extension(extension);
    let mut file = File::create(path.clone())
        .unwrap_or_else(|e| panic!("Could not create destination file {:?}: {:?}", path, e));
    let (data, _) = target.done()
        .expect("Could not finalize compression");
    file.write(data.as_ref())
        .expect("Could not write destination file");
    target.reset();
}

struct Options<'a> {
    parser: &'a Shift,
    format: Format,
    dest_dir: Option<PathBuf>,
    lazification: u32,
    show_ast: bool,
    quiet: bool,
}

macro_rules! progress {
    ($quiet:expr, $($args:tt)*) => {
        if !$quiet {
            println!($($args)*);
        }
    }
}

enum Source<'a> {
    FromFile { path: &'a Path },
    FromStdin { text: String },
}

struct EncodeParams<'a> {
    source: Source<'a>,
    dest_bin_path: Option<PathBuf>,
    dest_txt_path: Option<PathBuf>,
}

fn handle_path<'a>(options: &mut Options<'a>,
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
            handle_path(options, entry.path().as_path(), &sub_dir);
        }
        return;
    }
    if let Some(Some("js")) = source_path.extension().map(std::ffi::OsStr::to_str) {
        // Proceed
    } else {
        progress!(options.quiet, "Skipping {:?}", source_path);
        return;
    }
    let (dest_txt_path, dest_bin_path) = match options.dest_dir {
        None => (None, None), // Use stdout
        Some(ref d) => {
            let file_name = source_path.file_stem()
                .expect("Could not extract file name");

            std::fs::create_dir_all(d.join(sub_dir))
                .expect("Could not find or create destination directory");

            let mut bin_path = d.join(sub_dir);
            bin_path.push(file_name);
            bin_path.set_extension("binjs");

            let mut txt_path = d.join(sub_dir);
            txt_path.push(file_name);
            txt_path.set_extension("js");

            (Some(txt_path), Some(bin_path))
        }
    };

    if let Some(ref bin_path) = dest_bin_path {
        progress!(options.quiet, "Output: {}", bin_path.to_string_lossy());
    } else {
        progress!(options.quiet, "Compressing to memory");
    }

    progress!(options.quiet, "Parsing.");

    handle_path_or_text(options, EncodeParams {
        source: Source::FromFile { path: source_path },
        dest_bin_path,
        dest_txt_path,
    });
}

fn handle_path_or_text<'a>(options: &mut Options<'a>,
    params: EncodeParams)
{
    let (source_path, source_len, json) = match params.source {
        Source::FromFile { path } => {
            (Some(path),
             std::fs::metadata(path)
                 .expect("Could not open source")
                 .len(),
             options.parser.parse_file(path)
                 .expect("Could not parse source"))
        }
        Source::FromStdin { text } => {
            (None,
             text.len() as u64,
             options.parser.parse_str(text.as_str())
             .expect("Could not parse source"))
        }
    };
    let dest_bin_path = params.dest_bin_path;
    let dest_txt_path = params.dest_txt_path;

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

    if options.show_ast {
        use binjs::generic::ToJSON;
        let json = ast.export();
        println!("{:#}", json);
    }

    progress!(options.quiet, "Encoding.");
    let encoder = Encoder::new();
    let encoding_path = match dest_bin_path {
        None => None,
        Some(ref buf) => Some(buf.as_path())
    };
    let data = encoder.encode(encoding_path, &mut options.format, &ast)
        .expect("Could not encode");
    if dest_txt_path.is_some() {
        options.format.with_sections::<_, ()>(|contents, name| {
            export_section(&dest_bin_path, contents, name);
            Ok(())
        })
        .expect("Could not write sections");
    };
    let dest_len = data.as_ref().as_ref().len();

    if let Some(ref bin_path) = dest_bin_path {
        progress!(options.quiet, "Writing binary file.");
        let mut dest = File::create(bin_path)
            .unwrap_or_else(|e| panic!("Could not create destination file {:?}: {:?}", bin_path, e));
        dest.write((*data).as_ref())
            .expect("Could not write destination file");
    } else {
        stdout().write((*data).as_ref())
            .expect("Could not write to stdout");
    }

    if let Some(ref txt_path) = dest_txt_path {
        if txt_path.exists() {
            progress!(options.quiet, "A file with name {:?} already exists, skipping copy.", txt_path);
        } else {
            progress!(options.quiet, "Copying source file.");

            std::fs::copy(source_path.unwrap(), txt_path)
                .expect("Could not copy source file");
        }
    }

    progress!(options.quiet, "Successfully compressed {} bytes => {} bytes", source_len, dest_len);
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
        .about("Encode a JavaScript text source to a JavaScript binary source in the BinJS format.")
        .args(&[
            Arg::with_name("in")
                .long("in")
                .short("i")
                .multiple(true)
                .takes_value(true)
                .help("Input files to use. Must be JS source file. May be specified multiple times. If not specified, stdin is used."),
            Arg::with_name("out")
                .long("out")
                .short("o")
                .takes_value(true)
                .help("Output directory to use. Files in this directory may be overwritten. Requires --in. If not specified, stdout is used"),
            Arg::with_name("statistics")
                .long("show-stats")
                .help("Show statistics."),
            Arg::with_name("show-ast")
                .long("show-ast")
                .help("Show pos-processed ast"),
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
        ])
        .subcommand(binjs::io::Format::subcommand())
        .get_matches();

    // Common options.
    let sources : Vec<_> = matches.values_of("in")
        .map_or_else(|| Vec::new(),
                     |input| input
                     .map(Path::new)
                     .collect());

    let dest_dir = if sources.len() == 0 {
        // If --in is not specified, --out is not used even if specified.
        // Instead, the result is printed to stdout.
        None
    } else {
        match matches.value_of("out") {
            None => None,
            Some(path) => Some(Path::new(path).to_path_buf())
        }
    };

    let quiet = matches.is_present("quiet") || dest_dir.is_none();

    // Format options.
    let format = binjs::io::Format::from_matches(&matches)
        .expect("Could not parse encoding format");
    progress!(quiet, "Using format: {}", format.name());

    let show_stats = matches.is_present("statistics");

    // Setup.
    let parser = Shift::new();

    let lazification = str::parse(matches.value_of("lazify").expect("Missing lazify"))
        .expect("Invalid number");

    let mut options = Options {
        parser: &parser,
        format,
        dest_dir,
        lazification,
        show_ast: matches.is_present("show-ast"),
        quiet
    };

    if sources.len() == 0 {
        // Use stdin if --in is not specified.
        let mut buffer = String::new();
        stdin().read_to_string(&mut buffer)
            .expect("Failed to read from stdin");

        handle_path_or_text(&mut options, EncodeParams {
            source: Source::FromStdin { text: buffer },
            dest_bin_path: None,
            dest_txt_path: None
        });
    } else {
        for source_path in sources {
            handle_path(&mut options, source_path, PathBuf::new().as_path());
        }
    }

    if show_stats {
        match options.format {
            Format::Multipart { ref stats, .. } => {
                progress!(options.quiet, "Statistics: {}", stats.borrow());
            }
            Format::Entropy { options: ref entropy } => {
                progress!(options.quiet, "Statistics: {}", entropy.statistics_for_write());
            }
            _ => {
                progress!(options.quiet, "No stats available for this format");
            }
        }
    }
}
