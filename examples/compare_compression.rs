//! Compare compression results

extern crate binjs;
extern crate clap;
#[macro_use]
extern crate derive_more;
extern crate env_logger;
extern crate glob;
extern crate itertools;
extern crate rand;

use binjs::generic::FromJSON;
use binjs::source::*;

use clap::*;

use itertools::Itertools;

use std::collections::HashMap;
use std::io::Write;
use std::process::Command;

#[derive(Add, AddAssign, Clone, Default)]
struct Sizes {
    uncompressed: usize,
    gzip: usize,
    bzip2: usize,
    brotli: usize,
}


#[derive(Add, AddAssign, Clone, Default)]
struct FileStats {
    from_text: Sizes,
    from_binjs: Sizes,
}

fn get_compressed_sizes(path: &std::path::Path) -> Sizes {
    let uncompressed = std::fs::metadata(path)
        .expect("Could not open source")
        .len() as usize;
    let gzip = {
        let out = Command::new("gzip")
            .arg("--keep")
            .arg("--best")
            .arg("--stdout")
            .arg(path)
            .output()
            .expect("Error during gzip");
        assert!(out.status.success());
        assert!(out.stdout.len() != 0);
        out.stdout.len()
    };
    let bzip2 = {
        let out = Command::new("bzip2")
            .arg("--keep")
            .arg("--best")
            .arg("--stdout")
            .arg(path)
            .output()
            .expect("Error during bzip2");
        assert!(out.status.success());
        assert!(out.stdout.len() != 0);
        out.stdout.len()
    };
    let brotli = {
        let out = Command::new("brotli")
            .arg("--best")
            .arg("--keep")
            .arg("--stdout")
            .arg(path)
            .output()
            .expect("Error during brotli");
        assert!(out.status.success());
        assert!(out.stdout.len() != 0);
        out.stdout.len()
    };
    Sizes {
        bzip2,
        brotli,
        gzip,
        uncompressed,
    }
}

fn main() {
    env_logger::init();
    let dest_path_binjs = "/tmp/binjs-test.js.binjs";

    let matches = App::new("Compare BinJS compression and brotli/gzip compression")
        .author("David Teller <dteller@mozilla.com>")
        .args(&[Arg::with_name("in")
            .long("in")
            .short("i")
            .multiple(true)
            .required(true)
            .takes_value(true)
            .number_of_values(1)
            .help("Glob path towards source files")])
        .subcommand(binjs::io::Format::subcommand())
        .get_matches();

    let mut format =
        binjs::io::Format::from_matches(&matches).expect("Could not determine encoding format");
    println!("Using format: {}", format.name());
    println!("Source files: {}", matches.values_of("in").unwrap().format(", "));

    let parser = Shift::try_new().expect("Could not launch Shift");

    let mut all_stats = HashMap::new();

    let encoder = binjs::specialized::es6::io::Encoder::new();

    for path in matches.values_of("in").expect("Missing `in`") {
        for source_path in glob::glob(&path).expect("Invalid pattern") {
            let source_path = source_path.expect("I/O error");
            eprintln!(
                "Source: {}",
                source_path.to_str().expect("Could not display path")
            );

            let from_text = get_compressed_sizes(&source_path);

            eprintln!("Compressing with binjs");
            let json = parser
                .parse_file(source_path.clone())
                .expect("Could not parse source");
            let mut ast =
                binjs::specialized::es6::ast::Script::import(&json).expect("Could not import AST");
            binjs::specialized::es6::scopes::AnnotationVisitor::new().annotate_script(&mut ast);

            let data = encoder
                .encode(None, &mut format, &ast)
                .expect("Could not encode");

            {
                let mut binjs_encoded = std::fs::File::create(&dest_path_binjs)
                    .expect("Could not create binjs-encoded file");
                binjs_encoded
                    .write_all((*data).as_ref())
                    .expect("Could not write binjs-encoded file");
            }

            let from_binjs = get_compressed_sizes(&std::path::Path::new(dest_path_binjs));

            let file_stats = FileStats {
                from_binjs,
                from_text,
            };

            eprintln!("Compression results: source {source}b, source+gzip {source_gzip}, source+brotli {source_brotli}, source+bzip2 {source_bzip2}, binjs {binjs}b, binjs+gzip {binjs_gzip}, binjs+brotli {binjs_brotli}, binjs+bzip2 {binjs_bzip2}",
                source = file_stats.from_text.uncompressed,
                source_gzip = file_stats.from_text.gzip,
                source_brotli = file_stats.from_text.brotli,
                source_bzip2 = file_stats.from_text.bzip2,

                binjs = file_stats.from_binjs.uncompressed,
                binjs_gzip = file_stats.from_binjs.gzip,
                binjs_brotli = file_stats.from_binjs.brotli,
                binjs_bzip2 = file_stats.from_binjs.bzip2,
            );

            all_stats.insert(source_path, file_stats);
        }
    }

    eprintln!("*** Done");

    let all_stats = all_stats.into_iter().sorted_by(|a, b| Ord::cmp(&a.0, &b.0));
    println!("File, Source (b), BinAST/Source+Brotli");
    let mut sum_stats = FileStats::default();
    for (_, file_stats) in &all_stats {
        sum_stats += file_stats.clone();
    }
    print_one(&"<<TOTAL>>", &sum_stats);

    for (path, file_stats) in &all_stats {
        print_one(path, file_stats);
    }
}

fn print_one<T: std::fmt::Debug>(name: &T, file_stats: &FileStats) {
    println!("{name:?}, {source}, {binjs_to_brotli:.2}",
        source = file_stats.from_text.uncompressed,
        binjs_to_brotli = (file_stats.from_binjs.uncompressed as f64) / (file_stats.from_text.brotli as f64),
        name = name);
}