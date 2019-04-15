//! Given a directory, generate a dictionary from a random sample, use the
//! other files in the directory for testing purposes.

extern crate binjs;
extern crate clap;
extern crate rand;
extern crate separator;

use binjs::io::entropy::dictionary::{DictionaryBuilder, Options as DictionaryOptions};
use binjs::io::entropy::Options;
use binjs::io::{Path as IOPath, Serialization, TokenSerializer};
use binjs::source::{Shift, SourceParser};

use clap::{App, Arg};
use rand::{Rng, SeedableRng};
use separator::Separatable;

use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

#[derive(Debug)]
struct Entry {
    default_path: PathBuf,
    relative_path: PathBuf,
}

#[derive(Debug)]
struct Compression {
    original_size: u64,
    brotli_size: u64,
    binjs_size: u64,
    binjs_div_brotli: f64,
}

fn get_files<P: AsRef<Path>>(path: &P) -> io::Result<Vec<Entry>> {
    fn aux<P: AsRef<Path>>(path: &P, prefix: &Path, list: &mut Vec<Entry>) -> io::Result<()> {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                if let Some(filename) = path.file_name() {
                    let prefix = prefix.join(filename);
                    aux(&path, &prefix, list)?;
                }
            } else {
                let entry = Entry {
                    default_path: path,
                    relative_path: prefix.to_path_buf(),
                };
                list.push(entry);
            }
        }
        Ok(())
    }
    let mut vec = vec![];
    aux(path, &Path::new(""), &mut vec)?;
    Ok(vec)
}

fn validate<T>(value: String) -> Result<(), String>
where
    T: FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
{
    T::from_str(&value).map_err(|e| format!("{:?}", e))?;
    Ok(())
}

fn main() {
    let matches = App::new("BinJS encoder")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Evaluate the compression level of `binjs_encode` on a directory.")
        .args(&[
            Arg::with_name("in")
                .long("in")
                .short("i")
                .multiple(false)
                .takes_value(true)
                .required(true)
                .number_of_values(1)
                .help("Input directory to use."),
            Arg::with_name("out")
                .long("out")
                .short("o")
                .multiple(false)
                .takes_value(true)
                .required(false)
                .number_of_values(1)
                .help("Output directory to use for dumping streams. If absent, streams won't be dumped."),
            Arg::with_name("seed")
                .long("seed")
                .takes_value(true)
                .required(false)
                .default_value("0")
                .validator(validate::<usize>)
                .help("An arbitrary integer, used to pseudo-randomly pick which files to use to pick a dictionary. Different values will produce different results, but across instances of the same binary, the same value will produce the same result."),
            Arg::with_name("sampling")
                .long("sampling")
                .takes_value(true)
                .required(false)
                .default_value("0.1")
                .validator(|s| {
                    let ratio = f64::from_str(&s).map_err(|e| format!("{:?}", e))?;
                    if ratio < 0. || ratio > 1. {
                        return Err("Value must be in [0, 1]".to_string())
                    }
                    Ok(())
                })
                .help("A number in [0, 1], used to determine the percentage of files which will be used to build the dictionary. 0 = empty dictionary, 1 = empty test set"),
            Arg::with_name("depth")
                .long("depth")
                .takes_value(true)
                .required(false)
                .default_value("2")
                .validator(validate::<usize>)
                .help("The size of the context for the dictionary, in path depth levels."),
            Arg::with_name("dictionary-threshold")
                .long("dictionary-threshold")
                .value_name("N")
                .takes_value(true)
                .required(false)
                .default_value("2")
                .validator(validate::<usize>)
                .help("Transfer to the dictionary strings that appear in at least N files"),
            Arg::with_name("show-hall-of-shame")
                .long("show-hall-of-shame")
                .takes_value(false)
                .help("Display the list of files from worst compression ratio to best compression ratio"),
            Arg::with_name("min-size")
                .value_name("SIZE")
                .takes_value(true)
                .default_value("3072")
                .validator(validate::<usize>)
                .help("Filter out files smaller than SIZE bytes"), // 3kb
        ])
        .get_matches();

    let sampling = f64::from_str(matches.value_of("sampling").unwrap())
        .expect("Invalid argument value: sampling");

    let show_results = matches.is_present("show-hall-of-shame");
    let dict_threshold = usize::from_str(matches.value_of("dictionary-threshold").unwrap())
        .expect("Invalid argument value: dictionary-threshold");
    let depth = usize::from_str(matches.value_of("depth").unwrap()).unwrap();
    let min_size = usize::from_str(matches.value_of("min-size").unwrap()).unwrap();
    let seed = u64::from_str(matches.value_of("seed").unwrap()).unwrap();
    let dump_path = matches.value_of("out").map(Path::new);

    let spec = binjs::generic::es6::Library::spec();
    let parser = Shift::try_new().expect("Could not launch Shift");
    let mut rng = rand::rngs::SmallRng::seed_from_u64(seed);

    println!("** Collecting list of files");
    let files = get_files(&Path::new(matches.value_of("in").unwrap()))
        .expect("Could not build list of files");

    // Split file list pseudo-randomly across dictionary vs. control.
    let (dictionary, control): (Vec<_>, Vec<_>) = files
        .into_iter()
        .partition(|_| rng.gen_range(0., 1.) <= sampling);

    println!("** Building dictionary from {} files", dictionary.len());
    let mut dictionary_builder =
        DictionaryBuilder::new(DictionaryOptions::default().with_depth(depth).with_width(1));

    let dictionary_len = dictionary.len();
    for (index, entry) in dictionary.into_iter().enumerate() {
        println!("...file {}/{}", index, dictionary_len);
        let original_size = std::fs::metadata(&entry.default_path).unwrap().len();
        if original_size < min_size as u64 {
            // Skip files that are too small.
            continue;
        }
        let mut ast = match parser.parse_file(&entry.default_path) {
            Ok(ast) => ast,
            Err(e) => {
                // Could not parse source
                println!("...skipping file due to parse error {:?}", e);
                continue;
            }
        };

        println!("...adding {:?}", entry.default_path);
        let enricher = binjs::specialized::es6::Enrich::default();
        if let Err(e) = enricher.enrich(&mut ast) {
            println!("...skipping file due to enrichment error {:?}", e);
            continue;
        }

        let mut serializer = binjs::specialized::es6::io::Serializer::new(&mut dictionary_builder);
        serializer
            .serialize(&ast, &mut IOPath::new())
            .expect("Could not generate dictionary");
        serializer.done().expect("Could not finalize dictionary");
    }

    let dictionary = dictionary_builder.done(dict_threshold.into());

    let mut options = Options::new(&spec, dictionary);
    options.with_split_streams(dump_path.is_some());

    println!("** Testing on {} files", control.len());
    let mut total_original_size = 0;
    let mut total_binjs_size = 0;
    let mut total_brotli_size = 0;
    let mut results = Vec::with_capacity(control.len());

    let control_len = control.len();
    for (index, entry) in control.into_iter().enumerate() {
        println!("...file {}/{}", index, control_len);
        let original_size = match std::fs::metadata(&entry.default_path) {
            Ok(metadata) => metadata.len(),
            Err(e) => {
                println!(
                    "...skipping file {:?} due to access error {:?}",
                    entry.default_path, e
                );
                continue;
            }
        };
        if original_size < min_size as u64 {
            // Skip files that are too small.
            continue;
        }

        let mut ast = match parser.parse_file(&entry.default_path) {
            Ok(ast) => ast,
            Err(e) => {
                // Could not parse source
                println!("...skipping file due to parse error {:?}", e);
                continue;
            }
        };

        println!("...compressing {:?}", entry.default_path);
        println!(
            "...original size size: {}",
            original_size.separated_string()
        );
        total_original_size += original_size;

        let enricher = binjs::specialized::es6::Enrich::default();
        if let Err(e) = enricher.enrich(&mut ast) {
            println!("...skipping file due to enrichment error {:?}", e);
            continue;
        }

        let destination_path = match dump_path {
            Some(ref path) => Some(path.join(entry.relative_path.as_path())),
            None => None,
        };
        let destination_path = match destination_path {
            // Note: We need to keep `path` alive.
            Some(ref path) => Some(path.as_ref()),
            None => None,
        };
        let mut format = binjs::io::Format::Entropy {
            options: options.clone(),
        };
        let mut encoder = binjs::specialized::es6::io::Encoder::new();
        let data = encoder
            .encode(destination_path, &mut format, &ast)
            .expect("Could not compress to binast");

        let binjs_size = data.len() as u64;
        println!("...binjs size: {}", binjs_size.separated_string());
        total_binjs_size += binjs_size;

        let brotli_size = {
            let out = Command::new("brotli")
                .arg("--best")
                .arg("--keep")
                .arg("--stdout")
                .arg(&entry.default_path)
                .output()
                .expect("Error during brotli");
            assert!(out.status.success());
            assert!(out.stdout.len() != 0);
            out.stdout.len()
        } as u64;
        println!("...brotli size: {}", brotli_size.separated_string());
        total_brotli_size += brotli_size;

        let binjs_div_brotli = binjs_size as f64 / brotli_size as f64;
        println!("...binjs/brotli: {:.2}", binjs_div_brotli);

        let compression = Compression {
            original_size,
            brotli_size,
            binjs_size,
            binjs_div_brotli,
        };
        results.push((compression, entry.default_path));
    }

    if show_results {
        println!("** Ranked results (worse to best)");
        results.sort_by(|a, b| {
            f64::partial_cmp(&a.0.binjs_div_brotli, &b.0.binjs_div_brotli).unwrap()
        });
        for (compression, path) in results.into_iter() {
            println!("{:?}", path);
            println!(
                "...original size: {}",
                compression.original_size.separated_string()
            );
            println!(
                "...binjs size: {}",
                compression.binjs_size.separated_string()
            );
            println!(
                "...brotli size: {}",
                compression.brotli_size.separated_string()
            );
            println!("...ratio: {:.2}", compression.binjs_div_brotli);
        }
    }

    println!("** Final results");
    println!(
        "Total original size: {}",
        total_original_size.separated_string()
    );
    println!("Total binjs size: {}", total_binjs_size.separated_string());
    println!(
        "Total brotli size: {}",
        total_brotli_size.separated_string()
    );
    println!(
        "binjs/brotli: {:.2}",
        total_binjs_size as f64 / total_brotli_size as f64
    );
}
