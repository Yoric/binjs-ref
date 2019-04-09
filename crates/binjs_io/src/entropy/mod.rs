//! A format based on Entropy coding
//!
//! We generate a table of probabilities that let us determine the probability of a given
//! node appearing at a given position in the AST. We then use a mechanism of entropy coding
//! to represent the actual AST as a combination of these probabilities.
//!
//! See module `format` for details on the binary format.

pub mod format;

mod baseline;

pub mod collect;

pub mod dictionary;

/// Reading a compressed stream.
pub mod read;

/// Tools shared by `read` and `write`.
pub mod rw;

/// Misc convenience utilities.
mod util;

/// Writing to a compressed stream.
pub mod write;

mod predict;
pub mod probabilities;

use self::dictionary::{Dictionary, DictionaryFamily};
use self::probabilities::SymbolInfo;

use io::statistics::{
    Bytes, BytesAndInstances, Instances, PerStaticKind, PerUserExtensibleKind, ProbabilityHistogram,
};

use binjs_shared::SharedString;

use std::cell::RefCell;
use std::rc::Rc;

const_with_str! {
    /// The number of indices to reserve to recall
    /// recently used values in a content stream.
    ///
    /// In issue #302, we have established that identifier_names
    /// seems to benefit of a window length of 8, and others
    /// a window length of 0.
    ///
    /// The macro call generates a module `arg_as_str` holding the
    /// string representations of the consts. Used for the `clap`
    /// default args.

    const DEFAULT_WINDOW_LEN_FLOATS: usize = 0;
    const DEFAULT_WINDOW_LEN_UNSIGNED_LONGS: usize = 0;
    const DEFAULT_WINDOW_LEN_PROPERTY_KEYS: usize = 0;
    const DEFAULT_WINDOW_LEN_IDENTIFIER_NAMES: usize = 8;
    const DEFAULT_WINDOW_LEN_STRING_LITERALS: usize = 0;
    const DEFAULT_WINDOW_LEN_LIST_LENGTHS: usize = 0;

    mod arg_as_str;
}

#[derive(Clone)]
pub struct Options {
    /// A family of dictionaries used to encode/decode an AST.
    /// By convention
    /// - key `""` maps to the starting dictionary;
    /// - key `"*"` maps to the fallback dictionary, in which
    ///   all probabilities are equal.
    dictionaries: DictionaryFamily<SymbolInfo>,

    /// Statistics obtained while writing: number of bytes written.
    /// If several files are written with the same options, we accumulate
    /// statistics.
    content_lengths: Rc<RefCell<PerUserExtensibleKind<Bytes>>>,

    /// Statistics obtained while writing: number of instances of each
    /// kind written. If several files are written with the same options,
    /// we accumulate statistics.
    content_instances: Rc<RefCell<PerUserExtensibleKind<Instances>>>,

    probability_stats: Rc<RefCell<PerStaticKind<ProbabilityHistogram>>>,

    /// For each content section, the number of indices reserved to reference
    /// recently used values.
    content_window_len: PerUserExtensibleKind<usize>,

    /// If `true`, when compressing a file, also write streams to separate files,
    /// for analysis purposes.
    split_streams: bool,
}
impl Options {
    /// Create a fresh `Options` to encode/decode in a given grammar spec with
    /// a family of dictionaries.
    ///
    /// If `dictionaries` is empty, a default dictionary is insertred.
    pub fn new(
        spec: &binjs_meta::spec::Spec,
        mut dictionaries: DictionaryFamily<Instances>,
    ) -> Self {
        use entropy::probabilities::InstancesToProbabilities;

        // Introduce the fallback dictionary.
        let baseline = baseline::build(spec);
        for dictionary in dictionaries.values_mut() {
            *dictionary = dictionary.with_grammar_fallback(baseline.clone());
        }
        assert!(
            !dictionaries.insert_baseline(baseline),
            "The dictionary family MUST start without a definition for dictionary `*`"
        );
        let mut dictionaries = dictionaries.instances_to_probabilities("dictionary");
        dictionaries
            .enter_existing(&SharedString::from_str(""))
            .unwrap(); // We just created this dictionary with the call to `insert_baseline`.
        Options {
            dictionaries,
            content_lengths: Rc::new(RefCell::new(PerUserExtensibleKind::default())),
            content_instances: Rc::new(RefCell::new(PerUserExtensibleKind::default())),
            probability_stats: Rc::new(RefCell::new(PerStaticKind::default())),
            split_streams: false,
            content_window_len: PerUserExtensibleKind {
                floats: DEFAULT_WINDOW_LEN_FLOATS,
                unsigned_longs: DEFAULT_WINDOW_LEN_UNSIGNED_LONGS,
                property_keys: DEFAULT_WINDOW_LEN_PROPERTY_KEYS,
                identifier_names: DEFAULT_WINDOW_LEN_IDENTIFIER_NAMES,
                string_literals: DEFAULT_WINDOW_LEN_STRING_LITERALS,
                list_lengths: DEFAULT_WINDOW_LEN_LIST_LENGTHS,
            },
        }
    }

    pub fn user_extensible_depth(&self) -> usize {
        self.dictionaries.depth()
    }

    /// Return the statistics as (number of instances, number of bytes).
    pub fn statistics_for_write(&self) -> impl std::fmt::Display {
        let per_user_extensible_kind = {
            let borrow_lengths = self.content_lengths.borrow();
            let borrow_instances = self.content_instances.borrow();
            PerUserExtensibleKind {
                floats: BytesAndInstances::new(borrow_lengths.floats, borrow_instances.floats),
                unsigned_longs: BytesAndInstances::new(
                    borrow_lengths.unsigned_longs,
                    borrow_instances.unsigned_longs,
                ),
                property_keys: BytesAndInstances::new(
                    borrow_lengths.property_keys,
                    borrow_instances.property_keys,
                ),
                identifier_names: BytesAndInstances::new(
                    borrow_lengths.identifier_names,
                    borrow_instances.identifier_names,
                ),
                string_literals: BytesAndInstances::new(
                    borrow_lengths.string_literals,
                    borrow_instances.string_literals,
                ),
                list_lengths: BytesAndInstances::new(
                    borrow_lengths.list_lengths,
                    borrow_instances.list_lengths,
                ),
            }
        };

        let probability_stats = self.probability_stats.clone();

        struct Statistics<A, B> {
            bytes_and_instances_per_user_extensible_kind: A,
            probability_histogram_per_static_kind: Rc<RefCell<B>>,
        }
        impl<A, B> std::fmt::Display for Statistics<A, B>
        where
            A: std::fmt::Display,
            B: std::fmt::Display,
        {
            fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
                self.bytes_and_instances_per_user_extensible_kind
                    .fmt(formatter)?;
                self.probability_histogram_per_static_kind
                    .borrow()
                    .fmt(formatter)?;
                Ok(())
            }
        }
        Statistics {
            bytes_and_instances_per_user_extensible_kind: per_user_extensible_kind,
            probability_histogram_per_static_kind: probability_stats,
        }
    }

    /// Configure the `split-streams` option.
    ///
    /// If `value == true`, dump the individual streams for forensics purposes,
    /// otherwise don't.
    pub fn with_split_streams(&mut self, value: bool) -> &mut Self {
        self.split_streams = value;
        self
    }
}

/// Command-line management.
use clap;

pub struct FormatProvider;
impl ::FormatProvider for FormatProvider {
    fn subcommand<'a, 'b>(&self) -> clap::App<'a, 'b> {
        use clap::*;

        let validate_usize = |s: String| {
            usize::from_str_radix(&s, 10)
                .map(|_| ())
                .map_err(|e| format!("{}", e))
        };

        SubCommand::with_name("entropy")
            .about("(EXPERIMENTAL) Encode using entropy compression. This format produces a very good compression ratio in presence of a good dictionary.")
            .arg(Arg::with_name("dictionary")
                .help("(Recommended) path to external dictionary generated by binjs_generate_prediction_tables.")
                .long("dictionary")
                .takes_value(true)
                .required(false)
            )
            .arg(Arg::with_name("split-streams")
                .long("split-streams")
                .help("If specified, dump all streams to the disk, for forensics purposes.")
                .takes_value(false)
            )
            .arg(Arg::with_name("window-len-floats")
                .long("window-len-floats")
                .help("The length of the recall window for floats, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_FLOATS)
            )
            .arg(Arg::with_name("window-len-unsigned-longs")
                .long("window-len-unsigned-longs")
                .help("The length of the recall window for unsigned longs, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_UNSIGNED_LONGS)
            )
            .arg(Arg::with_name("window-len-property-keys")
                .long("window-len-property-keys")
                .help("The length of the recall window for property keys, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_PROPERTY_KEYS)
            )
            .arg(Arg::with_name("window-len-identifier-names")
                .long("window-len-identifier-names")
                .help("The length of the recall window for identifier names, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_IDENTIFIER_NAMES)
            )
            .arg(Arg::with_name("window-len-string-literals")
                .long("window-len-string-literals")
                .help("The length of the recall window for string literals, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_STRING_LITERALS)
            )
            .arg(Arg::with_name("window-len-list-lengths")
                .long("window-len-list-lengths")
                .help("The length of the recall window for list lengths, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_LIST_LENGTHS)
            )
    }

    fn handle_subcommand(
        &self,
        spec: &binjs_meta::spec::Spec,
        matches: Option<&clap::ArgMatches>,
    ) -> Result<::Format, ::std::io::Error> {
        use bincode;

        let matches = matches.unwrap();

        let dictionaries = match matches.value_of("dictionary") {
            None => DictionaryFamily::new(),
            Some(path) => {
                // Load a dictionary from disk.
                let source = std::fs::File::open(&path).expect("Could not open dictionary");
                let surface_dictionary: DictionaryFamily<Instances> =
                    bincode::deserialize_from(source).expect("Could not decode dictionary");
                surface_dictionary
            }
        };

        let split_streams = matches.is_present("split-streams");

        let convert_usize = |key: &str| {
            usize::from_str_radix(&matches.value_of(key).unwrap(), 10)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, format!("{}", e)))
        };

        let content_window_len = PerUserExtensibleKind {
            floats: convert_usize("window-len-floats")?,
            unsigned_longs: convert_usize("window-len-unsigned-longs")?,
            property_keys: convert_usize("window-len-property-keys")?,
            identifier_names: convert_usize("window-len-identifier-names")?,
            string_literals: convert_usize("window-len-string-literals")?,
            list_lengths: convert_usize("window-len-list-lengths")?,
        };

        Ok(::Format::Entropy {
            options: Options {
                content_lengths: Rc::new(RefCell::new(PerUserExtensibleKind::default())),
                content_instances: Rc::new(RefCell::new(PerUserExtensibleKind::default())),
                split_streams,
                content_window_len,
                ..Options::new(spec, dictionaries)
            },
        })
    }
}
