//! A format in which we adopt an arithmetic encoding per nature.
//!
//! # Overview
//!
//! The general idea of arithmetic coding is that we can achieve a high
//! density of information by encoding a sequence of symbols as a single
//! rational number in [0, 1), with the actual number being determined
//! from the probability of encountering each successive symbol in a
//! given context.
//!
//! For instance, consider `BinaryOperation(op, left, right)` in a context C.
//! For this example, let's assume that we are starting from the full segment
//! [0, 1). In context C, we may encounter a number of subtrees labelled S_1,
//! S_2, ..., S_n with probabilities p(C, S_1), p(C, S_2), ..., p(C, S_n).
//! For simplicity, let's assume that label `BinaryOperation` is S_1
//! (so, with probability p(C, S_1))
//! Any number in [0, p(C, S_1)) may be used to represent `BinaryOperation`.
//!
//! Now, we proceed to encode `op` in a context C + `BinaryOperation`. Again,
//! we have a number of possible values for `op`: op_1, ... op_n'. If `op`
//! is `op_1`, any number of [0, p(C, S_1) * P(C + BinaryOperation, op_1))
//! may be used to represent `op`.
//!
//! Similarly, we'll encode `left` and `right` in context C + `BinaryOperation`.
//!
//!
//! # Random notes on the format
//!
//! - We don't want too many dependencies between the dictionaries of two lazy
//!  functions, as we generally don't know in which order they will be loaded.
//!  The exception is if each lazy function has its own header and this header
//!  contains a dictionary, in which case we may easily load the headers and
//!  dictionaries in reading order. I guess we should do that.
//!
//! # Encoding
//!
//! Pass 1: walk the tree, compute the probabilities for each symbol.
//! Pass 2: use probabilities to actually encode the symbols.
//!   - Whenever we encounter a symbol that we have never seen in a given context,
//!     the symbol is followed by its definition as soon as possible.
//!
//! FIXME: Some symbols will need to be defined more than once. Need to estimate
//! how often this appears.
//!
//! FIXME: Wait, no, that doesn't work. If we want this to work, we first need
//! to deliver the probability table for everything. That's not what we want.
//!
//! Option: we may initialize the dictionary as follows:
//! - initial probability of a new symbol is 1.
//! - whenever we create a symbol, we followup with its actual probability
//! - the probability of new symbols decreases each time we add a symbol
//!
//!
//! ----- Initially, start with everything equi-likely. We'll add a predefined
//! and/or custom dictionary later.

pub mod dictionary;
pub mod read;
pub mod write;

mod predict;
pub mod probabilities;

use self::dictionary::Dictionary;
use self::predict::Instances;
use self::probabilities::SymbolInfo;
use self::write::ContentInfo;

use ::bytes::lengthwriter::Bytes;

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub struct Options {
    /// The (shared) AST probability tables, generally shipped separately
    /// from the compressed files and used to predict the probability
    /// of a symbol occurring at a specific position in the AST.
    probability_tables: Dictionary<SymbolInfo>,

    /// Statistics obtained while writing: number of bytes written.
    /// If several files are written with the same options, we accumulate
    /// statistics.
    content_lengths: Rc<RefCell<ContentInfo<Bytes>>>,

    /// Statistics obtained while writing: number of instances of each
    /// kind written. If several files are written with the same options,
    /// we accumulate statistics.
    content_instances: Rc<RefCell<ContentInfo<Instances>>>,
}
impl Options {
    pub fn new(probability_tables:Dictionary<SymbolInfo>) -> Self {
        Options {
            probability_tables,
            content_lengths: Rc::new(RefCell::new(write::ContentInfo::default())),
            content_instances: Rc::new(RefCell::new(write::ContentInfo::default())),
        }
    }

    /// Return the statistics as (number of instances, number of bytes).
    pub fn statistics_for_write(&self) -> ContentInfo<(Bytes, Instances)> {
        let borrow_lengths = self.content_lengths.borrow();
        let borrow_instances = self.content_instances.borrow();
        ContentInfo {
            bools: (borrow_lengths.bools, borrow_instances.bools),
            floats: (borrow_lengths.floats, borrow_instances.floats),
            unsigned_longs: (borrow_lengths.unsigned_longs, borrow_instances.unsigned_longs),
            string_enums: (borrow_lengths.string_enums, borrow_instances.string_enums),
            property_keys: (borrow_lengths.property_keys, borrow_instances.property_keys),
            identifier_names: (borrow_lengths.identifier_names, borrow_instances.identifier_names),
            interface_names: (borrow_lengths.interface_names, borrow_instances.interface_names),
            string_literals: (borrow_lengths.string_literals, borrow_instances.string_literals),
            list_lengths: (borrow_lengths.list_lengths, borrow_instances.list_lengths),
        }
    }
}

/// Command-line management.
use clap;

pub struct FormatProvider;
impl ::FormatProvider for FormatProvider {
    fn subcommand<'a, 'b>(&self) -> clap::App<'a, 'b> {
        use clap::*;
        SubCommand::with_name("entropy")
            .about("(EXPERIMENTAL) Encode using entropy compression. This format should eventually produce very good compression ratio.")
            .arg(Arg::with_name("dictionary")
                .help("Path to external probability tables dictionary (generated by binjs_generate_prediction_tables)")
                .long("dictionary")
                .takes_value(true)
                .required(true)
            )
            .arg(Arg::with_name("path-depth")
                .long("path-depth")
                .takes_value(true)
                .default_value("3")
            )
    }

    fn handle_subcommand(&self, matches: Option<&clap::ArgMatches>) -> Result<::Format, ::std::io::Error> {
        use bincode;
        use self::probabilities::InstancesToProbabilities;

        let matches = matches.unwrap();

        let probability_tables_path = matches.value_of("dictionary")
            .unwrap(); // Guaranteed by `clap`.
        let probability_tables_source = std::fs::File::open(&probability_tables_path)
            .expect("Could not open dictionary");
        let probability_tables : Dictionary<Instances> = bincode::deserialize_from(probability_tables_source)
            .expect("Could not decode dictionary");

        Ok(::Format::Entropy {
            options: Options {
                probability_tables: probability_tables.instances_to_probabilities("probability_tables"),
                content_lengths: Rc::new(RefCell::new(write::ContentInfo::default())),
                content_instances: Rc::new(RefCell::new(write::ContentInfo::default())),
            }
        })
    }
}
