//! Constants and data structures shared between entropy read and entropy write.

use entropy::dictionary::{Fetch, LinearTable, TableRef};

use std::collections::HashMap;
use std::marker::PhantomData;

use smallvec::SmallVec;

/// The magic number at the start of the file.
///
/// The early \0 is used to ensure tools such as github detect the file as binary.
/// The various \r, \n, \0 are there as canaries to avoid ASCII<->BINARY transcoding
/// errors.
pub const GLOBAL_HEADER_START: &'static [u8; 8] = b"\x89BJS\r\n\0\n";

/// The main section, containing the entropy-encoded stream.
pub const SECTION_MAIN: &'static [u8] = b"[[main]]";
pub const SECTION_MAIN_WITHOUT_BRACKETS: &'static [u8] = b"main";

/// The prelude section, containing the dictionary extensions,
/// encoded using e.g. brotli.
pub const SECTION_PRELUDE: &'static [u8] = b"[[prelude]]";
// pub const SECTION_PRELUDE_WITHOUT_BRACKETS: &'static [u8] = b"prelude";

/// The content section, containing the streams of indices of
/// user-extensible values, encoded e.g. using brotli.
pub const SECTION_CONTENT: &'static [u8] = b"[[content]]";
pub const SECTION_CONTENT_WITHOUT_BRACKETS: &'static [u8] = b"content";

/// Indicating that the stream is compressed with format "entropy 0.2".
pub const FORMAT_ENTROPY_0: &'static [u8] = b"entropy0.2;";
// pub const FORMAT_IDENTITY: &'static [u8] = b"identity;";

/// Indicating that the stream is compressed with format "brotli".
pub const FORMAT_BROTLI: &'static [u8] = b"br;";

/// Prelude data.
///
/// The prelude contains streams to read or write dictionary extensions for
/// user-extensible symbols (aka "prelude dictionaries").
///
/// Examples of `T`: `LazyStream` (for writing) or `Cursor<Vec<u8>>` (for reading).
pub struct PreludeStreams<T> {
    /// Instances of IdentifierName.
    ///
    /// To aid with compression, we typically store user-extensible strings
    /// as two distinct data structures:
    ///
    /// - a single string, containing all concatenated strings without delimiter;
    /// - a list of lengths, used to split the string into the successive strings.
    pub identifier_names: T,
    pub identifier_names_len: T,

    /// Instances of PropertyKey
    ///
    /// To aid with compression, we typically store user-extensible strings
    /// as two distinct data structures:
    ///
    /// - a single string, containing all concatenated strings without delimiter;
    /// - a list of lengths, used to split the string into the successive strings.
    pub property_keys: T,
    pub property_keys_len: T,

    /// Instances of string literals.
    ///
    /// To aid with compression, we typically store user-extensible strings
    /// as two distinct data structures:
    ///
    /// - a single string, containing all concatenated strings without delimiter;
    /// - a list of lengths, used to split the string into the successive strings.
    pub string_literals: T,
    pub string_literals_len: T,

    /// Instances of InterfaceName
    pub interface_names: T,
    pub interface_names_len: T,

    /// Instances of string enums.
    pub string_enums: T,
    pub string_enums_len: T,

    /// Instances of list lengths.
    pub list_lengths: T,

    /// Instances of floating-point numbers.
    pub floats: T,

    /// Instances of unsigned longs.
    pub unsigned_longs: T,

    pub probabilities: T,
    pub probabilities_len: T,
}


/// A macro used to generate code that will operate on all fields of a `PerUserExtensibleKind`.
#[macro_export]
macro_rules! for_field_in_prelude_streams {
    ( $cb: ident ) => {
        $cb!(
			(identifier_names, "identifier_names", b"identifier_names"),
			(identifier_names_len, "identifier_names_len", b"identifier_names_len"),
			(property_keys, "property_keys", b"property_keys"),
			(property_keys_len, "property_keys_len", b"property_keys_len"),
			(string_literals, "string_literals", b"string_literals"),
			(string_literals_len, "string_literals_len", b"string_literals_len"),
			(interface_names, "interface_names", b"interface_names"),
			(interface_names_len, "interface_names_len", b"interface_names_len"),
			(string_enums, "string_enums", b"string_enums"),
			(string_enums_len, "string_enums_len", b"string_enums_len"),
			(list_lengths, "list_lengths", b"list_lengths"),
			(floats, "floats", b"floats"),
			(unsigned_longs, "unsigned_longs", b"unsigned_longs"),
			(probabilities, "probabilities", b"probabilities"),
			(probabilities_len, "probabilities_len", b"probabilities_len")
        )
    };
}

impl<T> PreludeStreams<T> {
    /// Create a new PreludeStreams.
    pub fn with<F>(f: F) -> Self
    where
        F: Fn(&str) -> T,
    {
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            PreludeStreams {
                $(
                    $ident: f($name),
                )*
            }
        } };
        for_field_in_prelude_streams!(with_field)
    }

    /// Iterate through fields of PreludeStreams.
    pub fn into_iter(self) -> impl Iterator<Item = (&'static str, T)> {
        // Generate a vector with one item per field
        //    `(foo, self.foo)`.
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            vec![
                $(
                    ($name, self.$ident),
                )*
            ]
        } };
        for_field_in_prelude_streams!(with_field).into_iter()
    }

    /// Access a field by its name, specified as a sequence of bytes.
    ///
    /// This method is typically used to simplify parsing a file that
    /// contains sections explicitly labelled "identifier_names",
    /// "property_keys", etc.
    /// In such case, `field_name` is expected to be a user input.
    ///
    /// Return `None` if `field_name` is not one of the field names.
    pub fn get_mut_b(&mut self, field_name: &[u8]) -> Option<&mut T> {
        // Generate a `match` with for each field
        //    `b"foo" => Some(&mut self.foo)`
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            match field_name {
                $(
                    $bname => Some(&mut self.$ident),
                )*
                _ => None,
            }
        } };
        for_field_in_prelude_streams!(with_field)
    }
}

/// A stack-allocated buffer for names of sections and streams.
pub const NAME_MAX_LEN: usize = 32;
pub type NameData = SmallVec<[u8; NAME_MAX_LEN]>;

/// State of a input/output stream of `TableRef`.
///
/// Used to (de)serialize `TableRef`, as the encoding of an `TableRef`
/// may depend on that of previous values.
///
/// # Format
///
/// u32 representation: `TableRef` interpretation
///
/// - Next in Prelude
///    - `0`: `TableRef::Prelude(prelude_latest + 1)`
///           where `prelude_latest` is the latest prelude value encountered
///           so far or -1 if no prelude value has been encountered yet
///
/// - Recall Window Range [1, window_len] where `window_len` is the length
///           of the window, specified when creating the `TableRefStreamState`
///    - `1`: Latest value
///    - `2`: Second latest value
///    - ...
///    - `window_len`: ...
///
/// - Reference into Prelude Range: [window_len + 1, window_len + prelude_len]
///           where `prelude_len` is the number of elements in the Prelude.
///    - `window_len + 1`: `TableRef::Prelude(0)`
///    - `window_len + 2`: `TableRef::Prelude(1)`
///    - ...
///
///  - Imported Reference into Shared Range: [window_len + prelude_len + 1,
///            window_len + import_len], where `import_len` is the number of
///            **distinct** `TableRef::Shared` references encountere **so far**
///            in the stream.
///    - `window_len + prelude_len + 1`: `TableRef::Shared(s1)`
///            where `TableRef::Shared(s1)` was the first `TableRef::Shared`
///            reference encountered in the stream
///    - `window_len + prelude_len + 2`: `TableRef::Shared(s2)`
///            where `TableRef::Shared(s2)` was the first `TableRef::Shared`
///            reference encountered in the stream and distinct from `s1`
///    - `window_len + prelude_len + 3`: `TableRef::Shared(s3)`
///            where `TableRef::Shared(s1)` was the first `TableRef::Shared`
///            reference encountered in the stream and distinct from `s1`, `s2`,
///            ...
///    - ...
///
/// - First-time Reference into Shared Range: [window_len + prelude_len + import_len + 1,
///             ...[
///    - `window_len + prelude_len + import_len + 1`: `TableRef::Shared(0)`
///    - `window_len + prelude_len + import_len + 2`: `TableRef::Shared(2)`
///    - ...
///
/// # Design notes
///
/// The main goals of this format are to:
///
/// - support both stream reading;
/// - expose wherever possible patterns that a stream compressor (e.g. brotli) can
///    use to efficiently compress the stream.
///
/// Experiments show that the use of `0` and, for some streams, the Recall Window Range,
/// expose such useful patterns.
///
/// Experiments show that the dual Imported Shared References/First-time Shared
/// References mechanism considerably decrease the sparsity of references into the
/// shared dictionary. Where raw references into the shared dictionary are typically
/// represented by a 3 bytes long varnum, consisting in unpredictable bytes, this
/// mechanism tends to reduce them to varnums that may be represented with either 1
/// byte or 2 bytes with a small (hence easy to predict) first byte. In turn, this
/// exposes patterns that the stream compressor may use efficiently.
#[derive(Debug)]
pub struct TableRefStreamState<T> {
    /// The latest `TableRef::Prelude` value.
    ///
    /// Updated whenever we
    /// - encode a new `TableRef::Prelude`;
    /// - decode a value to an `TableRef::Prelude`, including the special value `0`.
    latest_in_prelude: Option<u32>,

    /// Phantom type, to ensure that we only ever use a given `TableRefStreamState` with
    /// a single (type of) LinearTable.
    phantom: PhantomData<T>,

    /// The latest references encountered.
    ///
    /// window[0] is the latest reference encountered,
    /// window[1] the second latest
    /// ...
    ///
    /// Invariant:
    /// - length is always <= window_max_len
    /// - if i ≠ j, window[i] ≠ window[j]
    window: Vec<TableRef>,

    /// The maximal length of `self.window`, which is also the number of
    /// integers to reserve to represent backreferences to values encountered
    /// recently.
    window_max_len: usize,

    /// A mapping from shared references (`TableRef::Shared(s)`) to the number
    /// of shared references in this stream encountered before `s`. In other
    /// words, the the first entry added (by chronological order) maps to `0`,
    /// the second maps to `1`, etc. Used to ensure that `TableRef::Shared` references
    /// fit in successive values, and are generally small enough to be represented by
    /// a varnum of 1 or 2 bytes, instead of being all over the place.
    ///
    /// If the `TableRefStreamState` is used for decoding, this map is empty.
    ///
    /// Invariant: Keys are always `TableRef::Shared`.
    /// Invariant: Values are contiguous, starting with 0.
    shared_reference_to_write_order: HashMap<TableRef, u32>,

    /// A mapping from read order to `TableRef::Shared` instances, i.e.
    /// the first entry read (by chronological order) is at position `0`,
    /// the second at position `1`, etc. Used to ensure that `TableRef::Shared`
    /// references are generally small enough to fit in small varnums.
    ///
    /// If the `TableRefStreamState` is used for encoding, this vector is empty.
    ///
    /// Invariant: Values are always `TableRef::Shared`.
    shared_reference_by_read_order: Vec<TableRef>,

    /// The number of entries in the prelude.
    prelude_len: usize,

    /// The number of entries in the shared dictionary.
    shared_len: usize,
}
impl<T> TableRefStreamState<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    /// Create a new `TableRefStreamState`.
    ///
    /// `window_max_len` represents the number of integers to reserve to
    /// represent backreferences to values encountered recently.
    pub fn new(window_max_len: usize, table: &LinearTable<T>) -> Self {
        TableRefStreamState {
            latest_in_prelude: None,
            phantom: PhantomData,
            window_max_len,
            window: Vec::with_capacity(window_max_len),
            shared_reference_to_write_order: HashMap::new(),
            shared_reference_by_read_order: vec![],
            shared_len: table.shared_len(),
            prelude_len: table.prelude_len(),
        }
    }

    /// The value of `latest_in_prelude + 1`, or `0` if `latest_in_prelude`
    /// has never been set.
    fn next_in_prelude(&self) -> u32 {
        match self.latest_in_prelude {
            Some(latest) => latest + 1,
            None => 0,
        }
    }

    /// Decode a `TableRef` from a `u32`.
    pub fn from_u32(&mut self, value: u32) -> Option<TableRef> {
        let index = loop {
            // Fake loop, used simply to be able to `break` out.
            if value == 0 {
                // Value `0` is reserved to represent `latest_in_prelude + 1` (or `0` if `latest_in_prelude` is `None`).
                let next_in_prelude = self.next_in_prelude();
                break TableRef::Prelude(next_in_prelude as usize);
            }

            // Renormalize from 0.
            let value = value - 1;
            debug!(target: "rw", "TableRefStreamState::from_u32, value is not 0, renormalizing to {}, checking if it's in the floating window",
                value);
            if (value as usize) < self.window_max_len {
                // this is a hit in the floating window.
                break *self.window.get(value as usize)?;
            }

            // Renormalize from 0.
            let value = value - self.window_max_len as u32;
            debug!(target: "rw", "TableRefStreamState::from_u32, renormalizing to {}, checking if it's in the prelude (prelude len is {})",
                value,
                self.prelude_len);
            if (value as usize) < self.prelude_len {
                break TableRef::Prelude(value as usize);
            }

            // Renormalize from 0.
            let value = value - self.prelude_len as u32;
            debug!(target: "rw", "TableRefStreamState::from_u32, renormalizing to {}, checking if it's an imported shared reference",
                value);
            if (value as usize) < self.shared_reference_by_read_order.len() {
                // this is a shared reference that we have already encountered
                break self.shared_reference_by_read_order[value as usize];
            }

            // Renormalize from 0.
            let value = value - self.shared_reference_by_read_order.len() as u32;
            debug!(target: "rw", "TableRefStreamState::from_u32, renormalizing to {}, checking if it's a fresh shared reference",
                value);
            if (value as usize) < self.shared_len {
                let result = TableRef::Shared(value as usize);
                self.shared_reference_by_read_order.push(result);
                break result;
            }

            debug!(target: "rw", "TableRefStreamState::from_u32, oh, we didn't find any result");

            return None;
        };
        // Update caches (latest in prelude, position in window).
        self.update_position_in_window(index);
        if let TableRef::Prelude(ref index_in_prelude) = index {
            self.latest_in_prelude = Some(*index_in_prelude as u32)
        }
        Some(index)
    }

    /// Update `window` using a LRU eviction strategy.
    ///
    /// Return:
    /// - `Some(pos)` if `value` was already in `window` at position `pos`;
    /// - `None` otherwise.
    fn update_position_in_window(&mut self, value: TableRef) -> Option<usize> {
        if self.window_max_len == 0 {
            return None;
        }
        if let Some(pos) = self.window.iter().position(|i| i == &value) {
            if pos == 0 {
                // `value` was already the latest value in `window`, nothing to update.
            } else {
                // Rotate the value to the first position.
                let ref mut slice = self.window.as_mut_slice()[0..pos + 1];
                slice.rotate_right(1);
            }
            return Some(pos);
        }
        if self.window.len() < self.window_max_len {
            // Insert a new latest value in the window.
            self.window.insert(0, value);
        } else {
            // Insert a new value at 0, shifting other values and getting
            // rid of the last value in window.
            let len = self.window.len();
            self.window[len - 1] = value;
            self.window.rotate_right(1);
        }
        assert!(self.window[0] == value);
        assert!(self.window.len() <= self.window_max_len);
        None
    }

    /// Mark a `TableRef::Shared` as encountered if necessary, return the information
    /// necessary to encode it as `u32`.
    ///
    /// If this is the first instance of `value` to be encoded, return `Fetch::Miss(index)`,
    /// where `index` is in `[import len, import len + shared len[` and `import len` is
    /// the number of distinct instances of `TableRef::Shared` encountered
    /// so far. Also, record `number of distinct instances of
    /// `TableRef::Shared` encountered so far as the future index for `value`.
    ///
    /// Otherwise, return `Fetch::Hit(index)`, where `index`  is the index recorded above,
    /// in `[0, total import len[`, and `total import len` is the total number of distinct
    /// instances of `TableRef::Shared` encountered in the file.
    ///
    /// # Failures
    ///
    /// Panics if `value` is not a `TableRef::Shared`.
    fn update_shared_reference_to_write_order(&mut self, value: TableRef) -> Fetch<u32> {
        use std::collections::hash_map::Entry;
        let len = self.shared_reference_to_write_order.len() as u32;
        debug_assert!(value.as_shared().is_some());
        debug!(target: "rw", "TableRefStreamState::update_shared_reference_to_write_order adding {:?} to {:?}",
            value,
            self.shared_reference_to_write_order);
        match self.shared_reference_to_write_order.entry(value) {
            Entry::Occupied(o) => Fetch::Hit(*o.get()),
            Entry::Vacant(v) => {
                // Insert a new value.
                v.insert(len);
                // Return the position in the .
                Fetch::Miss(len + value.as_shared().unwrap() as u32)
            }
        }
    }

    /// Encode an `TableRef` into a `u32`.
    ///
    /// This method does *not* perform any kind of sanity check on the `TableRef`.
    pub fn into_u32(&mut self, value: TableRef) -> u32 {
        debug!(target: "rw", "TableRefStreamState::into_u32, encoding {:?}", value);
        let maybe_position_in_window = self.update_position_in_window(value);

        match value {
            TableRef::Shared(_) => {
                debug!(target: "rw", "TableRefStreamState::into_u32, it's a shared value");
                if let Some(pos) = maybe_position_in_window {
                    debug!(target: "rw", "TableRefStreamState::into_u32, it's a repeat {}", pos);
                    // If we repeat a recent value, emit its position in the window.
                    // Use interval [1, self.window_max_len].
                    return pos as u32 + 1;
                }
                debug!(target: "rw", "TableRefStreamState::into_u32, it's not a repeat - prelude len is {}", self.prelude_len);
                match self.update_shared_reference_to_write_order(value) {
                    Fetch::Hit(pos) => {
                        debug!(target: "rw", "TableRefStreamState::into_u32, it's already imported at {}", pos);
                        // If we repeat a shared reference that we have seen at least once,
                        // emit its position in the table of imported shared references.
                        // Use interval ]self.window_max_len + prelude_len, self.window_max_len + prelude_len + self.imported_len].
                        return self.window_max_len as u32 + self.prelude_len as u32 + pos + 1;
                    }
                    Fetch::Miss(pos) => {
                        debug!(target: "rw", "TableRefStreamState::into_u32, we need to import it from {}", pos);
                        // Otherwise, emit the position in the shared dictionary.
                        // Use interval ]self.window_max_len + prelude_len + self.imported_len, ...].
                        return self.window_max_len as u32 + self.prelude_len as u32 + pos + 1;
                    }
                }
            }
            // References to the prelude dictionary may be 0 (for next) or shifted by 1.
            TableRef::Prelude(i) => {
                let i = i as u32;
                let next_in_prelude = self.next_in_prelude();
                self.latest_in_prelude = Some(i);
                if next_in_prelude == i {
                    // 0 is reserved exactly for this.
                    return 0;
                }
                match maybe_position_in_window {
                    // If we repeat a recent value, emit its position.
                    // Use interval [1, self.window_max_len].
                    Some(pos) => (pos + 1) as u32,
                    // Otherwise, emit a direct reference to the prelude dictionary.
                    // Use interval ]self.window_max_len + self.shared_len, ..]
                    None => (i + self.window_max_len as u32 + 1),
                }
            }
        }
    }
}
