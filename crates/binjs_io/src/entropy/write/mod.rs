//! An entropy encoder.

mod lazy_stream;

use self::lazy_stream::*;
use super::dictionary::{DictionaryFamily, Fetch, Introduction, LinearTable, TableRef};
use super::probabilities::{InstancesToProbabilities, SymbolInfo};
use super::rw::*;
use bytes::lengthwriter::LengthWriter;
use bytes::varnum::WriteVarNum;
use io::statistics::{Bytes, Instances, PerUserExtensibleKind, Rational};
use io::{Path, TokenWriter};
use TokenWriterError;

use binjs_shared::{
    FieldName, IdentifierName, InterfaceName, Node, PropertyKey, SharedString, F64,
};

use std::fs;
use std::io::Write;

#[allow(unused_imports)] // We keep enabling/disabling this.
use itertools::Itertools;
use range_encoding::opus;
use range_encoding::CumulativeDistributionFrequency;

/// An arbitrary initialization size for buffers.
const INITIAL_BUFFER_SIZE_BYTES: usize = 32768;

impl PerUserExtensibleKind<opus::Writer<LengthWriter>> {
    /// Finalize and return the number of compressed bytes written.
    ///
    /// This number is determined by examining the underlying LengthWriter.
    pub fn into_statistics(self) -> PerUserExtensibleKind<Bytes> {
        self.into_with(|_, value| value.done().unwrap().len().into())
    }
}

/// An entropy encoder, based on the Opus bit-level entropy coding.
pub struct Encoder {
    /// Shared dictionaries.
    options: ::entropy::Options,

    paths_encountered: usize,

    // -- Content
    /// Main stream compressed by entropy coding.
    writer: opus::Writer<Vec<u8>>,

    /// A file at which to dump the contents of the main stream.
    dump_path: Option<std::path::PathBuf>,

    /// Parts of the content that we do not know how to compress correctly
    /// with entropy coding yet, and that we rather compress by Brotli
    /// at the time of this writing.
    ///
    /// We're using an `LazyStream` rather than directly compressing, so
    /// as to simplify dumping of raw data to files, for forensics purposes,
    /// and also so as to let us entirely skip streams that have 0 bytes written.
    ///
    /// This is something of a hack and should be removed once we have a better
    /// idea of *what* we should encode with Brotli and what we shouldn't.
    content_streams: PerUserExtensibleKind<Vec<TableRef>>,

    /// Parts of the header that we compress with Brotli.
    prelude_streams: PreludeStreams<LazyStream>,

    // --- Extensible sets of symbols, as indexed tables.
    // We copy these tables to the Encoder as they are modified
    // as we encode the file.
    /// All unsigned longs.
    unsigned_longs: LinearTable<u32>,

    /// All string literals. `None` for `null`.
    string_literals: LinearTable<Option<SharedString>>,

    /// All identifier names. `None` for `null`.
    identifier_names: LinearTable<Option<IdentifierName>>,

    /// All property keys. `None` for `null`.
    property_keys: LinearTable<Option<PropertyKey>>,

    /// All list lenghts. `None` for `null`.
    list_lengths: LinearTable<Option<u32>>,

    /// All floats. `None` for `null`.
    floats: LinearTable<Option<F64>>,

    string_enums: LinearTable<Option<SharedString>>,
    interface_names: LinearTable<Option<InterfaceName>>,
    prelude_dictionary: DictionaryFamily<SymbolInfo>,

    // --- Statistics.
    /// Measure the number of bytes written.
    content_opus_lengths: PerUserExtensibleKind<opus::Writer<LengthWriter>>,

    /// Measure the number of entries written.
    content_instances: PerUserExtensibleKind<Instances>,

    /// The path of the file being written, if it's a file.
    path: Option<std::path::PathBuf>,
}

impl Encoder {
    /// Create a new Encoder.
    ///
    /// Note that cloning `options` is pretty cheap, as it is mostly a bunch
    /// of `Rc<>`.
    pub fn new(path: Option<&std::path::Path>, options: ::entropy::Options, prelude_dictionary: DictionaryFamily<Instances>) -> Self {
        let split_streams = options.split_streams;

        let mut prelude_dictionary = prelude_dictionary.instances_to_probabilities("Encoder::new");
        prelude_dictionary.enter_existing(&SharedString::from_str("")).unwrap();

        // We need to clone the instances of `LinearTable` as using them modifies
        // their content.
        let unsigned_longs = options.dictionaries.current().unsigned_longs().clone();
        let string_literals = options.dictionaries.current().string_literals().clone();
        let identifier_names = options.dictionaries.current().identifier_names().clone();
        let property_keys = options.dictionaries.current().property_keys().clone();
        let list_lengths = options.dictionaries.current().list_lengths().clone();
        let floats = options.dictionaries.current().floats().clone();
        let interface_names = options.dictionaries.current().interface_names().clone();
        let string_enums = options.dictionaries.current().string_enums().clone();
        Encoder {
            writer: opus::Writer::new(Vec::with_capacity(INITIAL_BUFFER_SIZE_BYTES)),
            dump_path: if split_streams {
                path.map(|path| {
                    let mut buf = std::path::PathBuf::new();
                    buf.push(path);
                    buf.set_extension("streams");
                    buf.push("main.entropy");
                    buf
                })
            } else {
                None
            },
            options,
            content_opus_lengths: PerUserExtensibleKind::with(|_| {
                opus::Writer::new(LengthWriter::new())
            }),
            content_streams: PerUserExtensibleKind::with(|_| Vec::new()),
            prelude_streams: PreludeStreams::with(|name| {
                let maybe_buf = match path {
                    Some(path) if split_streams => {
                        let mut buf = std::path::PathBuf::new();
                        buf.push(path);
                        buf.set_extension("streams");
                        buf.push(name);
                        buf.set_extension("prelude");
                        Some(buf)
                    }
                    _ => None,
                };
                LazyStream::new(maybe_buf)
            }),
            content_instances: PerUserExtensibleKind::with(|_| 0.into()),
            path: path.map(std::path::Path::to_path_buf),
            unsigned_longs,
            string_literals,
            identifier_names,
            property_keys,
            list_lengths,
            floats,
            interface_names,
            string_enums,
            prelude_dictionary,
            paths_encountered: 0,
        }
    }
}

/// Emit a single symbol to the main (entropy-compressed) stream.
///
/// Note that this macro could not be implemented as a simple method, as we need to adapt it to different field names.
///
/// Usage:
/// `emit_symbol_to_main_stream!(self, name_of_the_probability_table, "Description, used for debugging",  path_in_the_ast,  value_to_encode)`
macro_rules! emit_symbol_to_main_stream {
    ( $me: ident, $probability_table: expr, $description: expr, $path: expr, $value: expr) => {
        {
            use std::borrow::Borrow;
            let path = $path.borrow();

            // 1. Locate the `SymbolInfo` information for this value given the
            // path information.
            let symbol =
                $probability_table
                .stats_by_node_value(path, &$value)
                .ok_or_else(|| {
                    debug!(target: "entropy", "Couldn't find value {:?} at {:?} ({})",
                        $value, path, $description);
                    TokenWriterError::NotInDictionary(format!("{}: {:?} at {:?}", $description, $value, path))
                })?;

            // 2. This gives us an index (`symbol.index`) and a probability distribution
            // (`symbol.distribution`). Use them to write the probability at bit-level.
            let distribution = symbol.distribution
                .as_ref()
                .borrow();
            $me.writer.symbol(symbol.index.into(), &distribution)
                .map_err(TokenWriterError::WriteError)?;

            // FIXME: Update table of statistics.

            Ok(())
        }
    };
    ( $me: ident, $probability_table: ident, $table_of_statistics: ident, $description: expr, $path: expr, $value: expr ) => {
        {
            use std::borrow::Borrow;

            let path = $path.borrow();

            // 1. Locate the `SymbolInfo` information for this value given the
            // path information.
            let table = $me.options
                .dictionaries
                .current()
                .$probability_table();
            let symbol =
                table
                .stats_by_node_value(path, &$value)
                .ok_or_else(|| {
                    debug!(target: "entropy", "Couldn't find value {:?} at {:?} ({})",
                        $value, path, $description);
                    TokenWriterError::NotInDictionary(format!("{}: {:?} at {:?}", $description, $value, path))
                })?;

            // 2. This gives us an index (`symbol.index`) and a probability distribution
            // (`symbol.distribution`). Use them to write the probability at bit-level.
            let distribution = symbol.distribution
                .as_ref()
                .borrow();
            $me.writer.symbol(symbol.index.into(), &distribution)
                .map_err(TokenWriterError::WriteError)?;

            // 3. Also update our table of statistics
            let mut probability_stats = $me.options
                .probability_stats
                .borrow_mut();
            let probability = Rational {
                num: distribution.at_index(symbol.index.into()).unwrap().width() as usize,
                den: distribution.width() as usize
            };
            probability_stats.$table_of_statistics.add_probability(probability);

            Ok(())
        }
    }
}

/// Add a user-extensible symbol to one of the content streams.
/// If the symbol is not part of either the static dictionary
/// or the prelude dictionary, it is added to the latter.
///
/// This macro is designed for simple values whose binary representation
/// may be concatenated without loss of information. For string-like values
/// that need additional info, see `emit_string_symbol_to_streams`.
///
///
/// Note that this macro could not be implemented as a simple method, as we need to adapt it to different field names.
///
/// Usage:
/// `emit_simple_symbol_to_streams!(self, name_of_the_indexed_table, name_of_the_stream, value_to_encode, "Description, used for debugging")`
macro_rules! emit_simple_symbol_to_streams {
    ( $me: ident, $table: ident, $out: ident, $writer: ident, $value: expr, $description: expr ) => {
        if let Fetch::Miss(_) = emit_symbol_to_content_stream!($me, $table, $out, $value, $description) {
            // The value does not appear either in the static dictionary or in the prelude dictionary.
            // Add it to the latter.
            $me.prelude_streams.$out.$writer(*$value)
                .map_err(TokenWriterError::WriteError)?;
        }
    }
}

macro_rules! emit_table_to_streams {
    ( $me: ident, $probability_table: ident, $linear_table: ident, $out: ident, $out_len: ident, $description: expr, $path: expr ) => {
        {
            let path = $path;
            if let Introduction::NewTable = $me.next_introduction() {
                // This is the first time we encounter this table.
                // We need to write its definition.
                let probability_table = $me.prelude_dictionary.current().$probability_table();
                let frequencies = probability_table.frequencies_at(path).unwrap();
                let frequencies: Ref<range_encoding::CumulativeDistributionFrequency> = frequencies.as_ref().borrow();

                // Store the number of entries in the table.
                $me.prelude_streams.probabilities_len.write_varnum(frequencies.len() as u32)
                    .map_err(TokenWriterError::WriteError)?;

                // FIXME: We could optimize for the case of `frequencies.len() == 1`.
                // FIXME: We could renormalize to minimize the size of probabilities varnums.
                for index in 0..frequencies.len() {
                    // Store the number of instances.
                    $me.prelude_streams.probabilities.write_varnum(frequencies.at_index(index).unwrap().width())
                        .map_err(TokenWriterError::WriteError)?;

                    // Store the actual symbol definition.
                    // FIXME: We could make this lookup much more efficient.
                    let value = probability_table.value_by_symbol_index(path, index.into()).unwrap();
                    if let Fetch::Miss(_) = emit_symbol_to_content_stream!($me, $linear_table, $out, value, $description) {
                        // The table value does not appear either in the static dictionary or in the prelude dictionary.
                        // Add it to the latter.
                        match value {
                            Some(string) => {
                                // Write the binary representation of the length of string to the
                                // prelude stream `foo_len`, the binary representation of the string itself
                                // to the prelude stream `foo`.
                                let bytes = string.as_str()
                                    .as_bytes();
                                $me.prelude_streams.$out_len.write_maybe_varnum(Some(bytes.len() as u32))
                                    .map_err(TokenWriterError::WriteError)?;
                                $me.prelude_streams.$out.write_all(bytes)
                                    .map_err(TokenWriterError::WriteError)?;
                            }
                            None => {
                                // If the string is `None`, just use the `null` varnum as length.
                                $me.prelude_streams.$out_len.write_maybe_varnum(None)
                                    .map_err(TokenWriterError::WriteError)?;
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Add a user-extensible symbol to one of the content streams.
/// If the symbol is not part of either the static dictionary
/// or the prelude dictionary, it is added to the latter.
///
/// This macro is designed for string-like values, which may not
/// be simply concatenated to obtain a dictionary. For string-like
/// values, in addition to concatenation, we also record a list of
/// lengths, which we use during decoding to extract individual
/// strings.
///
/// Note that this macro could not be implemented as a simple method, as we need to adapt it to different field names.
///
/// Usage:
/// `emit_string_symbol_to_streams!(self, name_of_the_indexed_table, name_of_the_string_prelude_stream, name_of_the_string_length_prelude_stream, value_to_encode, "Description, used for debugging")`
macro_rules! emit_string_symbol_to_streams {
    ( $me: ident, $probability_table: ident, $linear_table: ident, $out: ident, $out_len: ident, $value: expr, $description: expr, $path: expr ) => {
        {
            use std::borrow::Borrow;
            use std::cell::Ref;
            let path = $path.borrow();
            emit_table_to_streams!($me, $probability_table, $linear_table, $out, $out_len, $description, path);
            let probability_table = $me.prelude_dictionary.current().$probability_table();
            emit_symbol_to_main_stream!($me, probability_table, "string_at (probabilities)", path, $value)?;
        }
    }
}

/// Implementation shared by `emit_simple_symbol_to_streams` and `emit_string_symbol_to_streams`.
///
/// Fetch the index of a value in the dictionary and write it to the relevant content
/// stream. If this causes a new slot to be allocated in the dictionary, return
/// `Miss(_)` - the caller is responsible to ensure that the value is written
/// to the prelude stream.
///
/// Note that this macro could not be implemented as a simple method, as we need to adapt it to different field names.
///
/// Usage:
/// `emit_symbol_to_content_stream!(self, name_of_the_indexed_table, name_of_the_string_content_stream, value_to_encode, "Description, used for debugging")`
macro_rules! emit_symbol_to_content_stream {
    ( $me: ident, $linear_table: ident, $out: ident, $value: expr, $description: expr ) => {
        {
            let value = $value;

            // 1. Fetch the index in the dictionary.
            let fetch = $me
                .$linear_table
                .fetch_index(value);

            debug!(target: "write", "Writing index {:?} as {:?} index to {}", $value, fetch, $description);

            let index = match fetch {
                Fetch::Hit(index) => index,
                Fetch::Miss(index) => index
            };
            // Note: We must make sure that we don't forget to write the value
            // to the prelude if it's a Miss.

            // 2. Append index for later compression
            let ref mut stream = $me.content_streams
                .$out;

            stream
                .push(index);

            // 3. Also, update statistics
            $me.content_instances
                .$out += Into::<Instances>::into(1);

            // Return value will instruct the caller to write data to the prelude.
            fetch
        }
    }
}

impl Encoder {
    /// Flush a stream of indices (a content stream) into a buffer.
    ///
    /// If the stream is empty, do nothing. Otherwise, add `[name_of_stream]compression_method;compressed_bytes`,
    /// where `compressed_bytes` starts with a varnum indicating the LRU window len.
    ///
    /// If `maybe_path` is specified, flush to a subdirectory of the path.
    fn flush_indices<T>(
        maybe_path: &Option<std::path::PathBuf>,
        name: &str,
        vec: &[TableRef],
        window_len: usize,
        table: &LinearTable<T>,
        out: &mut Vec<u8>,
    ) -> std::io::Result<Bytes>
    where
        T: std::hash::Hash + Eq + Clone + std::fmt::Debug,
    {
        debug!(target: "write", "Encoder::flush_indices {}, {} instances", name, vec.len());
        if vec.len() == 0 {
            // Nothing to write.
            return Ok(Into::<Bytes>::into(0));
        }

        // Initialize lazy stream.
        let mut lazy_stream = {
            let maybe_dump_path = match maybe_path {
                None => None,
                Some(path) => {
                    let mut buf = path
                        .with_extension("streams")
                        .join(name)
                        .with_extension("content");
                    Some(buf)
                }
            };
            LazyStream::new(maybe_dump_path)
        };

        // Write window length.
        lazy_stream.write_varnum(window_len as u32)?;

        // Write (and possibly dump) data.
        // In the current implementation, we just ignore any information other than the index.

        debug!(target: "write", "Flushing identifiers {:?} ({} shared, {} prelude)", table, table.shared_len(), table.prelude_len());
        let mut state = TableRefStreamState::new(window_len, table);
        for table_ref in vec {
            let varnum = state.into_u32(*table_ref);
            lazy_stream.write_varnum(varnum as u32)?;
        }

        Self::flush_stream(name, lazy_stream, out)
    }

    /// Flush a lazy stream (either a prelude stream or a content stream) into a buffer.
    ///
    /// If the stream is empty, do nothing. Otherwise, add `[name_of_stream]compression_method;compressed_bytes`.
    fn flush_stream(
        name: &str,
        mut stream: LazyStream,
        out: &mut Vec<u8>,
    ) -> std::io::Result<Bytes> {
        stream.flush()?;
        let bytes_written = stream.bytes_written();
        if let Some(data) = stream.done()? {
            debug!(target: "write", "Encoder::flush_stream: {} contains {} compressed bytes ({} uncompressed bytes written)",
                name,
                data.len(),
                bytes_written,
            );

            // Stream name
            out.write_all(b"[")?;
            out.write_all(name.as_bytes())?;
            out.write_all(b"]")?;
            out.write_all(FORMAT_BROTLI)?;

            // Stream length
            let len = data.len();
            out.write_varnum(len as u32)?;

            // Stream content
            out.write_all(&data)?;
            Ok(Into::<Bytes>::into(len))
        } else {
            Ok(Into::<Bytes>::into(0))
        }
    }

    fn next_introduction(&mut self) -> Introduction {
        let result = self.prelude_dictionary.introductions()[self.paths_encountered];
        self.paths_encountered += 1;
        result
    }
}

impl TokenWriter for Encoder {
    type Data = Box<[u8]>;

    fn done(self) -> Result<Self::Data, TokenWriterError> {
        let mut data: Vec<u8> = Vec::with_capacity(INITIAL_BUFFER_SIZE_BYTES);

        data.extend(GLOBAL_HEADER_START);

        // FIXME: Write additional headers.

        // Write prelude compressed streams, containing dictionaries.
        data.extend(SECTION_PRELUDE);
        for (name, stream) in self.prelude_streams.into_iter().sorted_by_key(|kv| kv.0) {
            Self::flush_stream(name, stream, &mut data).map_err(TokenWriterError::WriteError)?;
        }

        // Write content compressed streams, containing references to
        // both the prelude dictionaries and the static dictionaries.
        data.extend(SECTION_CONTENT);
        let path_for_flush = if self.options.split_streams {
            &self.path
        } else {
            &None
        };

        // For each content stream, fetch the `content_window_len` option,
        // and flush the indices.
        macro_rules! write_indices_with_window_len { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            $(
                let indices = &self.content_streams.$ident;
                let window_len = self.options.content_window_len.$ident;
                let len = Self::flush_indices(path_for_flush, $name, indices, window_len, &self.$ident, &mut data)
                    .map_err(TokenWriterError::WriteError)?;
                self
                    .options
                    .content_lengths
                    .borrow_mut()
                    .$ident += Into::<Bytes>::into(len);
            )*
        } };
        for_field_in_user_extensible!(write_indices_with_window_len);

        // Write main stream of entropy-compressed data.
        data.write_all(SECTION_MAIN)
            .map_err(TokenWriterError::WriteError)?;
        data.write_all(FORMAT_ENTROPY_0)
            .map_err(TokenWriterError::WriteError)?;

        let entropy = self.writer.done().map_err(TokenWriterError::WriteError)?;

        if let Some(path) = self.dump_path {
            fs::DirBuilder::new()
                .recursive(true)
                .create(path.parent().unwrap())
                .map_err(TokenWriterError::WriteError)?;

            fs::write(path, &entropy).map_err(TokenWriterError::WriteError)?;
        }

        data.write_all(&entropy)
            .map_err(TokenWriterError::WriteError)?;

        // Update byte lengths
        *self.options.content_lengths.borrow_mut() += self
            .content_opus_lengths
            .into_with(|_, field| field.done().unwrap().len());

        // Update number of instances
        *self.options.content_instances.borrow_mut() += self.content_instances;
        Ok(data.into())
    }

    // --- Fixed set

    fn bool_at(&mut self, value: Option<bool>, path: &Path) -> Result<(), TokenWriterError> {
        emit_symbol_to_main_stream!(self, bool_by_path, bools, "bool_by_path", path, value)
    }

    fn string_enum_at(
        &mut self,
        value: &SharedString,
        path: &Path,
    ) -> Result<(), TokenWriterError> {
        emit_string_symbol_to_streams!(
            self,
            string_enum_by_path,
            string_enums,
            string_enums,
            string_enums_len,
            Some(value.clone()),
            "string_enum_at",
            path
        );
        Ok(())
    }

    fn enter_tagged_tuple_at(
        &mut self,
        _node: &Node,
        tag: &InterfaceName,
        _children: &[&FieldName],
        path: &Path,
    ) -> Result<(), TokenWriterError> {
        emit_string_symbol_to_streams!(
            self,
            interface_name_by_path,
            interface_names,
            interface_names,
            interface_names_len,
            Some(tag.clone()),
            "tagged_tuple_at",
            path
        );
        Ok(())
    }

    // --- User-extensible values

    fn float_at(&mut self, value: Option<f64>, _path: &Path) -> Result<(), TokenWriterError> {
        use bytes::float::WriteVarFloat;
        emit_simple_symbol_to_streams!(
            self,
            floats,
            floats,
            write_maybe_varfloat2,
            &value.map(F64::from),
            "float_at"
        );
        Ok(())
    }

    fn unsigned_long_at(&mut self, value: u32, _path: &Path) -> Result<(), TokenWriterError> {
        emit_simple_symbol_to_streams!(
            self,
            unsigned_longs,
            unsigned_longs,
            write_varnum,
            &value,
            "unsigned_long_at"
        );
        Ok(())
    }

    fn string_at(
        &mut self,
        value: Option<&SharedString>,
        path: &Path,
    ) -> Result<(), TokenWriterError> {
        emit_string_symbol_to_streams!(
            self,
            string_literal_by_path,
            string_literals,
            string_literals,
            string_literals_len,
            &value.cloned(),
            "string_at",
            path
        );
        Ok(())
    }

    fn identifier_name_at(
        &mut self,
        value: Option<&IdentifierName>,
        path: &Path,
    ) -> Result<(), TokenWriterError> {
        emit_string_symbol_to_streams!(
            self,
            identifier_name_by_path,
            identifier_names,
            identifier_names,
            identifier_names_len,
            &value.cloned(),
            "identifier_name_at",
            path
        );
        Ok(())
    }

    fn property_key_at(
        &mut self,
        value: Option<&PropertyKey>,
        path: &Path,
    ) -> Result<(), TokenWriterError> {
        emit_string_symbol_to_streams!(
            self,
            property_key_by_path,
            property_keys,
            property_keys,
            property_keys_len,
            &value.cloned(),
            "property_key_at",
            path
        );
        Ok(())
    }

    fn enter_list_at(&mut self, len: usize, _path: &Path) -> Result<(), TokenWriterError> {
        emit_simple_symbol_to_streams!(
            self,
            list_lengths,
            list_lengths,
            write_maybe_varnum,
            &Some(len as u32),
            "enter_list_at"
        );
        Ok(())
    }

    fn enter_scoped_dictionary_at(
        &mut self,
        name: &SharedString,
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        self.options
            .dictionaries
            .enter_existing(name)
            .map_err(|_| TokenWriterError::DictionarySwitchingError(name.clone()))?;
        Ok(())
    }

    fn exit_scoped_dictionary_at(
        &mut self,
        name: &SharedString,
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        self.options.dictionaries.exit(name);
        Ok(())
    }

    fn offset_at(&mut self, _path: &Path) -> Result<(), TokenWriterError> {
        unimplemented!()
    }
}
