//! An entropy encoder.

mod lazy_stream;

use self::lazy_stream::*;
use super::dictionary::{Fetch, Index};
use super::rw::*;
use bytes::lengthwriter::LengthWriter;
use bytes::varnum::WriteVarNum;
use io::statistics::{Bytes, ContentInfo, Instances};
use io::{Path, TokenWriter};
use TokenWriterError;

use binjs_shared::{
    FieldName, IdentifierName, InterfaceName, Node, PropertyKey, SharedString, F64,
};

use std::io::Write;
use std::ops::DerefMut;

#[allow(unused_imports)] // We keep enabling/disabling this.
use itertools::Itertools;
use range_encoding::opus;

/// An arbitrary initialization size for buffers.
const INITIAL_BUFFER_SIZE_BYTES: usize = 32768;

impl ContentInfo<opus::Writer<LengthWriter>> {
    /// Finalize and return the number of compressed bytes written.
    ///
    /// This number is determined by examining the underlying LengthWriter.
    pub fn into_statistics(self) -> ContentInfo<Bytes> {
        self.into_with(|_, value| value.done().unwrap().len().into())
    }
}

/// An entropy encoder, based on the Opus bit-level entropy coding.
pub struct Encoder {
    /// Shared dictionaries.
    options: ::entropy::Options,

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
    content_streams: ContentInfo<Vec<Index>>,

    /// Parts of the header that we compress with Brotli.
    prelude_streams: PreludeStreams<LazyStream>,

    // --- Statistics.
    /// Measure the number of bytes written.
    content_opus_lengths: ContentInfo<opus::Writer<LengthWriter>>,

    /// Measure the number of entries written.
    content_instances: ContentInfo<Instances>,

    /// The path of the file being written, if it's a file.
    path: Option<std::path::PathBuf>,
}

impl Encoder {
    /// Create a new Encoder.
    pub fn new(path: Option<&std::path::Path>, options: ::entropy::Options) -> Self {
        // FIXME: We shouldn't need to clone the entire `options`. A shared immutable reference would do nicely.
        let split_streams = options.split_streams;
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
            content_opus_lengths: ContentInfo::with(|_| opus::Writer::new(LengthWriter::new())),
            content_streams: ContentInfo::with(|_| Vec::new()),
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
            content_instances: ContentInfo::with(|_| 0.into()),
            path: path.map(std::path::Path::to_path_buf),
        }
    }
}

/// Emit a single symbol to the main (entropy-compressed) stream.
///
/// Note that this macro could not be implemented as a simple method, as we need to adapt it to different field names.
///
/// Usage:
/// `emit_symbol_to_main_stream!(self, name_of_the_probability_table, name_of_the_ContentInfo_field, "Description, used for debugging",  path_in_the_ast,  value_to_encode)`
macro_rules! emit_symbol_to_main_stream {
    ( $me: ident, $table: ident, $info: ident, $description: expr, $path: expr, $value: expr ) => {
        {
            use std::borrow::Borrow;

            let path = $path.borrow();

            // 1. Locate the `SymbolInfo` information for this value given the
            // path information.
            let symbol = $me.options
                .probability_tables
                .$table
                .stats_by_node_value_mut(path, &$value)
                .ok_or_else(|| {
                    debug!(target: "entropy", "Couldn't find value {:?} at {:?} ({})",
                        $value, path, $description);
                    TokenWriterError::NotInDictionary(format!("{}: {:?} at {:?}", $description, $value, path))
                })?;

            // FIXME: For extensibility purposes, if the value is not in the dictionary, we should
            // add it to the prelude and a relevant content stream.

            // 2. This gives us an index (`symbol.index`) and a probability distribution
            // (`symbol.distribution`). Use them to write the probability at bit-level.
            let mut distribution = symbol.distribution
                .borrow_mut();
            $me.writer.symbol(symbol.index.into(), distribution.deref_mut())
                .map_err(TokenWriterError::WriteError)?;

            // 3. Also, update statistics
            $me.content_opus_lengths
                .$info
                .symbol(symbol.index.into(), distribution.deref_mut())
                .map_err(TokenWriterError::WriteError)?;
            $me.content_instances
                .$info += Into::<Instances>::into(1);
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
    ( $me: ident, $dictionary: ident, $out: ident, $writer: ident, $value: expr, $description: expr ) => {
        if let Fetch::Miss(_) = emit_symbol_to_content_stream!($me, $dictionary, $out, $value, $description) {
            // The value does not appear either in the static dictionary or in the prelude dictionary.
            // Add it to the latter.
            $me.prelude_streams.$out.$writer(*$value)
                .map_err(TokenWriterError::WriteError)?;
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
    ( $me: ident, $dictionary: ident, $out: ident, $len: ident, $value: expr, $description: expr ) => {
        if let Fetch::Miss(_) = emit_symbol_to_content_stream!($me, $dictionary, $out, $value, $description) {
            // The value does not appear either in the static dictionary or in the prelude dictionary.
            // Add it to the latter.
            match $value {
                Some(string) => {
                    // Write the binary representation of the length of string to the
                    // prelude stream `foo_len`, the binary representation of the string itself
                    // to the prelude stream `foo`.
                    let bytes = string.as_str()
                        .as_bytes();
                    $me.prelude_streams.$len.write_maybe_varnum(Some(bytes.len() as u32))
                        .map_err(TokenWriterError::WriteError)?;
                    $me.prelude_streams.$out.write_all(bytes)
                        .map_err(TokenWriterError::WriteError)?;
                }
                None => {
                    // If the string is `None`, just use the `null` varnum as length.
                    $me.prelude_streams.$len.write_maybe_varnum(None)
                        .map_err(TokenWriterError::WriteError)?;
                }
            }
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
    ( $me: ident, $dictionary:ident, $out:ident, $value: expr, $description: expr ) => {
        {
            let value = $value;

            // 1. Fetch the index in the dictionary.
            let fetch = $me.options
                .probability_tables
                .$dictionary
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
    /// If the stream is empty, do nothing. Otherwise, add `[name_of_stream]compression_method;compressed_bytes`.
    ///
    /// If `maybe_path` is specified
    fn flush_indices(
        maybe_path: &Option<std::path::PathBuf>,
        name: &str,
        vec: &mut Vec<Index>,
        out: &mut Vec<u8>,
    ) -> std::io::Result<Bytes> {
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

        // Write (and possibly dump) data.
        // In the current implementation, we just ignore any information other than the index.

        // Note: This is an approximation of bwt, as we'd need to avoid any embedded 0.
        let mut buf = vec![];
        for index in vec {
            buf.write_varnum(index.raw() as u32).unwrap();
        }
        buf.push(0);

        let suffix_array = bio::data_structures::suffix_array::suffix_array(&buf);
        let bwt = bio::data_structures::bwt::bwt(&buf, &suffix_array);
        lazy_stream.write_all(&bwt)?;
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
}

impl TokenWriter for Encoder {
    type Data = Vec<u8>;

    fn done(mut self) -> Result<Self::Data, TokenWriterError> {
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
        for (name, indices) in self.content_streams.iter_mut().sorted_by_key(|kv| kv.0) {
            let len = Self::flush_indices(path_for_flush, name, indices, &mut data)
                .map_err(TokenWriterError::WriteError)?;
            *self
                .options
                .content_lengths
                .borrow_mut()
                .get_mut(&name)
                .unwrap() += Into::<Bytes>::into(len);
        }

        // Write main stream of entropy-compressed data.
        data.write_all(SECTION_MAIN)
            .map_err(TokenWriterError::WriteError)?;
        data.write_all(FORMAT_ENTROPY_0)
            .map_err(TokenWriterError::WriteError)?;

        let entropy = self.writer.done().map_err(TokenWriterError::WriteError)?;

        if let Some(path) = self.dump_path {
            std::fs::DirBuilder::new()
                .recursive(true)
                .create(path.parent().unwrap())
                .map_err(TokenWriterError::WriteError)?;

            let mut file = std::fs::File::create(path).map_err(TokenWriterError::WriteError)?;
            file.write_all(&entropy)
                .map_err(TokenWriterError::WriteError)?;
            file.flush().map_err(TokenWriterError::WriteError)?;
        }

        data.write_all(&entropy)
            .map_err(TokenWriterError::WriteError)?;

        // Update byte lengths
        *self.options.content_lengths.borrow_mut() += self
            .content_opus_lengths
            .into_with(|_, field| field.done().unwrap().len());

        // Update number of instances
        *self.options.content_instances.borrow_mut() += self.content_instances;
        Ok(data)
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
        emit_symbol_to_main_stream!(
            self,
            string_enum_by_path,
            string_enums,
            "string_enum_by_path",
            path,
            value
        )
    }

    fn enter_tagged_tuple_at(
        &mut self,
        _node: &Node,
        tag: &InterfaceName,
        _children: &[&FieldName],
        path: &Path,
    ) -> Result<(), TokenWriterError> {
        emit_symbol_to_main_stream!(
            self,
            interface_name_by_path,
            interface_names,
            "interface_name_by_path",
            path,
            tag
        )
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
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        emit_string_symbol_to_streams!(
            self,
            string_literals,
            string_literals,
            string_literals_len,
            &value.cloned(),
            "string_at"
        );
        Ok(())
    }

    fn identifier_name_at(
        &mut self,
        value: Option<&IdentifierName>,
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        emit_string_symbol_to_streams!(
            self,
            identifier_names,
            identifier_names,
            identifier_names_len,
            &value.cloned(),
            "identifier_name_at"
        );
        Ok(())
    }

    fn property_key_at(
        &mut self,
        value: Option<&PropertyKey>,
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        emit_string_symbol_to_streams!(
            self,
            property_keys,
            property_keys,
            property_keys_len,
            &value.cloned(),
            "property_key_at"
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

    fn offset_at(&mut self, _path: &Path) -> Result<(), TokenWriterError> {
        unimplemented!()
    }
}
