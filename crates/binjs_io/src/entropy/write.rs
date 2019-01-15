//! An entropy encoder.
//!
//! At the time of this writing, the encoder bases everything on an external dictionary.
//! That dictionary is not encoded in the output.

// FIXME: Store strings
// FIXME: Split into packets
// FIXME: Implement lazy functions

use ::TokenWriterError;
use ::io::{ Path, TokenWriter };
use ::io::statistics::{ Bytes, ContentInfo, Instances };
use bytes::lengthwriter::LengthWriter;
use bytes::varnum::WriteVarNum;
use super::rw::*;
use super::dictionary::Fetch;
use super::probabilities::IntoStatistics;

use binjs_shared::{ F64, FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString };

use std::io::Write;
use std::ops::DerefMut;

use brotli;
#[allow(unused_imports)] // We keep enabling/disabling this.
use itertools::Itertools;
use range_encoding::opus;


const INITIAL_BUFFER_SIZE_BYTES : usize = 32768;

/// An arbitrary buffer size for Brotli compression.
///
/// This should not impact file size, only compression speed.
const BROTLI_BUFFER_SIZE: usize = 32768;

/// Highest Brotli compression level.
///
/// Higher values provide better file size but slower compression.
// FIXME: Should probably become a compression parameter.
const BROTLI_QUALITY: u32 = 11;

/// An arbitrary window size for Brotli compression.
///
/// Higher values provide better file size but slower compression.
// FIXME: SHould probably become a compression parameter.
const BROTLI_LG_WINDOW_SIZE: u32 = 20;


/// A segment of brotli data, initialized lazily, so that it may
/// remain entirely empty (without brotli header or crc) if absolutely
/// no data is written to it.
struct LazyStream {
    lazy_brotli: Option<brotli::CompressorWriter<Vec<u8>>>,
    dump_path: Option<std::path::PathBuf>,
    lazy_dump_file: Option<std::fs::File>,
    instances: usize,
    bytes_written: usize,
}
impl LazyStream {
    pub fn new(dump_path: Option<std::path::PathBuf>) -> Self {
        LazyStream {
            dump_path,
            lazy_brotli: None,
            lazy_dump_file: None,
            instances: 0,
            bytes_written: 0,
        }
    }
    pub fn increment(&mut self) {
        self.instances += 1;
    }
    pub fn instances(&self) -> usize {
        self.instances
    }
    pub fn bytes_written(&self) -> usize {
        self.bytes_written
    }
    fn get_writer(&mut self) -> Result<Option<&mut std::fs::File>, std::io::Error> {
        if let Some(ref mut writer) = self.lazy_dump_file {
            return Ok(Some(writer))
        }
        if let Some(ref path) = self.dump_path {
            let dir = path.parent()
                .unwrap();
            std::fs::DirBuilder::new()
                .recursive(true)
                .create(dir)?;
            let file = std::fs::File::create(path)?;
            self.lazy_dump_file = Some(file);
            return Ok(Some(self.lazy_dump_file
                .as_mut()
                .unwrap()))
        }
        Ok(None)
    }
}
impl std::io::Write for LazyStream {
    fn write(&mut self, data: &[u8]) -> Result<usize, std::io::Error> {
        // 1. Write to Brotli.
        {
            let brotli = self.lazy_brotli
                .get_or_insert_with(||
                    brotli::CompressorWriter::new(
                        Vec::with_capacity(BROTLI_BUFFER_SIZE),
                        BROTLI_BUFFER_SIZE,
                        BROTLI_QUALITY,
                    BROTLI_LG_WINDOW_SIZE)
                );
            brotli.write_all(data)?;
        }

        // 2. Write to file if necessary.
        if let Some(writer) = self.get_writer()? {
            writer.write_all(data)?;
        }

        // 3. Done.
        self.bytes_written += data.len();
        Ok(data.len())
    }

    fn flush(&mut self) -> Result<(), std::io::Error> {
        if let Some(ref mut writer) = self.lazy_brotli {
            writer.flush()?;
        }
        if let Some(ref mut writer) = self.lazy_dump_file {
            writer.flush()?;
        }
        Ok(())
    }
}

impl IntoStatistics for LazyStream {
    type AsStatistics = Bytes;
    fn into_statistics(self, _description: &str) -> Bytes {
        match self.lazy_brotli {
            None => 0.into(),
            Some(mut writer) => {
                writer.flush()
                    .unwrap();
                writer.get_ref()
                    .len()
                    .into()
            }
        }
    }
}

impl IntoStatistics for ContentInfo<LazyStream> {
    type AsStatistics = ContentInfo<Bytes>;
    /// Finalize and return the number of compressed bytes written.
    ///
    /// This number is determined by examining the length of the buffer
    /// to which this stream writes.
    fn into_statistics(self, _description: &str) -> ContentInfo<Bytes> {
        self.into_with(|name, field| field.into_statistics(name))
    }
}
impl ContentInfo<opus::Writer<LengthWriter>> {
    /// Finalize and return the number of compressed bytes written.
    ///
    /// This number is determined by examining the underlying LengthWriter.
    pub fn into_statistics(self) -> ContentInfo<Bytes> {
        self.into_with(|_, value| {
            value.done()
                .unwrap()
                .len()
                .into()
        })
    }
}



/// An entropy encoder, based on the Opus bit-level entropy coding.
pub struct Encoder {
    /// Shared dictionaries.
    options: ::entropy::Options,

    // -- Content

    /// Main stream compressed by entropy coding.
    writer: opus::Writer<Vec<u8>>,

    /// Parts of the content that we do not know how to compress correctly
    /// with entropy coding yet, and that we rather compress by Brotli.
    ///
    /// We're using an `Option<CompressorWriter<...>>` rather than a full-blown
    /// `CompressorWriter`, as some sections are actually not written with brotli
    /// and we don't want to store a brotli header for an empty content. We
    /// initialize from `None` to `Some` in macro `content_stream!`.
    ///
    /// This is something of a hack and should be removed once we have a better
    /// idea of *what* we should encode with Brotli and what we shouldn't.
    content_streams: ContentInfo<LazyStream>,

    /// Parts of the header that we compress with Brotli.
    prelude_streams: PreludeInfo<LazyStream>,

    // --- Statistics.

    /// Measure the number of bytes written.
    content_opus_lengths: ContentInfo<opus::Writer<LengthWriter>>,

    /// Measure the number of entries written.
    content_instances: ContentInfo<Instances>,
}

impl Encoder {
    /// Create a new Encoder.
    pub fn new(path: Option<&std::path::Path>, options: ::entropy::Options) -> Self { // FIXME: We shouldn't need to clone the entire `options`. A shared immutable reference would do nicely.
        // FIXME: Turn dictionary

        Encoder {
            writer: opus::Writer::new(Vec::with_capacity(INITIAL_BUFFER_SIZE_BYTES)),
            options,
            content_opus_lengths: ContentInfo::with(|_| opus::Writer::new(LengthWriter::new())),
            content_streams: ContentInfo::with(|name| {
                let maybe_buf = match path {
                    None => None,
                    Some(path) => {
                        let mut buf = std::path::PathBuf::new();
                        buf.push(path);
                        buf.set_extension("streams");
                        buf.push(name);
                        buf.set_extension("references");
                        Some(buf)
                    }
                };
                LazyStream::new(maybe_buf)
            }),
            prelude_streams: PreludeInfo::with(|name| {
                let maybe_buf = match path {
                    None => None,
                    Some(path) => {
                        let mut buf = std::path::PathBuf::new();
                        buf.push(path);
                        buf.set_extension("streams");
                        buf.push(name);
                        buf.set_extension("definitions");
                        Some(buf)
                    }
                };
                LazyStream::new(maybe_buf)
            }),
            content_instances: ContentInfo::with(|_| 0.into()),
        }
    }
}


/// Emit a single symbol.
///
/// Used instead of a method as we need to generality wrt the field name.
///
/// Usage:
/// `main_stream!(self, name_of_the_probability_table, name_of_the_ContentInfo_field, "Description, used for debugging",  path_in_the_ast,  value_to_encode)`
macro_rules! main_stream {
    ( $me: ident, $table:ident, $info:ident, $description: expr, $path:expr, $value: expr ) => {
        {
            use std::borrow::Borrow;

            let path = $path.borrow();
            debug!(target: "entropy_details", "Known paths ({}): [{}]",
                $description,
                $me.options
                    .probability_tables
                    .$table
                    .paths()
                    .map(|k| format!("{:?}", k))
                    .format(", "));

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
            let mut borrow = symbol.distribution
                .borrow_mut();
            $me.writer.symbol(symbol.index.into(), borrow.deref_mut())
                .map_err(TokenWriterError::WriteError)?;

            // 3. Also, update statistics
            $me.content_opus_lengths
                .$info
                .symbol(symbol.index.into(), borrow.deref_mut())
                .map_err(TokenWriterError::WriteError)?;
            $me.content_instances
                .$info += Into::<Instances>::into(1);
            Ok(())
        }
    }
}

macro_rules! simple_content_stream {
    ( $me: ident, $dictionary:ident, $out:ident, $writer: ident, $value: expr, $description: expr ) => {
        if let Fetch::Miss(_) = shared_content_stream!($me, $dictionary, $out, $value, $description) {
            $me.prelude_streams.$out.increment();
            $me.prelude_streams.$out.$writer(*$value)
                .map_err(TokenWriterError::WriteError)?;
        }
    }
}

macro_rules! string_content_stream {
    ( $me: ident, $dictionary:ident, $out:ident, $len:ident, $value: expr, $description:expr ) => {
        if let Fetch::Miss(_) = shared_content_stream!($me, $dictionary, $out, $value, $description) {
            $me.prelude_streams.$out.increment();
            match $value {
                Some(string) => {
                    let bytes = string.as_str()
                        .as_bytes();
                    $me.prelude_streams.$len.write_maybe_varnum(Some(bytes.len() as u32))
                        .map_err(TokenWriterError::WriteError)?;
                    $me.prelude_streams.$out.write_all(bytes)
                        .map_err(TokenWriterError::WriteError)?;
                }
                None => {
                    $me.prelude_streams.$len.write_maybe_varnum(None)
                        .map_err(TokenWriterError::WriteError)?;
                }
            }
        }
    }
}

macro_rules! shared_content_stream {
    ( $me: ident, $dictionary:ident, $out:ident, $value: expr, $description: expr ) => {
        {
            use bytes::varnum::WriteVarNum;
            let value = $value;

            // 1. Fetch the index in the dictionary.
            let fetch = $me.options
                .probability_tables
                .$dictionary
                .get(value);

            debug!(target: "write", "Writing index {:?} as {:?} index to {}", $value, fetch, $description);

            let index = match fetch {
                Fetch::Hit(index) => index,
                Fetch::Miss(index) => index
            };
            // Note: We must make sure that we don't forget to write the value
            // to the prelude if it's a Miss.

            let as_usize: usize = index.clone();
            let as_u32: u32 = as_usize as u32;

            // 2. Locate stream
            let ref mut stream = $me.content_streams
                .$out;

            // 3. Write the index to Brotli.
            stream
                .write_varnum(as_u32)
                .map_err(TokenWriterError::WriteError)?;

            // 4. Also, update statistics
            $me.content_instances
                .$out += Into::<Instances>::into(1);

            // Return value will instruct the caller to write data to the prelude.
            fetch
        }
    }
}

impl Encoder {
    fn write_stream(name: &str, stream: &mut LazyStream, out: &mut Vec<u8>) -> Result<(), std::io::Error> {
        debug!(target: "write", "Encoder::write_stream {}, {} instances", name, stream.instances());
        let bytes_written = stream.bytes_written();
        if let Some(ref mut stream) = stream.lazy_brotli {
            stream.flush()?;
            debug!(target: "write", "Encoder::write_stream: {} contains {} compressed bytes ({} uncompressed bytes written)",
                name,
                stream.get_ref().len(),
                bytes_written,
            );

            // Section name
            out.write_all(b"[")?;
            out.write_all(name.as_bytes())?;
            out.write_all(b"]")?;
            out.write_all(FORMAT_BROTLI)?;

            // Section length
            let len = stream.get_ref()
                .len();

            out.write_varnum(len as u32)?;

            // Section content
            out.write_all(stream.get_ref())?;
        }
        Ok(())
    }
}

impl TokenWriter for Encoder {
    type Data = Vec<u8>;

    fn done(mut self) -> Result<Self::Data, TokenWriterError> {
        let mut data: Vec<u8> = Vec::with_capacity(INITIAL_BUFFER_SIZE_BYTES);

        data.extend(GLOBAL_HEADER_START);

        // FIXME: Write magic number.
        // FIXME: Write additional headers.

        // Write prelude brotli-compressed streams.
        data.extend(SECTION_PRELUDE);
        for (name, stream) in self.prelude_streams.iter_mut()
            .sorted_by_key(|kv| kv.0)
        {
            Self::write_stream(name, stream, &mut data)
                .map_err(TokenWriterError::WriteError)?;
        }

        // Write content brotli-compressed streams.
        data.extend(SECTION_CONTENT);
        for (name, stream) in self.content_streams.iter_mut()
            .sorted_by_key(|kv| kv.0)
        {
            Self::write_stream(name, stream, &mut data)
                .map_err(TokenWriterError::WriteError)?;
        }

        // Write entropy-compressed data.
        data.write_all(SECTION_MAIN)
            .map_err(TokenWriterError::WriteError)?;
        data.write_all(FORMAT_ENTROPY_0)
            .map_err(TokenWriterError::WriteError)?;

        let entropy = self.writer.done()
            .map_err(TokenWriterError::WriteError)?;

        data.write_all(&entropy)
            .map_err(TokenWriterError::WriteError)?;

        // Update byte lengths
        *self.options
            .content_lengths
            .borrow_mut()
            +=
        self.content_opus_lengths
             .into_with(|_, field| field.done()
             .unwrap()
             .len());

        *self.options
            .content_lengths
            .borrow_mut()
            +=
        self.content_streams
            .into_statistics("brotli");

        // Update number of instances
        *self.options
            .content_instances
            .borrow_mut()
            +=
        self.content_instances;
        Ok(data)
    }

    // --- Fixed set

    fn bool_at(&mut self, value: Option<bool>, path: &Path) -> Result<(), TokenWriterError> {
        main_stream!(self, bool_by_path, bools, "bool_by_path",  path,  value)
    }

    fn string_enum_at(&mut self, value: &SharedString, path: &Path) -> Result<(), TokenWriterError> {
        main_stream!(self, string_enum_by_path, string_enums, "string_enum_by_path",  path,  value)
    }

    fn enter_tagged_tuple_at(&mut self, tag: &InterfaceName, _children: &[&FieldName], path: &Path) -> Result<(), TokenWriterError> {
        main_stream!(self, interface_name_by_path, interface_names, "interface_name_by_path",  path,  tag)
    }

    // --- User-extensible values

    fn float_at(&mut self, value: Option<f64>, _path: &Path) -> Result<(), TokenWriterError> {
        use bytes::float::WriteVarFloat;
        simple_content_stream!(self, floats, floats, write_maybe_varfloat2, &value.map(F64::from), "float_at");
        Ok(())
    }

    fn unsigned_long_at(&mut self, value: u32, _path: &Path) -> Result<(), TokenWriterError> {
        simple_content_stream!(self, unsigned_longs, unsigned_longs, write_varnum, &value, "unsigned_long_at");
        Ok(())
    }

    fn string_at(&mut self, value: Option<&SharedString>, _path: &Path) -> Result<(), TokenWriterError> {
        string_content_stream!(self, string_literals, string_literals, string_literals_len, &value.cloned(), "string_at");
        Ok(())
    }

    fn identifier_name_at(&mut self, value: Option<&IdentifierName>, _path: &Path) -> Result<(), TokenWriterError> {
        string_content_stream!(self, identifier_names, identifier_names, identifier_names_len, &value.cloned(), "identifier_name_at");
        Ok(())
    }

    fn property_key_at(&mut self, value: Option<&PropertyKey>, _path: &Path) -> Result<(), TokenWriterError> {
        string_content_stream!(self, property_keys, property_keys, property_keys_len, &value.cloned(), "property_key_at");
        Ok(())
    }

    fn enter_list_at(&mut self, len: usize, _path: &Path) -> Result<(), TokenWriterError> {
        simple_content_stream!(self, list_lengths, list_lengths, write_maybe_varnum, &Some(len as u32), "enter_list_at");
        Ok(())
    }

    fn offset_at(&mut self, _path: &Path) -> Result<(), TokenWriterError> {
        unimplemented!()
    }
}
