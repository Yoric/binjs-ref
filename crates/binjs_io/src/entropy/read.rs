//! An entropy decoder
use super::probabilities::SymbolIndex;
use super::rw::*;
use super::util::*;

use ::TokenReaderError;
use ::io::{ FileStructurePrinter, Path, TokenReader };
use ::statistics::ContentInfo;

use binjs_shared::{ self, F64, FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString };
use bytes::float::*;
use bytes::varnum::*;

use std::io::{ Cursor, Read };

use range_encoding::opus;

use brotli;
use smallvec::SmallVec;

/// An entropy decoder, based on the Opus bit-level entropy coding.
pub struct Decoder<'a> {
    /// The main stream, compressed using entropy and the dictionary
    /// in `self.options`.
    stream_main: opus::Reader<std::io::Cursor<Vec<u8>>>,

    /// Shared dictionaries.
    options: ::entropy::Options,

    /// Streams of user-extensible values, compressed using an off-the-shelf
    /// compressor (currently, Brotli).
    stream_floats: DictionaryStreamDecoder<'a, Option<F64>>,
    stream_unsigned_longs: DictionaryStreamDecoder<'a, u32>,
    stream_property_keys: DictionaryStreamDecoder<'a, Option<PropertyKey>>,
    stream_identifier_names: DictionaryStreamDecoder<'a, Option<IdentifierName>>,
    stream_string_literals: DictionaryStreamDecoder<'a, Option<SharedString>>,
    stream_list_lengths: DictionaryStreamDecoder<'a, Option<u32>>,
}

impl<'a> FileStructurePrinter for Decoder<'a> {

}


const NAME_MAX_LEN: usize = 128;
type Name = SmallVec<[u8; NAME_MAX_LEN]>;
struct SectionDecoder<T> where T: Read {
    name: Name,
    input: T,
    next_char: [u8; 1],
}
impl<T> SectionDecoder<T> where T: Read {
    pub fn new(input: T) -> Self {
        Self {
            name: SmallVec::new(),
            input,
            next_char: [b'?'], // Arbitrary char.
        }
    }
    pub fn name(&self) -> &[u8] {
        &self.name
    }
    pub fn done(self) -> T {
        self.input
    }
    fn continue_reading_name(&mut self) -> Result<(), TokenReaderError> {
        debug!(target: "read", "SectionDecoder::next_aux let's read the name");
        if self.next_char[0] == b'[' {
            self.input.read_exact(&mut self.next_char)
                .map_err(TokenReaderError::ReadError)?;
        }
        while self.next_char[0] != b']' && self.name.len() < NAME_MAX_LEN {
            self.name.push(self.next_char[0]);
            self.input.read_exact(&mut self.next_char)
                .map_err(TokenReaderError::ReadError)?;
        }
        Ok(())
    }
    fn next_aux(&mut self) -> Result<Option<Vec<u8>>, TokenReaderError> {
        debug!(target: "read", "SectionDecoder::next_aux");
        self.name.clear();
        self.input.expect(b"[")
            .map_err(|e| TokenReaderError::ReadError(e))?;

        self.input.read_exact(&mut self.next_char)
            .map_err(TokenReaderError::ReadError)?;

        if self.next_char[0] == b'[' {
            debug!(target: "read", "SectionDecoder::next_aux it's a [[");
            // `[[` means that we have ended the current section. Fetch the next name, but we're done.
            self.continue_reading_name()?;
            debug!(target: "read", "SectionDecoder::next_aux the next section is {:?}",
                std::str::from_utf8(&self.name));
            self.input.expect(b"]")
                .map_err(TokenReaderError::ReadError)?;
            return Ok(None);
        } else {
            // Find the name of the current field.
            self.continue_reading_name()?;
        }

        debug!(target: "read", "SectionDecoder::next_aux reading compression format");
        // Read the compression format.
        self.input.expect(FORMAT_BROTLI) // FIXME: Extend to other formats.
            .map_err(TokenReaderError::ReadError)?;

        debug!(target: "read", "SectionDecoder::next_aux reading byte length");
        // Read the byte length.
        let byte_len = self.input.read_varnum()
            .map_err(TokenReaderError::ReadError)?
            as usize;

        debug!(target: "read", "SectionDecoder::next_aux preparing to decompress {} bytes", byte_len);

        // Extract slice.
        let mut buf = Vec::with_capacity(byte_len); // FIXME: We may want some maximal size here.
        unsafe { buf.set_len(byte_len); }
        self.input.read_exact(buf.as_mut_slice())
            .map_err(TokenReaderError::ReadError)?;

        use std::io::Write;

        let mut result = Vec::new();
        {
            let mut brotli = brotli::DecompressorWriter::new(&mut result, 32);
            brotli.write_all(&buf)
                .map_err(TokenReaderError::ReadError)?;
            brotli.flush()
                .map_err(TokenReaderError::ReadError)?;
        }

        debug!(target: "read", "SectionDecoder::next_aux I have decompressed {} bytes into {} bytes",
            byte_len,
            result.len());
        Ok(Some(result))
    }
}

impl<'a, T> Iterator for &'a mut SectionDecoder<T> where T: Read {
    type Item = Result<(Name, Vec<u8>), TokenReaderError>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_aux() {
            Ok(Some(buf)) => Some(Ok((self.name.clone(), buf))),
            Ok(None) => None,
            Err(err) => Some(Err(err))
        }
    }
}


struct VarFloatDecoder {
    data: Cursor<Vec<u8>>
}
impl VarFloatDecoder {
    pub fn new(data: Option<Vec<u8>>) -> Option<Self> {
        if let Some(data) = data {
            Some(VarFloatDecoder {
                data: Cursor::new(data)
            })
        } else {
            None
        }
    }
}
impl Iterator for VarFloatDecoder {
    type Item = Result<Option<binjs_shared::F64>, TokenReaderError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.data.position() == self.data.get_ref().len() as u64 {
            return None;
        }
        match self.data.read_maybe_varfloat() {
            Ok(Some(result)) => Some(Ok(Some(result.into()))),
            Ok(None) => Some(Ok(None)),
            Err(err) => Some(Err(TokenReaderError::ReadError(err)))
        }
    }
}

struct MaybeVarNumDecoder {
    data: Cursor<Vec<u8>>
}
impl MaybeVarNumDecoder {
    pub fn new(data: Option<Vec<u8>>) -> Option<Self> {
        if let Some(data) = data {
            Some(MaybeVarNumDecoder {
                data: Cursor::new(data)
            })
        } else {
            None
        }
    }
}
impl Iterator for MaybeVarNumDecoder {
    type Item = Result<Option<u32>, TokenReaderError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.data.position() == self.data.get_ref().len() as u64 {
            return None;
        }
        match self.data.read_maybe_varnum() {
            Ok(result) => Some(Ok(result)),
            Err(err) => Some(Err(TokenReaderError::ReadError(err)))
        }
    }
}

struct StringDecoder {
    data: Cursor<Vec<u8>>,
    lengths: Cursor<Vec<u8>>,
}
impl StringDecoder {
    pub fn new(data: Option<Vec<u8>>, lengths: Option<Vec<u8>>) -> Result<Option<Self>, TokenReaderError> {
        debug!(target: "read", "StringDecoder::new: {:?}, {:?}", data, lengths);
        match (data, lengths) {
            (None, None) => Ok(None),
            (Some(data), Some(lengths)) => {
                Ok(Some(StringDecoder {
                    data: Cursor::new(data),
                    lengths: Cursor::new(lengths),
                }))
            }
            _ => unimplemented!() // FIXME: Raise an error
        }
    }
}
impl Iterator for StringDecoder {
    type Item = Result<String, TokenReaderError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.lengths.position() == self.lengths.get_ref().len() as u64 {
            debug!(target: "read", "StringDecoder::next - end reached");
            return None;
        }
        fn aux(myself: &mut StringDecoder) -> Result<String, TokenReaderError> {
            let byte_len = myself.lengths.read_varnum()
                .map_err(TokenReaderError::ReadError)?
                as usize;
            debug!(target: "read", "StringDecoder::next - byte length: {}", byte_len);
            let value = myself.data.read_string(byte_len)
                .map_err(TokenReaderError::ReadError)?;
            debug!(target: "read", "StringDecoder::next - value: {:?}", value);
            Ok(value)
        }
        Some(aux(self))
    }
}

struct DictionaryStreamDecoder<'a, T> {
    shared_dictionary: &'a [T],
    prelude_dictionary: Vec<T>,
    stream: Option<Cursor<Vec<u8>>>,
    name: SharedString,
}
impl<'a, T> DictionaryStreamDecoder<'a, T>{
    pub fn new(shared_dictionary: &'a [T], prelude_dictionary: Vec<T>, name: SharedString, stream: Option<Vec<u8>>) -> Self {
        debug!(target: "read", "DictionaryStreamDecoder::new {} a {}",
            name.as_str(),
            match stream {
                None => "EMPTY stream".to_string(),
                Some(ref vec) => format!("non-empty ({} bytes) stream", vec.len())
            }
        );
        Self {
            shared_dictionary,
            prelude_dictionary,
            stream: stream.map(Cursor::new),
            name
        }
    }
}
impl<'a, T> Iterator for DictionaryStreamDecoder<'a, T> where T: Clone + std::fmt::Debug {
    type Item = Result<T, TokenReaderError>;
    fn next(&mut self) -> Option<Self::Item> {
        debug!(target: "read", "DictionaryStreamDecoder::next on a {} stream",
            match self.stream {
                None => "EMPTY",
                _ => "non-empty"
            }
        );
        match self.stream {
            None => return None,
            Some(ref mut stream) => {
                debug!(target: "read", "DictionaryStreamDecoder::next position: {} / {}",
                    stream.position(),
                    stream.get_ref().len());
                if stream.position() == stream.get_ref().len() as u64 {
                    return None
                }
                let index = match stream.read_varnum() {
                    Ok(result) => result as usize,
                    Err(err) => return Some(Err(TokenReaderError::ReadError(err)))
                };

                debug!(target: "read", "DictionaryStreamDecoder::next index: {}", index);
                if index < self.shared_dictionary.len() {
                    debug!(target: "read", "That's in the shared dictionary");
                    return Some(Ok(self.shared_dictionary[index].clone()));
                }
                if index < self.shared_dictionary.len() + self.prelude_dictionary.len() {
                    debug!(target: "read", "That's in the prelude dictionary, at index {}: {:?}",
                        index - self.shared_dictionary.len(),
                        self.prelude_dictionary
                    );
                    return Some(Ok(self.prelude_dictionary[index - self.shared_dictionary.len()].clone()));
                }
                return Some(Err(TokenReaderError::BadDictionaryIndex {
                    index: index as u32,
                    dictionary: self.name.clone()
                }))
            }
        }
    }
}

impl<'a> Decoder<'a> {
    pub fn new<R>(options: &'a ::entropy::Options, mut input: R) -> Result<Self, TokenReaderError>
        where R: Read
    {
        // 1. Read headers
        debug!(target: "read", "Decoder::new()");
        input.expect(GLOBAL_HEADER_START)
            .map_err(TokenReaderError::ReadError)?;

        // 2. Read the prelude
        debug!(target: "read", "Decoder::new: Reading prelude");
        let mut prelude_data: PreludeInfo<Option<Vec<u8>>> = PreludeInfo::with(|_| None);
        input.expect(SECTION_PRELUDE)
            .map_err(TokenReaderError::ReadError)?;

        let mut decoder = SectionDecoder::new(input);
        for item in &mut decoder {
            // Extract data.
            let (name, data) = item?;
            debug!(target: "read", "Decoder::new: Reading prelude section {}", {
                let mut vec = name.iter().cloned().collect();
                let string = String::from_utf8(vec)
                    .expect("Could not convert name to string");
                string
            });

            // Store data.
            let field = prelude_data.get_mut_b(&name)
                .ok_or_else(|| TokenReaderError::BadHeaderName(name.iter().cloned().collect()))?;
            debug!(target: "read", "Decoder::new: Storing data");

            *field = Some(data);
        }

        debug!(target: "read", "Decoder::new: Prelude read complete");

        let after_prelude: Name = decoder.name().iter().cloned().collect();
        input = decoder.done();

        // 3. Decode prelude (could be made lazy/backgrounded)
        debug!(target: "read", "Decoder::new: Decoding prelude");
        let prelude_identifier_names = {
            let mut result = Vec::new();
            if let Some(decoder) = StringDecoder::new(prelude_data.identifier_names, prelude_data.identifier_names_len)? {
                debug!(target: "read", "Decoder::new: populating prelude_identifier_names");
                for value in decoder {
                    result.push(Some(IdentifierName::from_string(value?)));
                }
            }
            result
        };
        debug!(target: "read", "Decoder::new: prelude_identifier_names: {:?}", prelude_identifier_names);

        let prelude_property_keys = {
            let mut result = Vec::new();
            if let Some(decoder) = StringDecoder::new(prelude_data.property_keys, prelude_data.property_keys_len)? {
                for value in decoder {
                    result.push(Some(PropertyKey::from_string(value?)));
                }
            }
            result
        };

        let prelude_string_literals = {
            let mut result = Vec::new();
            if let Some(decoder) = StringDecoder::new(prelude_data.string_literals, prelude_data.string_literals_len)? {
                for value in decoder {
                    result.push(Some(SharedString::from_string(value?)));
                }
            }
            result
        };

        let prelude_floats = {
            let mut result = Vec::new();
            if let Some(decoder) = VarFloatDecoder::new(prelude_data.floats) {
                for value in decoder {
                    result.push(value?);
                }
            }
            result
        };

        let prelude_list_lengths = {
            let mut result = Vec::new();
            if let Some(decoder) = MaybeVarNumDecoder::new(prelude_data.list_lengths) {
                for value in decoder {
                    result.push(value?);
                }
            }
            result
        };

        let prelude_unsigned_longs = {
            let mut result = Vec::new();
            if let Some(decoder) = MaybeVarNumDecoder::new(prelude_data.unsigned_longs) {
                for value in decoder {
                    if let Some(value) = value? {
                        result.push(value);
                    } else {
                        unimplemented!(); // FIXME: Raise an error.
                    }
                }
            }
            result
        };

        // 4. Read byte-compressed streams
        let mut content_data: ContentInfo<Option<Vec<u8>>> = ContentInfo::with(|_| None);
        if after_prelude.as_slice() != b"content" { // FIXME: Make this a constant
            debug!(target: "read", "Invalid after_prelude: {:?}",
                std::str::from_utf8(after_prelude.as_slice())
            );
            // FIXME: Raise error
            unimplemented!();
        }

        let mut decoder = SectionDecoder::new(input);
        for item in &mut decoder {
            // Extract data.
            let (name, data) = item?;

            debug!(target: "read", "Decoder::new: Reading content section {}", {
                let mut vec = name.iter().cloned().collect();
                let string = String::from_utf8(vec)
                    .expect("Could not convert name to string");
                string
            });

            // Store data.
            let field = content_data.get_mut_b(&name)
                .ok_or_else(|| TokenReaderError::BadHeaderName(name.iter().cloned().collect()))?;

            debug!(target: "read", "Decoder::new: Storing {} bytes of data", data.len());

            *field = Some(data);
        }

        let after_content: Name = decoder.name()
            .iter()
            .cloned()
            .collect();
        input = decoder.done();

        // 5. Decode byte-compressed streams (could be made lazy/backgrounded)
        let stream_floats = DictionaryStreamDecoder::new(
            options.probability_tables.floats.as_slice(),
            prelude_floats,
            SharedString::from_str("floats"),
            content_data.floats);
        let stream_unsigned_longs = DictionaryStreamDecoder::new(
            options.probability_tables.unsigned_longs.as_slice(),
            prelude_unsigned_longs,
            SharedString::from_str("unsigned_longs"),
            content_data.unsigned_longs);
        let stream_property_keys = DictionaryStreamDecoder::new(
            options.probability_tables.property_keys.as_slice(),
            prelude_property_keys,
            SharedString::from_str("property_keys"),
            content_data.property_keys);
        let stream_identifier_names = DictionaryStreamDecoder::new(
            options.probability_tables.identifier_names.as_slice(),
            prelude_identifier_names,
            SharedString::from_str("identifier_names"),
            content_data.identifier_names);
        let stream_string_literals = DictionaryStreamDecoder::new(
            options.probability_tables.string_literals.as_slice(),
            prelude_string_literals,
            SharedString::from_str("string_literals"),
            content_data.string_literals);
        let stream_list_lengths = DictionaryStreamDecoder::new(
            options.probability_tables.list_lengths.as_slice(),
            prelude_list_lengths,
            SharedString::from_str("list_lengths"),
            content_data.list_lengths);

        // 6. Ready to read and decode main stream.
        if after_content.as_slice() != b"main" { // FIXME:Make this a named constant
            // FIXME: Handle error
            unimplemented!();
        }
        input.expect(FORMAT_ENTROPY_0)
            .map_err(TokenReaderError::ReadError)?;

        let mut data = Vec::new();
        input.read_to_end(&mut data)
            .map_err(TokenReaderError::ReadError)?;

        let stream_main = opus::Reader::new(Cursor::new(data))
            .map_err(TokenReaderError::ReadError)?;
        let result = Decoder {
            options: (*options).clone(), // No sharing here.
            stream_floats,
            stream_identifier_names,
            stream_list_lengths,
            stream_property_keys,
            stream_string_literals,
            stream_unsigned_longs,
            stream_main,
        };
        Ok(result)
    }
}

/// Read a single symbol from the main entropy stream.
///
/// Used instead of a method as we need to generality wrt the field name.
///
/// Usage:
/// `main_stream!(self, name_of_the_probability_table, "Description, used for debugging", path_in_the_ast)`
macro_rules! main_stream {
    ( $me: ident, $table:ident, $description: expr, $path:expr ) => {
        {
            use std::borrow::Borrow;
            use std::ops::DerefMut;
            let path = $path.borrow();

            let index = {
                // 1. Get the frequency information for this path.
                let frequencies = $me.options.probability_tables
                    .$table
                    .frequencies_at(path)
                    .ok_or_else(|| TokenReaderError::NotInDictionary(format!("{} at {:?}", $description, $path)))?;
                let mut borrow = frequencies
                    .borrow_mut();

                // 2. Let bit-level I/O determine the symbol index stored.
                let index = $me.stream_main.symbol(borrow.deref_mut())
                    .map_err(TokenReaderError::ReadError)?;
                index
            };

            // 3. Deduce the value we have just read.
            let value = $me.options.probability_tables
                .$table
                .value_by_symbol_index(path, SymbolIndex::new(index as usize))
                .ok_or_else(|| TokenReaderError::NotInDictionary(format!("{} [{}]", stringify!($ident), index)))?;
            Ok(value.clone())
        }
    }
}

/// Read a single symbol from a content stream.
///
/// Used instead of a method as we need to generality wrt the field name.
///
/// Usage:
/// `content_stream!(self, name_of_the_probability_table, "Description, used for debugging", path_in_the_ast)`
macro_rules! content_stream {
    ( $me: ident, $stream:ident, $description: expr) => {
        {
            debug!(target: "read", "Reading from content stream {}", $description);
            let value = $me.$stream.next()
                .unwrap_or_else(|| Err(TokenReaderError::UnexpectedEndOfStream($description.to_string())))?;
            Ok(value)
        }
    }
}

impl<'a> TokenReader for Decoder<'a> {

    // ---- Fixed sets

    fn bool_at(&mut self, path: &Path) -> Result<Option<bool>, TokenReaderError> {
        main_stream!(self, bool_by_path, "bool_by_path", path)
    }

    fn string_enum_at(&mut self, path: &Path) -> Result<SharedString, TokenReaderError> {
        main_stream!(self, string_enum_by_path, "string_enum_by_path", path)
    }

    fn enter_tagged_tuple_at(&mut self, path: &Path) -> Result<(InterfaceName, Option<std::rc::Rc<Box<[FieldName]>>>), TokenReaderError> {
        let name = main_stream!(self, interface_name_by_path, "interface_name_by_path", path)?;
        Ok((name, None))
    }

    // ---- Extensible sets

    fn float_at(&mut self, _path: &Path) -> Result<Option<f64>, TokenReaderError> {
        let value = content_stream!(self, stream_floats, "stream_floats")?;
        Ok(value.map(F64::into))
    }

    fn string_at(&mut self, _path: &Path) -> Result<Option<SharedString>, TokenReaderError> {
        content_stream!(self, stream_string_literals, "stream_string_literals")
    }

    fn identifier_name_at(&mut self, _path: &Path) -> Result<Option<IdentifierName>, TokenReaderError> {
        let result = content_stream!(self, stream_identifier_names, "stream_identifier_names")?;
        debug!(target: "read", "identifier_name_at {:?}", result);
        Ok(result)
    }

    fn property_key_at(&mut self, _path: &Path) -> Result<Option<PropertyKey>, TokenReaderError> {
        content_stream!(self, stream_property_keys, "stream_property_keys")
    }

    fn unsigned_long_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        content_stream!(self, stream_unsigned_longs, "stream_unsigned_longs")
    }

    fn enter_list_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        let length = content_stream!(self, stream_list_lengths, "stream_list_lengths")?
            .ok_or_else(|| TokenReaderError::EmptyList)?;
            // For the moment, we cannot read an optional list.
        Ok(length)
    }

    // ---- TBD

    fn offset_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        unimplemented!()
    }

    fn enter_untagged_tuple_at(&mut self, _path: &Path) -> Result<(), TokenReaderError> {
        unimplemented!()
    }
}
