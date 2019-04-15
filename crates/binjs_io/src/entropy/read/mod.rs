//! An entropy decoder

mod content_decoders;
mod prelude_decoders;
use self::content_decoders::*;
use self::prelude_decoders::*;

use super::probabilities::SymbolIndex;
use super::rw::*;
use super::util::*;

use io::{FileStructurePrinter, Path, TokenReader};
use statistics::PerUserExtensibleKind;
use TokenReaderError;

use binjs_shared::{FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString, F64};

use std::io::{Cursor, Read};

use range_encoding::opus;

/// An entropy decoder, based on the Opus bit-level entropy coding.
pub struct Decoder {
    /// The main stream, compressed using entropy and the dictionary
    /// in `self.options`.
    stream_main: opus::Reader<std::io::Cursor<Vec<u8>>>,

    /// Shared dictionaries.
    options: ::entropy::Options,

    /// Streams of user-extensible values, compressed using an off-the-shelf
    /// compressor (currently, Brotli).
    stream_floats: DictionaryStreamDecoder<Option<F64>>,
    stream_unsigned_longs: DictionaryStreamDecoder<u32>,
    stream_property_keys: DictionaryStreamDecoder<Option<PropertyKey>>,
    stream_identifier_names: DictionaryStreamDecoder<Option<IdentifierName>>,
    stream_string_literals: DictionaryStreamDecoder<Option<SharedString>>,
    stream_list_lengths: DictionaryStreamDecoder<Option<u32>>,
    stream_interface_names: DictionaryStreamDecoder<Option<InterfaceName>>,
    stream_string_enums: DictionaryStreamDecoder<Option<SharedString>>,
}

impl FileStructurePrinter for Decoder {}

impl Decoder {
    pub fn new<R>(options: &::entropy::Options, mut input: R) -> Result<Self, TokenReaderError>
    where
        R: Read,
    {
        // 1. Read headers
        debug!(target: "read", "Decoder::new()");
        input
            .expect(GLOBAL_HEADER_START)
            .map_err(TokenReaderError::ReadError)?;

        // 2. Read the prelude
        debug!(target: "read", "Decoder::new: Reading prelude");
        let mut prelude_data: PreludeStreams<Option<Vec<u8>>> = PreludeStreams::with(|_| None);
        input
            .expect(SECTION_PRELUDE)
            .map_err(TokenReaderError::ReadError)?;

        let mut decoder = SectionDecoder::new(input);
        for item in &mut decoder {
            // Extract data.
            let (name, data) = item?;
            debug!(target: "read", "Decoder::new: Reading prelude section {}", {
                let vec = name.iter().cloned().collect();
                let string = String::from_utf8(vec)
                    .expect("Could not convert name to string");
                string
            });

            // Store data.
            let field = prelude_data
                .get_mut_b(&name)
                .ok_or_else(|| TokenReaderError::BadHeaderName(name.iter().cloned().collect()))?;
            debug!(target: "read", "Decoder::new: Storing data");

            *field = Some(data);
        }

        debug!(target: "read", "Decoder::new: Prelude read complete");

        // Quick verification of what comes next.
        input = Self::check_upcoming_section(decoder, &SECTION_CONTENT_WITHOUT_BRACKETS)?;

        // 3. Decode prelude (could be made lazy/backgrounded)
        debug!(target: "read", "Decoder::new: Decoding prelude");
        let prelude_identifier_names = {
            let mut result = Vec::new();
            for value in StringDecoder::try_new(
                prelude_data.identifier_names,
                prelude_data.identifier_names_len,
            )? {
                result.push(value?.map(IdentifierName::from_string));
            }
            result
        };
        debug!(target: "read", "Decoder::new: prelude_identifier_names: {:?}", prelude_identifier_names);

        let prelude_property_keys = {
            let mut result = Vec::new();
            for value in
                StringDecoder::try_new(prelude_data.property_keys, prelude_data.property_keys_len)?
            {
                result.push(value?.map(PropertyKey::from_string));
            }
            result
        };

        let prelude_string_literals = {
            let mut result = Vec::new();
            for value in StringDecoder::try_new(
                prelude_data.string_literals,
                prelude_data.string_literals_len,
            )? {
                result.push(value?.map(SharedString::from_string));
            }
            result
        };

        let prelude_string_enums = {
            let mut result = Vec::new();
            for value in
                StringDecoder::try_new(prelude_data.string_enums, prelude_data.string_enums_len)?
            {
                result.push(value?.map(SharedString::from_string));
            }
            result
        };

        let prelude_interface_names = {
            let mut result = Vec::new();
            for value in StringDecoder::try_new(
                prelude_data.interface_names,
                prelude_data.interface_names_len,
            )? {
                result.push(value?.map(InterfaceName::from_string));
            }
            result
        };

        let prelude_floats = {
            let mut result = Vec::new();
            if let Some(data) = prelude_data.floats {
                for value in VarFloatDecoder::new(data) {
                    result.push(value?);
                }
            }
            result
        };

        let prelude_list_lengths = {
            let mut result = Vec::new();
            if let Some(data) = prelude_data.list_lengths {
                for value in MaybeVarNumDecoder::new(data) {
                    result.push(value?);
                }
            }
            result
        };

        let prelude_unsigned_longs = {
            let mut result = Vec::new();
            if let Some(data) = prelude_data.unsigned_longs {
                for value in MaybeVarNumDecoder::new(data) {
                    if let Some(value) = value? {
                        result.push(value);
                    } else {
                        return Err(TokenReaderError::EmptyNumber);
                    }
                }
            }
            result
        };

        // 4. Read byte-compressed streams
        let mut content_data: PerUserExtensibleKind<Option<Vec<u8>>> =
            PerUserExtensibleKind::with(|_| None);
        let mut decoder = SectionDecoder::new(input);
        for item in &mut decoder {
            // Extract data.
            let (name, data) = item?;

            debug!(target: "read", "Decoder::new: Reading content section {}", {
                let vec = name.iter().cloned().collect();
                let string = String::from_utf8(vec)
                    .expect("Could not convert name to string");
                string
            });

            // Store data.
            let field = content_data
                .get_mut_b(&name)
                .ok_or_else(|| TokenReaderError::BadHeaderName(name.iter().cloned().collect()))?;

            debug!(target: "read", "Decoder::new: Storing {} bytes of data", data.len());

            *field = Some(data);
        }

        input = Self::check_upcoming_section(decoder, &SECTION_MAIN_WITHOUT_BRACKETS)?;

        // 5. Decode byte-compressed streams (could be made lazy/backgrounded)
        // FIXME: copying all these known values is a waste of time,
        // it wouldn't be too hard to keep a single copy in memory
        let stream_floats = DictionaryStreamDecoder::try_new(
            options
                .known_values
                .floats
                .with_prelude(&prelude_floats)
                .map_err(|v| TokenReaderError::DuplicateInDictionary(format!("{:?}", v)))?,
            SharedString::from_str("floats"),
            content_data.floats,
        )?;
        let stream_unsigned_longs = DictionaryStreamDecoder::try_new(
            options
                .known_values
                .unsigned_longs
                .with_prelude(&prelude_unsigned_longs)
                .map_err(|v| TokenReaderError::DuplicateInDictionary(format!("{:?}", v)))?,
            SharedString::from_str("unsigned_longs"),
            content_data.unsigned_longs,
        )?;
        let stream_property_keys = DictionaryStreamDecoder::try_new(
            options
                .known_values
                .property_keys
                .with_prelude(&prelude_property_keys)
                .map_err(|v| TokenReaderError::DuplicateInDictionary(format!("{:?}", v)))?,
            SharedString::from_str("property_keys"),
            content_data.property_keys,
        )?;
        let stream_identifier_names = DictionaryStreamDecoder::try_new(
            options
                .known_values
                .identifier_names
                .with_prelude(&prelude_identifier_names)
                .map_err(|v| TokenReaderError::DuplicateInDictionary(format!("{:?}", v)))?,
            SharedString::from_str("identifier_names"),
            content_data.identifier_names,
        )?;
        let stream_string_literals = DictionaryStreamDecoder::try_new(
            options
                .known_values
                .string_literals
                .with_prelude(&prelude_string_literals)
                .map_err(|v| TokenReaderError::DuplicateInDictionary(format!("{:?}", v)))?,
            SharedString::from_str("string_literals"),
            content_data.string_literals,
        )?;
        let stream_list_lengths = DictionaryStreamDecoder::try_new(
            options
                .known_values
                .list_lengths
                .with_prelude(&prelude_list_lengths)
                .map_err(|v| TokenReaderError::DuplicateInDictionary(format!("{:?}", v)))?,
            SharedString::from_str("list_lengths"),
            content_data.list_lengths,
        )?;
        let stream_interface_names = DictionaryStreamDecoder::try_new(
            options
                .known_values
                .interface_names
                .with_prelude(&prelude_interface_names)
                .map_err(|v| TokenReaderError::DuplicateInDictionary(format!("{:?}", v)))?,
            SharedString::from_str("interface_names"),
            content_data.interface_names,
        )?;
        let stream_string_enums = DictionaryStreamDecoder::try_new(
            options
                .known_values
                .string_enums
                .with_prelude(&prelude_string_enums)
                .map_err(|v| TokenReaderError::DuplicateInDictionary(format!("{:?}", v)))?,
            SharedString::from_str("string_enums"),
            content_data.string_enums,
        )?;

        // 6. Ready to read and decode main stream.
        input
            .expect(FORMAT_ENTROPY_0)
            .map_err(TokenReaderError::ReadError)?;

        let mut data = Vec::new();
        input
            .read_to_end(&mut data)
            .map_err(TokenReaderError::ReadError)?;

        let stream_main =
            opus::Reader::new(Cursor::new(data)).map_err(TokenReaderError::ReadError)?;
        let result = Decoder {
            options: (*options).clone(), // No sharing here.
            stream_floats,
            stream_identifier_names,
            stream_list_lengths,
            stream_property_keys,
            stream_string_literals,
            stream_unsigned_longs,
            stream_main,
            stream_interface_names,
            stream_string_enums,
        };
        Ok(result)
    }

    /// Check that we have reached the beginning of a section.
    fn check_upcoming_section<R>(
        decoder: SectionDecoder<R>,
        expected: &[u8],
    ) -> Result<R, TokenReaderError>
    where
        R: Read,
    {
        match decoder.name() {
            Name::Section(section) if section == expected => {
                debug!(target: "read", "Expected section name [[{}]], got it.",
                    std::str::from_utf8(expected).unwrap());
            }
            Name::Section(section) => {
                debug!(target: "read", "Expected section name [[{expected}]], got [[{actual:?}]]",
                    actual = std::str::from_utf8(section),
                    expected = std::str::from_utf8(expected).unwrap(),
                );
                return Err(TokenReaderError::BadHeaderName(
                    section.iter().cloned().collect(),
                ));
            }
            Name::Nothing => {
                error!(target: "read", "Expected section name [[{expected}]], but there is nothing.",
                    expected = std::str::from_utf8(expected).unwrap()
                );
                return Err(TokenReaderError::BadHeaderName(vec![]));
            }
            Name::Stream(stream) => {
                // This is an internal parsing error.
                panic!("Expected section name [[{expected}]], but there is a stream name [{actual:?}].",
                    actual = std::str::from_utf8(stream),
                    expected = std::str::from_utf8(expected).unwrap()
                );
            }
        }
        Ok(decoder.into_input())
    }
}

/// Read a single symbol from the main entropy stream.
///
/// Used instead of a method as we need to generality wrt the field name.
///
/// Usage:
/// `main_stream!(self, name_of_the_probability_table, "Description, used for debugging", path_in_the_ast)`
macro_rules! main_stream {
    ( $me: ident, $table:ident, $description: expr, $path:expr ) => {{
        use std::borrow::Borrow;
        use std::ops::DerefMut;
        let path = $path.borrow();

        let table = $me.options.probabilities.current().$table();
        let index = {
            // 1. Get the frequency information for this path.
            let frequencies = table.frequencies_at(path).ok_or_else(|| {
                TokenReaderError::NotInDictionary(format!("{} at {:?}", $description, $path))
            })?;
            let mut borrow = frequencies.borrow_mut();

            // 2. Let bit-level I/O determine the symbol index stored.
            let index = $me
                .stream_main
                .symbol(borrow.deref_mut())
                .map_err(TokenReaderError::ReadError)?;
            index
        };

        // 3. Deduce the value we have just read.
        let value = table
            .value_by_symbol_index(path, SymbolIndex::new(index as usize))
            .ok_or_else(|| {
                TokenReaderError::NotInDictionary(format!("{} [{}]", stringify!($ident), index))
            })?;
        Ok(value.clone())
    }};
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
                .unwrap_or_else(|| Err(TokenReaderError::UnexpectedEndOfStream($description.to_string())) )?;
            Ok(value)
        }
    }
}

impl TokenReader for Decoder {
    // ---- Fixed sets

    fn bool_at(&mut self, path: &Path) -> Result<Option<bool>, TokenReaderError> {
        main_stream!(self, bool_by_path, "bool_by_path", path)
    }

    fn string_enum_at(&mut self, path: &Path) -> Result<SharedString, TokenReaderError> {
        main_stream!(self, string_enum_by_path, "string_enum_by_path", path).map(Option::unwrap)
    }

    fn enter_tagged_tuple_at(
        &mut self,
        path: &Path,
    ) -> Result<(InterfaceName, Option<std::rc::Rc<Box<[FieldName]>>>), TokenReaderError> {
        let name = main_stream!(self, interface_name_by_path, "interface_name_by_path", path)?;
        Ok((name.unwrap(), None))
    }

    // ---- Extensible sets

    fn float_at(&mut self, _path: &Path) -> Result<Option<f64>, TokenReaderError> {
        let value = content_stream!(self, stream_floats, "stream_floats")?;
        Ok(value.map(F64::into))
    }

    fn string_at(&mut self, _path: &Path) -> Result<Option<SharedString>, TokenReaderError> {
        content_stream!(self, stream_string_literals, "stream_string_literals")
    }

    fn identifier_name_at(
        &mut self,
        _path: &Path,
    ) -> Result<Option<IdentifierName>, TokenReaderError> {
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

    fn enter_scoped_dictionary_at(
        &mut self,
        name: &SharedString,
        _path: &Path,
    ) -> Result<(), TokenReaderError> {
        self.options
            .probabilities
            .enter_existing(name)
            .map_err(|_| TokenReaderError::DictionarySwitchingError(name.clone()))?;
        Ok(())
    }

    fn exit_scoped_dictionary_at(
        &mut self,
        name: &SharedString,
        _path: &Path,
    ) -> Result<(), TokenReaderError> {
        self.options.probabilities.exit(name);
        Ok(())
    }

    // ---- TBD

    fn offset_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        unimplemented!()
    }
}
