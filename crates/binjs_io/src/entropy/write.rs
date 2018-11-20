//! An entropy encoder.
//!
//! At the time of this writing, the encoder bases everything on an external dictionary.
//! That dictionary is not encoded in the output.

// FIXME: Store strings
// FIXME: Split into packets
// FIXME: Implement lazy functions

use ::TokenWriterError;
use ::io::{ Path, TokenWriter };
use bytes::lengthwriter::{ Bytes, LengthWriter };
use super::predict::Instances;

use binjs_shared::{ F64, FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString };

use std::io::Write;
use std::ops::DerefMut;

use brotli;
use byteorder::{ BigEndian, WriteBytesExt };
use itertools::Itertools;
use range_encoding::opus;

const INITIAL_BUFFER_SIZE_BYTES : usize = 32768;

/// A container for information associated with a type of data we write to the stream
/// as part of the content (i.e. not the header).
///
/// Typically used to collect/display the number of bytes written in each category.
#[derive(Debug, Default, Add, Clone, AddAssign)]
pub struct ContentInfo<T> {
    pub bools: T,
    pub floats: T,
    pub unsigned_longs: T,
    pub string_enums: T,
    pub property_keys: T,
    pub identifier_names: T,
    pub interface_names: T,
    pub string_literals: T,
    pub list_lengths: T,
}

impl ContentInfo<brotli::CompressorWriter<Vec<u8>>> {
    pub fn brotli() -> Self {
        const BROTLI_BUFFER_SIZE: usize = 32768;
        const BROTLI_QUALITY: u32 = 11;
        const BROTLI_LG_WINDOW_SIZE: u32 = 20;
        ContentInfo {
            bools: brotli::CompressorWriter::new(Vec::with_capacity(BROTLI_BUFFER_SIZE), BROTLI_BUFFER_SIZE, BROTLI_QUALITY, BROTLI_LG_WINDOW_SIZE),
            floats: brotli::CompressorWriter::new(Vec::with_capacity(BROTLI_BUFFER_SIZE), BROTLI_BUFFER_SIZE, BROTLI_QUALITY, BROTLI_LG_WINDOW_SIZE),
            unsigned_longs: brotli::CompressorWriter::new(Vec::with_capacity(BROTLI_BUFFER_SIZE), BROTLI_BUFFER_SIZE, BROTLI_QUALITY, BROTLI_LG_WINDOW_SIZE),
            string_enums: brotli::CompressorWriter::new(Vec::with_capacity(BROTLI_BUFFER_SIZE), BROTLI_BUFFER_SIZE, BROTLI_QUALITY, BROTLI_LG_WINDOW_SIZE),
            property_keys: brotli::CompressorWriter::new(Vec::with_capacity(BROTLI_BUFFER_SIZE), BROTLI_BUFFER_SIZE, BROTLI_QUALITY, BROTLI_LG_WINDOW_SIZE),
            identifier_names: brotli::CompressorWriter::new(Vec::with_capacity(BROTLI_BUFFER_SIZE), BROTLI_BUFFER_SIZE, BROTLI_QUALITY, BROTLI_LG_WINDOW_SIZE),
            interface_names: brotli::CompressorWriter::new(Vec::with_capacity(BROTLI_BUFFER_SIZE), BROTLI_BUFFER_SIZE, BROTLI_QUALITY, BROTLI_LG_WINDOW_SIZE),
            string_literals: brotli::CompressorWriter::new(Vec::with_capacity(BROTLI_BUFFER_SIZE), BROTLI_BUFFER_SIZE, BROTLI_QUALITY, BROTLI_LG_WINDOW_SIZE),
            list_lengths: brotli::CompressorWriter::new(Vec::with_capacity(BROTLI_BUFFER_SIZE), BROTLI_BUFFER_SIZE, BROTLI_QUALITY, BROTLI_LG_WINDOW_SIZE),
        }
    }
}
impl ContentInfo<brotli::CompressorWriter<Vec<u8>>> {
    pub fn into_statistics(mut self) -> ContentInfo<Bytes> {
        ContentInfo {
            bools: {
                self.bools.flush()
                    .unwrap();
                self.bools.get_ref().len().into()
            },
            floats: {
                self.floats.flush()
                    .unwrap();
                self.floats.get_ref().len().into()
            },
            unsigned_longs: {
                self.unsigned_longs.flush()
                    .unwrap();
                self.unsigned_longs.get_ref().len().into()
            },
            string_enums: {
                self.string_enums.flush()
                    .unwrap();
                self.string_enums.get_ref().len().into()
            },
            property_keys: {
                self.property_keys.flush()
                    .unwrap();
                self.property_keys.get_ref().len().into()
            },
            identifier_names: {
                self.identifier_names.flush()
                    .unwrap();
                self.identifier_names.get_ref().len().into()
            },
            interface_names: {
                self.interface_names.flush()
                    .unwrap();
                self.interface_names.get_ref().len().into()
            },
            string_literals: {
                self.string_literals.flush()
                    .unwrap();
                self.string_literals.get_ref().len().into()
            },
            list_lengths: {
                self.list_lengths.flush()
                    .unwrap();
                self.list_lengths.get_ref().len().into()
            }
        }
    }
}
impl ContentInfo<opus::Writer<LengthWriter>> {
    pub fn length_writer() -> Self {
        ContentInfo {
            bools: opus::Writer::new(LengthWriter::new()),
            floats: opus::Writer::new(LengthWriter::new()),
            unsigned_longs: opus::Writer::new(LengthWriter::new()),
            string_enums: opus::Writer::new(LengthWriter::new()),
            property_keys: opus::Writer::new(LengthWriter::new()),
            identifier_names: opus::Writer::new(LengthWriter::new()),
            interface_names: opus::Writer::new(LengthWriter::new()),
            string_literals: opus::Writer::new(LengthWriter::new()),
            list_lengths: opus::Writer::new(LengthWriter::new()),
        }
    }
    pub fn into_statistics(self) -> ContentInfo<Bytes> {
        ContentInfo {
            bools: self.bools.done()
				.unwrap()
				.len(),
            floats: self.floats.done()
				.unwrap()
				.len(),
            unsigned_longs: self.unsigned_longs.done()
				.unwrap()
				.len(),
            string_enums: self.string_enums.done()
				.unwrap()
				.len(),
            property_keys: self.property_keys.done()
				.unwrap()
				.len(),
            identifier_names: self.identifier_names.done()
				.unwrap()
				.len(),
            interface_names: self.interface_names.done()
				.unwrap()
				.len(),
            string_literals: self.string_literals.done()
				.unwrap()
				.len(),
            list_lengths: self.list_lengths.done()
				.unwrap()
				.len(),
        }
    }
}
impl std::fmt::Display for ContentInfo<(Bytes, Instances)> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let bools_bytes = Into::<usize>::into(self.bools.0);
        let floats_bytes = Into::<usize>::into(self.floats.0);
        let unsigned_longs_bytes = Into::<usize>::into(self.unsigned_longs.0);
        let string_enums_bytes = Into::<usize>::into(self.string_enums.0);
        let property_keys_bytes = Into::<usize>::into(self.property_keys.0);
        let identifier_names_bytes = Into::<usize>::into(self.identifier_names.0);
        let interface_names_bytes = Into::<usize>::into(self.interface_names.0);
        let string_literals_bytes = Into::<usize>::into(self.string_literals.0);
        let list_lengths_bytes = Into::<usize>::into(self.list_lengths.0);

        let bools_symbols = Into::<usize>::into(self.bools.1);
        let floats_symbols = Into::<usize>::into(self.floats.1);
        let unsigned_longs_symbols = Into::<usize>::into(self.unsigned_longs.1);
        let string_enums_symbols = Into::<usize>::into(self.string_enums.1);
        let property_keys_symbols = Into::<usize>::into(self.property_keys.1);
        let identifier_names_symbols = Into::<usize>::into(self.identifier_names.1);
        let interface_names_symbols = Into::<usize>::into(self.interface_names.1);
        let string_literals_symbols = Into::<usize>::into(self.string_literals.1);
        let list_lengths_symbols = Into::<usize>::into(self.list_lengths.1);

        write!(formatter, "Content:
    Primitives:
        bools: {bools_bytes} bytes, {bools_symbols} symbols ({bools_bits_per_symbol:.2} bits/symbol)
        floats: {floats_bytes} bytes, {floats_symbols} symbols ({floats_bits_per_symbol:.2} bits/symbol)
        unsigned longs: {unsigned_longs_bytes} bytes, {unsigned_longs_symbols} symbols ({unsigned_longs_bits_per_symbol:.2} bits/symbol)
        string enums: {string_enums_bytes} bytes, {string_enums_symbols} symbols ({string_enums_bits_per_symbol:.2} bits/symbol)
    User strings:
        property keys: {property_keys_bytes} bytes, {property_keys_symbols} symbols ({property_keys_bits_per_symbol:.2} bits/symbol)
        identifier names: {identifier_names_bytes} bytes, {identifier_names_symbols} symbols ({identifier_names_bits_per_symbol:.2} bits/symbol)
        string literals: {string_literals_bytes} bytes, {string_literals_symbols} symbols ({string_literals_bits_per_symbol:.2} bits/symbol)
    Composed:
        interface names: {interface_names_bytes} bytes, {interface_names_symbols} symbols ({interface_names_bits_per_symbol:.2} bits/symbol)
        list lengths: {list_lengths_bytes} bytes, {list_lengths_symbols} symbols ({list_lengths_bits_per_symbol:.2} bits/symbol)
}}
",
            bools_bytes = bools_bytes,
            floats_bytes = floats_bytes,
            unsigned_longs_bytes = unsigned_longs_bytes,
            string_enums_bytes = string_enums_bytes,
            property_keys_bytes = property_keys_bytes,
            identifier_names_bytes = identifier_names_bytes,
            interface_names_bytes = interface_names_bytes,
            string_literals_bytes = string_literals_bytes,
            list_lengths_bytes = list_lengths_bytes,
            bools_symbols = bools_symbols,
            floats_symbols = floats_symbols,
            unsigned_longs_symbols = unsigned_longs_symbols,
            string_enums_symbols = string_enums_symbols,
            property_keys_symbols = property_keys_symbols,
            identifier_names_symbols = identifier_names_symbols,
            interface_names_symbols = interface_names_symbols,
            string_literals_symbols = string_literals_symbols,
            list_lengths_symbols = list_lengths_symbols,
            bools_bits_per_symbol = (bools_bytes as f64 * 8.) / bools_symbols as f64,
            floats_bits_per_symbol = (floats_bytes as f64 * 8.) / floats_symbols as f64,
            unsigned_longs_bits_per_symbol = (unsigned_longs_bytes as f64 * 8.) / unsigned_longs_symbols as f64,
            string_enums_bits_per_symbol = (string_enums_bytes as f64 * 8.) / string_enums_symbols as f64,
            property_keys_bits_per_symbol = (property_keys_bytes as f64 * 8.) / property_keys_symbols as f64,
            identifier_names_bits_per_symbol = (identifier_names_bytes as f64 * 8.) / identifier_names_symbols as f64,
            interface_names_bits_per_symbol = (interface_names_bytes as f64 * 8.) / interface_names_symbols as f64,
            string_literals_bits_per_symbol = (string_literals_bytes as f64 * 8.) / string_literals_symbols as f64,
            list_lengths_bits_per_symbol = (list_lengths_bytes as f64 * 8.) / list_lengths_symbols as f64,
        )
    }
}

/// An entropy encoder, based on the Opus bit-level entropy coding.
pub struct Encoder {
    /// Bit-level manipulations.
    writer: opus::Writer<Vec<u8>>,

    /// Shared dictionaries.
    options: ::entropy::Options,

    // --- Statistics.

    /// Measure the number of bytes written.
    content_opus_lengths: ContentInfo<opus::Writer<LengthWriter>>,

    content_brotli: ContentInfo<brotli::CompressorWriter<Vec<u8>>>,

    /// Measure the number of entries written.
    content_instances: ContentInfo<Instances>,
}

impl Encoder {
    /// Create a new Encoder.
    pub fn new(options: ::entropy::Options) -> Self { // FIXME: We shouldn't need to clone the entire `options`. A shared immutable reference would do nicely.
        Encoder {
            writer: opus::Writer::new(Vec::with_capacity(INITIAL_BUFFER_SIZE_BYTES)),
            options,
            content_opus_lengths: ContentInfo::length_writer(),
            content_brotli:  ContentInfo::brotli(),
            content_instances: ContentInfo::default(),
        }
    }
}


/// Emit a single symbol.
///
/// Used instead of a method as we need to generality wrt the field name.
///
/// Usage:
/// `symbol!(self, name_of_the_probability_table, name_of_the_ContentInfo_field, "Description, used for debugging",  path_in_the_ast,  value_to_encode)`
macro_rules! symbol {
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

macro_rules! window {
    ( $me: ident, $table:ident, $info:ident, $description: expr, $value: expr ) => {
        {
            let symbol = $me.options
                .probability_tables
                .$table
                .stats_by_node_value_mut(&$value)
                .ok_or_else(|| {
                    debug!(target: "entropy", "Couldn't find value {:?} ({})",
                        $value, $description);
                    TokenWriterError::NotInDictionary(format!("{}: {:?}", $description, $value))
                })?;

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

macro_rules! brotli {
    ( $me: ident, $table:ident, $brotli:ident, $info:ident, $description: expr, $value: expr ) => {
        {
            use bytes::varnum::WriteVarNum;
            let index = $me.options
                .probability_tables
                .$table
                .get_dictionary_index(&$value)
                .ok_or_else(|| {
                    debug!(target: "entropy", "Couldn't find value {:?} ({})",
                        $value, $description);
                    TokenWriterError::NotInDictionary(format!("{}: {:?}", $description, $value))
                })?;

                // 2. Ignore distribution, just write the index to Brotli.
                let as_usize: usize = index.into();
                let as_u32: u32 = as_usize as u32;
                $me.content_brotli
                    .$brotli
                    .write_u32::<BigEndian>(as_u32)
                    .map_err(TokenWriterError::WriteError)?;

                // 3. Also, update statistics
                $me.content_instances
                    .$info += Into::<Instances>::into(1);
                Ok(())
        }
    }
}

macro_rules! brotli_window {
    ( $me: ident, $table:ident, $brotli:ident, $info:ident, $description: expr, $value: expr ) => {
        {
            use bytes::varnum::WriteVarNum;
            let symbol = $me.options
                .probability_tables
                .$table
                .stats_by_node_value_mut(&$value)
                .ok_or_else(|| {
                    debug!(target: "entropy", "Couldn't find value {:?} ({})",
                        $value, $description);
                    TokenWriterError::NotInDictionary(format!("{}: {:?}", $description, $value))
                })?;

                // 2. Ignore distribution, just write the index to Brotli.
                let as_usize: usize = symbol.index.into();
                let as_u32: u32 = as_usize as u32;
                $me.content_brotli
                    .$brotli
                    .write_varnum(as_u32)
                    .map_err(TokenWriterError::WriteError)?;

                // 3. Also, update statistics
                $me.content_instances
                    .$info += Into::<Instances>::into(1);
                Ok(())
        }
    }
}

impl TokenWriter for Encoder {
    type Data = Vec<u8>;

    fn done(self) -> Result<Self::Data, TokenWriterError> {
        let data = self.writer.done()
            .map_err(TokenWriterError::WriteError)?;

        // Update byte lengths
        *self.options
            .content_lengths
            .borrow_mut()
            +=
        self.content_opus_lengths
            .into_statistics();

        *self.options
            .content_lengths
            .borrow_mut()
            +=
        self.content_brotli
            .into_statistics();

        // Update number of instances
        *self.options
            .content_instances
            .borrow_mut()
            +=
        self.content_instances;
        Ok(data)
    }

    // --- Primitive values

    fn bool_at(&mut self, value: Option<bool>, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, bool_by_path, bools, "bool_by_path",  path,  value)
    }

    fn float_at(&mut self, value: Option<f64>, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, float_by_path, floats, "float_by_path",  path,  value.map(F64::from))
    }

    fn unsigned_long_at(&mut self, value: u32, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, unsigned_long_by_path, unsigned_longs, "unsigned_long_by_path",  path,  value)
    }

    fn string_enum_at(&mut self, value: &SharedString, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, string_enum_by_path, string_enums, "string_enum_by_path",  path,  value)
    }

    fn string_at(&mut self, value: Option<&SharedString>, _path: &Path) -> Result<(), TokenWriterError> {
        //window!(self, string_literal_by_window, string_literals, "string_literal_by_window",  value.cloned())
        brotli!(self, string_literal_by_window, string_literals, string_literals, "string_literal_by_window", value.cloned())
    }

    fn identifier_name_at(&mut self, value: Option<&IdentifierName>, _path: &Path) -> Result<(), TokenWriterError> {
        //window!(self, identifier_name_by_window, identifier_names, "identifier_name_by_window", value.cloned())
        brotli!(self, identifier_name_by_window, identifier_names, identifier_names, "identifier_name_by_window", value.cloned())
    }

    fn property_key_at(&mut self, value: Option<&PropertyKey>, _path: &Path) -> Result<(), TokenWriterError> {
        //window!(self, property_key_by_window, property_keys, "property_key_by_window",  value.cloned())
        brotli!(self, property_key_by_window, property_keys, property_keys, "property_key_by_window", value.cloned())
    }


    // --- Composite stuff

    fn enter_tagged_tuple_at(&mut self, tag: &InterfaceName, _children: &[&FieldName], path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, interface_name_by_path, interface_names, "interface_name_by_path",  path,  tag)
    }

    fn enter_list_at(&mut self, len: usize, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, list_length_by_path, list_lengths, "list_length_by_path",  path,  Some(len as u32))
    }

    fn offset_at(&mut self, _path: &Path) -> Result<(), TokenWriterError> {
        unimplemented!()
    }
}
