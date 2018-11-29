//! An entropy encoder.
//!
//! At the time of this writing, the encoder bases everything on an external dictionary.
//! That dictionary is not encoded in the output.

// FIXME: Store strings
// FIXME: Split into packets
// FIXME: Implement lazy functions

use ::TokenWriterError;
use ::io::{ Path, TokenWriter };
use ::io::statistics::{ Bytes, BytesAndInstances, ContentInfo, Instances };
use bytes::lengthwriter::LengthWriter;
use bytes::varnum::WriteVarNum;
use super::probabilities::IntoStatistics;

use binjs_shared::{ F64, FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString };

use std::ops::DerefMut;

use itertools::Itertools;
use range_encoding::opus;

const INITIAL_BUFFER_SIZE_BYTES : usize = 32768;

/// An entropy encoder, based on the Opus bit-level entropy coding.
pub struct Encoder {
    /// Bit-level manipulations.
    writer: opus::Writer<Vec<u8>>,

    /// Shared dictionaries.
    options: ::entropy::Options,

    // --- Statistics.

    /// Measure the number of bytes written.
    content_lengths: ContentInfo<opus::Writer<LengthWriter>>,

    /// Measure the number of entries written.
    content_instances: ContentInfo<Instances>,
}

impl Encoder {
    /// Create a new Encoder.
    pub fn new(options: ::entropy::Options) -> Self { // FIXME: We shouldn't need to clone the entire `options`. A shared immutable reference would do nicely.
        Encoder {
            writer: opus::Writer::new(Vec::with_capacity(INITIAL_BUFFER_SIZE_BYTES)),
            options,
            content_lengths: ContentInfo::with(|_| opus::Writer::new(LengthWriter::new())),
            content_instances: ContentInfo::with(|_| 0.into()),
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
            $me.content_lengths
                .$info
                .symbol(symbol.index.into(), borrow.deref_mut())
                .map_err(TokenWriterError::WriteError)?;
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
        *self.options
            .content_lengths
            .borrow_mut()
            +=
        self.content_lengths
            .into_with(|_, field| field.done()
                .unwrap()
                .len());
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

    fn string_at(&mut self, value: Option<&SharedString>, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, string_literal_by_path, string_literals, "string_literal_by_path",  path,  value.cloned())
    }

    fn string_enum_at(&mut self, value: &SharedString, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, string_enum_by_path, string_enums, "string_enum_by_path",  path,  value)
    }

    fn identifier_name_at(&mut self, value: Option<&IdentifierName>, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, identifier_name_by_path, identifier_names, "identifier_name_by_path",  path,  value.cloned())
    }

    fn property_key_at(&mut self, value: Option<&PropertyKey>, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, property_key_by_path, property_keys, "property_key_by_path",  path,  value.cloned())
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
