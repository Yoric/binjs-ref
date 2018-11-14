//! An entropy encoder.
//!
//! At the time of this writing, the encoder bases everything on an external dictionary.
//! That dictionary is not encoded in the output.

// FIXME: Store strings
// FIXME: Split into packets
// FIXME: Implement lazy functions

use ::TokenWriterError;
use ::io::{ Path, TokenWriter };

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
}

impl Encoder {
    pub fn new(options: ::entropy::Options) -> Self {
        Encoder {
            writer: opus::Writer::new(Vec::with_capacity(INITIAL_BUFFER_SIZE_BYTES)),
            options,
        }
    }
}


/// Emit a single symbol.
///
/// Used instead of a method as we need to generality wrt the field name.
///
/// Usage:
/// `symbol!(self, name_of_the_probability_table, "Description, used for debugging",  path_in_the_ast,  value_to_encode)`
macro_rules! symbol {
    ( $me: ident, $table:ident, $description: expr, $path:expr, $value: expr ) => {
        {
            // 1. Shorten the path.
            let path = $path.tail($me.options
                .probability_tables
                .depth);
            debug!(target: "entropy_details", "Known paths ({}, depth {}): [{}]",
                $description,
                $me.options.probability_tables.depth,
                $me.options
                    .probability_tables
                    .$table
                    .paths()
                    .map(|k| format!("{:?}", k))
                    .format(", "));

            // 2. Locate the `SymbolInfo` information for this value given the
            // shortened path information.
            let symbol = $me.options
                .probability_tables
                .$table
                .stats_by_node_value_mut(path, &$value)
                .ok_or_else(|| {
                    debug!(target: "entropy", "Couldn't find value {:?} at {:?} ({})",
                        $value, path, $description);
                    TokenWriterError::NotInDictionary(format!("{}: {:?} at {:?}", $description, $value, path))
                })?;

            // 3. This gives us an index (`symbol.index`) and a probability distribution
            // (`symbol.distribution`). Use them to write the probability at bit-level.
            let mut borrow = symbol.distribution
                .borrow_mut();
            $me.writer.symbol(symbol.index.into(), borrow.deref_mut())
                .map_err(TokenWriterError::WriteError)?;
            Ok(())
        }
    }
}

impl TokenWriter for Encoder {
    type Statistics = usize; // Placeholder
    type Data = Vec<u8>;

    fn done(self) -> Result<(Self::Data, Self::Statistics), TokenWriterError> {
        let data = self.writer.done()
            .map_err(TokenWriterError::WriteError)?;
        Ok((data, 0))
    }

    fn bool_at(&mut self, value: Option<bool>, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, bool_by_path, "bool_by_path",  path,  value)
    }

    fn float_at(&mut self, value: Option<f64>, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, float_by_path, "float_by_path",  path,  value.map(F64::from))
    }

    fn unsigned_long_at(&mut self, value: u32, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, unsigned_long_by_path, "unsigned_long_by_path",  path,  value)
    }

    fn string_at(&mut self, value: Option<&SharedString>, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, string_literal_by_path, "string_literal_by_path",  path,  value.cloned())
    }

    fn string_enum_at(&mut self, value: &SharedString, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, string_enum_by_path, "string_enum_by_path",  path,  value)
    }

    fn identifier_name_at(&mut self, value: Option<&IdentifierName>, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, identifier_name_by_path, "identifier_name_by_path",  path,  value.cloned())
    }

    fn property_key_at(&mut self, value: Option<&PropertyKey>, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, property_key_by_path, "property_key_by_path",  path,  value.cloned())
    }

    fn enter_tagged_tuple_at(&mut self, tag: &InterfaceName, _children: &[&FieldName], path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, interface_name_by_path, "interface_name_by_path",  path,  tag)
    }

    fn enter_list_at(&mut self, len: usize, path: &Path) -> Result<(), TokenWriterError> {
        symbol!(self, list_length_by_path, "list_length_by_path",  path,  Some(len as u32))
    }

    fn offset_at(&mut self, _path: &Path) -> Result<(), TokenWriterError> {
        unimplemented!()
    }
}
