//! An entropy decoder
use super::probabilities::SymbolIndex;

use ::TokenReaderError;
use ::io::{ FileStructurePrinter, Path, TokenReader, TrivialGuard };

use binjs_shared::{ F64, FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString };

use std::io::Read;

use range_encoding::opus;

/// An entropy decoder, based on the Opus bit-level entropy coding.
pub struct Decoder<R: Read> {
    /// Bit-level manipulations.
    reader: opus::Reader<R>,

    /// Shared dictionaries.
    options: ::entropy::Options,
}

impl<R: Read> FileStructurePrinter for Decoder<R> {

}

impl<R: Read> Decoder<R> {
    pub fn new(options: ::entropy::Options, source: R) -> Result<Self, TokenReaderError> {
        let reader = opus::Reader::new(source)
            .map_err(TokenReaderError::ReadError)?;
        Ok(Decoder {
            reader,
            options,
        })
    }
}

/// Read a single symbol.
///
/// Used instead of a method as we need to generality wrt the field name.
///
/// Usage:
/// `symbol!(self, name_of_the_probability_table, "Description, used for debugging", path_in_the_ast)`
macro_rules! symbol {
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
                let index = $me.reader.symbol(borrow.deref_mut())
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


macro_rules! window {
    ( $me: ident, $table:ident, $description: expr ) => {
        {
            use std::ops::DerefMut;

            let index = {
                // 1. Get the frequency information for this path.
                let frequencies = $me.options.probability_tables
                    .$table
                    .frequencies()
                    .ok_or_else(|| TokenReaderError::NotInDictionary(format!("{}", $description)))?;
                let mut borrow = frequencies
                    .borrow_mut();

                // 2. Let bit-level I/O determine the symbol index stored.
                let index = $me.reader.symbol(borrow.deref_mut())
                    .map_err(TokenReaderError::ReadError)?;
                index
            };

            // 3. Deduce the value we have just read.
            let value = $me.options.probability_tables
                .$table
                .value_by_symbol_index(SymbolIndex::new(index as usize))
                .ok_or_else(|| TokenReaderError::NotInDictionary(format!("{} [{}]", stringify!($ident), index)))?;
            Ok(value.clone())
        }
    }
}


impl<R: Read> TokenReader for Decoder<R> {
    type ListGuard = TrivialGuard;
    type TaggedGuard = TrivialGuard;
    type UntaggedGuard = TrivialGuard;

    // ---- String types

    fn string_enum_at(&mut self, path: &Path) -> Result<SharedString, TokenReaderError> {
        symbol!(self, string_enum_by_path, "string_enum_by_path", path)
    }

    fn string_at(&mut self, _path: &Path) -> Result<Option<SharedString>, TokenReaderError> {
        window!(self, string_literal_by_window, "string_literal_by_window")
    }

    fn identifier_name_at(&mut self, _path: &Path) -> Result<Option<IdentifierName>, TokenReaderError> {
        window!(self, identifier_name_by_window, "identifier_name_by_window")
    }

    fn property_key_at(&mut self, _path: &Path) -> Result<Option<PropertyKey>, TokenReaderError> {
        window!(self, property_key_by_window, "property_key_by_window")
    }

    // ---- Primitive types

    fn float_at(&mut self, path: &Path) -> Result<Option<f64>, TokenReaderError> {
        let value = symbol!(self, float_by_path, "float_by_path", path)?;
        Ok(value.map(F64::into))
    }

    fn unsigned_long_at(&mut self, path: &Path) -> Result<u32, TokenReaderError> {
        symbol!(self, unsigned_long_by_path, "unsigned_long_by_path", path)
    }

    fn bool_at(&mut self, path: &Path) -> Result<Option<bool>, TokenReaderError> {
        symbol!(self, bool_by_path, "bool_by_path", path)
    }

    // ---- Lazy

    fn offset_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        unimplemented!()
    }

    // ---- Composed types

    fn list_at(&mut self, path: &Path) -> Result<(u32, Self::ListGuard), TokenReaderError> {
        let length = symbol!(self, list_length_by_path, "list_length_by_path", path)?
            .ok_or_else(|| TokenReaderError::EmptyList)?;
            // For the moment, we cannot read an optional list.
        Ok((length, TrivialGuard::new()))
    }

    fn tagged_tuple_at(&mut self, path: &Path) -> Result<(InterfaceName, Option<std::rc::Rc<Box<[FieldName]>>>, Self::TaggedGuard), TokenReaderError> {
        let name = symbol!(self, interface_name_by_path, "interface_name_by_path", path)?;
        Ok((name, None, TrivialGuard::new()))
    }

    fn untagged_tuple_at(&mut self, _path: &Path) -> Result<Self::UntaggedGuard, TokenReaderError> {
        unimplemented!()
    }
}