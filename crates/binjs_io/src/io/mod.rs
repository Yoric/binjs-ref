//! Mid-level API used to write individual components of an AST tree to a
//! stream of tokens.
//!
//! Simple implementations may write bytes as they come, while complex
//! ones may decide to provide sophisticated compression.
//!
//! In practice, this API is kept as a trait to simplify unit testing and
//! experimentation of sophisticated compression schemes.

use binjs_shared::{ IdentifierName, InterfaceName, FieldName, PropertyKey, SharedString, self };

use ::{ TokenReaderError, TokenWriterError };

use std::rc::Rc;

pub mod content;

mod deprecated;

pub use self::deprecated::{ TokenWriterWithTree, TokenWriterTreeAdapter };


/// An API for printing the binary representation and its structural
/// interpretation of the file.
///
/// A struct which implements this trait is supposed to print the binary
/// representation of the file in the first column internally, and print the
/// structural interpretation in the second column, which is triggered by
/// print_file_structure!() macro below, from various places that interprets
/// the file structure.
///
/// ```ignore
/// (first column)   (second column)
/// 01             # Script {
///                # .scope
/// 08             # AssertedScriptGlobalScope {
///                # .declared_names
/// 02             # list (length=0) [
/// ...            # ...
///                # ]
///                # }
///                # }
/// ```
///
/// Default no-op implementations are provided for all methods, which can be
/// used by the following declaration:
///
/// ```ignore
/// impl FileStructurePrinter for T {}
/// ```
///
pub trait FileStructurePrinter {
    /// Enables printing the binary representation and the structural
    /// interpretation.
    fn enable_file_structure_print(&mut self) {}

    /// Disables printing the binary representation and the structural
    /// interpretation.
    fn disable_file_structure_print(&mut self) {}

    /// True if file structure print is enabled.
    /// With default implementation, nothing is printed.
    fn is_file_structure_print_enabled(&mut self) -> bool {
        false
    }

    /// Prints the column separator ("# " characters), and padding before it
    /// if necessary.
    ///
    /// Before calling this method, the output is supposed to be the following:
    ///
    /// ```ignore
    /// 00 01 02<= cursor position
    /// ```
    ///
    /// After calling this method, the output is supposed to be the following:
    ///
    /// ```ignore
    /// 00 01 02     # <= cursor position
    /// ```
    ///
    /// The column width is not defined but the implementation is supposed to
    /// keep the same column width through the entire output, regardless of the
    /// number of bytes it reads for each line.
    fn prepare_file_structure_column(&mut self) {}

    /// Prints newline after printing the structural interpretation column.
    /// The implementation is supposed to print newline character(s) and also
    /// reset the current column position internally, in order to print the
    /// right number of spaces in prepare_file_structure_column.
    ///
    /// Before calling this method, the output is supposed to be the following:
    ///
    /// ```ignore
    /// 00 01 02     # string="hello"<= cursor position
    /// ```
    ///
    /// After calling this method, the output is supposed to be the following:
    ///
    /// ```ignore
    /// 00 01 02     # string="hello"
    /// <= cursor position
    /// ```
    ///
    fn newline_for_file_structure_print(&mut self) {}
}

/// Prints the structural interpretation of the data read after the last time
/// this macro is called.
///
/// Before calling this macro, the output is supposed to be the following:
///
/// ```ignore
/// 00 01 02<= cursor position
/// ```
///
/// After calling this macro with the following code:
///
/// ```ignore
/// print_file_structure!(reader, "string=\"hello\"");
/// ```
///
/// the output is supposed to be the following:
///
/// ```ignore
/// 00 01 02     # string="hello"
/// <= cursor position
/// ```
///
/// This macro may be called multiple times before reading any data, and in
/// that case the output is supposed to be the following:
///
/// ```ignore
/// 00 01 02     # string="hello"
///              # }
///              # .directives
/// <= cursor position
/// ```
///
#[macro_export]
macro_rules! print_file_structure(
    ( $reader:expr, $fmt:expr $( , $more:expr )* ) => (
        if $reader.is_file_structure_print_enabled() {
            $reader.prepare_file_structure_column();
            print!( $fmt $( , $more )* );
            $reader.newline_for_file_structure_print();
        }
    )
);


pub type Path = binjs_shared::ast::Path<InterfaceName, /* Field */ (usize, FieldName)>;

/// An API for reading tokens.
///
/// Note that a `TokenReader` by itself *cannot* determine the nature of the
/// following token. Rather, the driver of the `TokenReader` must be able to
/// deduce the nature of the following token from what it has previously
/// read.
///
/// All the reading methods offer a version suffixed with `_at(path: &Path)`,
/// which lets the reader determine what item we're reading in the AST. This
/// may be used both for debugging purposes and for encodings that depend
/// on the current position in the AST (e.g. entropy coding).
pub trait TokenReader: FileStructurePrinter where Self::ListGuard: Guard,
                                                  Self::TaggedGuard: Guard,
                                                  Self::UntaggedGuard: Guard,
                                                  Self: Sized
{
    /// A guard, used to make sure that the consumer has properly read a list.
    ///
    /// See the documentation of `self.list`.
    type ListGuard;

    /// A guard, used to make sure that the consumer has properly read a tagged tuple.
    ///
    /// See the documentation of `self.tagged_tuple`.
    type TaggedGuard;

    /// A guard, used to make sure that the consumer has properly read an untagged tuple.
    ///
    /// See the documentation of `self.untagged_tuple`.
    type UntaggedGuard;

    /// Poison the reader, ensuring that it will never be used for reading again.
    fn poison(&mut self) {
        // Do nothing.
    }

    /// Read a single UTF-8 string.
    ///
    /// The returned string MUST be valid UTF-8.
    fn string_at(&mut self, _path: &Path) -> Result<Option<SharedString>, TokenReaderError>;

    /// Read a single UTF-8 value from a string enumeration.
    ///
    /// The default implementation uses `self.string``, but some encodings may use
    /// the extra information e.g. to represent the enumeration by an index in the
    /// list of possible values, or to encode string enums as interfaces.
    ///
    /// The returned string MUST be valid UTF-8.
    fn string_enum_at(&mut self, path: &Path) -> Result<SharedString, TokenReaderError> {
        self.string_at(path)?
            .ok_or_else(|| ::TokenReaderError::EmptyVariant.into())
    }

    /// Read a single identifier name.
    ///
    /// The default implementation uses `self.string_at`, but
    /// some encodings may use the extra information e.g. to represent the
    /// identifier as a DeBruijn index.
    fn identifier_name_at(&mut self, path: &Path) -> Result<Option<IdentifierName>, TokenReaderError> {
        let result = self.string_at(path)?
            .map(IdentifierName);
        Ok(result)
    }

    /// Read a single property name.
    ///
    /// The default implementation uses `self.string_at`, but
    /// some encodings may use the extra information e.g. to initialize
    /// dictionaries.
    fn property_key_at(&mut self, path: &Path) -> Result<Option<PropertyKey>, TokenReaderError> {
        let result = self.string_at(path)?
            .map(PropertyKey);
        Ok(result)
    }

    /// Read a single `f64`. Note that all user-level numbers are `f64`.
    fn float_at(&mut self, _path: &Path) -> Result<Option<f64>, TokenReaderError>;

    /// Read a single `u32`.
    fn unsigned_long_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError>;

    /// Read a single `bool`.
    fn bool_at(&mut self, _path: &Path) -> Result<Option<bool>, TokenReaderError>;

    /// Read a single number of bytes.
    fn offset_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError>;

    /// Start reading a list.
    ///
    /// Returns a guard for that list and the number of elements
    /// in the list. Once the list is read entirely, callers MUST
    /// call `guard.done()` to ensure that the list was properly
    /// read (in particular that all bytes were consumed). In most
    /// implementations, failure to do so will raise an assertion.
    fn list_at(&mut self, _path: &Path) -> Result<(u32, Self::ListGuard), TokenReaderError>;

    /// Start reading a tagged tuple. If the stream was encoded
    /// properly, the tag is attached to an **ordered** tuple of
    /// fields that may be extracted **in order**.
    ///
    /// Returns the tag name, the ordered array of fields in which
    /// the contents must be read, and a guard for that tuple.
    /// Once the tuple is read entirely, callers MUST
    /// call `guard.done()` to ensure that the tuple was properly
    /// read (in particular that all bytes were consumed). In most
    /// implementations, failure to do so will raise an assertion.
    fn tagged_tuple_at(&mut self, _path: &Path) -> Result<(InterfaceName, Option<Rc<Box<[FieldName]>>>, Self::TaggedGuard), TokenReaderError>;

    /// Start reading an untagged tuple.
    ///
    /// Once the tuple is read entirely, callers MUST
    /// call `guard.done()` to ensure that the tuple was properly
    /// read (in particular that all bytes were consumed). In most
    /// implementations, failure to do so will raise an assertion.
    fn untagged_tuple_at(&mut self, _path: &Path) -> Result<Self::UntaggedGuard, TokenReaderError>;
}

/// Build an in-memory representation of a BinTree.
///
/// Implementations may for instance introduce atoms,
/// maximal sharing, etc.
///
/// All the reading methods offer a version suffixed with `_at(..., path: &Path)`,
/// which lets the writer determine what item we're reading in the AST. This
/// may be used both for debugging purposes and for encodings that depend
/// on the current position in the AST (e.g. entropy coding).
pub trait TokenWriter {

    /// The type of data generated by this writer.
    /// Typically some variant of `Vec<u8>`.
    type Data: AsRef<[u8]>;

    /// Finish writing, produce data.
    fn done(self) -> Result<Self::Data, TokenWriterError>;

    /// Write a tagged tuple.
    ///
    /// The number of items is specified by the grammar, so it MAY not be
    /// recorded by the `TokenWriter`.
    ///
    /// By convention, a null tagged tuple is the special tagged tuple "null",
    /// with no children.
    fn enter_tagged_tuple_at(&mut self, _tag: &InterfaceName, _children: &[&FieldName], _path: &Path) -> Result<(), TokenWriterError>;
    fn exit_tagged_tuple_at(&mut self, _tag: &InterfaceName, _children: &[&FieldName], _path: &Path) -> Result<(), TokenWriterError> {
        Ok(())
    }

    /// Write a list.
    ///
    /// By opposition to a tuple, the number of items is variable and MUST
    /// be somehow recorded by the `TokenWriter`.
    fn enter_list_at(&mut self, _len: usize, _path: &Path) -> Result<(), TokenWriterError>;
    fn exit_list_at(&mut self, _path: &Path) -> Result<(), TokenWriterError> {
        Ok(())
    }

    /// Write a single UTF-8 string.
    ///
    /// If specified, the string MUST be UTF-8.
    fn string_at(&mut self, value: Option<&SharedString>, _path: &Path) -> Result<(), TokenWriterError>;

    /// Write a single UTF-8 value from a string enumeration.
    ///
    /// The default implementation uses `self.string``, but some encodings may use
    /// the extra information e.g. to represent the enumeration by an index in the
    /// list of possible values, or to encode string enums as interfaces.
    fn string_enum_at(&mut self, value: &SharedString, path: &Path) -> Result<(), TokenWriterError>;

    /// Write a single number.
    fn float_at(&mut self, value: Option<f64>, _path: &Path) -> Result<(), TokenWriterError>;

    /// Write a single u32.
    fn unsigned_long_at(&mut self, value: u32, _path: &Path) -> Result<(), TokenWriterError>;

    /// Write single bool.
    fn bool_at(&mut self, value: Option<bool>, _path: &Path) -> Result<(), TokenWriterError>;

    /// Write the number of bytes left in this tuple.
    fn offset_at(&mut self, _path: &Path) -> Result<(), TokenWriterError>;

    fn property_key_at(&mut self, value: Option<&PropertyKey>, path: &Path) -> Result<(), TokenWriterError> {
        let string = value.map(PropertyKey::as_shared_string);
        self.string_at(string, path)
    }

    fn identifier_name_at(&mut self, value: Option<&IdentifierName>, path: &Path) -> Result<(), TokenWriterError> {
        let string = value.map(IdentifierName::as_shared_string);
        self.string_at(string, path)
    }
}


/// A guard used to ensure that some subset of the input stream was read properly.
pub trait Guard {
    /// Ensure that the subset of the input stream was read properly.
    fn done(self) -> Result<(), TokenReaderError>;
}

/// Trivial implementation of a guard.
///
/// This implementation serves as a placeholder or as a building block for
/// more sophisticated implementations: it does not check anything
/// meaningful in `done()` but ensures that `done()` is eventually called.
pub struct TrivialGuard {
    /// `true` once `done()` has been called, `false` otherwise.
    pub finalized: bool,
}
impl TrivialGuard {
    /// Create a `TrivialGuard`.
    ///
    /// If the `TrivialGuard` is dropped before `done()` is called
    /// or `self.finalized` is set to `true`, the drop will cause
    /// an assertion failure.
    pub fn new() -> Self {
        TrivialGuard {
            finalized: false
        }
    }
}

impl Guard for TrivialGuard {
    /// Mark the guard as safe to be dropped.
    fn done(mut self) -> Result<(), TokenReaderError> {
        self.finalized = true;
        Ok(())
    }
}

impl Drop for TrivialGuard {
    /// # Failures
    ///
    /// If the `TrivialGuard` is dropped before `done()` is called
    /// or `self.finalized` is set to `true`, the drop will cause
    /// an assertion failure.
    fn drop(&mut self) {
        assert!(self.finalized)
    }
}

pub trait Serialization<W, T> where W: TokenWriter, T: Sized {
    /// Serialize a piece of data.
    ///
    /// `path` indicates the path from the root of the AST.
    fn serialize(&mut self, data: T, path: &mut Path) -> Result<(), TokenWriterError>;
}
pub trait TokenSerializer<W> where W: TokenWriter {
    fn done(self) -> Result<W::Data, TokenWriterError>;
}
pub trait RootedTokenSerializer<W, T>: Serialization<W, T> + TokenSerializer<W> where W: TokenWriter, T: Sized { }
pub trait TokenSerializerFamily<T> {
    fn make<W>(&self, writer: W) -> Box<RootedTokenSerializer<W, T>> where W: TokenWriter;
}

pub trait Deserialization<R, T> where R: TokenReader, T: Sized {
    fn deserialize(&mut self, &mut Path) -> Result<T, TokenReaderError>;
}
pub trait InnerDeserialization<R, T> where R: TokenReader, T: Sized {
    fn deserialize_inner(&mut self, &mut Path) -> Result<T, TokenReaderError>;
}
