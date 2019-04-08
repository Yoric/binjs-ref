use binjs_io::{
    self, Deserialization, TokenReader, TokenReaderError, TokenWriterError, TokenWriterTreeAdapter,
};
pub use binjs_io::{Serialization, TokenSerializer, TokenWriter};
use binjs_shared::{
    self, FieldName, IdentifierName, InterfaceName, Offset, PropertyKey, SharedString,
};

use std::io::{Read, Seek};

/// A path used when (de)serializing ES6 ASTs.
pub type IOPath = binjs_shared::ast::Path<
    InterfaceName,
    (
        /* child index */ usize,
        /* field name */ FieldName,
    ),
>;

/// A structure used for deserialization purposes.
pub struct Deserializer<R>
where
    R: TokenReader,
{
    pub reader: R,
}
impl<R> Deserializer<R>
where
    R: TokenReader,
{
    pub fn new(reader: R) -> Self {
        Self { reader }
    }
}

impl<R> Deserialization<Option<bool>> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<bool>, TokenReaderError> {
        self.reader.bool_at(path)
    }
}
impl<R> Deserialization<bool> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<bool, TokenReaderError> {
        let maybe = self.reader.bool_at(path)?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyBool)),
            Some(x) => Ok(x),
        }
    }
}
impl<R> Deserialization<Option<f64>> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<f64>, TokenReaderError> {
        self.reader.float_at(path)
    }
}
impl<R> Deserialization<f64> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<f64, TokenReaderError> {
        let maybe = self.reader.float_at(path)?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyBool)),
            Some(x) => Ok(x),
        }
    }
}
impl<R> Deserialization<u32> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<u32, TokenReaderError> {
        self.reader.unsigned_long_at(path)
    }
}
impl<R> Deserialization<Offset> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Offset, TokenReaderError> {
        Ok(Offset(self.reader.offset_at(path)?))
    }
}
impl<R> Deserialization<Option<SharedString>> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<SharedString>, TokenReaderError> {
        self.reader.string_at(path)
    }
}
impl<R> Deserialization<SharedString> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<SharedString, TokenReaderError> {
        let maybe = self.reader.string_at(path)?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyString)),
            Some(x) => Ok(x),
        }
    }
}
impl<R> Deserialization<IdentifierName> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<IdentifierName, TokenReaderError> {
        self.reader
            .identifier_name_at(path)?
            .ok_or_else(|| From::from(TokenReaderError::EmptyString))
    }
}
impl<R> Deserialization<PropertyKey> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<PropertyKey, TokenReaderError> {
        self.reader
            .property_key_at(path)?
            .ok_or_else(|| From::from(TokenReaderError::EmptyString))
    }
}

impl<R> Deserialization<Option<IdentifierName>> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(
        &mut self,
        path: &mut IOPath,
    ) -> Result<Option<IdentifierName>, TokenReaderError> {
        self.reader.identifier_name_at(path)
    }
}
impl<R> Deserialization<Option<PropertyKey>> for Deserializer<R>
where
    R: TokenReader,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<PropertyKey>, TokenReaderError> {
        self.reader.property_key_at(path)
    }
}

impl<R, T> Deserialization<Vec<T>> for Deserializer<R>
where
    R: TokenReader,
    Self: Deserialization<T>,
{
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Vec<T>, TokenReaderError> {
        let len = self.reader.enter_list_at(path)?;
        if len > 0 {
            print_file_structure!(self.reader, "list (length={}) [", len);
        } else {
            print_file_structure!(self.reader, "list (length=0) []");
        }
        let mut result = Vec::with_capacity(len as usize);
        for _ in 0..len {
            result.push(self.deserialize(path)?);
        }
        if len > 0 {
            print_file_structure!(self.reader, "]");
        }
        self.reader.exit_list_at(path)?;
        Ok(result)
    }
}

/// A structure used for deserialization purposes.
///
/// Use `Serializer.deserialize` to read a structure from a token self.writer.
pub struct Serializer<W>
where
    W: TokenWriter,
{
    pub writer: W,
}
impl<W> Serializer<W>
where
    W: TokenWriter,
{
    pub fn new(writer: W) -> Self {
        Self { writer }
    }
}

impl<W> TokenSerializer<W> for Serializer<W>
where
    W: TokenWriter,
{
    fn done(self) -> Result<W::Data, TokenWriterError> {
        self.writer.done()
    }
}

impl<W> Serialization<Option<bool>> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(
        &mut self,
        value: &Option<bool>,
        path: &mut IOPath,
    ) -> Result<(), TokenWriterError> {
        self.writer.bool_at(*value, path)
    }
}
impl<W> Serialization<bool> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(&mut self, value: &bool, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.bool_at(Some(*value), path)
    }
}
impl<W> Serialization<Option<f64>> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(
        &mut self,
        value: &Option<f64>,
        path: &mut IOPath,
    ) -> Result<(), TokenWriterError> {
        self.writer.float_at(*value, path)
    }
}
impl<W> Serialization<f64> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(&mut self, value: &f64, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.float_at(Some(*value), path)
    }
}
impl<W> Serialization<u32> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(&mut self, value: &u32, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.unsigned_long_at(*value, path)
    }
}
impl<W> Serialization<SharedString> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(
        &mut self,
        value: &SharedString,
        path: &mut IOPath,
    ) -> Result<(), TokenWriterError> {
        self.writer.string_at(Some(value), path)
    }
}
impl<W> Serialization<Option<SharedString>> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(
        &mut self,
        value: &Option<SharedString>,
        path: &mut IOPath,
    ) -> Result<(), TokenWriterError> {
        self.writer.string_at(value.as_ref(), path)
    }
}
impl<W> Serialization<IdentifierName> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(
        &mut self,
        value: &IdentifierName,
        path: &mut IOPath,
    ) -> Result<(), TokenWriterError> {
        self.writer.identifier_name_at(Some(&value), path)
    }
}
impl<W> Serialization<PropertyKey> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(
        &mut self,
        value: &PropertyKey,
        path: &mut IOPath,
    ) -> Result<(), TokenWriterError> {
        self.writer.property_key_at(Some(&value), path)
    }
}
impl<W> Serialization<Option<IdentifierName>> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(
        &mut self,
        value: &Option<IdentifierName>,
        path: &mut IOPath,
    ) -> Result<(), TokenWriterError> {
        self.writer.identifier_name_at(value.as_ref(), path)
    }
}
impl<W> Serialization<Option<PropertyKey>> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(
        &mut self,
        value: &Option<PropertyKey>,
        path: &mut IOPath,
    ) -> Result<(), TokenWriterError> {
        self.writer.property_key_at(value.as_ref(), path)
    }
}
impl<W> Serialization<Offset> for Serializer<W>
where
    W: TokenWriter,
{
    fn serialize(&mut self, _: &Offset, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.offset_at(path)
    }
}
impl<W, T> Serialization<Box<T>> for Serializer<W>
where
    W: TokenWriter,
    T: ?Sized,
    Self: Serialization<T>,
{
    fn serialize(&mut self, value: &Box<T>, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.serialize(value.as_ref(), path)
    }
}

pub struct Decoder;
impl Decoder {
    pub fn new() -> Self {
        Decoder
    }
    pub fn decode<R: Read + Seek, AST>(
        &self,
        format: &mut binjs_io::Format,
        source: R,
    ) -> Result<AST, TokenReaderError>
    where
        Deserializer<binjs_io::simple::TreeTokenReader<R>>: Deserialization<AST>,
        Deserializer<binjs_io::multipart::TreeTokenReader>: Deserialization<AST>,
        Deserializer<binjs_io::binjs_json::read::Decoder<R>>: Deserialization<AST>,
        Deserializer<binjs_io::entropy::read::Decoder>: Deserialization<AST>,
    {
        let mut path = IOPath::new();
        match *format {
            binjs_io::Format::Simple { .. } => {
                let reader = binjs_io::simple::TreeTokenReader::new(source);
                let mut deserializer = Deserializer::new(reader);
                let ast = deserializer.deserialize(&mut path)?;
                Ok(ast)
            }
            binjs_io::Format::Multipart { .. } => {
                let reader = binjs_io::multipart::TreeTokenReader::new(source)?;
                let mut deserializer = Deserializer::new(reader);
                let ast = deserializer.deserialize(&mut path)?;
                Ok(ast)
            }
            binjs_io::Format::JSON { .. } => {
                let reader = binjs_io::binjs_json::read::Decoder::new(source)?;
                let mut deserializer = Deserializer::new(reader);
                let ast = deserializer.deserialize(&mut path)?;
                Ok(ast)
            }
            binjs_io::Format::Entropy { ref options } => {
                let reader = binjs_io::entropy::read::Decoder::new(options, source)?;
                let mut deserializer = Deserializer::new(reader);
                let ast = deserializer.deserialize(&mut path)?;
                Ok(ast)
            }
            _ => unimplemented!(),
        }
    }
}
pub struct Encoder;
impl Encoder {
    pub fn new() -> Self {
        Encoder
    }
    pub fn encode<AST>(
        &self,
        path: Option<&std::path::Path>,
        format: &mut binjs_io::Format,
        ast: &AST,
    ) -> Result<Box<[u8]>, TokenWriterError>
    where
        Serializer<TokenWriterTreeAdapter<binjs_io::simple::TreeTokenWriter>>: Serialization<AST>,
        Serializer<TokenWriterTreeAdapter<binjs_io::multipart::TreeTokenWriter>>:
            Serialization<AST>,
        Serializer<TokenWriterTreeAdapter<binjs_io::xml::Encoder>>: Serialization<AST>,
        Serializer<binjs_io::binjs_json::write::TreeTokenWriter>: Serialization<AST>,
        Serializer<binjs_io::entropy::write::Encoder>: Serialization<AST>,
    {
        let mut io_path = IOPath::new();
        match *format {
            binjs_io::Format::Simple { .. } => {
                let writer = binjs_io::simple::TreeTokenWriter::new();
                let mut serializer = Serializer::new(TokenWriterTreeAdapter::new(writer));
                serializer.serialize(ast, &mut io_path)?;
                serializer.done()
            }
            binjs_io::Format::Multipart {
                ref mut targets, ..
            } => {
                let writer = binjs_io::multipart::TreeTokenWriter::new(targets.clone());
                let mut serializer = Serializer::new(TokenWriterTreeAdapter::new(writer));
                serializer.serialize(ast, &mut io_path)?;
                serializer.done()
            }

            binjs_io::Format::XML => {
                let writer = binjs_io::xml::Encoder::new();
                let mut serializer = Serializer::new(TokenWriterTreeAdapter::new(writer));
                serializer.serialize(ast, &mut io_path)?;
                serializer.done()
            }
            binjs_io::Format::JSON => {
                let writer = binjs_io::binjs_json::write::TreeTokenWriter::new();
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast, &mut io_path)?;
                serializer.done()
            }
            binjs_io::Format::Entropy { ref options } => {
                // Pass: Extract frequency information on user-extensible strings.
                let depth = options.dictionaries.depth();
                let collector = binjs_io::entropy::collector::UserExtensibleDictionary::new(depth);
                let mut serializer = Serializer::new(&mut collector);

                // Pass: Use dictionaries to actually write tree.
                let writer = binjs_io::entropy::write::Encoder::new(path, (*options).clone(), collector.into_statistics());
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast, &mut io_path)?;
                serializer.done()
            }
        }
    }
}
