use binjs_io::{ self, Deserialization, TokenReader, TokenReaderError, TokenWriterTreeAdapter, TokenWriterError };
pub use binjs_io::{ Serialization, TokenSerializer, TokenWriter };
use binjs_shared::{ FieldName, IdentifierName, InterfaceName, Offset, PropertyKey, SharedString, self };

use std::io::{ Read, Seek };

/// A path used when (de)serializing ES6 ASTs.
pub type IOPath = binjs_shared::ast::Path<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;

/// A structure used for deserialization purposes.
pub struct Deserializer<R> where R: TokenReader {
    pub reader: R
}
impl<R> Deserializer<R> where R: TokenReader {
    pub fn new(reader: R) -> Self {
        Self {
            reader
        }
    }
}


impl<R> Deserialization<R, Option<bool>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<bool>, TokenReaderError> {
        self.reader.bool_at(path)
    }
}
impl<R> Deserialization<R, bool> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<bool, TokenReaderError> {
        let maybe = self.reader.bool_at(path)?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyBool)),
            Some(x) => Ok(x)
        }
    }
}
impl<R> Deserialization<R, Option<f64>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<f64>, TokenReaderError> {
        self.reader.float_at(path)
    }
}
impl<R> Deserialization<R, f64> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<f64, TokenReaderError> {
        let maybe = self.reader.float_at(path)?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyBool)),
            Some(x) => Ok(x)
        }
    }
}
impl<R> Deserialization<R, u32> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<u32, TokenReaderError> {
        self.reader.unsigned_long_at(path)
    }
}
impl<R> Deserialization<R, Offset> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Offset, TokenReaderError> {
        Ok(Offset(self.reader.offset_at(path)?))
    }
}
impl<R> Deserialization<R, Option<SharedString>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<SharedString>, TokenReaderError> {
        self.reader.string_at(path)
    }
}
impl<R> Deserialization<R, SharedString> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<SharedString, TokenReaderError> {
        let maybe = self.reader.string_at(path)?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyString)),
            Some(x) => Ok(x)
        }
    }
}
impl<R> Deserialization<R, IdentifierName> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<IdentifierName, TokenReaderError> {
        self.reader.identifier_name_at(path)?
            .ok_or_else(|| From::from(TokenReaderError::EmptyString))
    }
}
impl<R> Deserialization<R, PropertyKey> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<PropertyKey, TokenReaderError> {
        self.reader.property_key_at(path)?
            .ok_or_else(|| From::from(TokenReaderError::EmptyString))
    }
}

impl<R> Deserialization<R, Option<IdentifierName>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<IdentifierName>, TokenReaderError> {
        self.reader.identifier_name_at(path)
    }
}
impl<R> Deserialization<R, Option<PropertyKey>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<PropertyKey>, TokenReaderError> {
        self.reader.property_key_at(path)
    }
}


impl<R, T> Deserialization<R, Vec<T>> for Deserializer<R> where R: TokenReader, Self: Deserialization<R, T> {
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
pub struct Serializer<W> where W: TokenWriter {
    pub writer: W
}
impl<W> Serializer<W> where W: TokenWriter {
    pub fn new(writer: W) -> Self {
        Self {
            writer
        }
    }
    pub fn serialize<T>(&mut self, value: T, path: &mut IOPath) -> Result<(), TokenWriterError> where Self: Serialization<W, T> {
        (self as &mut Serialization<W, T>).serialize(value, path)
    }
}

impl<W> TokenSerializer<W> for Serializer<W> where W: TokenWriter {
    fn done(self) -> Result<W::Data, TokenWriterError> {
        self.writer.done()
    }
}

impl<W> Serialization<W, Option<bool>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<bool>, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.bool_at(value, path)
    }
}
impl<W> Serialization<W, bool> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: bool, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.bool_at(Some(value), path)
    }
}
impl<W> Serialization<W, Option<f64>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<f64>, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.float_at(value, path)
    }
}
impl<W> Serialization<W, f64> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: f64, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.float_at(Some(value), path)
    }
}
impl<W> Serialization<W, u32> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: u32, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.unsigned_long_at(value, path)
    }
}
impl<'a, W> Serialization<W, &'a Option<bool>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<bool>, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.bool_at(value.clone(), path)
    }
}
impl<'a, W> Serialization<W, &'a bool> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a bool, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.bool_at(Some(*value), path)
    }
}
impl<'a, W> Serialization<W, &'a Option<f64>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<f64>, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.float_at(value.clone(), path)
    }
}
impl<'a, W> Serialization<W, &'a f64> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a f64, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.float_at(Some(*value), path)
    }
}
impl<'a, W> Serialization<W, &'a u32> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a u32, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.unsigned_long_at(value.clone(), path)
    }
}
/*
impl<'a, W> Serialization<W, Option<&'a str>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<&'a str>, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.string_at(value, path)
    }
}
impl<'a, W> Serialization<W, &'a str> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a str, path: &mut IOPath) -> Result<(), TokenWriterError> {
         self.writer.string_at(Some(value), path)
   }
}
*/
impl<'a, W> Serialization<W, &'a SharedString> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a SharedString, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.string_at(Some(value), path)
    }
}
impl<'a, W> Serialization<W, &'a Option<SharedString>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<SharedString>, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.string_at(value.as_ref(), path)
    }
}
impl<'a, W> Serialization<W, &'a IdentifierName> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a IdentifierName, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.identifier_name_at(Some(&value), path)
    }
}
impl<'a, W> Serialization<W, &'a PropertyKey> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a PropertyKey, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.property_key_at(Some(&value), path)
    }
}
impl<'a, W> Serialization<W, &'a Option<IdentifierName>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<IdentifierName>, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.identifier_name_at(value.as_ref(), path)
    }
}
impl<'a, W> Serialization<W, &'a Option<PropertyKey>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<PropertyKey>, path: &mut IOPath) -> Result<(), TokenWriterError> {
        self.writer.property_key_at(value.as_ref(), path)
    }
}
impl<'a, W> Serialization<W, &'a Offset> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, _: &'a Offset, path: &mut IOPath) -> Result<(), TokenWriterError> {
         self.writer.offset_at(path)
   }
}

pub struct Decoder;
impl Decoder {
    pub fn new() -> Self {
        Decoder
    }
    pub fn decode<'a, R: Read + Seek, AST>(&self, format: &'a mut binjs_io::Format, source: R) -> Result<AST, TokenReaderError>
        where
            Deserializer<binjs_io::simple::TreeTokenReader<R>> : Deserialization<binjs_io::simple::TreeTokenReader<R>, AST>,
            Deserializer<binjs_io::multipart::TreeTokenReader> : Deserialization<binjs_io::multipart::TreeTokenReader, AST>,
            Deserializer<binjs_io::entropy::read::Decoder<'a>> : Deserialization<binjs_io::entropy::read::Decoder<'a>, AST>,
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
            binjs_io::Format::Entropy { ref options } => {
                let reader = binjs_io::entropy::read::Decoder::new(options, source)?;
                let mut deserializer = Deserializer::new(reader);
                let ast = deserializer.deserialize(&mut path)?;
                Ok(ast)
            }
            _ => unimplemented!()
        }
    }
}
pub struct Encoder;
impl Encoder {
    pub fn new() -> Self {
        Encoder
    }
    pub fn encode<'a, AST>(&self, path: Option<&std::path::Path>, format: &'a mut binjs_io::Format, ast: &'a AST) -> Result<Box<AsRef<[u8]>>, TokenWriterError>
        where
            Serializer<TokenWriterTreeAdapter<binjs_io::simple::TreeTokenWriter>> : Serialization<TokenWriterTreeAdapter<binjs_io::simple::TreeTokenWriter>, &'a AST>,
            Serializer<TokenWriterTreeAdapter<binjs_io::multipart::TreeTokenWriter>> : Serialization<TokenWriterTreeAdapter<binjs_io::multipart::TreeTokenWriter>, &'a AST>,
            Serializer<TokenWriterTreeAdapter<binjs_io::xml::Encoder>> : Serialization<TokenWriterTreeAdapter<binjs_io::xml::Encoder>, &'a AST>,
            Serializer<binjs_io::entropy::write::Encoder> : Serialization<binjs_io::entropy::write::Encoder, &'a AST>
    {
        let mut io_path = IOPath::new();
        match *format {
            binjs_io::Format::Simple { .. } => {
                let writer = binjs_io::simple::TreeTokenWriter::new();
                let mut serializer = Serializer::new(TokenWriterTreeAdapter::new(writer));
                serializer.serialize(ast, &mut io_path)?;
                let data = serializer.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::Multipart { ref mut targets, .. } => {
                let writer = binjs_io::multipart::TreeTokenWriter::new(targets.clone());
                let mut serializer = Serializer::new(TokenWriterTreeAdapter::new(writer));
                serializer.serialize(ast, &mut io_path)?;
                let data = serializer.done()?;
                Ok(Box::new(data))
            }

            binjs_io::Format::XML => {
                let writer = binjs_io::xml::Encoder::new();
                let mut serializer = Serializer::new(TokenWriterTreeAdapter::new(writer));
                serializer.serialize(ast, &mut io_path)?;
                let data = serializer.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::Entropy { ref options } => {
                let writer = binjs_io::entropy::write::Encoder::new(path, (*options).clone());
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast, &mut io_path)?;
                let data = serializer.done()?;
                Ok(Box::new(data))
            }
        }
    }
}

