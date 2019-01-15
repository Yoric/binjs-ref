pub mod decode;
pub mod encode;

use io::encode::Encode;

use binjs_io;
use binjs_meta;

use std;

use json::JsonValue as JSON;

/// A mechanism for decoding from a Format.
pub struct Decoder;
impl Decoder {
    pub fn new() -> Self {
        Decoder
    }
    pub fn decode<R: std::io::Read + std::io::Seek>(&self, grammar: &binjs_meta::spec::Spec, format: &mut binjs_io::Format, source: R) -> Result<JSON, decode::Error> {
        match *format {
            binjs_io::Format::Simple { .. } => {
                let reader = binjs_io::simple::TreeTokenReader::new(source);
                let mut decoder = decode::Decoder::new(grammar, reader);
                let ast = decoder.decode()?;
                Ok(ast)
            }
            binjs_io::Format::Multipart { .. } => {
                let reader = binjs_io::multipart::TreeTokenReader::new(source)
                    .map_err(decode::Error::TokenReaderError)?;
                let mut decoder = decode::Decoder::new(grammar, reader);
                let ast = decoder.decode()?;
                Ok(ast)
            }
            _ => unimplemented!()
        }
    }
}


/// A mechanism for encoding from a Format.
pub struct Encoder;
impl Encoder {
    pub fn new() -> Self {
        Encoder
    }
    pub fn encode(&self, path: Option<&std::path::Path>, grammar: &binjs_meta::spec::Spec, format: &mut binjs_io::Format, ast: &JSON) -> Result<Box<AsRef<[u8]>>, std::io::Error>
    {
        use binjs_io::TokenWriterTreeAdapter;
        match *format {
            binjs_io::Format::Simple { .. } => {
                let writer = TokenWriterTreeAdapter::new(binjs_io::simple::TreeTokenWriter::new());
                let mut encoder = encode::Encoder::new(grammar, writer);
                encoder.generic_encode(ast)?;
                let data = encoder.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::Multipart { ref mut targets, .. } => {
                let writer = TokenWriterTreeAdapter::new(binjs_io::multipart::TreeTokenWriter::new(targets.clone()));
                let mut encoder = encode::Encoder::new(grammar, writer);
                encoder.generic_encode(ast)?;
                let data = encoder.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::XML => {
                let writer = TokenWriterTreeAdapter::new(binjs_io::xml::Encoder::new());
                let mut encoder = encode::Encoder::new(grammar, writer);
                encoder.generic_encode(ast)?;
                let data = encoder.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::Entropy { ref options } => {
                let writer = binjs_io::entropy::write::Encoder::new(path, (*options).clone());
                let mut encoder = encode::Encoder::new(grammar, writer);
                encoder.generic_encode(ast)?;
                let data = encoder.done()?;
                Ok(Box::new(data))
            }
        }
    }
}
