//! Compressing bytes from/to bytes.

use bytes::serialize::*;
use bytes::varnum::*;

use rand::Rng;
use rand::distributions::Distribution;
use rand::distributions::Standard;
use rand::thread_rng;
use rand::seq::SliceRandom;

use std;
use std::collections::HashSet;
use std::io::{ Cursor, Read, Write };

const BROTLI_BUFFER_SIZE : usize = 4096;
const BROTLI_QUALITY: u32 = 11;
const BROTLI_LG_WINDOW_SIZE: u32 = 20;
const LZW_MIN_CODE_SIZE: u8 = 8;

/// The compression mechanisms supported by this encoder.
/// They are designed to match HTTP's Accept-Encoding:
/// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Compression {
    /// no compression (`identity;`)
    Identity,
    /// gzip compression (`gzip;`)
    Gzip,
    /// zlib compression (`deflate;`)
    Deflate,
    /// brotly compression (`br;`)
    Brotli,
    /// Lwz compression (`compress;`)
    Lzw,
}

impl Distribution<Compression> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Compression {
        use self::Compression::*;
        let choices = [Identity, Gzip, /* Deflate is apprently broken https://github.com/alexcrichton/flate2-rs/issues/151 , */ Brotli /*, Lzw doesn't work yet */];
        choices.choose(rng)
            .unwrap()
            .clone()
    }
}


#[derive(Clone, Debug)]
pub struct CompressionResult {
    pub before_bytes: usize,
    pub after_bytes: usize,
    pub algorithms: HashSet<Compression>,
}

impl Compression {
    pub fn name(&self) -> &str {
        use self::Compression::*;
        match *self {
            Identity => "Identity",
            Gzip => "Gzip",
            Deflate => "Deflate",
            Brotli => "Brotli",
            Lzw => "Lzw",
        }
    }

    pub fn code(&self) -> &str {
        use self::Compression::*;
        match *self {
            Identity => "identity",
            Gzip => "gzip",
            Deflate => "deflate",
            Brotli => "br",
            Lzw => "lzw",
        }
    }

    pub fn parse(name: Option<&str>) -> Option<Compression> {
        let result = match name {
            None | Some("identity") => Compression::Identity,
            Some("lzw") => Compression::Lzw,
            Some("br") => Compression::Brotli,
            Some("gzip") => Compression::Gzip,
            Some("deflate") => Compression::Deflate,
            Some("random") => thread_rng().gen(),
            Some(_) => {
                return None;
            }
        };
        Some(result)
    }

    pub fn values() -> Box<[Self]> {
        use self::Compression::*;
        Box::new([Identity, Gzip, Deflate, Brotli /*, Lzw doesn't work yet*/])
    }

    pub fn is_compressed(&self) -> bool {
        if let Compression::Identity = *self {
            true
        } else {
            false
        }
    }

    // Format:
    // - compression type (string);
    // - compressed byte length (varnum);
    // - data.
    pub fn compress<W: Write>(&self, data: &[u8], out: &mut W) -> Result<CompressionResult, std::io::Error> {
        let before_bytes = data.len();
        let after_bytes = match *self {
            Compression::Identity => {
                out.write_all(b"identity;")?;
                out.write_varnum(data.len() as u32)?;
                out.write_all(data)?;
                data.len()
            }
            Compression::Gzip => {
                use flate2;
                out.write_all(b"gzip;")?;
                // Compress
                let buffer = Vec::with_capacity(data.len());
                let mut encoder = flate2::write::GzEncoder::new(buffer, flate2::Compression::best());
                encoder.write_all(data)?;
                let buffer = encoder.finish()?;
                // Write
                out.write_varnum(buffer.len() as u32)?;
                out.write_all(&buffer)?;
                buffer.len()
            }
            Compression::Deflate => {
                use flate2;
                out.write_all(b"deflate;")?;
                // Compress
                let buffer = Vec::with_capacity(data.len());
                let mut encoder = flate2::write::ZlibEncoder::new(buffer, flate2::Compression::best());
                encoder.write(data)?;
                let buffer = encoder.finish()?;
                // Write
                out.write_varnum(buffer.len() as u32)?;
                out.write_all(&buffer)?;
                buffer.len()
            }
            Compression::Brotli => {
                use brotli;
                out.write_all(b"br;")?;
                // Compress
                let mut buffer = Vec::with_capacity(data.len());
                {
                    let mut encoder = brotli::CompressorWriter::new(&mut buffer, BROTLI_BUFFER_SIZE, BROTLI_QUALITY, BROTLI_LG_WINDOW_SIZE);
                    encoder.write(data)?;
                }
                // Write
                out.write_varnum(buffer.len() as u32)?;
                out.write_all(&buffer)?;
                buffer.len()
            }
            Compression::Lzw => {
                use lzw;
                out.write_all(b"compress;")?;
                // Compress
                let mut buffer = Vec::with_capacity(data.len());
                {
                    let writer = lzw::LsbWriter::new(&mut buffer);
                    let mut encoder = lzw::Encoder::new(writer, LZW_MIN_CODE_SIZE)?;
                    encoder.encode_bytes(data)?;
                }
                // Write
                out.write_varnum(buffer.len() as u32)?;
                out.write_all(&buffer)?;
                buffer.len()
            }
        };
        Ok(CompressionResult {
            before_bytes,
            after_bytes,
            algorithms: [self.clone()].iter().cloned().collect()
        })
    }

    pub fn decompress<R: Read, T>(inp: &mut R, deserializer: &T) -> Result<T::Target, std::io::Error> where T: Deserializer {
        const MAX_LENGTH: usize = 32;
        let mut header = Vec::with_capacity(MAX_LENGTH);
        let mut found = false;

        // Scan for `;` in the first 32 bytes.
        for _ in 0..MAX_LENGTH {
            let mut buf = [0];
            inp.read_exact(&mut buf)?;
            if buf[0] != b';' {
                header.push(buf[0]);
            } else {
                found = true;
                break;
            }
        }

        if !found {
            return Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Invalid compression header"))
        }

        let compression =
            if &header == b"identity" {
                Compression::Identity
            } else if &header == b"gzip" {
                Compression::Gzip
            } else if &header == b"deflate" {
                Compression::Deflate
            } else if &header == b"br" {
                Compression::Brotli
            } else if &header == b"compress" {
                Compression::Lzw
            } else {
                return Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Invalid compression header"))
            };

        let mut byte_len = 0;
        inp.read_varnum_to(&mut byte_len)?;

        let mut compressed_bytes = Vec::with_capacity(byte_len as usize);
        unsafe { compressed_bytes.set_len(byte_len as usize )};
        inp.read_exact(&mut compressed_bytes)?;

        let decompressed_bytes = match compression {
            Compression::Identity => compressed_bytes,
            Compression::Gzip => {
                use flate2;
                let mut decoder = flate2::read::GzDecoder::new(Cursor::new(&compressed_bytes));
                let mut buf = Vec::with_capacity(1024);
                decoder.read_to_end(&mut buf)?;
                buf
            }
            Compression::Deflate => {
                use flate2;
                let mut decoder = flate2::read::ZlibDecoder::new(Cursor::new(&compressed_bytes));
                let mut buf = Vec::with_capacity(1024);
                decoder.read_to_end(&mut buf)?;
                buf
            }
            Compression::Brotli => {
                use brotli;
                let mut decoder = brotli::Decompressor::new(Cursor::new(&compressed_bytes), BROTLI_BUFFER_SIZE);
                let mut buf = Vec::with_capacity(1024);
                decoder.read_to_end(&mut buf)?;
                buf
            }
            Compression::Lzw => {
                use lzw;
                let reader = lzw::LsbReader::new();
                let mut decoder = lzw::Decoder::new(reader, LZW_MIN_CODE_SIZE);
                let (_, data) = decoder.decode_bytes(&compressed_bytes)?;
                let mut buf = Vec::with_capacity(data.len());
                buf.extend_from_slice(data);
                buf
            }
        };

        let value = deserializer.read(&mut Cursor::new(decompressed_bytes))?;
        Ok(value)
    }
}