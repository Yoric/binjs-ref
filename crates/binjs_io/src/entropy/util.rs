//! Miscellaneous utilities used for entropy (de)coding.

use binjs_shared::*;

use std::io::{Cursor, Read};

/// Utility: an extension of `Read` with a method `expect` that may be
/// used to ensure the presence of a sequence of bytes.
pub trait Expect {
    /// Fail if the following bytes are not identical to `bytes`.
    ///
    /// State of the stream is not specified in case of failure. In
    /// case of success, exactly `bytes` are consumed.
    fn expect(&mut self, bytes: &[u8]) -> Result<(), std::io::Error>;
}

impl<T> Expect for T
where
    T: Read,
{
    fn expect(&mut self, bytes: &[u8]) -> Result<(), std::io::Error> {
        let mut buf = [b'?'];
        for b in bytes {
            self.read_exact(&mut buf)?;
            if b != &buf[0] {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "Not the expected bytes",
                ));
            }
        }
        Ok(())
    }
}

/// Utility: An extension of `Read` that reads `bytes` bytes and attempts
/// to conver them into a string.
pub trait ReadStr {
    fn read_string(&mut self, bytes: usize) -> Result<String, std::io::Error>;
}
impl ReadStr for Cursor<Vec<u8>> {
    fn read_string(&mut self, bytes: usize) -> Result<String, std::io::Error> {
        use std::io::*;
        let mut buf = Vec::with_capacity(bytes);
        unsafe {
            buf.set_len(bytes);
        }
        self.read_exact(&mut buf)?;
        let result = String::from_utf8(buf)
            .map_err(|_| Error::new(ErrorKind::InvalidData, "Invalid UTF-8 data"))?;
        Ok(result)
    }
}

pub trait ShowMe {
    fn show() -> bool {
        false
    }
}

impl ShowMe for Option<InterfaceName> {
    fn show() -> bool {
        true
    }
}

impl ShowMe for Option<SharedString> {}
impl ShowMe for Option<IdentifierName> {}
impl ShowMe for Option<PropertyKey> {}
impl ShowMe for Option<u32> {}
impl ShowMe for u32 {}
impl ShowMe for Option<f64> {}
impl ShowMe for Option<F64> {}
