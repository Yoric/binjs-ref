use std;
use std::io::{Read, Write};

pub const VARNUM_INVALID_ZERO_1: [u8; 2] = [1, 0];
pub const VARNUM_INVALID_ZERO_2: [u8; 3] = [1, 1, 0];
pub const VARNUM_INVALID_ZERO_3: [u8; 4] = [1, 1, 1, 0];
pub const VARNUM_INVALID_ZERO_4: [u8; 5] = [1, 1, 1, 1, 0];
pub const VARNUM_INVALID_ZERO_5: [u8; 6] = [1, 1, 1, 1, 1, 0];
pub const VARNUM_INVALID_ZERO_6: [u8; 7] = [1, 1, 1, 1, 1, 1, 0];

pub trait WriteVarNum {
    fn write_maybe_varnum(&mut self, value: Option<u32>) -> Result<usize, std::io::Error>;
    fn write_varnum(&mut self, num: u32) -> Result<usize, std::io::Error>;
}

pub trait ReadVarNum {
    fn read_varnum(&mut self) -> Result<u32, std::io::Error>;
    fn read_maybe_varnum(&mut self) -> Result<Option<u32>, std::io::Error>;
    fn read_varnum_to(&mut self, num: &mut u32) -> Result<usize, std::io::Error>;

    /// As `read_varnum_to`, but ignore invalid 0s.
    ///
    /// Used to implement e.g. `read_varfloat`.
    fn read_extended_varnum_to(&mut self, num: &mut u32) -> Result<usize, std::io::Error>;
}

impl<T> WriteVarNum for T where T: Write {
    fn write_maybe_varnum(&mut self, value: Option<u32>) -> Result<usize, std::io::Error> {
        match value {
            Some(v) => self.write_varnum(v),
            None => {
                self.write_all(&VARNUM_INVALID_ZERO_1)?;
                Ok(VARNUM_INVALID_ZERO_1.len())
            }
        }
    }
    fn write_varnum(&mut self, mut value: u32) -> Result<usize, std::io::Error> {
        let mut bytes = Vec::with_capacity(4);
        loop {
            let mut byte = ((value & 0x7F) << 1) as u8;
            if value > 0x7F {
                byte |= 1;
            }
            bytes.push(byte);
            value >>= 7;
            if value == 0 {
                break
            }
        }
        self.write_all(&bytes)?;
        Ok(bytes.len())
    }
}

impl<T> ReadVarNum for T where T: Read {
    /// ```
    /// use binjs_io::bytes::varnum::*;
    /// use std::io::Cursor;
    ///
    /// let source = vec![0];
    /// eprintln!("Test {:?}", source);
    /// assert_eq!(Cursor::new(source).read_varnum().unwrap(), 0);
    ///
    /// // This odd encoding of 0 may be reserved as a magic header:
    /// let source = vec![1, 0];
    /// assert!(Cursor::new(source).read_varnum().is_err());
    ///
    /// // This odd encoding of 0 may be reserved as a magic header:
    /// let source = vec![1,1,0];
    /// assert!(Cursor::new(source).read_varnum().is_err());
    ///
    /// // This odd encoding of 0 may be reserved as a magic header:
    /// let source = vec![1,1,1,0];
    /// assert!(Cursor::new(source).read_varnum().is_err());
    /// ```
    fn read_varnum(&mut self) -> Result<u32, std::io::Error> {
        let mut result = 0;
        self.read_varnum_to(&mut result)?;
        Ok(result)
    }

    fn read_maybe_varnum(&mut self) -> Result<Option<u32>, std::io::Error> {
        let mut result = 0;
        let bytes = self.read_varnum_to(&mut result)?;
        if result == 1 && bytes == 2 {
            Ok(None)
        } else {
            Ok(Some(result))
        }
    }

    fn read_extended_varnum_to(&mut self, num: &mut u32) -> Result<usize, std::io::Error> {
        let mut bytes = 0;
        let mut result : u32 = 0;
        let mut shift : u32 = 0;
        let mut buf : [u8;1] = [0];
        loop {
            if shift >= 32 {
                return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid varnum (doesn't fit in 32 bits)"))
            }
            bytes += self.read(&mut buf)?;

            let byte = buf[0];
            result |= (byte as u32 >> 1) << shift;
            if byte & 1 == 0 {
                *num = result;
                return Ok(bytes);
            }
            shift += 7;
        }
    }

    fn read_varnum_to(&mut self, num: &mut u32) -> Result<usize, std::io::Error> {
        let bytes = self.read_extended_varnum_to(num)?;
        if *num == 0 && bytes > 1 {
            return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid varnum (invalid 0)"))
        }
        Ok(bytes)
    }
}

#[test]
fn test_varnum() {
    use std::io::Cursor;
    // Produce a reasonably unbiaised sample of numbers.
    for i in 1..5 {
        let mut start = i;
        for num in &[3, 5, 7, 11, 13] {
            start *= *num;

            println!("test_varnum, testing with {}", start);
            let mut encoded = vec![];
            let encoded_bytes = encoded.write_varnum(start).unwrap();
            assert_eq!(encoded_bytes, encoded.len());
            println!("test_varnum, encoded as {:?}", encoded);

            let mut decoded : u32 = 0;
            let decoded_bytes = Cursor::new(encoded).read_varnum_to(&mut decoded).unwrap();

            assert_eq!(start, decoded);
            assert_eq!(encoded_bytes, decoded_bytes);
        }
    }
}