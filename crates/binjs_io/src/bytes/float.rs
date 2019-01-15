use bytes::varnum::*;

use binjs_shared::F64;

use std;
use std::io::Write;

/// The representation of "no float", used for `float | null`.
const NONE_FLOAT_REPR: u64 = 0x7FF0000000000001;
const VARNUM_PREFIX_FLOAT: [u8; 2] = VARNUM_INVALID_ZERO_1;
const VARNUM_NULL: [u8; 3] = VARNUM_INVALID_ZERO_2;

pub fn varbytes_of_float(value: Option<f64>) -> Vec<u8> {
    let mut buf = Vec::with_capacity(4);
    buf.write_maybe_varfloat(value)
        .unwrap(); // The write cannot fail on a Vec<>
    buf
}

/// Encode a f64 | null, little-endian
pub fn bytes_of_float(value: Option<f64>) -> [u8; 8] {
    let mut as_u64 : u64 = match value {
        None => NONE_FLOAT_REPR,
        Some(value) => unsafe { std::mem::transmute::<f64, u64>(value) }
    };
    let mut buf: [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
    for i in 0..8 {
        buf[i] = (as_u64 % 256) as u8;
        as_u64 >>= 8;
    }
    buf
}

/// Utility for manipulating of `varfloats`, a somewhat optimized representation of floats.
///
/// This format is designed to help the most common floating point numbers (fairly short
/// integers) take fewer bytes.
///
/// Instead of always fitting in 64 bits, varfloats are represented as follows:
/// - null is represented as VARNUM_NULL (24 bits);
/// - floats with an i32 value are transmuted to u32s and represented as varnums (8 to 40 bits);
/// - other float values are prefixed with VARNUM_PREFIX_FLOAT (8 bits), then represented
///     with the usual 64 bits.
pub trait WriteVarFloat {
    fn write_maybe_varfloat(&mut self, value: Option<f64>) -> Result<usize, std::io::Error>;
    fn write_varfloat(&mut self, num: f64) -> Result<usize, std::io::Error>;

    /// Utility: as `write_maybe_varfloat` but with a `F64` instead of a `f64`.
    fn write_maybe_varfloat2(&mut self, value: Option<F64>) -> Result<usize, std::io::Error> {
        self.write_maybe_varfloat(value.map(Into::<f64>::into))
    }

    /// Utility: as `write_varfloat` but with a `F64` instead of a `f64`.
    fn write_varfloat2(&mut self, num: F64) -> Result<usize, std::io::Error> {
        self.write_varfloat(num.into())
    }
}

impl<T> WriteVarFloat for T where T: Write {
    fn write_maybe_varfloat(&mut self, value: Option<f64>) -> Result<usize, std::io::Error> {
        match value {
            None => {
                // Magic constant NULL.
                self.write_all(&VARNUM_NULL)?;
                Ok(VARNUM_NULL.len())
            }
            Some(v) => {
                self.write_varfloat(v)
            }
        }
    }

    fn write_varfloat(&mut self, value: f64) -> Result<usize, std::io::Error> {
        {
            // Let's see if we can represent this as an integer.
            // We can represent it as an integer if:
            // - it has the same value as its projection to i32;
            // - it's not -0.0
            let as_signed_integer = value as i32;
            if as_signed_integer as f64 == value && (as_signed_integer != 0 || value.is_sign_positive()) {
                // This is an i32. We can fit it in at most 5 7bit bytes.
                //
                // We pick a representation that favors small numbers, both
                // positive and negative.
                let as_unsigned = if as_signed_integer >= 0 {
                    // Map non-negative to even numbers.
                    // So, numbers in [0, 128) will fit in a single byte.
                    2 * (as_signed_integer as u32)
                } else {
                    // Map negatives to odd numbers.
                    // So, numbers in [-127, -1] will fit in a single byte.
                    2 * ((-as_signed_integer + 1) as u32) - 1
                };
                return self.write_varnum(as_unsigned)
            }
        }
        // Encode as a float prefixed by 0b00000001 0b00000000 (which is an invalid integer).
        let bytes = bytes_of_float(Some(value));
        self.write_all(&VARNUM_PREFIX_FLOAT)?;
        self.write_all(&bytes)?;
        Ok(bytes.len() + VARNUM_PREFIX_FLOAT.len())
    }
}

/// Utility for manipulating of `varfloats`, a somewhat optimized representation of floats.
///
/// This format is designed to help the most common floating point numbers (fairly short
/// integers) take fewer bytes.
///
/// Instead of always fitting in 64 bits, varfloats are represented as follows:
/// - null is represented as VARNUM_NULL (24 bits);
/// - floats with an i32 value are transmuted to u32s and represented as varnums (8 to 40 bits);
/// - other float values are prefixed with VARNUM_PREFIX_FLOAT (8 bits), then represented
///     with the usual 64 bits.
pub trait ReadVarFloat {
    fn read_maybe_varfloat(&mut self) -> Result<Option<f64>, std::io::Error>;
}
impl<T> ReadVarFloat for T where T: std::io::Read {
    fn read_maybe_varfloat(&mut self) -> Result<Option<f64>, std::io::Error> {
        let mut as_u32 = 0;
        let bytes_read = self.read_extended_varnum_to(&mut as_u32)?;
        if as_u32 == 0 {
            match bytes_read {
                1 => {
                    // 0 as one byte, that's the regular 0.
                    return Ok(Some(0.))
                }
                2 => {
                    // 0 as two bytes, that's VARNUM_PREFIX_FLOAT.
                    // The next 8 bytes are a IEEE f64.
                    let mut buf: [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0 ];
                    self.read_exact(&mut buf)?;
                    return Ok(float_of_bytes(&buf))
                }
                3 => {
                    // 0 as three bytes, that's VARNUM_NULL
                    return Ok(None);
                }
                _ => {
                    // That's an error.
                    return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid varfloat"));
                }
            }
        }
        // This float value maps to a i32.
        let as_i32 =
            if as_u32 % 2 == 0 {
                (as_u32 / 2) as i32
            } else {
                - (((as_u32 - 1) / 2) as i32)
            };
        Ok(Some(as_i32 as f64))
    }
}

/// Decode a f64 | null, little-endian
pub fn float_of_bytes(buf: &[u8; 8]) -> Option<f64> {
    let as_u64 =
          ((buf[0] as u64) << 0)
        | ((buf[1] as u64) << 8)
        | ((buf[2] as u64) << 16)
        | ((buf[3] as u64) << 24)
        | ((buf[4] as u64) << 32)
        | ((buf[5] as u64) << 40)
        | ((buf[6] as u64) << 48)
        | ((buf[7] as u64) << 56);
    if as_u64 == NONE_FLOAT_REPR {
        None
    } else {
        let as_f64 = unsafe { std::mem::transmute::<_, f64>(as_u64) };
        Some(as_f64)
    }
}

#[test]
fn test_floats() {
    use std::f64::*;
    for x in &[0., 100., 10., 1000., INFINITY, MIN, MAX, NEG_INFINITY] {
        let value = Some(*x);
        let encoded = bytes_of_float(value);
        let decoded = float_of_bytes(&encoded);
        println!("Encoded {:?} as {:?}, decoded as {:?}", x, encoded, decoded);
        assert_eq!(decoded, value);
    }

    assert_eq!(float_of_bytes(&bytes_of_float(None)), None);
}

#[test]
fn test_var_floats() {
    fn single_value(value: Option<f64>) {
        let mut buf = Vec::new();
        buf.write_maybe_varfloat(value)
            .unwrap();

        let mut input = std::io::Cursor::new(buf);
        let decoded = input.read_maybe_varfloat()
            .unwrap();
        assert_eq!(decoded, value);
    }
    use std::f64::*;
    for x in &[0., 100., -100., 10., -10., -256., 1000., 0.5, 1.7, 3.8, 11.1, INFINITY, MIN, MAX, NEG_INFINITY] {
        single_value(Some(*x));
    }
    single_value(None);
}