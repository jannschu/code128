//! This crate implements encoding and decoding of Code 128 linear barcodes
//! as defined in ISO/IEC 15417:2007.
//!
//! To achieve a minimal encoding size a dynamic programming approach is used.
//! The full 256 bit range can be encoded. For compatibility it is recommended
//! to stay within printable ASCII though.
//!
//! ## Example
//!
//! ```rust
//! use code128::{Code128, modules_to_blocks};
//!
//! let code = Code128::encode(b"Hello!");
//! println!("{}", modules_to_blocks(code.modules()));
//! ```
//! To create other outputs check out the [Code128] documentation.
//!
//! ## Charsets
//!
//! For best compatibility it is recommended to only encode printable
//! [ASCII](https://en.wikipedia.org/wiki/ASCII#Character_set)
//! characters, i.e. `0x20` (space) to `0x7E` (tilde).
//!
//! Code 128 interprets the range `0x00` to `0x7F` as ASCII, and `0xA0` to
//! `0xFF` as [ISO/IEC 8859-1](https://en.wikipedia.org/wiki/ISO/IEC_8859-1)
//! (Latin 1). The remaining range from `0x80` to `0x9F` can also be encoded
//! but has no visual representation.
//!
//! In the real world most implementations only handle `0x00` to `0x7F`, or
//! interpret results as UTF-8, although that is not covered by the standard.
//! However, if you are in control of encoding and decoding this is technically
//! possible and maybe even a contemporary choice.
mod decode;
mod encode;
mod latin1;
mod unicode;

pub use decode::{decode, decode_str, DecodingError};

pub use unicode::modules_to_blocks;

const SHIFT_MODE: u8 = 98;
const SWITCH_C: u8 = 99;
const SWITCH_B: u8 = 100;
const SWITCH_A: u8 = 101;
const START_A: u8 = 103;
const START_B: u8 = 104;
const START_C: u8 = 105;
const STOP: u8 = 108;

fn checksum(symbols: impl Iterator<Item = u8>) -> u8 {
    (symbols
        .enumerate()
        .map(|(i, idx)| (i.max(1) as u64) * idx as u64)
        .sum::<u64>()
        % 103) as u8
}

/// Representation of a "black line" in the code.
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Module {
    /// The width of the line.
    ///
    /// Ranges from one to four.
    pub width: u8,
    /// White space after the line.
    pub space: u8,
}

/// A Code 128.
///
/// You can use the [modules iterator](Self::modules) and the [size](Self::len)
/// to compute a visualization. A module corresponds to a "black
/// line" of the code and has a unitless width between one and four, as well
/// as a free space after it, also sized between one and four.
///
/// ## Pseudo code for visualization
///
/// Be aware that the standard demands a quiet zone of size 10 around the code.
/// To compute the size of a block, multiply its width with the available space
/// for the code (with a unit) divided by `20 + len()`. Here 20 corresponds to
/// the quiet zone.
///
/// ```rust
/// # use code128::Code128;
/// let code = Code128::encode(b"Code128 <3");
/// let available_space = 100.0; // unit is, say, "pt"
/// let line_width = available_space / code.len() as f64;
/// let mut pos = 10;
/// for module in code.modules() {
///      let x = pos as f64 * line_width;
///      let width = module.width as f64 * line_width;
///      // print line at `x` pt, `width` pt wide
///      pos += module.width + module.space;
/// }
/// ```
pub struct Code128 {
    indices: Vec<u8>,
}

impl Code128 {
    /// Encode the bytes as Code 128.
    ///
    /// See the [module documentation](crate) for hints on charsets.
    pub fn encode(data: &[u8]) -> Self {
        Code128Builder::default().encode(data)
    }

    /// Encode the string as Code 128 using Latin 1.
    ///
    /// The functions returns `None` if the string includes characters not
    /// included in Latin 1.
    ///
    /// The control characters of ASCII, `0x00` to `0x19`, are also encoded.
    pub fn encode_str(text: &str) -> Option<Self> {
        Code128Builder::default().encode_str(text)
    }

    /// Get the sequence of modules this Code 128 consists of.
    pub fn modules(&self) -> impl Iterator<Item = Module> + '_ {
        self.indices
            .iter()
            .flat_map(|idx| encode::bits_to_modules(encode::PATTERNS[*idx as usize]))
    }

    /// Get the total width of the code in units of the [Module](crate::Module)
    /// with the quiet zone included.
    pub fn len(&self) -> usize {
        self.indices.len() * 11 + 2 + 20
    }

    /// Whether this Code 128 encodes empty data.
    pub fn is_empty(&self) -> bool {
        self.indices.len() == 3
    }
}

#[doc(hidden)]
#[derive(Copy, Clone)]
pub enum Encoder {
    Mixed,
    DynamicProgramming,
}

/// Builder for encoding a Code 128 with more control.
#[doc(hidden)]
pub struct Code128Builder {
    encoder: Encoder,
}

impl Code128Builder {
    /// Which encoding should be used.
    pub fn with_encoder(self, encoder: Encoder) -> Self {
        Self { encoder }
    }

    /// Encode the bytes as Code 128.
    ///
    /// See the [module documentation](crate) for hints on charsets.
    pub fn encode(self, data: &[u8]) -> Code128 {
        let mut indices = match self.encoder {
            Encoder::Mixed => encode::encode_as_indices(data),
            Encoder::DynamicProgramming => encode::encode_as_indices_dp(data, vec![]),
        };
        indices.push(checksum(indices.iter().cloned()));
        indices.push(STOP);
        Code128 { indices }
    }

    /// Encode the string as Code 128 using Latin 1.
    ///
    /// The functions returns `None` if the string includes characters not
    /// included in Latin 1.
    ///
    /// The control characters of ASCII, `0x00` to `0x19`, are also encoded.
    pub fn encode_str(self, text: &str) -> Option<Code128> {
        latin1::utf8_to_latin1(text).map(|data| self.encode(&data))
    }
}

#[allow(clippy::derivable_impls)]
impl Default for Code128Builder {
    fn default() -> Self {
        Self {
            encoder: Encoder::Mixed,
        }
    }
}

#[test]
fn test_module_size() {
    for pattern in &encode::PATTERNS[0..107] {
        let size: u32 = encode::bits_to_modules(*pattern)
            .into_iter()
            .map(|m| m.width as u32 + m.space as u32)
            .sum();
        assert_eq!(size, 11);
    }

    let size: u32 = encode::bits_to_modules(encode::PATTERNS[STOP as usize])
        .into_iter()
        .map(|m| m.width as u32 + m.space as u32)
        .sum();
    assert_eq!(size, 13);
}

#[test]
fn test_code_size() {
    let code = Code128::encode(b"foo");
    let size = code
        .modules()
        .map(|m| m.width as u32 + m.space as u32)
        .sum::<u32>()
        + 20;
    assert_eq!(code.len(), size as usize);
}

#[test]
fn test_is_empty() {
    assert!(Code128::encode(b"").is_empty());
    assert!(!Code128::encode(b".").is_empty());
}

#[test]
fn test_256() {
    for x in 0..=255 {
        let code = Code128::encode(&[x]);
        let modules: Vec<Module> = code.modules().collect();
        assert_eq!(decode(&modules), Ok(vec![x]));
    }
}

#[test]
fn test_string_encoding_example() {
    let code = Code128::encode_str("Füße").unwrap();
    let modules: Vec<_> = code.modules().collect();
    assert_eq!(decode_str(&modules), Ok("Füße".into()));
}
