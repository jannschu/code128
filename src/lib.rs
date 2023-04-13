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
//! use code128::{Code128, bars_to_blocks};
//!
//! let code = Code128::encode(b"Hello!");
//! println!("{}", bars_to_blocks(code.bars()));
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
#![no_std]

#[cfg(not(feature = "std"))]
extern crate alloc as std;
#[cfg(feature = "std")]
extern crate std;

#[cfg(test)]
use std::vec;
use std::vec::Vec;

mod decode;
mod encode;
mod latin1;
#[cfg(feature = "unicode")]
mod unicode;

pub use decode::{decode, decode_str, DecodingError};

#[cfg(feature = "unicode")]
pub use unicode::bars_to_blocks;

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
pub struct Bar {
    /// The width of the line.
    ///
    /// Ranges from one to four.
    pub width: u8,
    /// White space after the line.
    pub space: u8,
}

/// A coordinate of a bar in a barcode.
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct BarCoordinate {
    /// The x coordinate, started from the left.
    ///
    /// The first bar will always be at 10, which is the
    /// offset for the left quiet zone.
    pub x: u32,
    /// The width of the bar.
    pub width: u8,
}

/// A Code 128.
///
/// You can use the bars iterators [`bars()`](Self::bars) or
/// [`bar_coordinates()`](Self::bar_coordinates), and the [size](Self::len)
/// to compute a visualization. A bars corresponds to a "black
/// line" of the code and has a unitless width between one and four, as well
/// as a free space after it, also sized between one and four.
///
/// A bar with width one is called a "module". You can say a bar consists of one
/// or more modules. The width of a module is often called "X", denoting one
/// unit in the "x-dimension" of the barcode. A multiple of a module's width is
/// usually written `<multiplier>X`, for example `1X` or `2.5X`.
///
/// ## Pseudo code for visualization
///
/// The standard demands a quiet zone of size 10X (see above for notation) on
/// the left and right side of the code. To compute the size of a bar, multiply
/// its width with the available space for the code divided by
/// the [code's length](Self::len).
///
/// ```rust
/// # use code128::Code128;
/// let code = Code128::encode(b"Code128 <3");
/// let available_space = 100.0; // unit is, say, "pt"
/// let line_width = available_space / code.len() as f64;
/// for bar in code.bar_coordinates() {
///      let x = bar.x as f64 * line_width;
///      let width = bar.width as f64 * line_width;
///      // print line at `x` pt, `width` pt wide
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

    /// Get the sequence of bars this Code 128 consists of.
    pub fn bars(&self) -> impl Iterator<Item = Bar> + '_ {
        self.indices
            .iter()
            .flat_map(|idx| encode::bits_to_bars(encode::PATTERNS[*idx as usize]))
    }

    /// Get the coordinates of the bars this Code 128 consists of.
    pub fn bar_coordinates(&self) -> impl Iterator<Item = BarCoordinate> + '_ {
        self.bars().scan(10, |pos, bar| {
            let x = *pos;
            *pos += bar.width as u32 + bar.space as u32;
            Some(BarCoordinate {
                x,
                width: bar.width,
            })
        })
    }

    /// Get the total width of the code in units of the [Bar](crate::Bar)
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
            Encoder::DynamicProgramming => encode::encode_as_indices_dp(data, Vec::new()),
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
fn test_bar_size() {
    for pattern in &encode::PATTERNS[0..107] {
        let size: u32 = encode::bits_to_bars(*pattern)
            .into_iter()
            .map(|m| m.width as u32 + m.space as u32)
            .sum();
        assert_eq!(size, 11);
    }

    let size: u32 = encode::bits_to_bars(encode::PATTERNS[STOP as usize])
        .into_iter()
        .map(|m| m.width as u32 + m.space as u32)
        .sum();
    assert_eq!(size, 13);
}

#[test]
fn test_code_size() {
    let code = Code128::encode(b"foo");
    let size = code
        .bars()
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
        let bars: Vec<Bar> = code.bars().collect();
        assert_eq!(decode(&bars), Ok(vec![x]));
    }
}

#[test]
fn test_string_encoding_example() {
    let code = Code128::encode_str("Füße").unwrap();
    let bars: Vec<_> = code.bars().collect();
    assert_eq!(decode_str(&bars), Ok("Füße".into()));
}

#[test]
fn test_bar_coordinates() {
    let code = Code128::encode(b"");
    let bars: Vec<_> = code.bar_coordinates().collect();
    assert_eq!(bars[0], BarCoordinate { x: 10, width: 2 });
    assert_eq!(bars[1], BarCoordinate { x: 13, width: 1 });
    assert_eq!(bars[2], BarCoordinate { x: 16, width: 1 });
    assert_eq!(bars[3], BarCoordinate { x: 21, width: 2 });
    assert_eq!(bars[4], BarCoordinate { x: 25, width: 2 });
    assert_eq!(bars[5], BarCoordinate { x: 28, width: 2 });
    assert_eq!(bars[6], BarCoordinate { x: 32, width: 2 });
    assert_eq!(bars[7], BarCoordinate { x: 37, width: 3 });
    assert_eq!(bars[8], BarCoordinate { x: 41, width: 1 });
    assert_eq!(bars[9], BarCoordinate { x: 43, width: 2 });
    assert_eq!(bars.len(), 10);
}
