use crate::std::string::String;
use crate::std::vec;
use crate::std::vec::Vec;

#[cfg(feature = "std")]
use thiserror::Error;

use crate::Module;

/// Errors that can occur during decoding.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "std", derive(Error))]
pub enum DecodingError {
    /// A sequence of modules resulted in an unknown pattern.
    #[cfg_attr(feature = "std", error("pattern {0:b} not recognized"))]
    Pattern(u16),
    /// A module's width or spacing is not valid.
    #[cfg_attr(feature = "std", error("modules are not valid"))]
    InvalidModules,
    /// The stop code at the end is wrong.
    #[cfg_attr(feature = "std", error("wrong stop code"))]
    WrongStop,
    /// The input was too short.
    #[cfg_attr(feature = "std", error("code too short to be valid"))]
    Short,
    /// The code's length can not fit an allowed sequence of modules.
    #[cfg_attr(feature = "std", error("length not correct"))]
    Length,
    /// The checksum did not match.
    #[cfg_attr(feature = "std", error("checksum mismatch"))]
    Checksum,
    /// The code did not start with a mode signal.
    #[cfg_attr(feature = "std", error("start char did not signal mode"))]
    NoMode,
    /// A symbol occurred in a mode that did not support it or is not implemented.
    #[cfg_attr(feature = "std", error("unexpected character {0:x}"))]
    Unexpected(u8),
    /// After decoding the data the conversion from Latin 1 failed.
    #[cfg_attr(
        feature = "std",
        error("characters not covered by Latin 1 were part of this symbol")
    )]
    Latin1,
}

pub(crate) fn lookup(pattern: u16) -> Result<u8, DecodingError> {
    const PATTERN_REVERSE: [u16; 109] = [
        0x426, 0x42c, 0x432, 0x434, 0x446, 0x44c, 0x458, 0x45e, 0x462, 0x464, 0x468, 0x46e, 0x476,
        0x47a, 0x486, 0x48c, 0x498, 0x49e, 0x4b0, 0x4bc, 0x4c2, 0x4c4, 0x4c8, 0x4ce, 0x4d0, 0x4dc,
        0x4e6, 0x4ec, 0x4f2, 0x4f4, 0x50c, 0x518, 0x51e, 0x530, 0x53c, 0x578, 0x584, 0x588, 0x58e,
        0x590, 0x59c, 0x5b8, 0x5c6, 0x5cc, 0x5d8, 0x5de, 0x5e2, 0x5e4, 0x5e8, 0x5ee, 0x612, 0x614,
        0x622, 0x624, 0x628, 0x62e, 0x636, 0x63a, 0x642, 0x644, 0x648, 0x64e, 0x650, 0x65c, 0x666,
        0x66c, 0x672, 0x674, 0x684, 0x688, 0x68e, 0x690, 0x69c, 0x6b8, 0x6c6, 0x6cc, 0x6d8, 0x6de,
        0x6e2, 0x6e4, 0x6e8, 0x6ee, 0x6f6, 0x716, 0x71a, 0x726, 0x72c, 0x732, 0x734, 0x746, 0x74c,
        0x758, 0x75e, 0x762, 0x764, 0x768, 0x76e, 0x776, 0x77a, 0x78a, 0x792, 0x794, 0x7a2, 0x7a4,
        0x7a8, 0x7ae, 0x7b6, 0x7ba, 0x18eb,
    ];
    const PATTERN_INDEX: [u8; 109] = [
        0x44, 0x43, 0x4a, 0x49, 0x23, 0x05, 0x22, 0x5e, 0x26, 0x08, 0x25, 0x2c, 0x2f, 0x4f, 0x42,
        0x04, 0x03, 0x52, 0x41, 0x51, 0x48, 0x07, 0x06, 0x0e, 0x47, 0x0d, 0x11, 0x10, 0x55, 0x54,
        0x40, 0x21, 0x5d, 0x3f, 0x50, 0x5c, 0x46, 0x24, 0x2b, 0x45, 0x0c, 0x2a, 0x2e, 0x0f, 0x2d,
        0x63, 0x60, 0x53, 0x5f, 0x64, 0x4b, 0x4e, 0x29, 0x0b, 0x28, 0x32, 0x20, 0x6a, 0x3d, 0x0a,
        0x09, 0x14, 0x4c, 0x13, 0x02, 0x01, 0x12, 0x16, 0x67, 0x27, 0x31, 0x68, 0x69, 0x6b, 0x1f,
        0x00, 0x1e, 0x59, 0x34, 0x15, 0x33, 0x35, 0x5a, 0x38, 0x3b, 0x1a, 0x19, 0x1d, 0x1c, 0x37,
        0x18, 0x36, 0x65, 0x3a, 0x1b, 0x39, 0x17, 0x30, 0x3c, 0x3e, 0x58, 0x57, 0x62, 0x56, 0x61,
        0x66, 0x5b, 0x4d, 0x6c,
    ];
    PATTERN_REVERSE
        .binary_search(&pattern)
        .map_err(|_| DecodingError::Pattern(pattern))
        .map(|i| PATTERN_INDEX[i])
}

fn modules_to_pattern(modules: &[Module]) -> Result<u16, DecodingError> {
    if modules.is_empty() {
        return Ok(0);
    }
    let mut result = 0;
    for module in modules {
        match module.width {
            1 => result = (result << 1) | 0b1,
            2 => result = (result << 2) | 0b11,
            3 => result = (result << 3) | 0b111,
            4 => result = (result << 4) | 0b1111,
            _ => return Err(DecodingError::InvalidModules),
        }
        result <<= module.space;
    }
    Ok(result)
}

fn decode_codes(modules: &[Module]) -> Result<Vec<u8>, DecodingError> {
    if modules.len() < 3 + 3 + 4 {
        return Err(DecodingError::Short);
    }
    let (init, stop) = modules.split_at(modules.len() - 4);

    if modules_to_pattern(stop)? != crate::encode::PATTERNS[crate::STOP as usize] {
        return Err(DecodingError::WrongStop);
    }

    let (data, checksum) = init.split_at(init.len() - 3);

    if data.len() % 3 != 0 {
        return Err(DecodingError::Length);
    }

    let checksum = lookup(modules_to_pattern(checksum)?)?;
    let result = data
        .chunks_exact(3)
        .map(|chunk| lookup(modules_to_pattern(chunk)?))
        .collect::<Result<Vec<u8>, _>>()?;
    let computed_checksum = crate::checksum(result.iter().cloned());
    if checksum != computed_checksum {
        return Err(DecodingError::Checksum);
    }
    Ok(result)
}

fn decode_a(ch: u8) -> Result<u8, DecodingError> {
    match ch {
        0..=0x3F => Ok(ch + b' '),
        0x40..=0x5F => Ok(ch - 0x40),
        _ => Err(DecodingError::Unexpected(ch)),
    }
}

fn decode_b(ch: u8) -> Result<u8, DecodingError> {
    match ch {
        0..=0x5F => Ok(ch + b' '),
        _ => Err(DecodingError::Unexpected(ch)),
    }
}

/// Decode a sequence of modules.
pub fn decode(modules: &[Module]) -> Result<Vec<u8>, DecodingError> {
    #[derive(Clone, Copy, Debug)]
    enum Mode {
        A,
        B,
        C,
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    enum Latin {
        Permanent(bool),
        Once(bool),
    }

    let codes = decode_codes(modules)?;
    let mut codes = codes.iter().cloned().peekable();
    let mut mode = match codes.next() {
        Some(crate::START_A) => Mode::A,
        Some(crate::START_B) => Mode::B,
        Some(crate::START_C) => Mode::C,
        _ => return Err(DecodingError::NoMode),
    };
    if codes.peek().is_none() {
        return Ok(vec![]);
    }
    let mut latin = Latin::Permanent(false);
    let mut data = Vec::with_capacity(codes.len() - 1);
    let mut switch_back = None;
    loop {
        let Some(ch) = codes.next() else {
            break;
        };
        match mode {
            Mode::A => match ch {
                crate::SHIFT_MODE => {
                    mode = Mode::B;
                    switch_back = Some(Mode::A);
                    continue;
                }
                crate::SWITCH_A => {
                    match latin {
                        Latin::Permanent(x) if codes.peek() == Some(&crate::SWITCH_A) => {
                            let _ = codes.next();
                            latin = Latin::Permanent(!x);
                        }
                        Latin::Permanent(x) => latin = Latin::Once(x),
                        _ => unreachable!(),
                    }
                    continue;
                }
                crate::SWITCH_B => {
                    mode = Mode::B;
                    switch_back = None;
                    continue;
                }
                crate::SWITCH_C => {
                    mode = Mode::C;
                    switch_back = None;
                    continue;
                }
                ch => data.push(decode_a(ch)?),
            },
            Mode::B => match ch {
                crate::SHIFT_MODE => {
                    mode = Mode::A;
                    switch_back = Some(Mode::B);
                    continue;
                }
                crate::SWITCH_A => {
                    mode = Mode::A;
                    switch_back = None;
                    continue;
                }
                crate::SWITCH_B => {
                    match latin {
                        Latin::Permanent(x) if codes.peek() == Some(&crate::SWITCH_B) => {
                            let _ = codes.next();
                            latin = Latin::Permanent(!x);
                        }
                        Latin::Permanent(x) => latin = Latin::Once(x),
                        _ => unreachable!(),
                    }
                    continue;
                }
                crate::SWITCH_C => {
                    mode = Mode::C;
                    switch_back = None;
                    continue;
                }
                ch => data.push(decode_b(ch)?),
            },
            Mode::C => {
                match ch {
                    0..=99 => {
                        data.push(ch / 10 + b'0');
                        data.push(ch % 10 + b'0');
                    }
                    crate::SWITCH_A => {
                        mode = Mode::A;
                        switch_back = None;
                    }
                    crate::SWITCH_B => {
                        mode = Mode::B;
                        switch_back = None;
                    }
                    _ => return Err(DecodingError::Unexpected(ch)),
                }
                continue;
            }
        }
        match latin {
            Latin::Permanent(true) => *data.last_mut().unwrap() += 128,
            Latin::Permanent(false) => (),
            Latin::Once(before) => {
                if !before {
                    *data.last_mut().unwrap() += 128;
                }
                latin = Latin::Permanent(before);
            }
        }
        if let Some(new_mode) = switch_back.take() {
            mode = new_mode;
        }
    }
    Ok(data)
}

/// Decode the modules and interpret the data as Latin 1.
///
/// The control characters of ASCII, `0x00` to `0x19`, are also decoded.
pub fn decode_str(modules: &[Module]) -> Result<String, DecodingError> {
    let data = decode(modules)?;
    crate::latin1::latin1_to_utf8(&data).ok_or(DecodingError::Latin1)
}

#[test]
fn test_modules_to_pattern() {
    assert_eq!(
        modules_to_pattern(&[Module { width: 2, space: 0 }]),
        Ok(0b11),
    );
    assert_eq!(
        modules_to_pattern(&[
            Module { width: 2, space: 1 },
            Module { width: 1, space: 2 },
            Module { width: 3, space: 2 },
        ]),
        Ok(0b11010011100),
    );

    for pattern in crate::encode::PATTERNS {
        let modules = crate::encode::bits_to_modules(pattern);
        assert_eq!(modules_to_pattern(&modules), Ok(pattern));
    }
}

#[test]
fn test_hello_world() {
    let msg = b"HELLO\n123456w0r1\rd";
    let modules: Vec<Module> = super::Code128::encode(msg).modules().collect();
    assert_eq!(decode(&modules), Ok(msg.as_slice().into()));
}

#[test]
fn test_latin() {
    let messages: Vec<&[u8]> = vec![
        b"\x00\x80",
        b"\x00\x80\x81\x82",
        b"\x00\x80000000\x80\x81\x82",
        b"|\xF0\xF1\xF21",
    ];
    for msg in messages {
        let modules: Vec<Module> = super::Code128::encode(msg).modules().collect();
        assert_eq!(decode(&modules), Ok(msg.into()));
    }
}

#[test]
fn test_empty() {
    let msg = b"";
    let modules: Vec<Module> = super::Code128::encode(msg).modules().collect();
    assert_eq!(decode(&modules), Ok(msg.as_slice().into()));
}

#[test]
fn test_case1() {
    let msg = &[10, 246];
    let code = super::Code128::encode(msg);
    let modules: Vec<Module> = code.modules().collect();
    assert_eq!(decode(&modules), Ok(msg.as_slice().into()));
}

#[test]
fn test_case2() {
    let msg = &[255, 145, 246, 246];
    let code = super::Code128::encode(msg);
    let modules: Vec<Module> = code.modules().collect();
    assert_eq!(decode(&modules), Ok(msg.as_slice().into()));
}
