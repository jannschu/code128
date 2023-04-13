# code128

[![crates.io](https://img.shields.io/crates/d/code128.svg)](https://crates.io/crates/code128)
[![Documentation](https://docs.rs/code128/badge.svg)](https://docs.rs/code128)
![License](https://img.shields.io/crates/l/code128)

This crate implements encoding and decoding of Code 128 barcodes as defined in
ISO/IEC 15417:2007.

<p align="center">
  <img src="src/code128.png" alt="Code 128 encoding 'This is a Code 128'">
</p>

To achieve a minimal encoding size a dynamic programming approach is used. The
full 256 bit range can be encoded. For best compatibility it is recommended to
stay within printable ASCII though.

The implementation goals in order of importance are

- correctness,
- optimal size,
- performance,
- and "natural" encoding choices.

## Implementation

The first encoder that was implemented is using dynamic programming. With Code
128 having three character sets that can be switched during encoding, the basic
idea is to incrementally compute for every charset the best possible encodation
that ends in that charset. The details are a bit intricate but the result will
be an optimal encodation.

This dynamic programming based encoder was fuzzed using the astonishing
[cargo fuzz](https://github.com/rust-fuzz/cargo-fuzz) and
[LibFuzzer](https://llvm.org/docs/LibFuzzer.html) project. During fuzzing the
input was encoded and then decoded, which was checked to give us the input back.
This uncovered a few bugs.

For further verification the encoder as specified in ISO/IEC 15417 was
implemented. Roughly speaking this encoder decides based on rules on the
remaining data which character set should be chosen. These rules lead to a good
but not optimal encoding size if input from the upper byte range (`0x80` to
`0xFF`) shall be encoded. The rules were improved but that also made them
harder to check and understand, as well as slower. Here again fuzzing was used to
compare the rule based encoder with the dynamic programming encoder, which again
uncovered bugs in booth encoders and led to many test cases.

To avoid an overcomplicated implementation the current encoder now uses the rule
based encoder until an input from the upper byte range is seen at which point
we switch to the dynamic programming encoder. For inputs from `0x00` to `0x7F`
this will give us the low memory footprint and good performance of the rule
based encoder.