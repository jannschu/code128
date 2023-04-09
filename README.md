# code128

[![crates.io](https://img.shields.io/crates/d/code128.svg)](https://crates.io/crates/code128)
[![Documentation](https://docs.rs/code128/badge.svg)](https://docs.rs/code128)
![License](https://img.shields.io/crates/l/code128)

This crate implements encoding and decoding of Code 128 barcodes as defined in
ISO/IEC 15417:2007.

<p align="center">
  <img src="src/code128.png" alt="Code 128 encoding 'This is a Code 128'">
</p>

To achieve a minimal encoding size a dynamic programming approach is used. The
full 256 bit range can be encoded. For compatibility it is recommended to stay
within printable ASCII though.

The implementation goals in order of importance are

- correctness,
- optimal size,
- performance,
- and "natural" encoding choices.