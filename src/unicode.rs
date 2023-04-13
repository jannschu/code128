use crate::std::string::{String, ToString};
use crate::std::vec::Vec;

use crate::Bar;

struct Buffer(String, Vec<bool>);

impl Buffer {
    fn new() -> Self {
        Self("     ".to_string(), Vec::with_capacity(2))
    }

    fn push(&mut self, bar: bool) {
        self.1.push(bar);
        if self.1.len() == 2 {
            match (self.1[0], self.1[1]) {
                (true, true) => self.0.push('█'),
                (true, false) => self.0.push('▌'),
                (false, true) => self.0.push('▐'),
                (false, false) => self.0.push(' '),
            }
            self.1.clear();
        }
    }

    fn into_string(mut self) -> String {
        if !self.1.is_empty() {
            self.push(false);
        }
        self.0
    }
}

/// Create a string representation of the bars using Unicode block characters.
///
/// The quiet zones are included.
///
/// ## Example
/// ```
/// # use code128::{Code128, bars_to_blocks};
/// assert_eq!(
///     bars_to_blocks(Code128::encode(b"<3").bars()),
///     "     █▐ ▌ ▐█ █▐ █ ▌█▌▐  ▌█ █ ▐█▐▐▌     ",
/// );
/// ```
pub fn bars_to_blocks(bars: impl IntoIterator<Item = Bar>) -> String {
    let mut buf = Buffer::new();
    for bar in bars {
        for _ in 0..bar.width {
            buf.push(true);
        }
        for _ in 0..bar.space {
            buf.push(false);
        }
    }
    for _ in 0..10 {
        buf.push(false);
    }
    buf.into_string()
}
