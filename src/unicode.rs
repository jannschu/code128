use crate::Module;

struct Buffer(String, Vec<bool>);

impl Buffer {
    fn new() -> Self {
        Self("     ".to_string(), Vec::with_capacity(2))
    }

    fn push(&mut self, module: bool) {
        self.1.push(module);
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

/// Create a string representation of the modules using Unicode block characters.
///
/// The quiet zones are included.
///
/// ## Example
/// ```
/// # use code128::{Code128, modules_to_blocks};
/// assert_eq!(
///     modules_to_blocks(Code128::encode(b"<3").modules()),
///     "     █▐ ▌ ▐█ █▐ █ ▌█▌▐  ▌█ █ ▐█▐▐▌     ",
/// );
/// ```
pub fn modules_to_blocks(modules: impl IntoIterator<Item = Module>) -> String {
    let mut buf = Buffer::new();
    for module in modules {
        for _ in 0..module.width {
            buf.push(true);
        }
        for _ in 0..module.space {
            buf.push(false);
        }
    }
    for _ in 0..10 {
        buf.push(false);
    }
    buf.into_string()
}
