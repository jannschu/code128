mod decode;
mod encode;
mod unicode;

pub use decode::DecodingError;

pub use unicode::modules_to_blocks;

const SHIFT: u8 = 98;
const SWITCH_C: u8 = 99;
const SWITCH_B: u8 = 100;
const SWITCH_A: u8 = 101;
const START_A: u8 = 103;
const START_B: u8 = 104;
const START_C: u8 = 105;
const STOP: u8 = 108;

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Module {
    pub width: u8,
    pub space: u8,
}

pub struct Code128 {
    indices: Vec<u8>,
}

impl Code128 {
    pub fn encode(data: &[u8]) -> Option<Self> {
        let indices = encode::encode_as_indices(data)?;
        Some(Self { indices })
    }

    pub fn decode(modules: &[Module]) -> Result<Vec<u8>, DecodingError> {
        decode::decode(modules)
    }

    pub fn modules(&self) -> impl Iterator<Item = Module> + '_ {
        self.indices
            .iter()
            .flat_map(|idx| encode::bits_to_modules(encode::PATTERNS[*idx as usize]))
    }

    pub fn len(&self) -> usize {
        self.indices.len() * 11 + 2
    }

    pub fn is_empty(&self) -> bool {
        self.indices.len() == 3
    }
}
