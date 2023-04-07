use super::{Module, SHIFT, START_A, START_B, START_C, STOP, SWITCH_A, SWITCH_B, SWITCH_C};

pub(crate) const PATTERNS: [u16; 109] = [
    0x6cc, 0x66c, 0x666, 0x498, 0x48c, 0x44c, 0x4c8, 0x4c4, 0x464, 0x648, 0x644, 0x624, 0x59c,
    0x4dc, 0x4ce, 0x5cc, 0x4ec, 0x4e6, 0x672, 0x65c, 0x64e, 0x6e4, 0x674, 0x76e, 0x74c, 0x72c,
    0x726, 0x764, 0x734, 0x732, 0x6d8, 0x6c6, 0x636, 0x518, 0x458, 0x446, 0x588, 0x468, 0x462,
    0x688, 0x628, 0x622, 0x5b8, 0x58e, 0x46e, 0x5d8, 0x5c6, 0x476, 0x776, 0x68e, 0x62e, 0x6e8,
    0x6e2, 0x6ee, 0x758, 0x746, 0x716, 0x768, 0x762, 0x71a, 0x77a, 0x642, 0x78a, 0x530, 0x50c,
    0x4b0, 0x486, 0x42c, 0x426, 0x590, 0x584, 0x4d0, 0x4c2, 0x434, 0x432, 0x612, 0x650, 0x7ba,
    0x614, 0x47a, 0x53c, 0x4bc, 0x49e, 0x5e4, 0x4f4, 0x4f2, 0x7a4, 0x794, 0x792, 0x6de, 0x6f6,
    0x7b6, 0x578, 0x51e, 0x45e, 0x5e8, 0x5e2, 0x7a8, 0x7a2, 0x5de, 0x5ee, 0x75e, 0x7ae, 0x684,
    0x690, 0x69c, 0x63a, 0x6b8, 0x18eb,
];

fn encode_a(val: u8) -> Option<u8> {
    match val {
        b' '..=b'_' => Some(val - b' '),
        0..=0x1F => Some(val + 0x40),
        _ => None,
    }
}

fn encode_b(val: u8) -> Option<u8> {
    match val {
        b' '..=0x7F => Some(val - b' '),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Encodation {
    patterns: Vec<u8>,
}

impl Encodation {
    fn cost(&self) -> usize {
        self.patterns.len()
    }

    fn push(mut self, i: u8) -> Self {
        self.patterns.push(i);
        self
    }

    fn push2(mut self, i1: u8, i2: u8) -> Self {
        self.patterns.push(i1);
        self.patterns.push(i2);
        self
    }

    fn checksum(self) -> Self {
        let weighted_sum = self
            .patterns
            .iter()
            .enumerate()
            .map(|(i, idx)| (i.max(1) as u64) * *idx as u64)
            .sum::<u64>();
        self.push((weighted_sum % 103) as u8)
    }
}

fn add_c_switch_maybe(bytes: &mut &[u8], enc: &mut Encodation) -> bool {
    match *bytes {
        [c1 @ b'0'..=b'9', c2 @ b'0'..=b'9', c3 @ b'0'..=b'9', c4 @ b'0'..=b'9'] => {
            enc.patterns.push(SWITCH_C);
            enc.patterns.push((c1 - b'0') * 10 + (c2 - b'0'));
            enc.patterns.push((c3 - b'0') * 10 + (c4 - b'0'));
            *bytes = &[];
            true
        }
        [c1 @ b'0'..=b'9', c2 @ b'0'..=b'9', c3 @ b'0'..=b'9', c4 @ b'0'..=b'9', c5 @ b'0'..=b'9', c6 @ b'0'..=b'9', ..] =>
        {
            enc.patterns.push(SWITCH_C);
            enc.patterns.push((c1 - b'0') * 10 + (c2 - b'0'));
            enc.patterns.push((c3 - b'0') * 10 + (c4 - b'0'));
            enc.patterns.push((c5 - b'0') * 10 + (c6 - b'0'));
            *bytes = &bytes[6..];
            true
        }
        _ => false,
    }
}

pub(super) fn encode_as_indices(mut bytes: &[u8]) -> Option<Vec<u8>> {
    let mut best_a = Some(Encodation {
        patterns: vec![START_A],
    });
    let mut best_b = Some(Encodation {
        patterns: vec![START_B],
    });
    let mut best_c = Some(Encodation {
        patterns: vec![START_C],
    });

    while !bytes.is_empty() && (best_a.is_some() || best_b.is_some() || best_c.is_some()) {
        if let Some(code) = best_c.take() {
            if matches!(bytes, [b'0'..=b'9', b'0'..=b'9', ..]) {
                best_a = None;
                best_b = None;
                best_c = Some(code.push((bytes[0] - b'0') * 10 + (bytes[1] - b'0')));
                bytes = &bytes[2..];
                continue;
            } else if best_a.is_none() && best_b.is_none() {
                let byte = bytes[0];
                match (encode_a(byte), encode_b(byte)) {
                    (Some(a), Some(b)) => {
                        best_a = Some(code.clone().push2(SWITCH_A, a));
                        best_b = Some(code.push2(SWITCH_B, b));
                    }
                    (Some(a), None) => {
                        best_a = Some(code.push2(SWITCH_A, a));
                    }
                    (None, Some(b)) => {
                        best_b = Some(code.push2(SWITCH_B, b));
                    }
                    (None, None) => return None,
                }
                bytes = &bytes[1..];
                continue;
            } else {
                best_c = None;
            }
        }
        let byte = bytes[0];
        let mut b = best_b.take();
        if let Some(mut code) = best_a.take() {
            if add_c_switch_maybe(&mut bytes, &mut code) {
                debug_assert_eq!(best_c, None);
                best_c = Some(code);
                best_b = None;
                continue;
            }
            if let Some(pattern) = encode_a(byte) {
                best_a = Some(code.push(pattern));
            } else if let Some(pattern) = encode_b(byte) {
                if b.is_none() {
                    best_b = Some(code.clone().push2(SWITCH_B, pattern));
                }
                best_a = Some(code.push2(SHIFT, pattern));
            }
        }
        if let Some(mut code) = b.take() {
            if add_c_switch_maybe(&mut bytes, &mut code) {
                debug_assert_eq!(best_c, None);
                best_c = Some(code);
                best_a = None;
                continue;
            }
            if let Some(pattern) = encode_b(byte) {
                best_b = Some(code.push(pattern));
            } else if let Some(pattern) = encode_a(byte) {
                if best_a.is_none() {
                    best_a = Some(code.clone().push2(SWITCH_A, pattern));
                }
                best_b = Some(code.push2(SHIFT, pattern));
            }
        }

        match (
            best_a.as_ref().map(Encodation::cost),
            best_b.as_ref().map(Encodation::cost),
        ) {
            (Some(a), Some(b)) if a < b => best_b = None,
            (Some(a), Some(b)) if a > b => best_a = None,
            _ => (),
        }

        bytes = &bytes[1..];
    }

    best_c
        .or(best_a)
        .or(best_b)
        .map(|c| c.checksum().push(STOP).patterns)
}

pub(super) fn bits_to_modules(mut bits: u16) -> Vec<Module> {
    let mut modules = Vec::with_capacity(3);
    while bits != 0 {
        let mut width = 0;
        let zeroes = bits.leading_zeros();
        for i in (0..(16 - zeroes)).rev() {
            if bits & (1 << i) != 0 {
                width += 1;
                bits ^= 1 << i;
            } else {
                break;
            }
        }
        modules.push(Module {
            width: width as u8,
            space: (bits.leading_zeros() - width - zeroes) as u8,
        });
    }
    modules
}

#[test]
fn test_bits_to_modules() {
    assert_eq!(bits_to_modules(0), vec![]);
    let modules = bits_to_modules(0b10000011010);
    assert_eq!(
        modules,
        vec![
            Module { width: 1, space: 5 },
            Module { width: 2, space: 1 },
            Module { width: 1, space: 1 },
        ]
    );
    let modules = bits_to_modules(0b1100011101011);
    assert_eq!(
        modules,
        vec![
            Module { width: 2, space: 3 },
            Module { width: 3, space: 1 },
            Module { width: 1, space: 1 },
            Module { width: 2, space: 0 },
        ]
    );
}

#[test]
fn test_reverse() {
    for (i, pattern) in PATTERNS.iter().cloned().enumerate() {
        assert_eq!(crate::decode::lookup(pattern), Ok(i as u8));
    }
}

#[test]
fn test_switch_instead_of_shift() {
    let msg = b"\nab";
    let mut indices = encode_as_indices(msg).unwrap();
    indices.pop();
    indices.pop();
    assert_eq!(
        indices,
        vec![START_A, b'\n' + 0x40, SWITCH_B, b'a' - b' ', b'b' - b' ']
    )
}
