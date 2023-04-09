use super::{Module, SHIFT_MODE, START_A, START_B, START_C, SWITCH_A, SWITCH_B, SWITCH_C};

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

fn encode_a(val: u8) -> Option<(bool, u8)> {
    match val {
        0..=0x1F => Some((false, val + 0x40)),
        b' '..=b'_' => Some((false, val - b' ')),
        0x80..=0x9F => Some((true, val - 128 + 0x40)),
        0xA0..=0xDF => Some((true, val - 128 - b' ')),
        _ => None,
    }
}

fn encode_b(val: u8) -> Option<(bool, u8)> {
    match val {
        b' '..=0x7F => Some((false, val - b' ')),
        0xA0..=0xFF => Some((true, val - 128 - b' ')),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Mode {
    A,
    B,
    C,
}

impl Mode {
    #[inline]
    fn switch(self) -> u8 {
        match self {
            Mode::A => SWITCH_A,
            Mode::B => SWITCH_B,
            Mode::C => SWITCH_C,
        }
    }

    #[inline]
    fn encode(self, val: u8) -> Option<(bool, u8)> {
        match self {
            Mode::A => encode_a(val),
            Mode::B => encode_b(val),
            Mode::C => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Encodation {
    mode: Mode,
    latin: bool,
    symbols: Vec<u8>,
}

impl Encodation {
    fn new(mode: Mode) -> Self {
        let start_pattern = match mode {
            Mode::A => START_A,
            Mode::B => START_B,
            Mode::C => START_C,
        };
        Self {
            mode,
            latin: false,
            symbols: vec![start_pattern],
        }
    }

    #[inline]
    fn switch(&mut self, mode: Mode) {
        self.symbols.push(mode.switch());
        self.mode = mode;
    }
}

impl Encodation {
    fn push<const N: usize>(mut self, symbols: [u8; N]) -> Self {
        for symbol in symbols {
            self.symbols.push(symbol);
        }
        self
    }
}

fn eat_double_digits<'a>(mut bytes: &'a [u8], encs: &mut [Encodation]) -> &'a [u8] {
    while let [c1 @ b'0'..=b'9', c2 @ b'0'..=b'9', ..] = bytes {
        for enc in encs.iter_mut() {
            enc.symbols.push((c1 - b'0') * 10 + (c2 - b'0'));
        }
        bytes = &bytes[2..];
    }
    bytes
}

fn c_start(mut bytes: &[u8]) -> Option<Encodation> {
    match bytes {
        [c1 @ b'0'..=b'9', c2 @ b'0'..=b'9'] => {
            Some(Encodation::new(Mode::C).push([(c1 - b'0') * 10 + (c2 - b'0')]))
        }
        [c1 @ b'0'..=b'9', c2 @ b'0'..=b'9', c3 @ b'0'..=b'9', c4 @ b'0'..=b'9', ..] => {
            let mut enc = Encodation::new(Mode::C).push([
                (c1 - b'0') * 10 + (c2 - b'0'),
                (c3 - b'0') * 10 + (c4 - b'0'),
            ]);
            bytes = &bytes[4..];
            while let [c1 @ b'0'..=b'9', c2 @ b'0'..=b'9'] = bytes {
                enc.symbols.push((c1 - b'0') * 10 + (c2 - b'0'));
                bytes = &bytes[2..];
            }
            Some(enc)
        }
        _ => None,
    }
}

pub(super) fn encode_as_indices(mut bytes: &[u8]) -> Vec<u8> {
    let mut head = Vec::new();

    let mut candidates = Vec::new();
    if let Some(c) = c_start(bytes) {
        let eaten = (c.symbols.len() - 1) * 2;
        candidates.push(c);
        bytes = &bytes[eaten..];
    } else {
        candidates.clear();
        candidates.push(Encodation::new(Mode::A));
        candidates.push(Encodation::new(Mode::B));
    }

    let mut new_candidates = Vec::new();

    macro_rules! enplace_new_candidates {
        () => {
            std::mem::swap(&mut candidates, &mut new_candidates);

            // remove hopeless cases
            candidates.sort_unstable_by_key(|c| c.symbols.len());
            for i in 0..candidates.len() {
                for j in ((i + 1)..candidates.len()).rev() {
                    let switch_cost = (candidates[i].mode != candidates[j].mode) as usize
                        + 2 * (candidates[i].latin != candidates[j].latin) as usize;
                    if candidates[i].symbols.len() + switch_cost <= candidates[j].symbols.len() {
                        candidates.pop();
                    }
                }
            }

            if candidates.len() == 1 {
                // to avoid copying the same symbol
                head.append(&mut candidates[0].symbols);
            }
        };
    }

    while !bytes.is_empty() {
        debug_assert!(new_candidates.is_empty());

        // check if a switch to C is necessary,
        // candidates contains either only candidates in mode C or only ones in A or B
        if candidates[0].mode != Mode::C {
            match *bytes {
                // for two digits at the end of data it is advantageous
                // to switch if a candidate is in latin mode
                [c1 @ b'0'..=b'9', c2 @ b'0'..=b'9'] => {
                    for enc in candidates.iter_mut() {
                        enc.switch(Mode::C);
                        enc.symbols.push((c1 - b'0') * 10 + (c2 - b'0'));
                    }
                    break;
                }
                // at least four digits are the break even point for switching to
                // C mode, and its advantageous for candidates in latin mode
                [c1 @ b'0'..=b'9', c2 @ b'0'..=b'9', c3 @ b'0'..=b'9', c4 @ b'0'..=b'9', ..] => {
                    let pair1 = (c1 - b'0') * 10 + (c2 - b'0');
                    let pair2 = (c3 - b'0') * 10 + (c4 - b'0');
                    for enc in candidates.iter_mut() {
                        enc.switch(Mode::C);
                        enc.symbols.push(pair1);
                        enc.symbols.push(pair2);
                    }
                    bytes = eat_double_digits(&bytes[4..], &mut candidates);
                    continue;
                }
                // Two digits are advantageous for candidates in
                // latin mode, but disadvantageous for the rest unless they are last.
                //
                // Because this branch is last we know that the next character(s)
                // can not be encoded in C mode.
                [c1 @ b'0'..=b'9', c2 @ b'0'..=b'9', ..] if candidates.iter().any(|c| c.latin) => {
                    let digits = (c1 - b'0') * 10 + (c2 - b'0');
                    for enc in candidates.drain(..) {
                        if enc.latin {
                            // switch to C, then back to A or B
                            let mut alt1 = enc.push([SWITCH_C, digits]);
                            let mut alt2 = alt1.clone();
                            alt1.switch(Mode::A);
                            alt2.switch(Mode::B);
                            new_candidates.push(alt1);
                            new_candidates.push(alt2);
                        } else {
                            // encode directly
                            new_candidates.push(enc.push([c1 - b' ', c2 - b' ']));
                        }
                    }
                    bytes = &bytes[2..];
                    enplace_new_candidates!();
                }
                _ => (),
            }
        }

        let byte = bytes[0];
        bytes = &bytes[1..];

        for mut candidate in candidates.drain(..) {
            // modes A and B are hanled symmetrically, we use a macro for these
            macro_rules! handle_ab {
                ($candidate:ident, $me:expr, ($latin:expr, $symbol:expr)) => {
                    if $candidate.latin == $latin {
                        new_candidates.push($candidate.push([$symbol]));
                    } else {
                        // shift latin
                        let opt1 = $candidate.clone();
                        new_candidates.push(opt1.push([$me.switch(), $symbol]));

                        // switch latin
                        let mut opt2 = $candidate;
                        opt2.latin = $latin;
                        new_candidates.push(opt2.push([$me.switch(), $me.switch(), $symbol]));
                    }
                };
                ($me:expr, $other:expr) => {
                    if let Some((latin, symbol)) = $me.encode(byte) {
                        // if this mode can encode the symbol there is no need to consider a switch
                        handle_ab!(candidate, $me, (latin, symbol));
                    } else if let Some((latin, symbol)) = $other.encode(byte) {
                        // other mode can encode, we can shift or switch
                        if candidate.latin == latin {
                            // shift mode
                            let opt1 = candidate.clone();
                            new_candidates.push(opt1.push([SHIFT_MODE, symbol]));

                            // switch mode
                            let mut opt2 = candidate;
                            opt2.switch($other);
                            new_candidates.push(opt2.push([symbol]));
                        } else {
                            // shift mode, shift latin
                            let opt1 = candidate.clone();
                            new_candidates.push(opt1.push([SHIFT_MODE, $other.switch(), symbol]));

                            // shift mode, switch latin
                            let mut opt2 = candidate.clone();
                            opt2.latin = latin;
                            new_candidates.push(opt2.push([
                                SHIFT_MODE,
                                $other.switch(),
                                $other.switch(),
                                symbol,
                            ]));

                            // switch mode, shift latin
                            let mut opt3 = candidate.clone();
                            opt3.switch($other);
                            new_candidates.push(opt3.push([$other.switch(), symbol]));

                            // switch mode, switch latin
                            let mut opt4 = candidate;
                            opt4.switch($other);
                            opt4.latin = latin;
                            new_candidates.push(opt4.push([
                                $other.switch(),
                                $other.switch(),
                                symbol,
                            ]));
                        }
                    }
                };
            }
            match candidate.mode {
                Mode::A => handle_ab!(Mode::A, Mode::B),
                Mode::B => handle_ab!(Mode::B, Mode::A),
                Mode::C => {
                    // C can no longer encode the symbol, we switch to A or B
                    match (encode_a(byte), encode_b(byte)) {
                        (Some((latin_a, symbol_a)), Some((latin_b, symbol_b))) => {
                            let mut opt1 = candidate.clone();
                            opt1.switch(Mode::A);
                            handle_ab!(opt1, Mode::A, (latin_a, symbol_a));
                            let mut opt2 = candidate;
                            opt2.switch(Mode::B);
                            handle_ab!(opt2, Mode::B, (latin_b, symbol_b));
                        }
                        (Some((latin, symbol)), None) => {
                            candidate.switch(Mode::A);
                            handle_ab!(candidate, Mode::A, (latin, symbol));
                        }
                        (None, Some((latin, symbol))) => {
                            candidate.switch(Mode::B);
                            handle_ab!(candidate, Mode::B, (latin, symbol));
                        }
                        (None, None) => unreachable!(),
                    }
                }
            }
        }

        enplace_new_candidates!();
    }

    let mut candidate = candidates
        .into_iter()
        .min_by_key(|c| c.symbols.len())
        .unwrap();
    head.append(&mut candidate.symbols);
    head
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
fn test_patterns() {
    for (i, pattern) in PATTERNS.iter().cloned().enumerate() {
        assert_eq!(crate::decode::lookup(pattern), Ok(i as u8));
    }
}

#[test]
fn test_switch_instead_of_shift() {
    assert_eq!(
        encode_as_indices(b"\nab"),
        vec![START_A, b'\n' + 0x40, SWITCH_B, b'a' - b' ', b'b' - b' '],
    )
}

#[test]
fn test_latin_shift() {
    assert_eq!(
        encode_as_indices(b"\x00\x80"),
        vec![START_A, b'\x00' + 0x40, SWITCH_A, 0x40],
    )
}

#[test]
fn test_latin_switch() {
    assert_eq!(
        encode_as_indices(b"\x00\x80\x81\x82"),
        vec![
            START_A,
            b'\x00' + 0x40,
            SWITCH_A,
            SWITCH_A,
            0x40,
            0x41,
            0x42
        ],
    )
}

#[test]
fn test_latin_switch_cross_c1() {
    assert_eq!(
        encode_as_indices(b"\x80\x80\x8000\x80"),
        vec![START_A, SWITCH_A, SWITCH_A, 0x40, 0x40, 0x40, SWITCH_C, 0, SWITCH_A, 0x40],
    )
}

#[test]
fn test_latin_switch_cross_c2() {
    assert_eq!(
        encode_as_indices(b"\x00\x8000000001\x80\x81\x82"),
        vec![
            START_A,
            b'\x00' + 0x40,
            SWITCH_A,
            SWITCH_A,
            0x40,
            SWITCH_C,
            0,
            0,
            0,
            1,
            SWITCH_A,
            0x40,
            0x41,
            0x42
        ],
    )
}

#[test]
fn test_latin_shift_back() {
    assert_eq!(
        encode_as_indices(b"|\xF0\xF1\xF21"),
        vec![
            START_B,
            b'|' - b' ',
            SWITCH_B,
            SWITCH_B,
            0xF0 - 128 - b' ',
            0xF1 - 128 - b' ',
            0xF2 - 128 - b' ',
            SWITCH_B,
            b'1' - b' ',
        ]
    )
}

#[test]
fn test_digit_start() {
    for msg in [b"000", b"00a"] {
        let indices = encode_as_indices(msg);
        assert_ne!(indices[0], START_C);
        assert_eq!(indices.len(), 4);
    }

    assert_eq!(encode_as_indices(b"99").len(), 2);
}
