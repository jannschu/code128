use std::io::Read;

use code128::Code128;

fn main() {
    let mut msg = Vec::new();
    if atty::isnt(atty::Stream::Stdin) {
        let mut stdin = std::io::stdin();
        stdin.read_to_end(&mut msg).unwrap();
    }
    let msg = String::from_utf8(msg).unwrap();
    let code = Code128::encode_str(&msg).unwrap();
    let string = code128::bars_to_blocks(code.bars());
    println!("{}", &string);
    println!("{}", &string);
    println!("{}", &string);
}
