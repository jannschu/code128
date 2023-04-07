use std::io::Read;

use code128::Code128;

fn main() {
    let mut msg = Vec::new();
    if atty::isnt(atty::Stream::Stdin) {
        let mut stdin = std::io::stdin();
        stdin.read_to_end(&mut msg).unwrap();
    }
    let code = Code128::encode(&msg).unwrap();
    let string = code128::modules_to_blocks(code.modules());
    println!("{}", &string);
    println!("{}", &string);
    println!("{}", &string);
}
