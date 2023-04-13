use std::io::Read;

use code128::Code128;

fn main() {
    let mut msg = Vec::new();
    if atty::isnt(atty::Stream::Stdin) {
        let mut stdin = std::io::stdin();
        stdin.read_to_end(&mut msg).unwrap();
    }
    let code = Code128::encode(&msg);
    let mut svg = String::new();
    let height = 20;
    svg += &format!(
        r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {} {}"><path d="M10 0"#,
        code.len(),
        height,
    );
    for bar in code.bars() {
        svg += &format!(
            r#"h{0}v{1}h-{0}zm{2} 0"#,
            bar.width,
            height,
            bar.space + bar.width
        );
    }
    svg += r#""/></svg>"#;
    println!("{}", svg);
}
