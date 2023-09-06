pub fn run(s: &str) {
    let buf = match wast::parser::ParseBuffer::new(s) {
        Ok(b) => b,
        Err(_) => return,
    };
    drop(wast::parser::parse::<wast::Wast>(&buf));
}
