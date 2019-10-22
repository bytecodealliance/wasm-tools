pub fn wabt_may_disagree_on_binary(wast: &str) -> bool {
    for token in wast::lexer::Lexer::new(wast) {
        let t = match token.unwrap() {
            wast::lexer::Source::Token(t) => t,
            _ => continue,
        };
        match t {
            wast::lexer::Token::Keyword(k) => {
                // Ignore `(module binary ...)` modules since wabt postprocesses
                // those but we don't
                if k == "binary" {
                    return true;
                }

                // Wabt likes to drop empty `else` blocks but we pass them
                // through as-is without trying to collapse empty ones. Let's
                // just not fuzz these programs against wabt.
                if k == "else" {
                    return true;
                }
            }

            _ => {}
        }
    }
    false
}

pub fn remove_name_section(bytes: &[u8]) -> Vec<u8> {
    use wasmparser::*;

    if let Ok(mut r) = ModuleReader::new(bytes) {
        loop {
            let start = r.current_position();
            if let Ok(s) = r.read() {
                match s.code {
                    SectionCode::Custom { name: "name", .. } => {
                        let mut bytes = bytes.to_vec();
                        bytes.drain(start..s.range().end);
                        return bytes;
                    }
                    _ => {}
                }
            } else {
                break;
            }
        }
    }
    return bytes.to_vec();
}

