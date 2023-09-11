use std::str;

pub fn run(string: &str) {
    write_file("wasm1.wat", &string);
    // Weed out `(module binary ...)` because when we print the bytes and
    // convert it back to binary it's not guaranteed to be exactly the same.
    // (think of something like an over-long LEB encoding)
    if string.contains("binary") {
        return;
    }

    // Also weed out `@custom` custom sections since we don't print those right
    // now.
    if string.contains("@custom") {
        return;
    }
    let wasm = match wat::parse_str(string) {
        Ok(bytes) => bytes,
        Err(_) => return,
    };
    write_file("wasm1.wasm", &wasm);

    // Only roundtrip valid modules for now since invalid modules can often have
    // bizarre structures which aren't intended to print correctly or roundtrip
    // well.
    if wasmparser::validate(&wasm).is_err() {
        return;
    }

    // And finally validate that the name section, if present, is valid. This
    // can be invalid if names in the name section are too long (e.g. exceeding
    // the maximum length of a string). The printing process will skip invalid
    // name sections, so if it's invalid then our roundtrip'd bytes will
    // trivially not match, but not in an interesting way.
    if validate_name_section(&wasm).is_err() {
        return;
    }
    let string2 = match wasmprinter::print_bytes(&wasm) {
        Ok(s) => s,
        Err(_) => return,
    };
    write_file("wasm2.wat", &string2);

    let wasm2 = wat::parse_str(&string2).unwrap();
    write_file("wasm2.wasm", &wasm2);
    if wasm == wasm2 {
        return;
    }

    panic!("wasm bytes differ on roundtrip");
}

fn write_file(path: &str, contents: impl AsRef<[u8]>) {
    if !log::log_enabled!(log::Level::Debug) {
        return;
    }
    log::debug!("writing file {path}");
    std::fs::write(path, contents.as_ref()).unwrap();
}

fn validate_name_section(wasm: &[u8]) -> wasmparser::Result<()> {
    use wasmparser::*;
    for payload in Parser::new(0).parse_all(wasm) {
        let reader = match payload? {
            Payload::CustomSection(c) if c.name() == "name" => {
                NameSectionReader::new(c.data(), c.data_offset())
            }
            _ => continue,
        };
        for section in reader {
            match section? {
                Name::Module { .. } => {}
                Name::Function(n)
                | Name::Type(n)
                | Name::Table(n)
                | Name::Memory(n)
                | Name::Global(n)
                | Name::Element(n)
                | Name::Data(n) => {
                    for name in n {
                        name?;
                    }
                }
                Name::Local(n) | Name::Label(n) => {
                    for name in n {
                        for name in name?.names {
                            name?;
                        }
                    }
                }
                Name::Unknown { .. } => {}
            }
        }
    }
    Ok(())
}
