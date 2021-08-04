#![no_main]

use libfuzzer_sys::*;
use std::str;

fuzz_target!(|data: &[u8]| {
    let string = match str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };
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

    let wasm2 = wat::parse_str(&string2).unwrap();
    if wasm == wasm2 {
        return;
    }

    std::fs::write("wasm1.wasm", &wasm).unwrap();
    std::fs::write("wasm1.wat", &string).unwrap();
    std::fs::write("wasm2.wasm", &wasm2).unwrap();
    std::fs::write("wasm2.wat", &string2).unwrap();
    panic!("wasm bytes differ on roundtrip");
});

fn validate_name_section(wasm: &[u8]) -> wasmparser::Result<()> {
    use wasmparser::*;
    for payload in Parser::new(0).parse_all(wasm) {
        let reader = match payload? {
            Payload::CustomSection {
                name: "name",
                data_offset,
                data,
                range: _,
            } => NameSectionReader::new(data, data_offset)?,
            _ => continue,
        };
        for section in reader {
            match section? {
                Name::Module(n) => {
                    n.get_name()?;
                }
                Name::Function(n)
                | Name::Type(n)
                | Name::Table(n)
                | Name::Memory(n)
                | Name::Global(n)
                | Name::Element(n)
                | Name::Data(n) => {
                    let mut map = n.get_map()?;
                    for _ in 0..map.get_count() {
                        map.read()?;
                    }
                }
                Name::Local(n) | Name::Label(n) => {
                    let mut reader = n.get_indirect_map()?;
                    for _ in 0..reader.get_indirect_count() {
                        let local_name = reader.read()?;
                        let mut map = local_name.get_map()?;
                        for _ in 0..map.get_count() {
                            map.read()?;
                        }
                    }
                }
                Name::Unknown { .. } => {}
            }
        }
    }
    Ok(())
}
