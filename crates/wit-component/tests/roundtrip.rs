//! Testing the round tripping of interfaces to component encodings and back.

use anyhow::Result;
use std::fs;
use wit_component::{decode_interface_component, encode_interface_component, InterfacePrinter};

#[test]
fn roundtrip_interfaces() -> Result<()> {
    for file in fs::read_dir("tests/wit")? {
        let file = file?;
        let bytes = encode_interface_component("test", file.path())?;
        let interface = decode_interface_component(&bytes)?;

        let mut printer = InterfacePrinter::default();
        let output = printer.print(&interface)?;

        if std::env::var_os("BLESS").is_some() {
            fs::write(file.path(), output)?;
        } else {
            assert_eq!(
                output,
                fs::read_to_string(file.path())?.replace("\r\n", "\n")
            );
        }
    }

    Ok(())
}
