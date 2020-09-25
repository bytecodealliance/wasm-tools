use std::fs::{read, write};
use std::path::PathBuf;
use std::process::Command;
use tempfile::NamedTempFile;
use wasmemit::*;

fn check(bytes: Vec<u8>) -> anyhow::Result<()> {
    let expected = wasmprinter::print_bytes(&bytes)?;
    let mut module = AstReader::new().read_all(&bytes)?.0;
    let bytes = module.encode()?;
    let actual = wasmprinter::print_bytes(&bytes)?;
    if actual == expected {
        return Ok(());
    }

    let file1 = NamedTempFile::new()?;
    write(file1.path(), actual)?;
    let file2 = NamedTempFile::new()?;
    write(file2.path(), expected)?;
    let output = Command::new("diff")
        .arg(file1.path())
        .arg(file2.path())
        .output()?;
    let diff = std::str::from_utf8(&output.stdout)?;
    anyhow::bail!("failed:\n{}", diff);
}

const TEST_BASE: &str = "../../tests/testsuite";

#[test]
fn test_emit_testsuite() -> anyhow::Result<()> {
    for f in PathBuf::from(TEST_BASE).read_dir().unwrap() {
        let f = f.unwrap();
        if f.file_type().unwrap().is_dir() {
            continue;
        }

        match f.path().extension().and_then(|s| s.to_str()) {
            Some("wast") => {
                use wast::parser::ParseBuffer;

                match f.file_name().to_str().unwrap() {
                    "<ignore>.wast" => (),
                    _ => {
                        println!("Process {:?}", f.path());
                        let contents = String::from_utf8(read(f.path())?)?;
                        let buf = ParseBuffer::new(&contents)?;
                        let wast = parser::parse::<Wast>(&buf)?;
                        for d in wast.directives {
                            match d {
                                WastDirective::Module(mut module) => {
                                    let actual = module.encode()?;
                                    check(actual)?;
                                }
                                _ => (),
                            }
                        }
                    }
                }
            }
            _ => (),
        }
    }

    Ok(())
}
