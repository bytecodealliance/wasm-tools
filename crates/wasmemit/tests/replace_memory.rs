use anyhow::{bail, Result};
use wasmemit::*;

macro_rules! assert_wasm_eq {
    ($left:expr, $right:expr) => {
        if ($left != $right) {
            bail!(
                "assertion failed: `(wasm left == wasm right)\n  left: {}\n  right: {}",
                wasmprinter::print_bytes($left)?,
                wasmprinter::print_bytes($right)?,
            );
        }
    };
}

#[test]
fn test_replace_memory() -> Result<()> {
    const TEST: &[u8] = br#"(module
    (memory 1 10)
    (memory 1 20)
    (func
        i32.const 42 
        i32.store 0 offset=0
    ))"#;
    let origin_wasm = wat::parse_bytes(TEST)?;

    const EXPECTED: &[u8] = br#"(module
    (import "env" "memory" (memory 1 20))
    (memory 1 10)
    (func
        i32.const 42 
        i32.store 1 offset=0
    ))"#;
    let expected_wasm = wat::parse_bytes(EXPECTED)?;

    let mut module = read(&origin_wasm)?;

    match module {
        Module {
            kind: ModuleKind::Text(ref mut fields),
            ..
        } => {
            let (i, f) = fields
                .iter_mut()
                .enumerate()
                .find(|(_, f)| {
                    if let ModuleField::Memory(Memory {
                        kind:
                            MemoryKind::Normal(MemoryType::B32 {
                                limits: Limits { max: Some(20), .. },
                                ..
                            }),
                        ..
                    }) = f
                    {
                        true
                    } else {
                        false
                    }
                })
                .unwrap();
            // replace memory to be import
            if let ModuleField::Memory(mem) = f {
                match mem.kind {
                    MemoryKind::Normal(ty) => {
                        let import = InlineImport {
                            module: "env",
                            field: Some("memory"),
                        };
                        mem.kind = MemoryKind::Import { ty, import };
                    }
                    _ => bail!("unexpected mem kind"),
                }
            }
            // also move it before non-import
            let m = fields.remove(i);
            fields.insert(i - 1, m);
        }
        _ => bail!("expected text"),
    }
    let wasm = module.encode()?;
    assert_wasm_eq!(wasm, expected_wasm.to_vec());

    Ok(())
}
