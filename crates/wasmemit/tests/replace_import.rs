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
fn test_replace_import() -> Result<()> {
    const TEST: &[u8] = br#"(module
    (import "env" "test" (func (param i64)))
    (func
        i64.const 42 
        call 0
    ))"#;
    let origin_wasm = wat::parse_bytes(TEST)?;

    // replacing i64 import with two i32 one, and
    // appending trampoline function to call/convert params.

    const EXPECTED: &[u8] = br#"(module
    (import "env" "test" (func (param i32) (param i32)))
    (func
        i64.const 42 
        call 2
    )
    (func (param i64)
       unreachable
       call 0
    ))"#;
    let expected_wasm = wat::parse_bytes(EXPECTED)?;

    let (mut module, mut id_gen) = AstReader::new().read_all(&origin_wasm)?;

    match module {
        Module {
            kind: ModuleKind::Text(ref mut fields),
            ..
        } => {
            use std::collections::HashMap;
            use std::iter::FromIterator;

            // scan and collect types info
            let types = fields.iter().enumerate().filter_map(|(i, f)| {
                if let ModuleField::Type(Type { id, .. }) = f {
                    Some((id.unwrap().clone(), i))
                } else {
                    None
                }
            });
            let types: HashMap<Id, usize> = HashMap::from_iter(types);
            let next_type_index = types.values().max().map_or(0, |i| *i + 1);

            // add new function type with (i32, i32)
            let import_type_id = id_gen.next(Span::from_offset(0));
            fields.insert(
                next_type_index,
                ModuleField::Type(Type {
                    span: Span::from_offset(0),
                    id: Some(import_type_id),
                    def: TypeDef::Func(FunctionType {
                        params: Box::new([(None, None, ValType::I32), (None, None, ValType::I32)]),
                        results: Box::new([]),
                    }),
                }),
            );

            // replace type at the found import, keep its information in f_id, f_name, f_ty
            let f = fields
                .iter_mut()
                .find(|f| {
                    if let ModuleField::Import(_) = f {
                        true
                    } else {
                        false
                    }
                })
                .unwrap();

            let new_import_id = id_gen.next(Span::from_offset(0));
            let mut f_id = new_import_id;
            let mut f_name = None;
            let mut f_ty = TypeUse {
                index: Some(Index::Id(import_type_id)),
                inline: None,
            };

            if let ModuleField::Import(Import {
                item:
                    ItemSig {
                        id: Some(ref mut id),
                        ref mut name,
                        kind: ItemKind::Func(ref mut ty),
                        ..
                    },
                ..
            }) = f
            {
                std::mem::swap(id, &mut f_id);
                std::mem::swap(name, &mut f_name);
                std::mem::swap(ty, &mut f_ty);
            } else {
                panic!();
            }

            // re-order types for assert_wasm_eq
            if let TypeUse {
                index: Some(Index::Id(ref id)),
                ..
            } = f_ty
            {
                fields.swap(next_type_index, types[id]);
            }

            // use f_id, f_name, f_ty to create trampoline function.
            fields.push(ModuleField::Func(Func {
                span: Span::from_offset(0),
                id: Some(f_id),
                name: f_name,
                exports: InlineExport { names: vec![] },
                kind: FuncKind::Inline {
                    locals: vec![],
                    expression: Expression {
                        instrs: Box::new([
                            Instruction::Unreachable,
                            Instruction::Call(Index::Id(new_import_id)),
                        ]),
                    },
                },
                ty: f_ty,
            }));
        }
        _ => bail!("expected text"),
    }
    let wasm = module.encode()?;
    assert_wasm_eq!(wasm, expected_wasm.to_vec());

    Ok(())
}
