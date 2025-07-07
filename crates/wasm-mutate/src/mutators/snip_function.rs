//! Mutator that replaces the body of a function with an empty body

use super::Mutator;
use crate::module::{PrimitiveTypeInfo, TypeInfo};
use crate::{Result, WasmMutate};
use rand::Rng;
use wasm_encoder::{CodeSection, Function, HeapType, Module};
use wasmparser::CodeSectionReader;

/// Mutator that replaces the body of a function with an empty body
#[derive(Clone, Copy)]
pub struct SnipMutator;

impl Mutator for SnipMutator {
    fn mutate<'a>(
        &self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let mut codes = CodeSection::new();
        let code_section = config.info().code.unwrap();
        let reader = config.info().get_binary_reader(code_section);
        let reader = CodeSectionReader::new(reader)?;
        let count = reader.count();
        let function_to_mutate = config.rng().random_range(0..count);
        let ftype = config
            .info()
            .get_functype_idx(function_to_mutate + config.info().num_imported_functions())
            .clone();

        for (i, func) in reader.into_iter().enumerate() {
            config.consume_fuel(1)?;
            let f = func?;

            if i as u32 != function_to_mutate {
                codes.raw(f.as_bytes());
                continue;
            }

            log::trace!("Snipping function {function_to_mutate}");

            let locals = vec![];
            let mut f = Function::new(locals);

            match &ftype {
                TypeInfo::Func(t) => {
                    for primitive in t.returns.iter() {
                        match primitive {
                            PrimitiveTypeInfo::I32 => {
                                f.instructions().i32_const(0);
                            }
                            PrimitiveTypeInfo::I64 => {
                                f.instructions().i64_const(0);
                            }
                            PrimitiveTypeInfo::F32 => {
                                f.instructions().f32_const(0.0.into());
                            }
                            PrimitiveTypeInfo::F64 => {
                                f.instructions().f64_const(0.0.into());
                            }
                            PrimitiveTypeInfo::V128 => {
                                f.instructions().v128_const(0);
                            }
                            PrimitiveTypeInfo::FuncRef => {
                                f.instructions().ref_null(HeapType::FUNC);
                            }
                            PrimitiveTypeInfo::ExternRef => {
                                f.instructions().ref_null(HeapType::EXTERN);
                            }
                            PrimitiveTypeInfo::Empty => {
                                unreachable!()
                            }
                        }
                    }
                }
            }

            f.instructions().end();
            codes.function(&f);
        }

        Ok(Box::new(std::iter::once(Ok(config
            .info()
            .replace_section(config.info().code.unwrap(), &codes)))))
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate) -> bool {
        !config.preserve_semantics && config.info().has_nonempty_code()
    }
}

#[cfg(test)]
mod tests {
    use super::SnipMutator;
    use crate::mutators::Mutator;

    #[test]
    fn test_code_snip_mutator() {
        crate::mutators::match_mutation(
            r#"
        (module
            (func (result i64)
                i64.const 42
            )
        )
        "#,
            SnipMutator,
            r#"
        (module
            (type (;0;) (func (result i64)))
            (func (;0;) (type 0) (result i64)
              i64.const 0)
        )
        "#,
        );
    }

    #[test]
    fn test_snip_function_empty_code_section() {
        let wasm = b"\x00\x61\x73\x6d\x01\x00\x00\x00\x0a\x02\x00\x0b";
        let mut config = crate::WasmMutate::default();
        config.setup(wasm).unwrap();
        assert_eq!(SnipMutator.can_mutate(&config), false);
    }
}
