//! Mutator that replaces a function's body with an `unreachable` instruction.

use crate::{Result, WasmMutate};

use rand::Rng;
use wasm_encoder::{CodeSection, Function, Instruction, Module};
use wasmparser::CodeSectionReader;

use super::Mutator;

/// Sets the body of a function to unreachable
#[derive(Clone, Copy)]
pub struct FunctionBodyUnreachable;

impl Mutator for FunctionBodyUnreachable {
    fn mutate<'a>(
        &self,
        config: &mut WasmMutate<'a>,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let mut codes = CodeSection::new();

        let code_section = config.info().get_code_section();
        let reader = CodeSectionReader::new(code_section.data, 0)?;

        let count = reader.count();
        let function_to_mutate = config.rng().gen_range(0..count);

        for (i, f) in reader.into_iter().enumerate() {
            config.consume_fuel(1)?;

            let f = f?;
            if i as u32 == function_to_mutate {
                log::trace!("Mutating function {}", i);
                let locals = vec![];
                let mut f = Function::new(locals);
                f.instruction(&Instruction::Unreachable);
                f.instruction(&Instruction::End);

                codes.function(&f);
            } else {
                codes.raw(&code_section.data[f.range().start..f.range().end]);
            }
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
    use super::FunctionBodyUnreachable;
    use crate::mutators::Mutator;

    #[test]
    fn test_code_unreachable_mutator() {
        crate::mutators::match_mutation(
            r#"
            (module
                (func (result i64)
                    i64.const 42
                )
                (func (result i64)
                    i64.const 42
                )
                (func (export "exported_func") (result i32)
                    i32.const 42
                )
            )
        "#,
            FunctionBodyUnreachable,
            r#"(module
              (type (;0;) (func (result i64)))
              (type (;1;) (func (result i32)))
              (func (;0;) (type 0) (result i64)
                  i64.const 42)  (func (;1;) (type 0) (result i64)
                  i64.const 42)  (func (;2;) (type 1) (result i32)    unreachable)
                (export "exported_func" (func 2)))"#,
        );
    }

    #[test]
    fn test_fn_body_unreachable_empty_code_section() {
        let wasm = b"\x00\x61\x73\x6d\x01\x00\x00\x00\x0a\x02\x00\x0b";
        let mut config = crate::WasmMutate::default();
        config.setup(wasm).unwrap();
        assert_eq!(FunctionBodyUnreachable.can_mutate(&config), false);
    }
}
