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
        self,
        config: &mut WasmMutate<'a>,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let mut codes = CodeSection::new();

        let code_section = config.info().get_code_section();
        let mut reader = CodeSectionReader::new(code_section.data, 0)?;

        let count = reader.get_count();
        let function_to_mutate = config.rng().gen_range(0, count);

        (0..count)
            .map(|i| {
                config.consume_fuel(1)?;

                let f = reader.read().unwrap();
                if i == function_to_mutate {
                    log::trace!("Mutating function {}", i);
                    let locals = vec![];
                    let mut f = Function::new(locals);
                    f.instruction(&Instruction::Unreachable);
                    f.instruction(&Instruction::End);

                    codes.function(&f);
                } else {
                    codes.raw(&code_section.data[f.range().start..f.range().end]);
                }
                Ok(())
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Box::new(std::iter::once(Ok(config
            .info()
            .replace_section(config.info().code.unwrap(), &codes)))))
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate) -> bool {
        !config.preserve_semantics && config.info().has_code()
    }
}

#[cfg(test)]
mod tests {
    use super::FunctionBodyUnreachable;

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
}
