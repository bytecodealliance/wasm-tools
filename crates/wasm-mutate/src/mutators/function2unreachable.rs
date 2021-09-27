use crate::{ModuleInfo, Result, WasmMutate};
use rand::prelude::SmallRng;
use rand::{Rng, RngCore};
use wasm_encoder::{CodeSection, Function, Instruction, Module};
use wasmparser::CodeSectionReader;

use super::Mutator;

pub struct SetFunction2Unreachable;

impl Mutator for SetFunction2Unreachable {
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        info: &mut ModuleInfo,
    ) -> Result<Module> {
        let mut codes = CodeSection::new();
        let code_section = info.get_code_section();
        let mut reader = CodeSectionReader::new(code_section.data, 0)?;
        let count = reader.get_count();
        let function_to_mutate = rnd.gen_range(0, count);
        (0..count).for_each(|i| {
            if i == function_to_mutate {
                log::debug!("Changing function idx {:?}", i);
                let locals = vec![];
                let mut f = Function::new(locals);
                f.instruction(Instruction::Unreachable);
                f.instruction(Instruction::End);

                codes.function(&f);
            } else {
                let f = reader.read().unwrap();
                codes.raw(&code_section.data[f.range().start..f.range().end]);
            }
        });
        Ok(info.replace_section(info.code.unwrap(), &codes))
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate, info: &ModuleInfo) -> Result<bool> {
        Ok(!config.preserve_semantics && info.has_code())
    }
}

#[cfg(test)]
mod tests {
    use crate::WasmMutate;
    use rand::{rngs::SmallRng, SeedableRng};

    use super::{Mutator, SetFunction2Unreachable};

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
            &SetFunction2Unreachable,
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
