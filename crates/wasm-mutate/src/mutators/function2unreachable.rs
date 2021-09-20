
use rand::prelude::SmallRng;
use rand::{Rng, RngCore};
use wasm_encoder::{CodeSection,Function, Instruction, Module};
use wasmparser::{CodeSectionReader};
use crate::{ModuleInfo, WasmMutate, Result};

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
                codes.raw(&info.input_wasm[f.range().start..f.range().end]);
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
    use crate::{
        WasmMutate,
    };
    use rand::{rngs::SmallRng, SeedableRng};

    use super::{Mutator, SetFunction2Unreachable};


    #[test]
    fn test_code_unreachable_mutator() {
        let wat = r#"
        (module
            (func (result i32)
                i32.const 42
            )
        )
        "#;
        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(wat).unwrap();

        let mutator = SetFunction2Unreachable {};

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        assert_eq!(can_mutate, true);

        let mut rnd = SmallRng::seed_from_u64(0);
        let mutation = mutator.mutate(&wasmmutate, &mut rnd, &mut info);

        let mutation_bytes = mutation.unwrap().finish();

        // If it fails, it is probably an invalid
        let text = wasmprinter::print_bytes(mutation_bytes).unwrap();

        assert_eq!("(module\n  (type (;0;) (func (result i32)))\n  (func (;0;) (type 0) (result i32)\n    unreachable))", text)
    }
}
