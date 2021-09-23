use rand::{prelude::SmallRng, Rng, RngCore};
use wasm_encoder::{CodeSection, Function, Instruction, MemArg, Module, ValType};
use wasmparser::{CodeSectionReader, FunctionBody, Operator, SectionReader};

use crate::{error::EitherType, module::*, Error, ModuleInfo, Result, WasmMutate};

use super::{CodeMutator, TupleType};

pub struct ShiftLoad;

impl ShiftLoad {
    fn is_load(&self, op: &Operator) -> bool {
        match op {
              Operator::I32Load{..}
              | Operator::F32Load{..}
              | Operator::F64Load{..}
              | Operator::I32Load16S{..}
              | Operator::I32Load16U{..}
              | Operator::I32Load8S{..}
              | Operator::I32Load8U{..}
              | Operator::I64Load{..}
              | Operator::I64Load16S{..}
              | Operator::I64Load16U{..}
              | Operator::I64Load32S{..}
              | Operator::I64Load32U{..}
              | Operator::I64Load8S{..}
              | Operator::I64Load8U{..}
              // TODO add others
            => {
                true
            }
            _ => {
                false
            }
        }
    }
}

impl CodeMutator for ShiftLoad {
    fn mutate(
        &self,
        _: &WasmMutate,
        rnd: &mut SmallRng,
        operator_index: usize,
        operators: Vec<TupleType>,
        funcreader: FunctionBody,
        body_range: wasmparser::Range,
        function_stream: &[u8],
    ) -> Result<Function> {
        let mut localreader = funcreader.get_locals_reader().unwrap();
        // Get current locals and map to encoder types
        let mut local_count = 0;
        let mut current_locals = (0..localreader.get_count())
            .map(|f| {
                let (count, ty) = localreader.read().unwrap();
                local_count += count;
                (count, map_type(ty).unwrap())
            })
            .collect::<Vec<(u32, ValType)>>();

        let mut newf = Function::new(current_locals);
        let mut idx = 0;

        let mut newoffset = 0;
        for (_, offset) in operators {
            newoffset = offset;
            if idx == operator_index - 1 {
                // Copy previous code to the body
                let previous = &function_stream[body_range.start..offset];
                newf.raw(previous.iter().copied());
                // Inject new offset
                let random_shift = rnd.gen();
                newf.instruction(Instruction::I32Const(random_shift));
                newf.instruction(Instruction::I32Add);

                break; // this break allows to copy the remaining buffer of the current reader
            }
            idx += 1;
        }

        // Copy last part of the function body
        let remaining = &function_stream[newoffset..body_range.end];
        newf.raw(remaining.iter().copied());
        Ok(newf)
    }

    fn can_mutate<'a>(
        &self,
        config: &'a WasmMutate,
        info: &crate::ModuleInfo,
        operators: &Vec<TupleType<'a>>,
        at: usize,
    ) -> Result<bool> {
        let (operator, _) = &operators[at];
        Ok(info.memory_count > 0 && self.is_load(operator))
    }
}

#[cfg(test)]
mod tests {
    use rand::{rngs::SmallRng, SeedableRng};
    use wasm_encoder::{CodeSection, FunctionSection, Module, TypeSection, ValType};
    use wasmparser::{Chunk, Parser};

    use crate::mutators::peephole::shift_load::ShiftLoad;
    use crate::mutators::peephole::TupleType;
    use crate::{mutators::peephole::CodeMutator, WasmMutate};
    use wasm_encoder::{RawSection, SectionId};
    use wasmparser::{Payload, SectionReader};

    #[test]
    fn test_shift() {
        let original = r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
                i32.const 42
                i32.add
                i32.load
            )
            (memory 0)
        )
        "#;

        let expected = r#"
        (module
            (type (;0;) (func (result i32)))
            (func (;0;) (type 0) (result i32)
              (local i32 i32)
              i32.const 42
              i32.const 42
              i32.add
              i32.const 1081994402
              i32.add
              i32.load)
            (memory (;0;) 0)
            (export "exported_func" (func 0)))
        "#;

        crate::match_code_mutation!(
            original,
            move |config: &WasmMutate, operators, mut reader, range, function_stream: &[u8]| {
                let mutator = ShiftLoad;
                let mut rnd = SmallRng::seed_from_u64(0);

                mutator
                    .mutate(
                        &config,
                        &mut rnd,
                        4,
                        operators,
                        reader,
                        range,
                        &function_stream,
                    )
                    .unwrap()
            },
            expected
        )
    }
}
