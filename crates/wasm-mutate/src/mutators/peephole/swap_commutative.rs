use rand::{prelude::SmallRng, Rng};
use wasm_encoder::{CodeSection, Function, Instruction, Module, ValType};
use wasmparser::{CodeSectionReader, FunctionBody, Operator, SectionReader};

use crate::{error::EitherType, module::*, Error, ModuleInfo, Result, WasmMutate};

use super::{CodeMutator, TupleType};

pub struct SwapCommutativeOperator;

impl SwapCommutativeOperator {
    fn is_commutative(&self, op: &Operator) -> bool {
        match op {
              Operator::I32Add | Operator::I32Mul | Operator::I32Or | Operator::I32And | Operator::I32Xor
            | Operator::I64Add | Operator::I64Mul | Operator::I64And | Operator::I64Or | Operator::I64Xor
            | Operator::F32Add | Operator::F32Mul // Check for float incosistency 
            | Operator::F64Add | Operator::F64Mul // Check for float incosistency
            // TODO do the others
            => {
                true
            }
            _ => {
                false
            }
        }
    }

    fn get_operator_type(&self, op: &Operator) -> Result<ValType> {
        match op {
            Operator::I32Add
            | Operator::I32Mul
            | Operator::I32Or
            | Operator::I32And
            | Operator::I32Xor => Ok(ValType::I32),
            Operator::I64Add
            | Operator::I64Mul
            | Operator::I64And
            | Operator::I64Or
            | Operator::I64Xor => Ok(ValType::I64),
            Operator::F32Add | Operator::F32Mul => Ok(ValType::F32),
            Operator::F64Add | Operator::F64Mul => Ok(ValType::F64),
            // TODO do the others
            _ => Err(Error::UnsupportedType(EitherType::Operator(format!(
                "{:?}",
                op
            )))),
        }
    }
}

impl CodeMutator for SwapCommutativeOperator {
    fn mutate(
        &self,
        _: &WasmMutate,
        _: &mut SmallRng,
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

        // add two temporary locals, the last two
        let (operator, _) = &operators[operator_index];
        current_locals.push((2, self.get_operator_type(operator)?));

        let mut newf = Function::new(current_locals);
        let mut idx = 0;

        let mut newoffset = 0;
        for (_, offset) in operators {
            newoffset = offset;
            if idx == operator_index {
                // Copy previous code to the body
                let previous = &function_stream[body_range.start..offset];
                newf.raw(previous.iter().copied());
                // Inject new code to swap operands
                newf.instruction(Instruction::LocalSet(local_count + 1));
                newf.instruction(Instruction::LocalSet(local_count + 2));
                newf.instruction(Instruction::LocalGet(local_count + 1));
                newf.instruction(Instruction::LocalGet(local_count + 2));
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
        _: &'a WasmMutate,
        operators: &Vec<TupleType<'a>>,
        at: usize,
    ) -> Result<bool> {
        let (operator, _) = &operators[at];
        Ok(self.is_commutative(operator))
    }
}

#[cfg(test)]
mod tests {
    use rand::{rngs::SmallRng, SeedableRng};
    use wasm_encoder::{CodeSection, FunctionSection, Module, TypeSection, ValType};
    use wasmparser::{Chunk, Parser};

    use crate::{
        mutators::{
            peephole::{CodeMutator, PeepholeMutator},
            Mutator,
        },
        WasmMutate,
    };
    use wasm_encoder::{RawSection, SectionId};
    use wasmparser::{Payload, SectionReader};
    use crate::mutators::peephole::TupleType;

    use super::SwapCommutativeOperator;

    #[test]
    fn test_swap() {
        let original = r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
                i32.const 42
                i32.add
                i32.const 56
                i32.add
            )
        )
        "#;

        let expected = r#"
        (module
            (type (;0;) (func (result i32)))
            (func (;0;) (type 0) (result i32)
              (local i32 i32 i32 i32)
              i32.const 42
              i32.const 42
              i32.add
              i32.const 56
              local.set 3
              local.set 4
              local.get 3
              local.get 4
              i32.add))
        "#;

        crate::match_code_mutation!(
            original,
            move |config: &WasmMutate, operators, mut reader, range,function_stream: &[u8]| {
                let mutator = SwapCommutativeOperator;
                let mut rnd = SmallRng::seed_from_u64(0);

                mutator
                    .mutate(&config, &mut rnd, 4,operators, reader, range, &function_stream)
                    .unwrap()
            },
            expected
        )
    }
}
