use rand::{prelude::SmallRng, Rng};
use wasm_encoder::{CodeSection, Function, Instruction, Module, ValType};
use wasmparser::{CodeSectionReader, FunctionBody, Operator, SectionReader};

use crate::{error::EitherType, module::*, Error, ModuleInfo, Result, WasmMutate};

use super::{CodeMutator, TupleType};

pub struct StrengthReduction {
    // If true, place all x constants first and then perform the same x multiplications
    stress_stack: bool,
}

impl StrengthReduction {
    pub fn new(stress_stack: bool) -> Self {
        StrengthReduction { stress_stack }
    }

    fn is_shl(&self, op: &Operator) -> bool {
        match op {
            Operator::I32Shl | Operator::I64Shl => true,
            _ => false,
        }
    }

    fn is_valid_const(&self, op: &Operator) -> bool {
        match op {
            Operator::I32Const { .. } | Operator::I64Const { .. } => true,
            _ => false,
        }
    }
}

impl CodeMutator for StrengthReduction {
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

        let mut newf = Function::new(current_locals);
        let mut idx = 0;

        let mut newoffset = 0;
        for (operator, offset) in operators {
            newoffset = offset;
            if idx == operator_index - 1 {
                // Stop at previous const
                // Copy previous code to the body
                let previous = &function_stream[body_range.start..offset];
                newf.raw(previous.iter().copied());
                // Inject new code to undo the strength reduction
                // transform the constat to a 2^x = 2*2*2*2*2...
                match operator {
                    Operator::I32Const { value } => {
                        (0..value).for_each(|_| {
                            newf.instruction(Instruction::I32Const(2));
                            if !self.stress_stack {
                                newf.instruction(Instruction::I32Mul);
                            }
                        });
                        if self.stress_stack {
                            (0..value).for_each(|_| {
                                newf.instruction(Instruction::I32Mul);
                            })
                        }
                    }
                    Operator::I64Const { value } => {
                        (0..value).for_each(|_| {
                            newf.instruction(Instruction::I64Const(2));
                            if !self.stress_stack {
                                newf.instruction(Instruction::I64Mul);
                            }
                        });
                        if self.stress_stack {
                            (0..value).for_each(|_| {
                                newf.instruction(Instruction::I64Mul);
                            })
                        }
                    }
                    _ => {
                        return Err(Error::UnsupportedType(EitherType::Operator(format!(
                            "{:?}",
                            operator
                        ))))
                    }
                }
            }
            if idx == operator_index + 1 {
                // Bypass the shift
                break;
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

        if self.is_shl(operator) {
            // Check the previous instruction to check for constant
            let (previous, _) = &operators[at - 1];
            return Ok(self.is_valid_const(previous));
        }

        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use rand::{rngs::SmallRng, SeedableRng};
    use wasm_encoder::{CodeSection, FunctionSection, Module, TypeSection, ValType};
    use wasmparser::{Chunk, Parser};

    use crate::mutators::peephole::TupleType;
    use crate::{
        mutators::{
            peephole::{CodeMutator, PeepholeMutator},
            Mutator,
        },
        WasmMutate,
    };
    use wasm_encoder::{RawSection, SectionId};
    use wasmparser::{Payload, SectionReader};

    use super::StrengthReduction;

    #[test]
    fn test_strength_reduction_i32() {
        let original = r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
                i32.const 42
                i32.add
                i32.const 4
                i32.shl
            )
        )
        "#;

        let expected = r#"
        (module
            (type (;0;) (func (result i32)))
            (func (export "exported_func") (;0;) (type 0) (result i32)
              (local i32 i32)
              i32.const 42
              i32.const 42
              i32.add
              i32.const 2
              i32.mul
              i32.const 2
              i32.mul
              i32.const 2
              i32.mul
              i32.const 2
              i32.mul))
        "#;

        crate::match_code_mutation!(
            original,
            move |config: &WasmMutate, operators, mut reader, range, function_stream: &[u8]| {
                let mutator = StrengthReduction {
                    stress_stack: false,
                };
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

    #[test]
    fn test_strength_reduction_i64() {
        let original = r#"
        (module
            (func (export "exported_func") (result i64) (local i32 i32)
                i64.const 42
                i64.const 42
                i64.add
                i64.const 4
                i64.shl
            )
        )
        "#;

        let expected = r#"
        (module
            (type (;0;) (func (result i64)))
            (func (export "exported_func") (;0;) (type 0) (result i64)
              (local i32 i32)
              i64.const 42
              i64.const 42
              i64.add
              i64.const 2
              i64.const 2
              i64.const 2
              i64.const 2
              i64.mul
              i64.mul
              i64.mul
              i64.mul))
        "#;

        crate::match_code_mutation!(
            original,
            move |config: &WasmMutate, operators, mut reader, range, function_stream: &[u8]| {
                let mutator = StrengthReduction { stress_stack: true };
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
