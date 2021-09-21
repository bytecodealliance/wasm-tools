use rand::{Rng, prelude::SmallRng};
use wasm_encoder::{CodeSection, Function, Instruction, Module, ValType};
use wasmparser::{CodeSectionReader, FunctionBody, Operator, SectionReader};

use crate::{Error, Result, ModuleInfo, WasmMutate, module::*};

use super::CodeMutator;


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

    fn get_operator_type(&self, op: &Operator) -> ValType {
        match op {
            Operator::I32Add | Operator::I32Mul | Operator::I32Or | Operator::I32And | Operator::I32Xor
            => {
                ValType::I32
            }
            Operator::I64Add | Operator::I64Mul | Operator::I64And | Operator::I64Or | Operator::I64Xor
            => {
                ValType::I64
            }
            Operator::F32Add | Operator::F32Mul 
            => {
                ValType::F32
            }   
            Operator::F64Add | Operator::F64Mul 
            => {
                ValType::F64
            }
            // TODO do the others
            _ => {
                // TODO as err
                panic!("Unknown return type")
            }
        }
    }
}

impl CodeMutator for SwapCommutativeOperator {
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        funcreader: &mut FunctionBody,
        operator_index: usize,
        input_wasm: &[u8]
    ) -> Result<Function> {
        
        let mut opreader= funcreader.get_operators_reader().unwrap();     
        let body_range = opreader.range();
        let opreaderiterator = opreader.into_iter_with_offsets();   
        let operators = opreaderiterator.collect::<wasmparser::Result<Vec<(Operator, usize)>>>().unwrap();

        let mut localreader = funcreader.get_locals_reader().unwrap();
        // Get current locals and map to encoder types
        let mut local_count = 0;
        let mut current_locals = (0..localreader.get_count()).map(|f|{
            let (count, ty) = localreader.read().unwrap();
            local_count += count;
            (count, map_type(ty).unwrap())
        }).collect::<Vec<(u32, ValType)>>();


        // add two temporary locals, the last two
        let (operator, _) = &operators[operator_index];
        current_locals.push((2, self.get_operator_type(operator)));

        println!("{:?}", current_locals);

        let mut newf = Function::new(current_locals);
        let mut idx = 0;

        

        let mut newoffset = 0;
        //println!("init {:?} {:?} {:?} body {:?}", input_wasm, operator_index, body_range, &input_wasm[body_range.start..body_range.end]);
        for (_, offset) in operators {
            newoffset = offset;
            if idx == operator_index {
                // Copy previous code to the body
                let previous =  &input_wasm[body_range.start..offset];        
                //println!("previous {:?} {:?}", previous, offset);
                newf.raw(previous.iter().copied());
                // Inject new code to swap operands
                newf.instruction(Instruction::LocalTee(local_count + 1));
                newf.instruction(Instruction::LocalTee(local_count + 2));
                break; // this allows to copy the remaining buffer of the current reader
            } 
            idx += 1;
        };

        // Copy last part of the function body
        let remaining = &input_wasm[newoffset..body_range.end];        
        newf.raw(remaining.iter().copied());
        Ok(newf)
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate, operators: &Vec<Operator<'a>>, at: usize) -> Result<bool> {
        let operator = &operators[at];
        Ok(self.is_commutative(operator))
    }
}
