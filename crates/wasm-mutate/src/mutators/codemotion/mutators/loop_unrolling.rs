//! This mutator selects a random `if` construction in a function and swap its branches.
//! Since this mutator preserves the original semantic of the input Wasm,
//! before the mutated if structure is encoded, a "negation" of the previous operand
//! in the stack is written. The "negation" is encoded with a `i32.eqz` operator.
use rand::prelude::SliceRandom;
use wasm_encoder::{Function, Instruction, ValType};

use crate::{
    mutators::{
        codemotion::{
            ir::{parse_context::Ast, AstWriter},
            AstMutator,
        },
        OperatorAndByteOffset,
    },
};

pub struct LoopUnrollMutator;

#[derive(Default)]
struct LoopUnrollWriter {
    loop_to_mutate: usize,
}

impl LoopUnrollWriter {
    fn unroll_loop<'a>(&self, ast: &Ast, nodeidx: usize, body: &[usize], newfunc: &mut Function, operators: &Vec<OperatorAndByteOffset>, input_wasm: &'a [u8], ty: &wasmparser::TypeOrFuncType) -> crate::Result<()> {
        
        todo!();
    }
}

impl AstWriter for LoopUnrollWriter {

    fn write_loop<'a>(&self, ast: &Ast, nodeidx: usize, body: &[usize], newfunc: &mut Function, operators: &Vec<OperatorAndByteOffset>, input_wasm: &'a [u8], ty: &wasmparser::TypeOrFuncType) -> crate::Result<()> {
        if self.loop_to_mutate == nodeidx {
            self.unroll_loop(
                ast,
                nodeidx,
                body,
                newfunc,
                operators,
                input_wasm,
                ty,
            )?;
        } else {
            self.write_loop_default(
                ast,
                nodeidx,
                body,
                newfunc,
                operators,
                input_wasm,
                ty,
            )?;
        }
        Ok(())
    }
}

impl AstMutator for LoopUnrollMutator {
    fn can_mutate<'a>(&self, _: &'a crate::WasmMutate, _: &crate::ModuleInfo, ast: &Ast) -> bool {
        ast.has_loop()
    }

    fn mutate<'a>(
        &self,
        _: &'a crate::WasmMutate,
        _: &crate::ModuleInfo,
        rnd: &mut rand::prelude::SmallRng,
        ast: &Ast,
        locals: &[(u32, ValType)],
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
    ) -> crate::Result<Function> {
        // Select the if index
        let mut newfunc = Function::new(locals.to_vec());
        let loop_index = ast
            .get_loops()
            .choose(rnd)
            .expect("This mutator should check first if the AST contains at least one loop node");
        let writer = LoopUnrollWriter {
            loop_to_mutate: *loop_index,
        };
        writer.write(ast, ast.get_root(), &mut newfunc, operators, input_wasm)?;
        Ok(newfunc)
    }
}
