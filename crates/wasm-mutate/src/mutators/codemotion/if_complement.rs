//! This mutator selects a random `if` construction in a function and swap its branches.
//! Since this mutator preserves the original semantic of the input Wasm,
//! before the mutated if structure is encoded, a "negation" of the previous operand
//! in the stack is written. The "negation" is encoded with a `i32.eqz` operator.
use rand::prelude::*;
use wasm_encoder::{Function, ValType};

use crate::{
    WasmMutate,
    module::map_block_type,
    mutators::{
        OperatorAndByteOffset,
        codemotion::{
            AstMutator,
            ir::{AstWriter, parse_context::Ast},
        },
    },
};

/// This mutator selects a random `if` construction in a function and swap its branches.
/// Since this mutator preserves the original semantic of the input Wasm,
/// before the mutated if structure is encoded, a "negation" of the previous operand
/// in the stack is written. The "negation" is encoded with a `i32.eqz` operator.
pub struct IfComplementMutator;

#[derive(Default)]
struct IfComplementWriter {
    if_to_mutate: usize,
}

impl IfComplementWriter {
    /// Swap the then and alternative branches and negates the expected value for the "if" condition
    fn write_complement<'a>(
        &self,
        ast: &Ast,
        _: usize,
        then: &[usize],
        alternative: &Option<Vec<usize>>,
        newfunc: &mut wasm_encoder::Function,
        operators: &Vec<crate::mutators::OperatorAndByteOffset>,
        input_wasm: &'a [u8],
        ty: &wasmparser::BlockType,
    ) -> crate::Result<()> {
        // negate the value on the stack
        newfunc.instructions().i32_eqz();
        newfunc.instructions().if_(map_block_type(*ty)?);

        // Swap, write alternative first
        if let Some(alternative) = alternative {
            for ch in alternative {
                self.write(ast, *ch, newfunc, operators, input_wasm)?;
            }
        } else {
            // Write an unreachable instruction
            newfunc.instructions().nop();
        }
        newfunc.instructions().else_();
        for ch in then {
            self.write(ast, *ch, newfunc, operators, input_wasm)?;
        }
        newfunc.instructions().end();

        Ok(())
    }
}

impl AstWriter for IfComplementWriter {
    /// Replaces the default implementation of the if_else_writing
    fn write_if_else<'a>(
        &self,
        ast: &Ast,
        nodeidx: usize,
        then: &[usize],
        alternative: &Option<Vec<usize>>,
        newfunc: &mut wasm_encoder::Function,
        operators: &Vec<crate::mutators::OperatorAndByteOffset>,
        input_wasm: &'a [u8],
        ty: &wasmparser::BlockType,
    ) -> crate::Result<()> {
        if self.if_to_mutate == nodeidx {
            self.write_complement(
                ast,
                nodeidx,
                then,
                alternative,
                newfunc,
                operators,
                input_wasm,
                ty,
            )?;
        } else {
            self.write_if_else_default(
                ast,
                nodeidx,
                then,
                alternative,
                newfunc,
                operators,
                input_wasm,
                ty,
            )?;
        }
        Ok(())
    }
}

impl AstMutator for IfComplementMutator {
    fn can_mutate(&self, _: &crate::WasmMutate, ast: &Ast) -> bool {
        ast.has_if()
    }

    fn mutate<'a>(
        &self,
        config: &'a mut WasmMutate,
        ast: &Ast,
        locals: &[(u32, ValType)],
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
    ) -> crate::Result<Function> {
        // Select the if index
        let mut newfunc = Function::new(locals.to_vec());
        let if_index = ast
            .get_ifs()
            .choose(config.rng())
            .expect("This mutator should check first if the AST contains at least one if");
        let writer = IfComplementWriter {
            if_to_mutate: *if_index,
        };
        writer.write(ast, ast.get_root(), &mut newfunc, operators, input_wasm)?;
        Ok(newfunc)
    }
}
