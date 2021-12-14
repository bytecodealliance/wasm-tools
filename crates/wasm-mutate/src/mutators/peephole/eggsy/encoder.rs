//! Helper methods for encoding eterm expressions to Wasm and back

use crate::mutators::peephole::dfg::MiniDFG;
use crate::mutators::peephole::eggsy::encoder::expr2wasm::expr2wasm;
use crate::mutators::peephole::{dfg::BBlock, OperatorAndByteOffset};
use crate::mutators::peephole::{Lang, EG};
use crate::WasmMutate;
use egg::RecExpr;

use wasm_encoder::Function;

use self::expr2wasm::ResourceRequest;

pub mod expr2wasm;
pub mod rebuild;

/// Turns wasm to eterm and back
pub struct Encoder;

/// Traversing node events
enum TraversalEvent {
    Enter,
    Exit,
}

impl Encoder {
    /// Reassembles the mutated function and return a `Function` entry
    pub fn build_function(
        config: &mut WasmMutate,
        insertion_point: usize,
        expr: &RecExpr<Lang>,
        operators: &[OperatorAndByteOffset],
        basicblock: &BBlock, // move to the analysis
        newfunc: &mut Function,
        dfg: &MiniDFG,
        egraph: &EG,
    ) -> crate::Result<Vec<ResourceRequest>> {
        // Copy previous code
        let range = basicblock.range;
        let byterange = (&operators[0].1, &operators[range.start].1);
        let bytes = &config.info().get_code_section().data[*byterange.0..*byterange.1];
        newfunc.raw(bytes.iter().copied());

        // Write all entries in the minidfg in reverse order
        // The stack neutral will be preserved but the position of the changed operands not that much :(
        // The edges of the stackentries are always backward in the array, so, it consistent to
        // do the writing in reverse
        let mut resource_request = vec![];
        for (entryidx, parent) in dfg.parents.iter().enumerate() {
            // It is a root, write then
            if parent.is_none() {
                let entry = &dfg.entries[entryidx];
                let to_encode = if entry.operator_idx == insertion_point {
                    expr.clone()
                } else {
                    dfg.get_expr(entry.operator_idx)
                };
                resource_request.extend(expr2wasm(config, &to_encode, newfunc, egraph)?);
            }
        }

        // Copy remaining function
        let range = basicblock.range;
        let byterange = (
            &operators[range.end].1, // In the worst case the next instruction will be and end
            &operators[operators.len() - 1].1,
        );
        let bytes = &config.info().get_code_section().data[*byterange.0..=*byterange.1];

        newfunc.raw(bytes.iter().copied());
        Ok(resource_request)
    }
}
