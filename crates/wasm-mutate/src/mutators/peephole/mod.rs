use std::collections::HashMap;

use rand::{
    prelude::{IteratorRandom, SliceRandom, SmallRng},
    Rng,
};
use wasm_encoder::{CodeSection, Function, Module};
use wasmparser::{BinaryReaderError, CodeSectionReader, FunctionBody, Operator};

use crate::{
    mutators::peephole::swap_commutative::SwapCommutativeOperator, ModuleInfo, Result, WasmMutate,
};

use super::Mutator;

pub mod swap_commutative;

pub struct PeepholeMutator;

// Code mutator, function id, operator id
type MutationContext = (Function, u32);
// Helper type to return operator and ofsset inside the byte stream
type TupleType<'a> = (Operator<'a>, usize);
impl PeepholeMutator {
    fn random_mutate(
        &self,
        config: &crate::WasmMutate,
        rnd: &mut rand::prelude::SmallRng,
        info: &mut crate::ModuleInfo,
        peepholes: Vec<Box<dyn CodeMutator>>,
    ) -> Result<MutationContext> {
        let code_section = info.get_code_section();
        let mut sectionreader = CodeSectionReader::new(code_section.data, 0)?;
        let function_count = sectionreader.get_count();

        // Split where to start looking for mutable function
        // In theory random split will provide a mutable location faster
        let function_to_mutate = rnd.gen_range(0, function_count);
        let all_readers = (0..function_count)
            .map(|fidx| sectionreader.read().unwrap())
            .collect::<Vec<FunctionBody>>();

        for fidx in (function_to_mutate..function_count).chain(0..function_to_mutate) {
            let reader = all_readers[fidx as usize];
            let operatorreader = reader.get_operators_reader()?;
            let operatorsrange = operatorreader.reader.range();
            let operators = operatorreader
                .into_iter_with_offsets()
                .collect::<wasmparser::Result<Vec<TupleType>>>()?;
            let operatorscount = operators.len();
            let opcode_to_mutate = rnd.gen_range(0, operatorscount);

            for oidx in (opcode_to_mutate..operatorscount).chain(0..opcode_to_mutate) {
                let mut applicable = Vec::new();
                for peephole in &peepholes {
                    if peephole.can_mutate(config, &operators, oidx)? {
                        applicable.push(peephole);
                    }
                }
                if applicable.len() > 0 {
                    // Call the random mutator now :)
                    let mutator = applicable
                        .choose(rnd)
                        .ok_or(crate::Error::NoMutationsAplicable)?;
                    let reader = all_readers[function_to_mutate as usize];
                    let f = mutator.mutate(
                        config,
                        rnd,
                        oidx,
                        operators,
                        reader,
                        operatorsrange,
                        code_section.data,
                    )?;
                    return Ok((f, fidx));
                }
                // continue otherwise
            }
        }

        Err(crate::Error::NotMatchingPeepholes)
    }
}

/// Meta mutator for peephole
impl Mutator for PeepholeMutator {
    fn mutate(
        &self,
        config: &crate::WasmMutate,
        rnd: &mut rand::prelude::SmallRng,
        info: &mut crate::ModuleInfo,
    ) -> Result<Module> {
        let peepholes: Vec<Box<dyn CodeMutator>> = vec![Box::new(SwapCommutativeOperator)];
        let (new_function, function_to_mutate) =
            self.random_mutate(config, rnd, info, peepholes)?;

        let mut codes = CodeSection::new();
        let code_section = info.get_code_section();
        let mut sectionreader = CodeSectionReader::new(code_section.data, 0)?;

        for fidx in 0..info.function_count {
            if fidx == function_to_mutate {
                codes.function(&new_function);
            } else {
                let mut reader = sectionreader.read()?;
                codes.raw(&code_section.data[reader.range().start..reader.range().end]);
            }
        }

        let module = info.replace_section(info.code.unwrap(), &codes);
        Ok(module)
    }

    fn can_mutate<'a>(
        &self,
        config: &'a crate::WasmMutate,
        info: &crate::ModuleInfo,
    ) -> Result<bool> {
        Ok(info.has_code() && info.function_count > 0)
    }
}

use std::fmt::Debug;
impl Debug for Box<dyn CodeMutator> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Code mutator").finish()
    }
}
pub(crate) trait CodeMutator {
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        operator_index: usize,
        operators: Vec<TupleType>,
        funcreader: FunctionBody,
        body_range: wasmparser::Range,
        function_data: &[u8],
    ) -> Result<Function>;

    /// Returns if this mutator can be applied to the opcode at index i
    fn can_mutate<'a>(
        &self,
        config: &'a WasmMutate,
        operators: &Vec<TupleType<'a>>,
        at: usize,
    ) -> Result<bool>;

    /// Provides the name of the mutator, mostly used for debugging purposes
    fn name(&self) -> String {
        return format!("{:?}", std::any::type_name::<Self>());
    }
}

// This macro is meant to be used for testing deep mutators
// It receives the original wat text variable, the expression returning the mutated function and the expected wat
// For an example, look at SwapCommutativeOperator
#[cfg(test)]
#[macro_export]
macro_rules! match_code_mutation {
    ($wat: ident, $mutation:expr, $expected:ident) => {{
        let original = &wat::parse_str($wat).unwrap();

        let mut parser = Parser::new(0);
        let config = WasmMutate::default();

        let mut offset = 0;

        let mut modu = Module::new();
        let mut codesection = CodeSection::new();

        loop {
            let (payload, chunksize) = match parser.parse(&original[offset..], true).unwrap() {
                Chunk::NeedMoreData(_) => {
                    panic!("This should not be reached");
                }
                Chunk::Parsed { consumed, payload } => (payload, consumed),
            };
            offset += chunksize;

            match payload {
                Payload::TypeSection(reader) => {
                    modu.section(&RawSection {
                        id: SectionId::Type.into(),
                        data: &original[reader.range().start..reader.range().end],
                    });
                }
                Payload::FunctionSection(reader) => {
                    modu.section(&RawSection {
                        id: SectionId::Function.into(),
                        data: &original[reader.range().start..reader.range().end],
                    });
                }
                Payload::ExportSection(reader) => {
                    modu.section(&RawSection {
                        id: SectionId::Export.into(),
                        data: &original[reader.range().start..reader.range().end],
                    });
                }
                Payload::CodeSectionEntry(reader) => {
                    let operatorsreader = reader.get_operators_reader().unwrap();
                    let range = operatorsreader.reader.range();
                    let operators = operatorsreader
                        .into_iter_with_offsets()
                        .collect::<wasmparser::Result<Vec<TupleType>>>()
                        .unwrap();
                    let mutated = $mutation(&config, operators, reader, range, original);
                    codesection.function(&mutated);
                }
                wasmparser::Payload::End => break,
                _ => {
                    // do nothing
                }
            }
        }
        modu.section(&codesection);
        let mutated = modu.finish();
        let mut validator = wasmparser::Validator::new();
        crate::validate(&mut validator, &mutated);

        let text = wasmprinter::print_bytes(mutated).unwrap();

        // parse expected to use the same formatter
        let expected_bytes = &wat::parse_str($expected).unwrap();
        let expectedtext = wasmprinter::print_bytes(expected_bytes).unwrap();

        assert_eq!(text, expectedtext);
    }};
}

#[cfg(test)]
mod tests {
    use crate::{
        mutators::{peephole::PeepholeMutator, Mutator},
        WasmMutate,
    };
    use rand::{rngs::SmallRng, SeedableRng};

    #[test]
    fn test_peephole_mutator() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
                i32.const 42
                i32.add
                i32.const 56
                i32.add
            )
            (func (export "exported_func3") (result i32) (local i32 i32)
                i32.const 42
                i32.const 42
                i32.add
            )
        )
        "#;

        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(wat).unwrap();

        let mutator = PeepholeMutator; // the string is empty

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        assert_eq!(can_mutate, true);
    }
}
