use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, rc::Rc};

use rand::{
    Rng,
};
use wasm_encoder::{Module};
use wasmparser::{BinaryReaderError, CodeSectionReader, FunctionBody, Operator, Range};
use crate::{ModuleInfo, Result, WasmMutate};

use super::Mutator;

mod ir;

pub struct CodemotionMutator;

impl CodemotionMutator {
    
    
}

/// Meta mutator for peephole
impl Mutator for CodemotionMutator {
    fn mutate(
        &self,
        config: &crate::WasmMutate,
        rnd: &mut rand::prelude::SmallRng,
        info: &crate::ModuleInfo,
    ) -> Result<Module> {
        
        /*
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
            let operatorsrange = Range::default(); // TODO change
            let operators = operatorreader
                .into_iter_with_offsets()
                .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()?;
            let operatorscount = operators.len();
            let opcode_to_mutate = rnd.gen_range(0, operatorscount);

            let bbs = self.collect_bbs(&operators)?;
        } */

        todo!();
    }

    fn can_mutate<'a>(
        &self,
        config: &'a crate::WasmMutate,
        info: &crate::ModuleInfo,
    ) -> bool {
        info.has_code() && info.function_count > 0
    }
}



#[cfg(test)]
mod tests {
    use crate::{WasmMutate, mutators::{Mutator,  peephole::PeepholeMutator}};
    use rand::{rngs::SmallRng, SeedableRng};

    #[test]
    fn test_code_motion_mutator() {
       /*// From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
                i32.const 42
                if 
                    i32.const 50
                    if 
                        i32.const 45
                    else
                        i32.const 60
                else
                    i32.const 60
            )
        )
        "#;

        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(wat).unwrap();

        let mutator = CodemotionMutator; // the string is empty

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        assert_eq!(can_mutate, true);
        let mut rnd = SmallRng::seed_from_u64(0);

        mutator.mutate(&wasmmutate, &mut rnd, &mut info);*/
    }
}
