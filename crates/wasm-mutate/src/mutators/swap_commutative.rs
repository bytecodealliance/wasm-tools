use rand::{Rng, prelude::SmallRng};
use wasm_encoder::{CodeSection, Function, Instruction, Module, ValType};
use wasmparser::{CodeSectionReader, Operator};

use crate::{Error, Result, ModuleInfo, WasmMutate, module::*};

use super::{Mutator};


pub struct SwapCommutativeOperator;

impl SwapCommutativeOperator {

    fn is_commutative(&self, op: &Operator) -> bool {
        match op {
            Operator::I32Add | Operator::I32Mul => {
                // TODO do the others
                true
            }
            _ => {
                false
            }
        }
    }

    fn get_operator_type(&self, op: &Operator) -> ValType {
        match op {
            Operator::I32Add | Operator::I32Mul => {
                // TODO do the others
                ValType::I32
            }
            _ => {
                // TODO as err
                panic!("Unknown return type")
            }
        }
    }

    fn get_mutable_ops(&self, info: &ModuleInfo) -> super::Result<Vec<(u32, u32, ValType)>>{
        if !info.has_code() {
            return Err(Error::NoMutationsAplicable)
        }
        // Check is some function body has commutative operators
        // Save where this operators are in the AST ?
        let code_section = info.get_code_section();
        let mut reader = CodeSectionReader::new(code_section.data, 0)?;
        let f = reader.read().unwrap();
        let opcodereader  = f.get_operators_reader()?;
        let mut idx = 0;
        let mut v = Vec::new();

        for op in opcodereader{
            let borrowed = op.unwrap();
            if self.is_commutative(&borrowed){
                v.push((0, idx, self.get_operator_type(&borrowed)))

            }
            idx += 1;
        }

        Ok(v)
    }
}

impl Mutator for SwapCommutativeOperator {
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
        let commutative_operators = self.get_mutable_ops(info)?;
        let operation_to_mutate = rnd.gen_range(0, commutative_operators.len());
        let (fidx, opidx, ity) = commutative_operators[operation_to_mutate];


        (0..count).for_each(|i| {
            let f = reader.read().unwrap();

            if i == fidx {
                log::debug!("Changing function idx {:?}", i);
                let mut localreader = f.get_locals_reader().unwrap();

                // Get current locals and map to encoder types
                let mut local_count = 0;
                let mut current_locals = (0..localreader.get_count()).map(|f|{
                    let (count, ty) = localreader.read().unwrap();
                    local_count += count;
                    (count, map_type(ty).unwrap())
                }).collect::<Vec<(u32, ValType)>>();

                // add two temporary locals, the last two
                current_locals.push((2, ity));

                let mut newf = Function::new(current_locals);
                let opreader= f.get_operators_reader().unwrap();
                let mut idx = 0;

                for op in opreader{

                    let borrowed = op.unwrap();
                    if idx == opidx {
                        // Inject new code to swap operands
                        newf.instruction(Instruction::LocalTee(local_count + 1));
                        newf.instruction(Instruction::LocalTee(local_count + 2));
                    } 
                    newf.instruction(map_operator(borrowed).unwrap());
                    idx += 1
                }

                codes.function(&newf);
            } else {
                codes.raw(&info.input_wasm[f.range().start..f.range().end]);
            }
        });
        Ok(info.replace_section(info.code.unwrap(), &codes))
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate, info: &ModuleInfo) -> Result<bool> {
        Ok(info.has_code() && self.get_mutable_ops(info)?.len() != 0) // && self.commutative_op_locations.len() > 0)
    }
}


#[cfg(test)]
mod tests {
    use rand::{rngs::SmallRng, SeedableRng};
    use wasm_encoder::ValType;

    use crate::{WasmMutate, mutators::swap_commutative::SwapCommutativeOperator};

    use super::{Mutator};


    #[test]
    fn test_commutative_op_mutator() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
                i32.const 76
                i32.add
            )
        )
        "#;

        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(wat).unwrap();

        let mutator = SwapCommutativeOperator; // the string is empty

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        assert_eq!(can_mutate, true);

        let mut rnd = SmallRng::seed_from_u64(0);
        let mutation = mutator.mutate(&wasmmutate, &mut rnd, &mut info);

        let mutation_bytes = mutation.unwrap().finish();

        // If it fails, it is probably an invalid
        let text = wasmprinter::print_bytes(mutation_bytes).unwrap();
        assert_eq!("(module\n  (type (;0;) (func (result i32)))\n  (func (;0;) (type 0) (result i32)\n    (local i32 i32 i32 i32)\n    i32.const 42\n    i32.const 76\n    local.tee 3\n    local.tee 4\n    i32.add)\n  (export \"exported_func\" (func 0)))", text)
    }


    #[test]
    fn test_commutative_op_mutator_idempotent() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 76
            )
        )
        "#;

        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(wat).unwrap();

        let mutator = SwapCommutativeOperator; // the string is empty

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        assert_eq!(can_mutate, false); // No commutative operations
    }
}
