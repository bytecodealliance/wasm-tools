use crate::{
    module::map_type,
    mutators::{
        codemotion::{
            ir::AstBuilder,
            mutators::{if_complement::IfComplementMutator, loop_unrolling::LoopUnrollMutator},
        },
        OperatorAndByteOffset,
    },
    Error, Result,
};
use rand::{prelude::SliceRandom, Rng};
use wasm_encoder::{CodeSection, Function, Module, ValType};
use wasmparser::{CodeSectionReader, FunctionBody};

use self::ir::parse_context::Ast;

use super::Mutator;
// Hack to show debug messages in tests
#[cfg(not(test))]
use log::debug;
#[cfg(test)]
use std::println as debug;

mod ir;
mod mutators;

pub struct CodemotionMutator;

impl CodemotionMutator {
    fn copy_locals(&self, reader: FunctionBody) -> Result<Vec<(u32, ValType)>> {
        // Create the new function
        let mut localreader = reader.get_locals_reader()?;
        // Get current locals and map to encoder types
        let mut local_count = 0;
        let current_locals = (0..localreader.get_count())
            .map(|_| {
                let (count, ty) = localreader.read().unwrap();
                local_count += count;
                (count, map_type(ty).unwrap())
            })
            .collect::<Vec<(u32, ValType)>>();

        Ok(current_locals)
    }

    fn random_mutate(
        &self,
        config: &crate::WasmMutate,
        rnd: &mut rand::prelude::SmallRng,
        info: &crate::ModuleInfo,
        mutators: &[Box<dyn AstMutator>],
    ) -> crate::Result<(Function, u32)> {
        let original_code_section = info.get_code_section();
        let mut sectionreader = CodeSectionReader::new(original_code_section.data, 0)?;
        let function_count = sectionreader.get_count();

        // This split strategy will avoid very often mutating the first function
        // and very rarely mutating the last function
        let function_to_mutate = rnd.gen_range(0, function_count);
        let all_readers = (0..function_count)
            .map(|_| sectionreader.read().unwrap())
            .collect::<Vec<FunctionBody>>();

        for fidx in (function_to_mutate..function_count).chain(0..function_to_mutate) {
            config.consume_fuel(1)?;
            let reader = all_readers[fidx as usize];
            let operatorreader = reader.get_operators_reader()?;

            let operators = operatorreader
                .into_iter_with_offsets()
                .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()?;

            // build Ast
            let ast = AstBuilder.build_ast(&operators)?;
            // filter mutators by those applicable
            let filtered = mutators
                .iter()
                .filter(|m| m.can_mutate(config, info, &ast))
                .collect::<Vec<_>>();
            // If no mutator, just continue to the next function
            if filtered.is_empty() {
                continue;
            }

            match filtered.choose(rnd) {
                Some(choosen_mutator) => {
                    let newfunc = choosen_mutator.mutate(
                        config,
                        info,
                        rnd,
                        &ast,
                        &self.copy_locals(reader)?,
                        &operators,
                        original_code_section.data,
                    )?;
                    return Ok((newfunc, fidx));
                }
                None => continue,
            }
        }

        Err(Error::NoMutationsApplicable)
    }
}
/// Trait to be implemented by all code motion mutators
pub trait AstMutator {
    fn mutate<'a>(
        &self,
        config: &'a crate::WasmMutate,
        info: &crate::ModuleInfo,
        rnd: &mut rand::prelude::SmallRng,
        ast: &Ast,
        locals: &[(u32, ValType)],
        operators: &Vec<OperatorAndByteOffset>,
        input_wasm: &'a [u8],
    ) -> Result<Function>;

    fn can_mutate<'a>(
        &self,
        config: &'a crate::WasmMutate,
        info: &crate::ModuleInfo,
        ast: &Ast,
    ) -> bool;
}

/// Meta mutator for peephole
impl Mutator for CodemotionMutator {
    fn mutate(
        &self,
        config: &crate::WasmMutate,
        rnd: &mut rand::prelude::SmallRng,
        info: &crate::ModuleInfo,
    ) -> Result<Module> {
        // Initialize mutators
        let mutators: Vec<Box<dyn AstMutator>> = vec![
            Box::new(IfComplementMutator),
            Box::new(LoopUnrollMutator), // Add the other here
        ];

        let (newfunc, function_to_mutate) = self.random_mutate(config, rnd, info, &mutators)?;

        let mut codes = CodeSection::new();
        let code_section = info.get_code_section();
        let mut sectionreader = CodeSectionReader::new(code_section.data, 0)?;

        for fidx in 0..info.function_count {
            let reader = sectionreader.read()?;
            if fidx == function_to_mutate {
                debug!("Mutating function  idx {:?}", fidx);
                codes.function(&newfunc);
            } else {
                codes.raw(&code_section.data[reader.range().start..reader.range().end]);
            }
        }
        let module = info.replace_section(info.code.unwrap(), &codes);
        Ok(module)
    }

    fn can_mutate<'a>(&self, _: &'a crate::WasmMutate, info: &crate::ModuleInfo) -> bool {
        info.has_code() && info.function_count > 0
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        info::ModuleInfo,
        mutators::{codemotion::CodemotionMutator, Mutator},
        WasmMutate,
    };
    use rand::{rngs::SmallRng, SeedableRng};

    fn test_motion_mutator(original: &str, expected: &str, seed: u64) {
        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(original).unwrap();

        let mutator = CodemotionMutator; // the string is empty

        let mut info = ModuleInfo::new(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info);

        let mut rnd = SmallRng::seed_from_u64(seed);

        assert_eq!(can_mutate, true);

        let mutated = mutator.mutate(&wasmmutate, &mut rnd, &mut info).unwrap();

        let mut validator = wasmparser::Validator::new();
        let mutated_bytes = &mutated.finish();
        let text = wasmprinter::print_bytes(mutated_bytes).unwrap();
        crate::validate(&mut validator, mutated_bytes);
        let expected_bytes = &wat::parse_str(expected).unwrap();
        let expectedtext = wasmprinter::print_bytes(expected_bytes).unwrap();
        assert_eq!(expectedtext, text);
    }

    #[test]
    fn test_if_swap() {
        test_motion_mutator(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                (local i32 i32)
                local.get 0
                if (result i32)
                    i32.const 50
                else
                    i32.const 41
                end
            )
        )
        "#,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                (local i32 i32)
                  local.get 0
                  i32.eqz
                  if (result i32)  ;; label = @1
                    i32.const 41
                  else
                    i32.const 50
                  end)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
            0,
        );
    }

    #[test]
    fn test_if_swap2() {
        test_motion_mutator(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                local.get 0
                local.get 0
                i32.add
                local.get 0
                if 
                    i32.const 150
                    drop
                else
                    i32.const 200
                    drop
                end
                if (result i32)
                    i32.const 50
                else
                    i32.const 41
                end
            )
        )
        "#,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                  local.get 0
                  local.get 0
                  i32.add
                  local.get 0
                  if  ;; label = @1
                    i32.const 150
                    drop
                  else
                    i32.const 200
                    drop
                  end
                  i32.eqz
                  if (result i32)  ;; label = @1
                    i32.const 41
                  else
                    i32.const 50
                  end)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
            1,
        );
    }

    #[test]
    fn test_if_swap3() {
        test_motion_mutator(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                local.get 0
                local.get 0
                i32.add
                local.get 0
                if 
                    i32.const 150
                    drop
                else
                    i32.const 200
                    drop
                end
                if (result i32)
                    i32.const 50
                end
            )
        )
        "#,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                  local.get 0
                  local.get 0
                  i32.add
                  local.get 0
                  if  ;; label = @1
                    i32.const 150
                    drop
                  else
                    i32.const 200
                    drop
                  end
                  i32.eqz
                  if (result i32)  ;; label = @1
                    unreachable
                  else
                    i32.const 50
                  end)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
            1,
        );
    }

    #[test]
    fn test_unrolling1() {
        test_motion_mutator(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                local.get 0
                local.get 0
                i32.add
                drop
                loop
                    i32.const 1
                    local.get 0
                    i32.add
                    local.tee 0
                    i32.const 100
                    i32.le_u
                    br_if 0
                end
                local.get 0
            )
        )
        "#,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                  local.get 0
                  local.get 0
                  i32.add
                  drop
                  block  ;; label = @1
                    block  ;; label = @2
                      i32.const 1
                      local.get 0
                      i32.add
                      local.tee 0
                      i32.const 100
                      i32.le_u
                      br_if 0 (;@2;)
                      br 1 (;@1;)
                    end
                    loop  ;; label = @2
                      i32.const 1
                      local.get 0
                      i32.add
                      local.tee 0
                      i32.const 100
                      i32.le_u
                      br_if 0 (;@2;)
                    end
                  end
                  local.get 0)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))

        "#,
            1,
        );
    }

    #[test]
    fn test_unrolling2() {
        test_motion_mutator(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                local.get 0
                local.get 0
                i32.add
                drop
                loop
                    i32.const 1
                    local.get 0
                    i32.add
                    local.tee 0
                    i32.const 100
                    i32.le_u
                    br_if 0
                    local.get 0
                    if 
                        i32.const 200
                        local.get 0
                        i32.add
                        local.set 0
                    else
                        i32.const 300
                        local.get 0
                        i32.add
                        local.set 0
                    end
                end
                local.get 0
            )
        )
        "#,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                  local.get 0
                  local.get 0
                  i32.add
                  drop
                  block  ;; label = @1
                    block  ;; label = @2
                      i32.const 1
                      local.get 0
                      i32.add
                      local.tee 0
                      i32.const 100
                      i32.le_u
                      br_if 0 (;@2;)
                      local.get 0
                      if  ;; label = @3
                        i32.const 200
                        local.get 0
                        i32.add
                        local.set 0
                      else
                        i32.const 300
                        local.get 0
                        i32.add
                        local.set 0
                      end
                      br 1 (;@1;)
                    end
                    loop  ;; label = @2
                      i32.const 1
                      local.get 0
                      i32.add
                      local.tee 0
                      i32.const 100
                      i32.le_u
                      br_if 0 (;@2;)
                      local.get 0
                      if  ;; label = @3
                        i32.const 200
                        local.get 0
                        i32.add
                        local.set 0
                      else
                        i32.const 300
                        local.get 0
                        i32.add
                        local.set 0
                      end
                    end
                  end
                  local.get 0)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))

        "#,
            1,
        );
    }

    #[test]
    fn test_unrolling3() {
        test_motion_mutator(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                local.get 0
                local.get 0
                i32.add
                drop
                loop
                    i32.const 1
                    local.get 0
                    i32.add
                    local.tee 0
                    if 
                        i32.const 200
                        local.get 0
                        i32.add
                        local.tee 0
                        br_if 1
                    else
                        local.get 0
                        br_if 1
                    end
                    local.get 0
                    br_if 0
                end
                local.get 0
            )
        )
        "#,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                  local.get 0
                  local.get 0
                  i32.add
                  drop
                  block  ;; label = @1
                    block  ;; label = @2
                      i32.const 1
                      local.get 0
                      i32.add
                      local.tee 0
                      if  ;; label = @3
                        i32.const 200
                        local.get 0
                        i32.add
                        local.tee 0
                        br_if 1 (;@2;)
                      else
                        local.get 0
                        br_if 1 (;@2;)
                      end
                      local.get 0
                      br_if 0 (;@2;)
                      br 1 (;@1;)
                    end
                    loop  ;; label = @2
                      i32.const 1
                      local.get 0
                      i32.add
                      local.tee 0
                      if  ;; label = @3
                        i32.const 200
                        local.get 0
                        i32.add
                        local.tee 0
                        br_if 1 (;@2;)
                      else
                        local.get 0
                        br_if 1 (;@2;)
                      end
                      local.get 0
                      br_if 0 (;@2;)
                    end
                  end
                  local.get 0)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))

        "#,
            1,
        );
    }

    #[test]
    fn test_unrolling4() {
        test_motion_mutator(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                block
                    loop
                        loop 
                            local.get 0
                            i32.const 100
                            i32.ge_s 
                            br_if 2
                        end
                        local.get 0
                        i32.const 200
                        i32.le_s 
                        br_if 0
                    end
                end
                local.get 0
            )
        )
        "#,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                  block  ;; label = @1
                    block  ;; label = @2
                      block  ;; label = @3
                        loop  ;; label = @4
                          local.get 0
                          i32.const 100
                          i32.ge_s
                          br_if 3 (;@1;)
                        end
                        local.get 0
                        i32.const 200
                        i32.le_s
                        br_if 0 (;@3;)
                        br 1 (;@2;)
                      end
                      loop  ;; label = @3
                        loop  ;; label = @4
                          local.get 0
                          i32.const 100
                          i32.ge_s
                          br_if 3 (;@1;)
                        end
                        local.get 0
                        i32.const 200
                        i32.le_s
                        br_if 0 (;@3;)
                      end
                    end
                  end
                  local.get 0)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))

        "#,
            1,
        );
    }

    #[test]
    fn test_unrolling5() {
        test_motion_mutator(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                block
                    loop
                        loop 
                            local.get 0
                            br_table 1 2 2 2 2
                        end
                        local.get 0
                        i32.const 200
                        i32.le_s 
                        br_if 0
                    end
                end
                local.get 0
            )
        )
        "#,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                  block  ;; label = @1
                    block  ;; label = @2
                      block  ;; label = @3
                        loop  ;; label = @4
                          local.get 0
                          br_table 1 (;@3;) 3 (;@1;) 3 (;@1;) 3 (;@1;) 3 (;@1;)
                        end
                        local.get 0
                        i32.const 200
                        i32.le_s
                        br_if 0 (;@3;)
                        br 1 (;@2;)
                      end
                      loop  ;; label = @3
                        loop  ;; label = @4
                          local.get 0
                          br_table 1 (;@3;) 3 (;@1;) 3 (;@1;) 3 (;@1;) 3 (;@1;)
                        end
                        local.get 0
                        i32.const 200
                        i32.le_s
                        br_if 0 (;@3;)
                      end
                    end
                  end
                  local.get 0)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
            1,
        );
    }
}
