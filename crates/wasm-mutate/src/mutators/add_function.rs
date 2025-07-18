//! Mutator that adds new, empty functions.

use super::Mutator;
use crate::module::{PrimitiveTypeInfo, TypeInfo};
use crate::{Result, WasmMutate};
use rand::Rng;
use wasm_encoder::{AbstractHeapType, HeapType, Module};

/// Mutator that adds new, empty functions to a Wasm module.
#[derive(Clone, Copy)]
pub struct AddFunctionMutator;

impl Mutator for AddFunctionMutator {
    fn mutate<'a>(
        &self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let max_ty_idx = config.info().num_types() - 1;
        let ty_idx = config.rng().random_range(0..=max_ty_idx);

        // (Re)encode the function section and add this new entry.
        let mut func_sec_enc = wasm_encoder::FunctionSection::new();
        if let Some(func_sec_idx) = config.info().functions {
            let reader = config.info().get_binary_reader(func_sec_idx);
            let reader = wasmparser::FunctionSectionReader::new(reader)?;
            for x in reader {
                func_sec_enc.function(x?);
            }
        }
        func_sec_enc.function(ty_idx);

        // Copy the existing code bodies over and then add a new dummy body for
        // this function.
        let mut code_sec_enc = wasm_encoder::CodeSection::new();
        if let Some(code_sec_idx) = config.info().code {
            let reader = config.info().get_binary_reader(code_sec_idx);
            let reader = wasmparser::CodeSectionReader::new(reader)?;
            for body in reader {
                let body = body?;
                code_sec_enc.raw(body.as_bytes());
            }
        }
        let func_ty = match &config.info().types_map[usize::try_from(ty_idx).unwrap()] {
            TypeInfo::Func(func_ty) => func_ty,
        };
        let mut func = wasm_encoder::Function::new(vec![]);
        for ty in &func_ty.returns {
            match ty {
                PrimitiveTypeInfo::I32 => {
                    func.instructions().i32_const(0);
                }
                PrimitiveTypeInfo::I64 => {
                    func.instructions().i64_const(0);
                }
                PrimitiveTypeInfo::F32 => {
                    func.instructions().f32_const(0.0.into());
                }
                PrimitiveTypeInfo::F64 => {
                    func.instructions().f64_const(0.0.into());
                }
                PrimitiveTypeInfo::V128 => {
                    func.instructions().v128_const(0);
                }
                PrimitiveTypeInfo::FuncRef => {
                    func.instructions().ref_null(HeapType::Abstract {
                        shared: false,
                        ty: AbstractHeapType::Func,
                    });
                }
                PrimitiveTypeInfo::ExternRef => {
                    func.instructions().ref_null(HeapType::Abstract {
                        shared: false,
                        ty: AbstractHeapType::Extern,
                    });
                }
                PrimitiveTypeInfo::Empty => unreachable!(),
            }
        }
        func.instructions().end();
        code_sec_enc.function(&func);

        let module = if config.info().functions.is_some() {
            // Replace the old sections with the new ones.
            config
                .info()
                .replace_multiple_sections(|_, sec_id, module| match sec_id {
                    x if x == wasm_encoder::SectionId::Function as u8 => {
                        module.section(&func_sec_enc);
                        true
                    }
                    x if x == wasm_encoder::SectionId::Code as u8 => {
                        module.section(&code_sec_enc);
                        true
                    }
                    _ => false,
                })
        } else {
            // Insert the new sections in their respective places.
            let mut added_func = false;
            let mut added_code = false;
            let mut module = config
                .info()
                .replace_multiple_sections(|_, sec_id, module| {
                    if !added_func && sec_id >= wasm_encoder::SectionId::Function as u8 {
                        module.section(&func_sec_enc);
                        added_func = true;
                    }

                    if !added_code
                        && sec_id >= wasm_encoder::SectionId::Code as u8
                        && sec_id != wasm_encoder::SectionId::DataCount as u8
                    {
                        module.section(&code_sec_enc);
                        added_code = true;
                    }

                    sec_id == wasm_encoder::SectionId::Function as u8
                        || sec_id == wasm_encoder::SectionId::Code as u8
                });
            if !added_func {
                module.section(&func_sec_enc);
            }
            if !added_code {
                module.section(&code_sec_enc);
            }
            module
        };

        Ok(Box::new(std::iter::once(Ok(module))))
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate) -> bool {
        // Note: adding a new, never-called function preserves semantics so we
        // don't need to gate on whether `config.preserve_semantics` is set or
        // not.
        !config.reduce && config.info().num_types() > 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_first_function() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (type (;0;) (func (param i32 f32) (result i64)))
                )
            "#,
            AddFunctionMutator,
            r#"
                (module
                    (type (;0;) (func (param i32 f32) (result i64)))
                    (func (;0;) (type 0) (param i32 f32) (result i64)
                      i64.const 0)
                )
            "#,
        );
    }

    #[test]
    fn test_add_another_function() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (type (;0;) (func (param i32 f32) (result i64 f64)))
                    (func (;0;) (type 0) (param i32 f32) (result i64 f64)
                        i64.const 0
                        f64.const 0.0
                    )
                )
            "#,
            AddFunctionMutator,
            r#"
                (module
                    (type (;0;) (func (param i32 f32) (result i64 f64)))
                    (func (;0;) (type 0) (param i32 f32) (result i64 f64)
                        i64.const 0
                        f64.const 0.0
                    )
                    (func (;0;) (type 0) (param i32 f32) (result i64 f64)
                        i64.const 0
                        f64.const 0.0
                    )
                )
            "#,
        );
    }
}
