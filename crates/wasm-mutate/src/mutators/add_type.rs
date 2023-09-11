//! A mutator to add a new type to a Wasm module.

use super::Mutator;
use crate::module::map_type;
use crate::Result;
use rand::Rng;
use std::iter;

/// A mutator that appends a new type to the type section.
///
/// This mutator will create the type section if none exists.
#[derive(Clone, Copy)]
pub struct AddTypeMutator {
    pub(crate) max_params: usize,
    pub(crate) max_results: usize,
}

impl AddTypeMutator {
    fn random_valtype(&self, rng: &mut impl Rng) -> wasm_encoder::ValType {
        match rng.gen_range(0..=6) {
            0 => wasm_encoder::ValType::I32,
            1 => wasm_encoder::ValType::I64,
            2 => wasm_encoder::ValType::F32,
            3 => wasm_encoder::ValType::F64,
            4 => wasm_encoder::ValType::V128,
            5 => wasm_encoder::ValType::EXTERNREF,
            6 => wasm_encoder::ValType::FUNCREF,
            _ => unreachable!(),
        }
    }
}

impl Mutator for AddTypeMutator {
    fn can_mutate(&self, config: &crate::WasmMutate) -> bool {
        !config.reduce
    }

    fn mutate<'a>(
        &self,
        config: &'a mut crate::WasmMutate,
    ) -> crate::Result<Box<dyn Iterator<Item = crate::Result<wasm_encoder::Module>> + 'a>> {
        let count = config.rng().gen_range(0..=self.max_params);
        let mut params = Vec::with_capacity(count);
        for _ in 0..count {
            params.push(self.random_valtype(config.rng()));
        }

        let count = config.rng().gen_range(0..=self.max_results);
        let mut results = Vec::with_capacity(count);
        for _ in 0..count {
            results.push(self.random_valtype(config.rng()));
        }

        let mut types = wasm_encoder::TypeSection::new();
        if let Some(old_types) = config.info().get_type_section() {
            // Copy the existing types section over into the encoder.
            let reader = wasmparser::TypeSectionReader::new(old_types.data, 0)?;
            for ty in reader.into_iter_err_on_gc_types() {
                let ty = ty?;
                let params = ty
                    .params()
                    .iter()
                    .copied()
                    .map(map_type)
                    .collect::<Result<Vec<_>, _>>()?;
                let results = ty
                    .results()
                    .iter()
                    .copied()
                    .map(map_type)
                    .collect::<Result<Vec<_>, _>>()?;
                types.function(params, results);
            }
            // And then add our new type.
            types.function(params, results);
            let types_section_index = config.info().types.unwrap();
            Ok(Box::new(iter::once(Ok(config
                .info()
                .replace_section(types_section_index, &types)))))
        } else {
            types.function(params, results);
            Ok(Box::new(iter::once(Ok(config
                .info()
                .insert_section(0, &types)))))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_type_to_empty_module() {
        crate::mutators::match_mutation(
            r#"
                (module)
            "#,
            AddTypeMutator {
                max_params: 1,
                max_results: 1,
            },
            r#"
                (module
                    (type (;0;) (func (param i32) (result i64)))
                )
            "#,
        );
    }

    #[test]
    fn add_type_to_non_empty_module() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (type (;0;) (func (param i32) (result i64)))
                )
            "#,
            AddTypeMutator {
                max_params: 1,
                max_results: 1,
            },
            r#"
                (module
                    (type (;0;) (func (param i32) (result i64)))
                    (type (;0;) (func (param i64) (result i32)))
                )
            "#,
        );
    }

    #[test]
    fn add_type_with_custom_section_first() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (@custom "HI" (before type) "xxx")
                    (type (;0;) (func (param i32) (result i64)))
                )
            "#,
            AddTypeMutator {
                max_params: 1,
                max_results: 1,
            },
            r#"
                (module
                    (@custom "HI" (before type) "xxx")
                    (type (;0;) (func (param i32) (result i64)))
                    (type (;0;) (func (param i64) (result i32)))
                )
            "#,
        );
    }
}
