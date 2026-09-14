//! Mutator that adds a new global.

use super::Mutator;
use crate::{Result, WasmMutate};
use rand::RngExt;
use wasm_encoder::reencode::{Reencode, RoundtripReencoder};
use wasm_encoder::{ConstExpr, GlobalSection, GlobalType, Module, ValType};

/// Mutator that appends a new, zero-initialized global to a Wasm module.
#[derive(Clone, Copy)]
pub struct AddGlobalMutator;

impl Mutator for AddGlobalMutator {
    fn mutate<'a>(
        &self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let (val_type, init) = match config.rng().random_range(0..5) {
            0 => (ValType::I32, ConstExpr::i32_const(0)),
            1 => (ValType::I64, ConstExpr::i64_const(0)),
            2 => (ValType::F32, ConstExpr::f32_const(0.0.into())),
            3 => (ValType::F64, ConstExpr::f64_const(0.0.into())),
            4 => (ValType::V128, ConstExpr::v128_const(0)),
            _ => unreachable!(),
        };
        let ty = GlobalType {
            val_type,
            mutable: config.rng().random(),
            shared: false,
        };

        let mut globals = GlobalSection::new();
        if let Some(section) = config.info().globals {
            let reader = config.info().get_binary_reader(section);
            let reader = wasmparser::GlobalSectionReader::new(reader)?;
            RoundtripReencoder.parse_global_section(&mut globals, reader)?;
        }
        globals.global(ty, &init);

        let module = if let Some(section) = config.info().globals {
            config.info().replace_section(section, &globals)
        } else {
            // Globals appear after memories and before exports in the canonical
            // section order. Find the first possible following section, or
            // append when the module has no such section.
            let insertion_index = config
                .info()
                .exports
                .or(config.info().start)
                .or(config.info().elements)
                .or(config.info().data_count)
                .or(config.info().code)
                .or(config.info().data)
                .unwrap_or(config.info().raw_sections.len());
            config.info().insert_section(insertion_index, &globals)
        };

        Ok(Box::new(std::iter::once(Ok(module))))
    }

    fn can_mutate(&self, config: &WasmMutate) -> bool {
        // An unreferenced global does not change observable module behavior.
        !config.reduce
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_global_to_empty_module() {
        crate::mutators::match_mutation(
            "(module)",
            AddGlobalMutator,
            r#"
                (module
                    (global (;0;) (mut f64) f64.const 0x0p+0)
                )
            "#,
        );
    }

    #[test]
    fn append_global_to_existing_section() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (global i32 i32.const 42)
                )
            "#,
            AddGlobalMutator,
            r#"
                (module
                    (global (;0;) i32 i32.const 42)
                    (global (;1;) (mut f64) f64.const 0x0p+0)
                )
            "#,
        );
    }

    #[test]
    fn insert_global_before_export() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (func (export "f"))
                )
            "#,
            AddGlobalMutator,
            r#"
                (module
                    (type (;0;) (func))
                    (func (;0;) (type 0))
                    (global (;0;) (mut f64) f64.const 0x0p+0)
                    (export "f" (func 0))
                )
            "#,
        );
    }

    #[test]
    fn applicability() {
        let wasm = wat::parse_str("(module)").unwrap();

        let mut config = WasmMutate::default();
        config.setup(&wasm).unwrap();
        assert!(AddGlobalMutator.can_mutate(&config));

        config.reduce(true);
        assert!(!AddGlobalMutator.can_mutate(&config));
    }
}
