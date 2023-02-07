//! Mutator that removes a random preexisting export

use super::Mutator;
use crate::{Result, WasmMutate};
use rand::Rng;
use wasm_encoder::{ExportKind, ExportSection, Module};
use wasmparser::ExportSectionReader;

/// Mutator that removes a random preexisting export
#[derive(Clone, Copy)]
pub struct RemoveExportMutator;

impl Mutator for RemoveExportMutator {
    fn mutate<'a>(
        &self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let mut exports = ExportSection::new();
        let reader = ExportSectionReader::new(config.info().get_exports_section().data, 0)?;
        let max_exports = u64::from(reader.count());
        let skip_at = config.rng().gen_range(0..max_exports);

        for (i, export) in reader.into_iter().enumerate() {
            let export = export?;
            config.consume_fuel(1)?;

            if skip_at == i as u64 {
                log::trace!("Removing export {:?} at index {}", export, skip_at);
                continue;
            }

            match export.kind {
                wasmparser::ExternalKind::Func => {
                    exports.export(export.name, ExportKind::Func, export.index);
                }
                wasmparser::ExternalKind::Table => {
                    exports.export(export.name, ExportKind::Table, export.index);
                }
                wasmparser::ExternalKind::Memory => {
                    exports.export(export.name, ExportKind::Memory, export.index);
                }
                wasmparser::ExternalKind::Global => {
                    exports.export(export.name, ExportKind::Global, export.index);
                }
                _ => {
                    panic!("Unknown export {:?}", export)
                }
            }
        }

        Ok(Box::new(std::iter::once(Ok(config
            .info()
            .replace_section(
                config.info().exports.unwrap(),
                &exports,
            )))))
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate) -> bool {
        !config.preserve_semantics && config.info().has_exports() && config.info().exports_count > 0
    }
}

#[cfg(test)]
mod tests {
    use super::RemoveExportMutator;

    #[test]
    fn test_remove_export_mutator() {
        crate::mutators::match_mutation(
            r#"
            (module
                (func (export "exported_func") (result i32)
                    i32.const 42
                )
            )
            "#,
            RemoveExportMutator,
            r#"
                (module  (type (;0;)
                 (func (result i32)))
                 (func (;0;) (type 0) (result i32)
                    i32.const 42))"#,
        );
    }
}
