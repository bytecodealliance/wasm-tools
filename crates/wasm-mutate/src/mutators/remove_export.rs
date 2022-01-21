//! Mutator that removes a random prexisting export

use super::Mutator;
use crate::{Result, WasmMutate};

use rand::Rng;
use wasm_encoder::{Export, ExportSection, Module};
use wasmparser::ExportSectionReader;

/// Mutator that removes a random prexisting export
#[derive(Clone, Copy)]
pub struct RemoveExportMutator;

impl Mutator for RemoveExportMutator {
    fn mutate<'a>(
        self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let mut exports = ExportSection::new();
        let mut reader = ExportSectionReader::new(config.info().get_exports_section().data, 0)?;
        let max_exports = reader.get_count() as u64;
        let skip_at = config.rng().gen_range(0, max_exports);

        for i in 0..max_exports {
            config.consume_fuel(1)?;
            let export = reader.read().unwrap();

            if skip_at == i {
                log::trace!("Removing export {:?} at index {}", export, skip_at);
                continue;
            }

            match export.kind {
                wasmparser::ExternalKind::Function => {
                    exports.export(export.field, Export::Function(export.index));
                }
                wasmparser::ExternalKind::Table => {
                    exports.export(export.field, Export::Table(export.index));
                }
                wasmparser::ExternalKind::Memory => {
                    exports.export(export.field, Export::Memory(export.index));
                }
                wasmparser::ExternalKind::Global => {
                    exports.export(export.field, Export::Global(export.index));
                }
                wasmparser::ExternalKind::Module => {
                    exports.export(export.field, Export::Module(export.index));
                }
                wasmparser::ExternalKind::Instance => {
                    exports.export(export.field, Export::Instance(export.index));
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
