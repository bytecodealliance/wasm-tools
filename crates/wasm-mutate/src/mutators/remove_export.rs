//! Mutator that removes a random prexisting export

use super::Mutator;
use crate::{ModuleInfo, Result, WasmMutate};
use rand::prelude::SmallRng;
use rand::Rng;
use wasm_encoder::{Export, ExportSection, Module};
use wasmparser::ExportSectionReader;

/// Mutator that removes a random prexisting export
pub struct RemoveExportMutator;

impl Mutator for RemoveExportMutator {
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        info: &ModuleInfo,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>>>> {
        let mut exports = ExportSection::new();
        let mut reader = ExportSectionReader::new(info.get_exports_section().data, 0)?;
        let max_exports = reader.get_count() as u64;
        let skip_at = rnd.gen_range(0, max_exports);

        (0..max_exports)
            .map(|i| {
                config.consume_fuel(1)?;
                let export = reader.read().unwrap();
                if skip_at != i {
                    // otherwise bypass
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
                } else {
                    log::debug!("Removing export {:?} idx {:?}", export, skip_at);
                }
                Ok(())
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Box::new(std::iter::once(Ok(
            info.replace_section(info.exports.unwrap(), &exports)
        ))))
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate, info: &ModuleInfo) -> bool {
        !config.preserve_semantics && info.has_exports() && info.exports_count > 0
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
            &RemoveExportMutator,
            r#"
                (module  (type (;0;)
                 (func (result i32)))
                 (func (;0;) (type 0) (result i32)
                    i32.const 42))"#,
        );
    }
}
