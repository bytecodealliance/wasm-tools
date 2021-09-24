use super::Mutator;
use crate::{ModuleInfo, Result, WasmMutate};
use rand::prelude::SmallRng;
use rand::{Rng, RngCore};
use wasm_encoder::{CodeSection, Export, ExportSection, Function, Instruction, Module};
use wasmparser::{CodeSectionReader, ExportSectionReader};

pub struct RemoveExportMutator;

impl Mutator for RemoveExportMutator {
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        info: &mut ModuleInfo,
    ) -> Result<Module> {
        let mut exports = ExportSection::new();
        let mut reader = ExportSectionReader::new(info.get_exports_section().data, 0)?;
        let max_exports = reader.get_count() as u64;
        let skip_at = rnd.gen_range(0, max_exports);

        (0..max_exports).for_each(|i| {
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
        });
        Ok(info.replace_section(info.exports.unwrap(), &exports))
    }

    fn can_mutate<'a>(&self, _: &'a WasmMutate, info: &ModuleInfo) -> Result<bool> {
        Ok(info.has_exports() && info.exports_count > 0)
    }
}

#[cfg(test)]
mod tests {
    use crate::WasmMutate;
    use rand::{rngs::SmallRng, SeedableRng};

    use super::{Mutator, RemoveExportMutator};

    #[test]
    fn test_remove_export_mutator() {
        crate::match_mutation!(
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
                    i32.const 42))"#
        );
    }
}
