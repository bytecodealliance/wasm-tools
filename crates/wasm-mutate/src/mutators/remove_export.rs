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
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
        (module
            (func (export "exported_func") (result i32)
                i32.const 42
            )
        )
        "#;

        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(wat).unwrap();

        let mutator = RemoveExportMutator {};

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        assert_eq!(can_mutate, true);

        let mut rnd = SmallRng::seed_from_u64(0);
        let mutation = mutator.mutate(&wasmmutate, &mut rnd, &mut info);

        let mutation_bytes = mutation.unwrap().finish();

        // validate
        let mut validator = wasmparser::Validator::new();
        crate::validate(&mut validator, &mutation_bytes);
        // If it fails, it is probably an invalid
        let text = wasmprinter::print_bytes(mutation_bytes).unwrap();
        assert_eq!("(module\n  (type (;0;) (func (result i32)))\n  (func (;0;) (type 0) (result i32)\n    i32.const 42))", text)
    }
}
