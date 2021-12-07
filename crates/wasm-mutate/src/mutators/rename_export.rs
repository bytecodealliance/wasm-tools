//! Mutator that generates a random renaming of a prexisting export
use super::Mutator;
use crate::{Result, WasmMutate};

use rand::{Rng, RngCore};
use wasm_encoder::{Export, ExportSection, Module};
use wasmparser::ExportSectionReader;

/// Generates a random renaming of pre-existing exports.
///
/// The export entry is selected randomly and then a new `field` name is
/// generated.
#[derive(Clone, Copy)]
pub struct RenameExportMutator {
    /// The maximum length of the generated export entry
    pub max_name_size: u32,
}

impl RenameExportMutator {
    /// Copied and transformed from wasm-smith name generation
    fn limited_string(
        &self,
        config: &mut WasmMutate,
        max_name_size: u32,
        result: &mut String,
    ) -> crate::Result<()> {
        let size = config.rng().gen_range(1, max_name_size);
        let mut str = vec![0u8; size as usize];

        if let Some(fillfunc) = &config.raw_mutate_func {
            fillfunc(&mut str)?;
        } else {
            config.rng().fill_bytes(&mut str);
        }

        match std::str::from_utf8(&str) {
            Ok(s) => result.push_str(s),
            Err(e) => {
                let i = e.valid_up_to();
                let valid = &str[0..i];
                let s = unsafe {
                    debug_assert!(std::str::from_utf8(valid).is_ok());
                    std::str::from_utf8_unchecked(valid)
                };
                result.push_str(s);
            }
        };

        // Add one symbol at a time until it is not contained in the export field names
        while config.info().export_names.contains(result) {
            let _ = &self.limited_string(config, 2, result)?;
        }

        Ok(())
    }
}

impl Mutator for RenameExportMutator {
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

            let new_name = if skip_at != i {
                // otherwise bypass
                String::from(export.field)
            } else {
                let mut new_name = String::default();
                self.limited_string(config, self.max_name_size, &mut new_name)?;
                log::debug!("Renaming export {:?} by {:?}", export, new_name);
                new_name
            };

            match export.kind {
                wasmparser::ExternalKind::Function => {
                    exports.export(new_name.as_str(), Export::Function(export.index));
                }
                wasmparser::ExternalKind::Table => {
                    exports.export(new_name.as_str(), Export::Table(export.index));
                }
                wasmparser::ExternalKind::Memory => {
                    exports.export(new_name.as_str(), Export::Memory(export.index));
                }
                wasmparser::ExternalKind::Global => {
                    exports.export(new_name.as_str(), Export::Global(export.index));
                }
                wasmparser::ExternalKind::Module => {
                    exports.export(new_name.as_str(), Export::Module(export.index));
                }
                wasmparser::ExternalKind::Instance => {
                    exports.export(new_name.as_str(), Export::Instance(export.index));
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

    use super::RenameExportMutator;

    #[test]
    fn test_rename_export_mutator() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm

        crate::mutators::match_mutation(
            r#"
        (module
            (func (export "exported_func") (result i32)
                i32.const 42
            )
        )
        "#,
            RenameExportMutator { max_name_size: 2 }, // the string is empty,
            r#"(module
            (type (;0;) (func (result i32)))
            (func (;0;) (type 0) (result i32)
            i32.const 42)
        (export "" (func 0)))"#,
        );
    }
}
