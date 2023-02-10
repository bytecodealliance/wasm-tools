//! Mutator that generates a random renaming of a preexisting export
use super::Mutator;
use crate::{Result, WasmMutate};
use rand::Rng;
use wasm_encoder::{ExportKind, ExportSection, Module};
use wasmparser::ExportSectionReader;

/// Generates a random renaming of pre-existing exports.
///
/// The export entry is selected randomly and then a new `field` name is
/// generated.
#[derive(Clone, Copy)]
pub struct RenameExportMutator {
    /// The maximum length of the generated export entry
    pub max_name_size: usize,
}

impl RenameExportMutator {
    /// Copied and transformed from wasm-smith name generation
    fn limited_string(&self, config: &mut WasmMutate, original: &str) -> crate::Result<String> {
        loop {
            config.consume_fuel(1)?;
            let mut bytes = original.as_bytes().to_vec();
            config.raw_mutate(&mut bytes, self.max_name_size)?;

            match std::str::from_utf8(&bytes) {
                Ok(_) => {}
                Err(e) => {
                    let i = e.valid_up_to();
                    bytes.drain(i..);
                }
            };
            if bytes.len() > self.max_name_size {
                continue;
            }
            let ret = String::from_utf8(bytes).unwrap();
            if ret != original && config.info().export_names.contains(&ret) {
                continue;
            }
            return Ok(ret);
        }
    }
}

impl Mutator for RenameExportMutator {
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

            let new_name = if skip_at != i as u64 {
                // otherwise bypass
                String::from(export.name)
            } else {
                let new_name = self.limited_string(config, export.name)?;
                log::debug!("Renaming export {:?} by {:?}", export, new_name);
                new_name
            };

            match export.kind {
                wasmparser::ExternalKind::Func => {
                    exports.export(new_name.as_str(), ExportKind::Func, export.index);
                }
                wasmparser::ExternalKind::Table => {
                    exports.export(new_name.as_str(), ExportKind::Table, export.index);
                }
                wasmparser::ExternalKind::Memory => {
                    exports.export(new_name.as_str(), ExportKind::Memory, export.index);
                }
                wasmparser::ExternalKind::Global => {
                    exports.export(new_name.as_str(), ExportKind::Global, export.index);
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
    use crate::WasmMutate;
    use std::sync::Arc;

    #[test]
    fn test_rename_export_mutator() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm

        let mut config = WasmMutate::default();
        config.raw_mutate_func(Some(Arc::new(|data, _| {
            assert_eq!(data, b"exported_func");
            *data = Vec::new();
            Ok(())
        })));
        config.match_mutation(
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
