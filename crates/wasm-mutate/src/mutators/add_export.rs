//! Mutator that adds an export for a random existing item.

use super::Mutator;
use crate::{Result, WasmMutate};
use rand::RngExt;
use wasm_encoder::{ExportKind, ExportSection, Module, SectionId};
use wasmparser::ExportSectionReader;

/// A mutator that adds an export for a random existing item.
#[derive(Clone, Copy)]
pub struct AddExportMutator {
    /// The maximum length of the generated export name.
    pub max_name_size: usize,
}

impl AddExportMutator {
    fn new_name(&self, config: &mut WasmMutate) -> Result<String> {
        loop {
            config.consume_fuel(1)?;

            let mut bytes = Vec::new();
            config.raw_mutate(&mut bytes, self.max_name_size)?;
            if bytes.len() > self.max_name_size {
                continue;
            }

            if let Err(error) = std::str::from_utf8(&bytes) {
                bytes.truncate(error.valid_up_to());
            }

            let name = String::from_utf8(bytes).unwrap();
            if !config.info().export_names.contains(&name) {
                return Ok(name);
            }
        }
    }

    fn existing_item(&self, config: &mut WasmMutate) -> (ExportKind, u32) {
        let counts = [
            (ExportKind::Func, config.info().num_functions()),
            (ExportKind::Table, config.info().num_tables()),
            (ExportKind::Memory, config.info().num_memories()),
            (ExportKind::Global, config.info().num_globals()),
            (ExportKind::Tag, config.info().num_tags()),
        ];
        let total = counts.iter().map(|(_, count)| u64::from(*count)).sum();
        let mut selected = config.rng().random_range(0..total);

        for (kind, count) in counts {
            if selected < u64::from(count) {
                return (kind, selected as u32);
            }
            selected -= u64::from(count);
        }

        unreachable!()
    }

    fn copy_exports(&self, config: &mut WasmMutate, exports: &mut ExportSection) -> Result<()> {
        let Some(exports_idx) = config.info().exports else {
            return Ok(());
        };
        let reader = ExportSectionReader::new(config.info().get_binary_reader(exports_idx))?;

        for export in reader {
            let export = export?;
            config.consume_fuel(1)?;
            let kind = match export.kind {
                wasmparser::ExternalKind::Func => ExportKind::Func,
                wasmparser::ExternalKind::Table => ExportKind::Table,
                wasmparser::ExternalKind::Memory => ExportKind::Memory,
                wasmparser::ExternalKind::Global => ExportKind::Global,
                wasmparser::ExternalKind::Tag => ExportKind::Tag,
                wasmparser::ExternalKind::FuncExact => unreachable!(),
            };
            exports.export(export.name, kind, export.index);
        }

        Ok(())
    }

    fn export_section_insertion_index(&self, config: &WasmMutate) -> usize {
        config
            .info()
            .raw_sections
            .iter()
            .position(|section| {
                !matches!(
                    section.id,
                    id if id == SectionId::Custom as u8
                        || id == SectionId::Type as u8
                        || id == SectionId::Import as u8
                        || id == SectionId::Function as u8
                        || id == SectionId::Table as u8
                        || id == SectionId::Memory as u8
                        || id == SectionId::Tag as u8
                        || id == SectionId::Global as u8
                )
            })
            .unwrap_or(config.info().raw_sections.len())
    }
}

impl Mutator for AddExportMutator {
    fn can_mutate(&self, config: &WasmMutate) -> bool {
        !config.preserve_semantics
            && !config.reduce
            && (config.info().num_functions() > 0
                || config.info().num_tables() > 0
                || config.info().num_memories() > 0
                || config.info().num_globals() > 0
                || config.info().num_tags() > 0)
    }

    fn mutate<'a>(
        &self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let mut exports = ExportSection::new();
        self.copy_exports(config, &mut exports)?;

        let name = self.new_name(config)?;
        let (kind, index) = self.existing_item(config);
        exports.export(&name, kind, index);

        let module = match config.info().exports {
            Some(exports_idx) => config.info().replace_section(exports_idx, &exports),
            None => config
                .info()
                .insert_section(self.export_section_insertion_index(config), &exports),
        };

        Ok(Box::new(std::iter::once(Ok(module))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn fixed_name_config(name: &'static str) -> WasmMutate<'static> {
        let mut config = WasmMutate::default();
        config.raw_mutate_func(Some(Arc::new(move |data, max_size| {
            assert!(name.len() <= max_size);
            *data = name.as_bytes().to_vec();
            Ok(())
        })));
        config
    }

    #[test]
    fn adds_to_existing_export_section() {
        fixed_name_config("new").match_mutation(
            r#"
                (module
                    (global (export "old") i32 (i32.const 0))
                )
            "#,
            AddExportMutator { max_name_size: 100 },
            r#"
                (module
                    (global (;0;) i32 (i32.const 0))
                    (export "old" (global 0))
                    (export "new" (global 0))
                )
            "#,
        );
    }

    #[test]
    fn creates_export_section_in_order() {
        fixed_name_config("new").match_mutation(
            r#"
                (module
                    (func)
                    (@custom "metadata" (after code) "contents")
                )
            "#,
            AddExportMutator { max_name_size: 100 },
            r#"
                (module
                    (type (;0;) (func))
                    (func (;0;) (type 0))
                    (export "new" (func 0))
                    (@custom "metadata" (after code) "contents")
                )
            "#,
        );
    }

    #[test]
    fn can_export_each_item_kind() {
        let wasm = wat::parse_str(
            r#"
                (module
                    (type (func))
                    (func (type 0))
                    (table 1 funcref)
                    (memory 1)
                    (tag (type 0))
                    (global i32 (i32.const 0))
                )
            "#,
        )
        .unwrap();
        let mut seen = [false; 5];

        for seed in 0..100 {
            let mut config = fixed_name_config("new");
            config.seed(seed);
            config.setup(&wasm).unwrap();

            let module = AddExportMutator { max_name_size: 100 }
                .mutate(&mut config)
                .unwrap()
                .next()
                .unwrap()
                .unwrap()
                .finish();
            crate::validate(&module);

            for payload in wasmparser::Parser::new(0).parse_all(&module) {
                if let wasmparser::Payload::ExportSection(reader) = payload.unwrap() {
                    let export = reader.into_iter().next().unwrap().unwrap();
                    seen[match export.kind {
                        wasmparser::ExternalKind::Func => 0,
                        wasmparser::ExternalKind::Table => 1,
                        wasmparser::ExternalKind::Memory => 2,
                        wasmparser::ExternalKind::Global => 3,
                        wasmparser::ExternalKind::Tag => 4,
                        wasmparser::ExternalKind::FuncExact => unreachable!(),
                    }] = true;
                }
            }
        }

        assert!(seen.into_iter().all(|seen| seen));
    }

    #[test]
    fn rejects_inapplicable_configurations() {
        let empty = wat::parse_str("(module)").unwrap();
        let function = wat::parse_str("(module (func))").unwrap();
        let mut config = WasmMutate::default();
        config.setup(&empty).unwrap();
        assert!(!AddExportMutator { max_name_size: 100 }.can_mutate(&config));

        config.preserve_semantics(true).setup(&function).unwrap();
        assert!(!AddExportMutator { max_name_size: 100 }.can_mutate(&config));

        config
            .preserve_semantics(false)
            .reduce(true)
            .setup(&function)
            .unwrap();
        assert!(!AddExportMutator { max_name_size: 100 }.can_mutate(&config));
    }
}
