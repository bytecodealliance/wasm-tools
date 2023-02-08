//! Mutators related to custom sections.

use super::Mutator;
use crate::{Result, WasmMutate};
use rand::seq::SliceRandom;
use wasm_encoder::{Module, SectionId};

/// A mutator that removes a section.
#[derive(Clone, Copy)]
pub enum RemoveSection {
    /// Remove a random custom section, even if it contains data.
    Custom,
    /// Remove a random empty section.
    Empty,
}

fn is_empty_section(section: &wasm_encoder::RawSection) -> bool {
    use wasmparser::*;
    crate::module::match_section_id! {
        match section.id;
        Custom => Ok(section.data.is_empty()),
        Type => TypeSectionReader::new(section.data, 0).map(|r| r.count() == 0),
        Import => ImportSectionReader::new(section.data, 0).map(|r| r.count() == 0),
        Function => FunctionSectionReader::new(section.data, 0).map(|r| r.count() == 0),
        Table => FunctionSectionReader::new(section.data, 0).map(|r| r.count() == 0),
        Memory => MemorySectionReader::new(section.data, 0).map(|r| r.count() == 0),
        Global => GlobalSectionReader::new(section.data, 0).map(|r| r.count() == 0),
        Export => ExportSectionReader::new(section.data, 0).map(|r| r.count() == 0),
        Start => Ok(section.data.is_empty()),
        Element => ElementSectionReader::new(section.data, 0).map(|r| r.count() == 0),
        Code => CodeSectionReader::new(section.data, 0).map(|r| r.count() == 0),
        Data => DataSectionReader::new(section.data, 0).map(|r| r.count() == 0),
        DataCount => Ok(section.data.is_empty()),
        Tag => TagSectionReader::new(section.data, 0).map(|r| r.count() == 0),
        _ => Ok(section.data.is_empty()),
    }
    .unwrap_or(false)
}

impl Mutator for RemoveSection {
    fn can_mutate(&self, config: &WasmMutate) -> bool {
        match self {
            &Self::Custom => config.info().has_custom_section(),
            &Self::Empty => config
                .info()
                .raw_sections
                .iter()
                .any(|s| is_empty_section(s)),
        }
    }

    fn mutate<'a>(
        &self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let removal_candidates = config
            .info()
            .raw_sections
            .iter()
            .enumerate()
            .filter_map(|(i, s)| match self {
                Self::Empty if is_empty_section(s) => Some(i),
                Self::Custom if s.id == wasm_encoder::SectionId::Custom as u8 => Some(i),
                _ => None,
            })
            .collect::<Vec<_>>();
        let section_index_to_remove = *removal_candidates.choose(config.rng()).unwrap();

        let mut module = Module::new();
        for (i, section) in config.info().raw_sections.iter().enumerate() {
            if i == section_index_to_remove {
                continue;
            }
            module.section(section);
        }
        Ok(Box::new(std::iter::once(Ok(module))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_remove_custom_section() {
        // This will remove the name section so `$f` becomes `(;0;)`.
        crate::mutators::match_mutation(
            r#"
                (module
                    (func $f (param i32) (result i32)
                        local.get 0
                    )
                )
            "#,
            RemoveSection::Custom,
            r#"
                (module
                    (func (;0;) (param i32) (result i32)
                        local.get 0
                    )
                )
            "#,
        );
    }

    #[test]
    fn test_remove_empty_section() {
        // This will remove the import section as it is empty.
        crate::mutators::match_mutation(
            r#"(module binary
                 "\00asm\01\00\00\00"
                 "\01\05"
                 "\01\60\00\01\7f"
                 "\02\01\00"
            )"#,
            RemoveSection::Empty,
            r#"(module binary
                 "\00asm\01\00\00\00"
                 "\01\05"
                 "\01\60\00\01\7f"
            )"#,
        );
    }
}
