//! Mutators related to custom sections.

use super::Mutator;
use crate::{Result, WasmMutate};
use rand::seq::SliceRandom;
use wasm_encoder::Module;

/// A mutator that removes a custom section.
#[derive(Clone, Copy)]
pub struct RemoveCustomSection;

impl Mutator for RemoveCustomSection {
    fn can_mutate(&self, config: &WasmMutate) -> bool {
        config
            .info()
            .raw_sections
            .iter()
            .any(|s| s.id == wasm_encoder::SectionId::Custom as u8)
    }

    fn mutate<'a>(
        self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>>
    where
        Self: Copy,
    {
        let custom_sections = config
            .info()
            .raw_sections
            .iter()
            .enumerate()
            .filter_map(|(i, s)| {
                if s.id == wasm_encoder::SectionId::Custom as u8 {
                    Some(i)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        let section_index_to_remove = *custom_sections.choose(config.rng()).unwrap();

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
            RemoveCustomSection,
            r#"
                (module
                    (func (;0;) (param i32) (result i32)
                        local.get 0
                    )
                )
            "#,
        );
    }
}
