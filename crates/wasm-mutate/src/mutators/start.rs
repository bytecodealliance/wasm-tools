//! Mutators related to the start section.

use super::Mutator;
use crate::{Result, WasmMutate};
use wasm_encoder::Module;

/// A mutator to remove the start section.
#[derive(Clone, Copy)]
pub struct RemoveStartSection;

impl Mutator for RemoveStartSection {
    fn can_mutate(&self, config: &WasmMutate) -> bool {
        !config.preserve_semantics && config.info().start.is_some()
    }

    fn mutate<'a>(
        self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = crate::Result<Module>> + 'a>>
    where
        Self: Copy,
    {
        let mut module = Module::new();
        let start_section_index = config.info().start.unwrap();

        for (i, section) in config.info().raw_sections.iter().enumerate() {
            if i == start_section_index {
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
    fn test_remove_start_segment() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (func $f (param i32) (result i32)
                        local.get 0
                    )
                    (start $f)
                )
            "#,
            RemoveStartSection,
            r#"
                (module
                    (func $f (param i32) (result i32)
                        local.get 0
                    )
                )
            "#,
        );
    }
}
