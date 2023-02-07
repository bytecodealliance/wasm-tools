//! Add custom sections.

use super::Mutator;
use rand::Rng;

#[derive(Clone, Copy)]
pub struct AddCustomSectionMutator;

const MAX_NEW_DATA_LEN: usize = 100;
const MAX_NEW_NAME_LEN: usize = 20;

impl Mutator for AddCustomSectionMutator {
    fn can_mutate(&self, config: &crate::WasmMutate) -> bool {
        !config.reduce
    }

    fn mutate<'a>(
        &self,
        config: &'a mut crate::WasmMutate,
    ) -> crate::Result<Box<dyn Iterator<Item = crate::Result<wasm_encoder::Module>> + 'a>> {
        let num_sections = config.info().raw_sections.len();
        let new_custom_section_idx = config.rng().gen_range(0..=num_sections);
        let mut name = vec![];
        config.raw_mutate(&mut name, MAX_NEW_NAME_LEN)?;
        let name = String::from_utf8_lossy(&name);
        let mut data = vec![];
        config.raw_mutate(&mut data, MAX_NEW_DATA_LEN)?;

        Ok(Box::new(std::iter::once(Ok(config.info().insert_section(
            new_custom_section_idx,
            &wasm_encoder::CustomSection {
                name: &name,
                data: &data,
            },
        )))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_custom_section() {
        crate::mutators::match_mutation(
            r#"
                (module)
            "#,
            AddCustomSectionMutator,
            r#"
                (module
                    (@custom "a" "b")
                )
            "#,
        );
    }
}
