//! Mutate custom sections.

use super::Mutator;
use rand::{seq::SliceRandom, Rng};

#[derive(Clone, Copy)]
pub struct CustomSectionMutator;

impl Mutator for CustomSectionMutator {
    fn can_mutate(&self, config: &crate::WasmMutate) -> bool {
        config.info().has_custom_section()
    }

    fn mutate<'a>(
        self,
        config: &'a mut crate::WasmMutate,
    ) -> crate::Result<Box<dyn Iterator<Item = crate::Result<wasm_encoder::Module>> + 'a>> {
        let custom_section_indices: Vec<_> = config
            .info()
            .raw_sections
            .iter()
            .enumerate()
            .filter(|(_i, s)| s.id == wasm_encoder::SectionId::Custom as u8)
            .map(|(i, _s)| i)
            .collect();
        assert!(!custom_section_indices.is_empty());

        let custom_section_index = *custom_section_indices.choose(config.rng()).unwrap();
        let old_custom_section = &config.info().raw_sections[custom_section_index];
        let old_custom_section =
            wasmparser::CustomSectionReader::new(old_custom_section.data, 0).unwrap();

        let name_string;
        let data_vec;
        let mut name = old_custom_section.name();
        let mut data = old_custom_section.data();

        if config.rng().gen_ratio(1, 20) {
            // Mutate the custom section's name.
            let mut new_name = name.to_string().into_bytes();
            config.raw_mutate(
                &mut new_name,
                if config.reduce {
                    name.len().saturating_sub(1)
                } else {
                    std::cmp::max(name.len() * 2, 32)
                },
            )?;
            name_string = String::from_utf8_lossy(&new_name).to_string();
            name = &name_string;
        } else {
            // Mutate the custom section's data.
            let mut new_data = data.to_vec();
            config.raw_mutate(
                &mut new_data,
                if config.reduce {
                    data.len().saturating_sub(1)
                } else {
                    std::cmp::max(data.len() * 2, 32)
                },
            )?;
            data_vec = new_data;
            data = &data_vec;
        };

        Ok(Box::new(std::iter::once(Ok(config
            .info()
            .replace_section(
                custom_section_index,
                &wasm_encoder::CustomSection { name, data },
            )))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grow_custom_section() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (@custom "name" "data")
                )
            "#,
            CustomSectionMutator,
            r#"
                (module
                    (@custom "name" "datadata")
                )
            "#,
        );
    }

    #[test]
    fn test_shrink_custom_section() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (@custom "name" "data")
                )
            "#,
            CustomSectionMutator,
            r#"
                (module
                    (@custom "name" "d")
                )
            "#,
        );
    }

    #[test]
    fn test_mutate_custom_section() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (@custom "name" "data")
                )
            "#,
            CustomSectionMutator,
            r#"
                (module
                    (@custom "name" "aaaa")
                )
            "#,
        );
    }

    #[test]
    fn test_mutate_custom_section_name() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (@custom "name" "data")
                )
            "#,
            CustomSectionMutator,
            r#"
                (module
                    (@custom "n" "data")
                )
            "#,
        );
    }
}
