//! Mutate custom sections.

use std::borrow::Cow;

use super::Mutator;
use rand::{seq::SliceRandom, Rng};

#[derive(Clone, Copy)]
pub struct CustomSectionMutator;

impl Mutator for CustomSectionMutator {
    fn can_mutate(&self, config: &crate::WasmMutate) -> bool {
        config.info().has_custom_section()
    }

    fn mutate<'a>(
        &self,
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
                &wasm_encoder::CustomSection {
                    name: name.into(),
                    data: Cow::Borrowed(data),
                },
            )))))
    }
}

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
                name: name.into(),
                data: Cow::Borrowed(&data),
            },
        )))))
    }
}

#[derive(Copy, Clone)]
pub struct ReorderCustomSectionMutator;

impl Mutator for ReorderCustomSectionMutator {
    fn can_mutate(&self, config: &crate::WasmMutate) -> bool {
        config.info().has_custom_section() && config.info().raw_sections.len() > 1
    }

    fn mutate<'a>(
        &self,
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

        let src_idx = *custom_section_indices.choose(config.rng()).unwrap();
        let num_sections = config.info().raw_sections.len();
        let mut dest_idx;
        loop {
            dest_idx = config.rng().gen_range(0..num_sections);
            if dest_idx != src_idx {
                break;
            }
            config.consume_fuel(1)?;
        }

        Ok(Box::new(std::iter::once(Ok(config
            .info()
            .move_section(src_idx, dest_idx)))))
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

    #[test]
    fn test_reorder_custom_section() {
        crate::mutators::match_mutation(
            r#"
            (module
                (@custom "name" "data")
                (@custom "name2" "data")
            )
            "#,
            ReorderCustomSectionMutator,
            r#"
            (module
                (@custom "name2" "data")
                (@custom "name" "data")
            )
            "#,
        )
    }
}
