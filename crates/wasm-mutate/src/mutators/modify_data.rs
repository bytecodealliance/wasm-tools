use super::Mutator;
use crate::{Result, WasmMutate};
use rand::Rng;
use wasm_encoder::reencode::{Reencode, RoundtripReencoder};
use wasm_encoder::{DataSection, Module};
use wasmparser::{DataKind, DataSectionReader};

/// Mutator that modifies a data segment, either adding or removing bytes.
#[derive(Clone, Copy)]
pub struct ModifyDataMutator {
    pub max_data_size: usize,
}

impl Mutator for ModifyDataMutator {
    fn mutate<'a>(
        &self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let mut new_section = DataSection::new();
        let section_idx = config.info().data.unwrap();
        let reader = config.info().get_binary_reader(section_idx);
        let reader = DataSectionReader::new(reader)?;

        // Select an arbitrary data segment to modify.
        let data_to_modify = config.rng().gen_range(0..reader.count());

        // Iterate over all data segments in the old data section and re-add
        // them to the `new_section` one-by-one.
        for (i, data) in reader.into_iter().enumerate() {
            let data = data?;
            // If this is the correct data segment apply the mutation,
            // otherwise preserve the data.
            let mut contents = data.data.to_vec();
            if i as u32 == data_to_modify {
                config.raw_mutate(&mut contents, self.max_data_size)?;
            }

            // Add the data segment to the section that we're building
            match data.kind {
                DataKind::Active {
                    memory_index,
                    offset_expr,
                } => {
                    new_section.active(
                        memory_index,
                        &RoundtripReencoder.const_expr(offset_expr)?,
                        contents,
                    );
                }
                DataKind::Passive => {
                    new_section.passive(contents);
                }
            };
        }

        Ok(Box::new(std::iter::once(Ok(config
            .info()
            .replace_section(
                config.info().data.unwrap(),
                &new_section,
            )))))
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate) -> bool {
        // Modifying a data segment doesn't preserve the semantics of the
        // original module and also only works if there's actually some data.
        !config.preserve_semantics && config.info().num_data() > 0
    }
}

#[cfg(test)]
mod tests {
    use super::ModifyDataMutator;
    use crate::WasmMutate;
    use std::sync::Arc;

    #[test]
    fn test_remove_export_mutator() {
        let mut config = WasmMutate::default();
        config.raw_mutate_func(Some(Arc::new(|data, _| {
            assert_eq!(data, b"x");
            *data = "y".to_string().into_bytes();
            Ok(())
        })));
        config.match_mutation(
            r#"(module (data "x"))"#,
            ModifyDataMutator { max_data_size: 100 },
            r#"(module (data "y"))"#,
        );
    }
}
