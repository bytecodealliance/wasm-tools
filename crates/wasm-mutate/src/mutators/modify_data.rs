use super::Mutator;
use crate::{Result, WasmMutate};

use crate::mutators::{DefaultTranslator, Translator};
use rand::Rng;
use wasm_encoder::{DataSection, DataSegment, DataSegmentMode, Module};
use wasmparser::{DataKind, DataSectionReader};

/// Mutator that modifies a data segment, either adding or removing bytes.
#[derive(Clone, Copy)]
pub struct ModifyDataMutator;

impl Mutator for ModifyDataMutator {
    fn mutate<'a>(
        self,
        config: &'a mut WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let mut new_section = DataSection::new();
        let mut reader = DataSectionReader::new(config.info().get_data_section().data, 0)?;

        // Select an arbitrary data segment to modify.
        let data_to_modify = config.rng().gen_range(0, reader.get_count());

        // Iterate over all data segments in the old data section and re-add
        // them to the `new_section` one-by-one.
        for i in 0..reader.get_count() {
            let data = reader.read()?;
            let offset;
            // Preserve the mode of the data segment
            let mode = match &data.kind {
                DataKind::Active {
                    memory_index,
                    init_expr,
                } => {
                    offset = DefaultTranslator.translate_init_expr(init_expr)?;
                    DataSegmentMode::Active {
                        memory_index: *memory_index,
                        offset: &offset,
                    }
                }
                DataKind::Passive => DataSegmentMode::Passive,
            };
            // If this is the correct data segment apply the mutation,
            // otherwise preserve the data.
            let data = if i == data_to_modify {
                self.mutate_data(config, data.data)
            } else {
                data.data.to_vec()
            };
            new_section.segment(DataSegment { mode, data });
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

impl ModifyDataMutator {
    fn mutate_data(&self, config: &mut WasmMutate, data: &[u8]) -> Vec<u8> {
        // If reduction is configured then we never add data, otherwise
        // arbitrarily choose to add or remove data.
        if config.reduce || config.rng().gen() {
            if data.len() > 0 {
                // Remove a random subslice of data, if there's actually some
                // data.
                let start = config.rng().gen_range(0, data.len());
                let end = config.rng().gen_range(start, data.len());
                data[start..end].to_vec()
            } else {
                Vec::new()
            }
        } else {
            // Insert some random bytes.
            let start = config.rng().gen_range(0, data.len());
            let len = config.rng().gen_range(0, 100);
            data[0..start]
                .iter()
                .copied()
                .chain(
                    config
                        .rng()
                        .sample_iter(rand::distributions::Standard)
                        .take(len),
                )
                .chain(data[start..].iter().copied())
                .collect()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ModifyDataMutator;

    #[test]
    fn test_remove_export_mutator() {
        crate::mutators::match_mutation(
            r#"(module (data "x"))"#,
            ModifyDataMutator,
            r#"(module (data ""))"#,
        );
    }
}
