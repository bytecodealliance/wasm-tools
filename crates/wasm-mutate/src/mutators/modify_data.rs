use super::translate::ConstExprKind;
use super::Mutator;
use crate::mutators::{DefaultTranslator, Translator};
use crate::{Result, WasmMutate};
use rand::Rng;
use wasm_encoder::{DataSection, DataSegment, DataSegmentMode, Module};
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
        let reader = DataSectionReader::new(config.info().get_data_section().data, 0)?;

        // Select an arbitrary data segment to modify.
        let data_to_modify = config.rng().gen_range(0..reader.count());

        // Iterate over all data segments in the old data section and re-add
        // them to the `new_section` one-by-one.
        for (i, data) in reader.into_iter().enumerate() {
            let data = data?;
            let offset;
            // Preserve the mode of the data segment
            let mode = match &data.kind {
                DataKind::Active {
                    memory_index,
                    offset_expr,
                } => {
                    offset = DefaultTranslator.translate_const_expr(
                        offset_expr,
                        &wasmparser::ValType::I32,
                        ConstExprKind::DataOffset,
                    )?;
                    DataSegmentMode::Active {
                        memory_index: *memory_index,
                        offset: &offset,
                    }
                }
                DataKind::Passive => DataSegmentMode::Passive,
            };
            // If this is the correct data segment apply the mutation,
            // otherwise preserve the data.
            let mut data = data.data.to_vec();
            if i as u32 == data_to_modify {
                config.raw_mutate(&mut data, self.max_data_size)?;
            }
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
