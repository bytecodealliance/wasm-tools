//! Mutators that add, edit, or remove data segments.

use super::Mutator;
use rand::Rng;

/// A mutator to remove data segments.
#[derive(Copy, Clone)]
pub struct RemoveDataSegment;

impl Mutator for RemoveDataSegment {
    fn can_mutate(&self, config: &crate::WasmMutate) -> bool {
        !config.preserve_semantics && config.info().data_segments_count > 0
    }

    fn mutate<'a>(
        self,
        config: &'a mut crate::WasmMutate,
    ) -> crate::Result<Box<dyn Iterator<Item = crate::Result<wasm_encoder::Module>> + 'a>>
    where
        Self: Copy,
    {
        let data_section = config.info().get_data_section();
        let mut reader = wasmparser::DataSectionReader::new(data_section.data, 0)?;
        let mut data = wasm_encoder::DataSection::new();

        let count = reader.get_count();
        let index_to_remove = config.rng().gen_range(0, count);

        for i in 0..count {
            let seg = reader.read()?;

            if let wasmparser::DataKind::Passive = seg.kind {
                // TODO: to support passive segments, we'll need to keep track
                // of segment renumberings and then fixup the code section.
                return Err(crate::Error::UnsupportedType(
                    crate::error::EitherType::Operator(
                        "Can't remove data segments when some are passive".into(),
                    ),
                ));
            }

            if i == index_to_remove {
                continue;
            }

            // Copy this data segment into our new data section.
            let encoded_data = &data_section.data[seg.range.start..seg.range.end];
            data.raw(encoded_data);
        }

        let data_section_index = config.info().data.unwrap();
        Ok(Box::new(std::iter::once(Ok(config
            .info()
            .replace_section(data_section_index, &data)))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_remove_elem_segment() {
        crate::mutators::match_mutation(
            r#"
                (module
                    (memory 1)
                    (data 0 (i32.const 10) "a")
                    (data 0 (i32.const 20) "b")
                    (data 0 (i32.const 30) "c")
                )
            "#,
            RemoveDataSegment,
            r#"
                (module
                    (memory 1)
                    (data 0 (i32.const 10) "a")
                    (data 0 (i32.const 20) "b")
                )
            "#,
        );
    }
}
