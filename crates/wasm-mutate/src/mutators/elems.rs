//! Mutators that operate on the elements section.

use super::Mutator;
use crate::{Error, Result};
use rand::Rng;
use wasm_encoder::Module;

/// A mutator that removes element segments.
#[derive(Clone, Copy)]
pub struct RemoveElemSegment;

impl Mutator for RemoveElemSegment {
    fn can_mutate(&self, config: &crate::WasmMutate) -> bool {
        !config.preserve_semantics && config.info().elements_count > 0
    }

    fn mutate<'a>(
        self,
        config: &'a mut crate::WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = crate::Result<Module>> + 'a>>
    where
        Self: Copy,
    {
        let elems_section = config.info().get_elements_section();
        let mut reader = wasmparser::ElementSectionReader::new(elems_section.data, 0)?;
        let mut elems = wasm_encoder::ElementSection::new();

        let count = reader.get_count();
        let index_of_elem_to_remove = config.rng().gen_range(0, count);

        for i in 0..count {
            let elem = reader.read()?;

            if let wasmparser::ElementKind::Passive = elem.kind {
                // TODO: to support passive segments, we'll need to keep track
                // of segment renumberings and then fixup the code section.
                return Err(Error::unsupported(
                    "Can't remove element segments when some are passive",
                ));
            }

            if i == index_of_elem_to_remove {
                continue;
            }

            // Copy this element into our new elements section without
            // re-encoding or parsing the element's substructure.
            let encoded_elem = &elems_section.data[elem.range.start..elem.range.end];
            elems.raw(encoded_elem);
        }

        let elem_section_index = config.info().elements.unwrap();
        Ok(Box::new(std::iter::once(Ok(config
            .info()
            .replace_section(elem_section_index, &elems)))))
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
                    (table 1 funcref)
                    (func $f (param i32) (result i32) local.get 0)
                    (elem 0 (i32.const 10) $f)
                    (elem 0 (i32.const 20) $f $f)
                    (elem 0 (i32.const 30) $f $f $f)
                )
            "#,
            RemoveElemSegment,
            r#"
                (module
                    (table (;0;) 1 funcref)
                    (func $f (param i32) (result i32) local.get 0)
                    (elem 0 (i32.const 10) $f)
                    (elem 0 (i32.const 20) $f $f)
                )
            "#,
        );
    }
}
