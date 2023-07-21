//! This mutator modifies the constant initializer expressions between various valid forms in
//! entities which require constant initializers.

use crate::mutators::translate::{self, ConstExprKind, DefaultTranslator, Item, Translator};
use crate::{Error, Mutator, Result};
use rand::Rng;
use wasm_encoder::{ElementSection, GlobalSection};
use wasmparser::{ConstExpr, ElementSectionReader, GlobalSectionReader, ValType};

#[derive(Copy, Clone)]
pub enum ConstExpressionMutator {
    Global,
    ElementOffset,
    ElementFunc,
}

struct InitTranslator<'cfg, 'wasm> {
    config: &'cfg mut crate::WasmMutate<'wasm>,
    skip_inits: u32,
    kind: ConstExprKind,
}

impl<'cfg, 'wasm> InitTranslator<'cfg, 'wasm> {
    /// Reduces the expression skip counter by 1 and indicates whether the current expression
    /// should be processed.
    ///
    /// If current expression ought to be modified, this function will return `true`.
    fn should_process(&mut self) -> bool {
        // NB: by wrapping the counter here we ensure that we usually apply just one transformation
        // during a walk of a WASM module, because we'd need to skip u32::MAX initializers once we
        // apply the first transformation.
        let (new_counter, was_zero) = self.skip_inits.overflowing_sub(1);
        self.skip_inits = new_counter;
        was_zero
    }
}

impl<'cfg, 'wasm> Translator for InitTranslator<'cfg, 'wasm> {
    fn as_obj(&mut self) -> &mut dyn Translator {
        self
    }

    /// Handle `elem`s with values of the `ElementItem::Func` kind. This function will not be
    /// called for values of the `ElementItem::Expr` kind.
    fn remap(&mut self, item: Item, idx: u32) -> Result<u32> {
        Ok(match (self.kind, item) {
            (ConstExprKind::ElementFunction, Item::Function) if self.should_process() => {
                log::trace!("... replacing referenced function index with 0");
                // FIXME: generate random function indices when `!config.reduce`.
                0
            }
            _ => idx,
        })
    }

    /// Handle `global` initalizers and `elem`s with values of the `ElementItem::Expr` kind.
    ///
    /// This function will not be called for `elem` values of the `ElementItem::Func` kind.
    fn translate_const_expr(
        &mut self,
        e: &ConstExpr<'_>,
        ty: &ValType,
        kind: ConstExprKind,
    ) -> Result<wasm_encoder::ConstExpr> {
        use wasm_encoder::ConstExpr as CE;
        use wasmparser::{Operator as O, ValType as T};

        if kind != self.kind || !self.should_process() {
            return translate::const_expr(self.as_obj(), e, kind);
        }
        let mut reader = e.get_operators_reader();
        let op = reader.read()?;
        // Don't mutate further if the expressions are already their most reduced form.
        let is_simplest = match op {
            O::RefNull { .. } | O::I32Const { value: 0 | 1 } | O::I64Const { value: 0 | 1 } => true,
            O::F32Const { value } => value.bits() == 0,
            O::F64Const { value } => value.bits() == 0,
            O::V128Const { value } => value.i128() == 0,
            _ => false,
        };
        if self.config.reduce && is_simplest {
            return Err(Error::no_mutations_applicable());
        }

        let new_op = if self.config.reduce {
            // For globals give a 25% chance to produce a const with 0 value (arguably the simplest
            // representation) to give a chance to quickly discover this final reduction if it is
            // in fact applicable.
            //
            // For element offsets always generate `i32.const 0` (effectively removing the offset)
            // as other values may not necessarily be valid (e.g. maximum table size is limited)
            let is_element_offset = matches!(kind, ConstExprKind::ElementOffset);
            let should_zero = is_element_offset || self.config.rng().gen::<u8>() & 0b11 == 0;
            match *ty {
                T::I32 if should_zero => CE::i32_const(0),
                T::I64 if should_zero => CE::i64_const(0),
                T::V128 if should_zero => CE::v128_const(0),
                T::F32 if should_zero => CE::f32_const(0.0),
                T::F64 if should_zero => CE::f64_const(0.0),
                T::I32 => CE::i32_const(if let O::I32Const { value } = op {
                    let range = if value < 0 { value..0 } else { 0..value };
                    self.config.rng().gen_range(range)
                } else {
                    self.config.rng().gen()
                }),
                T::I64 => CE::i64_const(if let O::I64Const { value } = op {
                    let range = if value < 0 { value..0 } else { 0..value };
                    self.config.rng().gen_range(range)
                } else {
                    self.config.rng().gen()
                }),
                T::V128 => CE::v128_const(if let O::V128Const { value } = op {
                    self.config.rng().gen_range(0..value.i128() as u128) as i128
                } else {
                    self.config.rng().gen()
                }),
                T::F32 => CE::f32_const(if let O::F32Const { value } = op {
                    f32::from_bits(value.bits()) / 2.0
                } else {
                    f32::from_bits(self.config.rng().gen())
                }),
                T::F64 => CE::f64_const(if let O::F64Const { value } = op {
                    f64::from_bits(value.bits()) / 2.0
                } else {
                    f64::from_bits(self.config.rng().gen())
                }),
                T::FUNCREF => CE::ref_null(wasm_encoder::HeapType::Func),
                T::EXTERNREF => CE::ref_null(wasm_encoder::HeapType::Extern),
                T::Ref(_) => unimplemented!(),
            }
        } else {
            // FIXME: implement non-reducing mutations for constant expressions.
            return Err(Error::no_mutations_applicable());
        };

        log::trace!("... replacing original expression with {:?}", new_op);
        Ok(new_op)
    }
}

impl Mutator for ConstExpressionMutator {
    fn mutate<'a>(
        &self,
        config: &'a mut crate::WasmMutate,
    ) -> crate::Result<Box<dyn Iterator<Item = crate::Result<wasm_encoder::Module>> + 'a>> {
        let translator_kind = match self {
            Self::Global => ConstExprKind::Global,
            Self::ElementOffset => ConstExprKind::ElementOffset,
            Self::ElementFunc => ConstExprKind::ElementFunction,
        };
        let skip_err = Error::no_mutations_applicable();
        match self {
            Self::Global => {
                let num_total = config.info().num_local_globals();
                let mutate_idx = config.rng().gen_range(0..num_total);
                let section = config.info().globals.ok_or(skip_err)?;
                let mut new_section = GlobalSection::new();
                let reader = GlobalSectionReader::new(config.info().raw_sections[section].data, 0)?;
                let mut translator = InitTranslator {
                    config,
                    skip_inits: 0,
                    kind: translator_kind,
                };
                for (idx, global) in reader.into_iter().enumerate() {
                    translator.config.consume_fuel(1)?;
                    let global = global?;
                    if idx as u32 == mutate_idx {
                        log::trace!("Modifying global at index {}...", idx);
                        translator.translate_global(global, &mut new_section)?;
                    } else {
                        DefaultTranslator.translate_global(global, &mut new_section)?;
                    }
                }
                let new_module = config.info().replace_section(section, &new_section);
                Ok(Box::new(std::iter::once(Ok(new_module))))
            }
            Self::ElementOffset | Self::ElementFunc => {
                let num_total = config.info().num_elements();
                let mutate_idx = config.rng().gen_range(0..num_total);
                let section = config.info().elements.ok_or(skip_err)?;
                let mut new_section = ElementSection::new();
                let reader =
                    ElementSectionReader::new(config.info().raw_sections[section].data, 0)?;
                let mut translator = InitTranslator {
                    config,
                    skip_inits: 0,
                    kind: translator_kind,
                };
                for (idx, element) in reader.into_iter().enumerate() {
                    translator.config.consume_fuel(1)?;
                    let element = element?;
                    if idx as u32 == mutate_idx {
                        if let Self::ElementFunc = self {
                            // Pick a specific element item to mutate. We do this through an option
                            // to skip a specific number of activations of the Translator methods.
                            let item_count = match &element.items {
                                wasmparser::ElementItems::Functions(r) => r.count(),
                                wasmparser::ElementItems::Expressions(_, r) => r.count(),
                            };
                            if item_count > 0 {
                                let skip = translator.config.rng().gen_range(0..item_count);
                                translator.skip_inits = skip
                            } else {
                                return Err(Error::no_mutations_applicable());
                            }
                        }
                        log::trace!(
                            "Modifying {} element's {:?}({})...",
                            idx,
                            translator_kind,
                            translator.skip_inits
                        );
                        translator.translate_element(element, &mut new_section)?;
                    } else {
                        DefaultTranslator.translate_element(element, &mut new_section)?;
                    }
                }
                let new_module = config.info().replace_section(section, &new_section);
                Ok(Box::new(std::iter::once(Ok(new_module))))
            }
        }
    }

    fn can_mutate(&self, config: &crate::WasmMutate) -> bool {
        // the implementation here can only reduce for now,
        // but could be extended to mutate arbitrarily.
        if !config.reduce {
            return false;
        }

        let any_data = match self {
            Self::Global => config.info().num_local_globals() > 0,
            Self::ElementOffset | Self::ElementFunc => config.info().num_elements() > 0,
        };
        !config.preserve_semantics && any_data
    }
}

#[cfg(test)]
mod tests {
    fn match_reduction<T>(original: &str, mutator: T, expected: &str)
    where
        T: crate::Mutator + Clone,
    {
        let mut config = crate::WasmMutate::default();
        config.reduce = true;
        config.match_mutation(original, mutator, expected)
    }

    #[test]
    fn reduce_global_const_i32() {
        match_reduction(
            "(module (global i32 (i32.const 42)))",
            super::ConstExpressionMutator::Global,
            "(module (global i32 (i32.const 0)))",
        );
        match_reduction(
            "(module (global i32 (i32.const 10)))",
            super::ConstExpressionMutator::Global,
            "(module (global i32 (i32.const 5)))",
        );
    }

    #[test]
    fn reduce_global_xref() {
        match_reduction(
            r#"(module (import "m" "g" (global i32)) (global i32 (global.get 0)))"#,
            super::ConstExpressionMutator::Global,
            r#"(module (import "m" "g" (global i32)) (global i32 (i32.const 0)))"#,
        );
    }

    #[test]
    fn reduce_global_const_f32() {
        match_reduction(
            r#"(module (global f32 (f32.const 2.0)))"#,
            super::ConstExpressionMutator::Global,
            r#"(module (global f32 (f32.const 1.0)))"#,
        );
        match_reduction(
            r#"(module (global f32 (f32.const 2.0)))"#,
            super::ConstExpressionMutator::Global,
            r#"(module (global f32 (f32.const 0.0)))"#,
        );
    }

    #[test]
    fn reduce_elem_funcref() {
        match_reduction(
            r#"(module (table 0 funcref) (elem func $a $b) (func $a) (func $b))"#,
            super::ConstExpressionMutator::ElementFunc,
            r#"(module (table 0 funcref) (elem func $a $a) (func $a) (func $b))"#,
        );
    }

    #[test]
    fn reduce_elem_expr() {
        match_reduction(
            r#"(module (table 0 funcref) (elem funcref (ref.func 0)) (func $a))"#,
            super::ConstExpressionMutator::ElementFunc,
            r#"(module (table 0 funcref) (elem funcref (ref.null func)) (func $a))"#,
        );
    }

    #[test]
    fn reduce_elem_base() {
        match_reduction(
            r#"(module
                (import "m" "g" (global i32))
                (table 0 funcref)
                (func $f)
                (elem (offset (global.get 0)) $f))"#,
            super::ConstExpressionMutator::ElementOffset,
            r#"(module
                (import "m" "g" (global i32))
                (table 0 funcref)
                (func $f)
                (elem (offset (i32.const 0)) $f))"#,
        );
    }
}
