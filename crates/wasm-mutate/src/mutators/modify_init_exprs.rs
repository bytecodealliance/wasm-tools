//! This mutator modifies the constant initializer expressions between various valid forms in
//! entities which require constant initializers.

use crate::mutators::translate::{self, InitExprContext, Item, Translator};
use crate::{Error, Mutator, Result};

use rand::Rng;
use wasm_encoder::{ElementSection, GlobalSection, Instruction};
use wasmparser::{ElementSectionReader, GlobalSectionReader, InitExpr, Operator, Type};

#[derive(Copy, Clone)]
pub enum InitExpressionMutator {
    Global,
    ElementOffset,
    ElementFunc,
}

struct InitTranslator {
    new_value: u128,
    skip_inits: u32,
    context: InitExprContext,
}

impl Translator for InitTranslator {
    fn as_obj(&mut self) -> &mut dyn Translator {
        self
    }

    fn remap(&mut self, item: Item, idx: u32) -> Result<u32> {
        match (self.context, item) {
            // Element's function items can either be presented to `Translator` via `remap` here or
            // via expressions (the function below). The API here is insufficiently flexible to
            // produce null funcrefs so at least lets try pointing at the lowest numbered function
            // instead.
            //
            // TODO: may make sense to actually generate the functions pointed at more randomly.
            (InitExprContext::ElementFunction, Item::Function) => {
                if self.skip_inits != 0 {
                    self.skip_inits -= 1;
                    Ok(idx)
                } else {
                    log::trace!("... replacing referenced function index with 0");
                    self.skip_inits = u32::MAX;
                    Ok(0)
                }
            }
            _ => Ok(idx),
        }
    }

    fn translate_init_expr(
        &mut self,
        e: &InitExpr<'_>,
        ty: &Type,
        ctx: InitExprContext,
    ) -> Result<Instruction<'static>> {
        use {Instruction as I, Type as T};
        if ctx != self.context {
            return translate::init_expr(self.as_obj(), e);
        } else if self.skip_inits != 0 {
            self.skip_inits -= 1;
            return translate::init_expr(self.as_obj(), e);
        }
        let mut reader = e.get_operators_reader();
        let op = reader.read()?;
        // Don't mutate further if the expressions are already their most reduced form.
        let simplest = match op {
            Operator::RefNull { .. }
            | Operator::I32Const { value: 0 | 1 }
            | Operator::I64Const { value: 0 | 1 } => true,
            Operator::F32Const { value } => value.bits() == 0,
            Operator::F64Const { value } => value.bits() == 0,
            Operator::V128Const { value } => value.i128() == 0,
            _ => false,
        };
        if simplest {
            return Err(Error::no_mutations_applicable());
        }
        let new_op = match ty {
            T::I32 => I::I32Const(self.new_value as u32 as _),
            T::I64 => I::I64Const(self.new_value as u64 as _),
            T::F32 => I::F32Const(f32::from_bits(self.new_value as u32)),
            T::F64 => I::F64Const(f64::from_bits(self.new_value as u64)),
            T::V128 => I::V128Const(self.new_value as i128),
            T::FuncRef => I::RefNull(wasm_encoder::ValType::FuncRef),
            T::ExternRef => I::RefNull(wasm_encoder::ValType::ExternRef),
            T::ExnRef | T::Func | T::EmptyBlockType => return Err(Error::no_mutations_applicable()),
        };
        log::trace!("... replacing original expression with {:?}", new_op);
        // We applied a change. Don't apply any more during this translation/mutation.
        self.skip_inits = u32::MAX;
        Ok(new_op)
    }
}

impl Mutator for InitExpressionMutator {
    fn mutate<'a>(
        self,
        config: &'a mut crate::WasmMutate,
    ) -> crate::Result<Box<dyn Iterator<Item = crate::Result<wasm_encoder::Module>> + 'a>> {
        let mut translator = InitTranslator {
            new_value: 0,
            skip_inits: 0,
            context: match self {
                Self::Global => InitExprContext::Global,
                Self::ElementOffset => InitExprContext::ElementOffset,
                Self::ElementFunc => InitExprContext::ElementFunction,
            },
        };
        match self {
            Self::Global => {
                let num_total = config.info().num_local_globals();
                translator.new_value = match config.rng().gen::<u8>() {
                    0..=99 => 0,
                    100..=199 => 1,
                    200.. => config.rng().gen::<u128>(),
                };
                let mutate_idx = config.rng().gen_range(0..num_total);
                let info = config.info();
                let section = info.globals.ok_or(Error::no_mutations_applicable())?;
                let mut new_section = GlobalSection::new();
                let mut reader = GlobalSectionReader::new(info.raw_sections[section].data, 0)?;
                for idx in 0..reader.get_count() {
                    config.consume_fuel(1)?;
                    let start = reader.original_position();
                    let global = reader.read()?;
                    let end = reader.original_position();
                    if idx == mutate_idx {
                        log::trace!("Modifying global at index {}...", idx);
                        translator.translate_global(global, &mut new_section)?;
                    } else {
                        new_section.raw(&info.raw_sections[section].data[start..end]);
                    }
                }
                Ok(Box::new(std::iter::once(Ok(
                    info.replace_section(section, &new_section)
                ))))
            }
            Self::ElementOffset | Self::ElementFunc => {
                let num_total = config.info().num_elements();
                let mutate_idx = config.rng().gen_range(0..num_total);
                let section = config
                    .info()
                    .elements
                    .ok_or(Error::no_mutations_applicable())?;
                let mut new_section = ElementSection::new();
                let mut reader =
                    ElementSectionReader::new(config.info().raw_sections[section].data, 0)?;
                for idx in 0..reader.get_count() {
                    config.consume_fuel(1)?;
                    let start = reader.original_position();
                    let element = reader.read()?;
                    let end = reader.original_position();
                    if idx == mutate_idx {
                        if let Self::ElementFunc = self {
                            // Pick a specific element item to mutate. We do this through an option
                            // to skip a specific number of activations of the Translator methods.
                            let item_count = element.items.get_items_reader()?.get_count();
                            if item_count > 0 {
                                translator.skip_inits = config.rng().gen_range(0..item_count);
                            } else {
                                return Err(Error::no_mutations_applicable());
                            }
                        }
                        log::trace!(
                            "Modifying {} element's {:?}({})...",
                            idx,
                            translator.context,
                            translator.skip_inits
                        );
                        translator.translate_element(element, &mut new_section)?;
                    } else {
                        new_section.raw(&config.info().raw_sections[section].data[start..end]);
                    }
                }
                Ok(Box::new(std::iter::once(Ok(config
                    .info()
                    .replace_section(section, &new_section)))))
            }
        }
    }

    fn can_mutate(&self, config: &crate::WasmMutate) -> bool {
        !config.preserve_semantics
            && match self {
                Self::Global => config.info().num_local_globals() > 0,
                Self::ElementOffset | Self::ElementFunc => config.info().num_elements() > 0,
            }
    }
}
