use core::borrow;
use std::convert::TryFrom;

use super::Result;
use crate::module::*;
use crate::{ModuleInfo, WasmMutate};
use rand::prelude::SmallRng;
use rand::{Rng, RngCore};
use wasm_encoder::{CodeSection, Export, ExportSection, Function, Instruction, Module, ValType};
use wasmparser::{CodeSectionReader, ExportSectionReader};
pub trait Mutator {
    /// Method where the mutation happpens
    ///
    /// * `config` instance of WasmMutate
    /// * `rnd` random number generator
    /// * `info` parsed lane AST of the input Wasm module
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        info: &mut ModuleInfo,
    ) -> Result<Module>;

    /// Returns if this mutator can be applied with the info and the byte range in which it can be applied
    fn can_mutate<'a>(&self, config: &'a WasmMutate, info: &ModuleInfo) -> Result<bool>;

    /// Provides the name of the mutator, mostly used for debugging purposes
    fn name(&self) -> String {
        return format!("{:?}", std::any::type_name::<Self>());
    }
}
pub struct RemoveExportMutator;

impl Mutator for RemoveExportMutator {
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        info: &mut ModuleInfo,
    ) -> Result<Module> {
        let mut exports = ExportSection::new();
        let mut reader = ExportSectionReader::new(info.get_exports_section().data, 0)?;
        let max_exports = reader.get_count() as u64;
        let skip_at = rnd.gen_range(0, max_exports);

        (0..max_exports).for_each(|i| {
            let export = reader.read().unwrap();
            if skip_at != i {
                // otherwise bypass
                match export.kind {
                    wasmparser::ExternalKind::Function => {
                        exports.export(export.field, Export::Function(export.index));
                    }
                    wasmparser::ExternalKind::Table => {
                        exports.export(export.field, Export::Table(export.index));
                    }
                    wasmparser::ExternalKind::Memory => {
                        exports.export(export.field, Export::Memory(export.index));
                    }
                    wasmparser::ExternalKind::Global => {
                        exports.export(export.field, Export::Global(export.index));
                    }
                    wasmparser::ExternalKind::Module => {
                        exports.export(export.field, Export::Module(export.index));
                    }
                    wasmparser::ExternalKind::Instance => {
                        exports.export(export.field, Export::Instance(export.index));
                    }
                    _ => {
                        panic!("Unknown export {:?}", export)
                    }
                }
            } else {
                log::debug!("Removing export {:?} idx {:?}", export, skip_at);
            }
        });
        Ok(info.replace_section(info.exports.unwrap(), &exports))
    }

    fn can_mutate<'a>(&self, _: &'a WasmMutate, info: &ModuleInfo) -> Result<bool> {
        Ok(info.has_exports())
    }
}

pub struct RenameExportMutator {
    pub max_name_size: u32,
}

impl RenameExportMutator {
    // Copied and transformed from wasm-smith name generation
    fn limited_string(&self, rnd: &mut SmallRng) -> String {
        let size = rnd.gen_range(1, self.max_name_size);
        let size = std::cmp::min(size, self.max_name_size);
        let mut str = vec![0u8; size as usize];
        rnd.fill_bytes(&mut str);

        match std::str::from_utf8(&str) {
            Ok(s) => String::from(s),
            Err(e) => {
                let i = e.valid_up_to();
                let valid = &str[0..i];
                let s = unsafe {
                    debug_assert!(std::str::from_utf8(valid).is_ok());
                    std::str::from_utf8_unchecked(valid)
                };
                String::from(s)
            }
        }
    }
}

impl Mutator for RenameExportMutator {
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        info: &mut ModuleInfo,
    ) -> Result<Module> {
        let mut exports = ExportSection::new();
        let mut reader = ExportSectionReader::new(info.get_exports_section().data, 0)?;
        let max_exports = reader.get_count() as u64;
        let skip_at = rnd.gen_range(0, max_exports);

        (0..max_exports).for_each(|i| {
            let export = reader.read().unwrap();

            let new_name = if skip_at != i {
                // otherwise bypass
                String::from(export.field)
            } else {
                let new_name = self.limited_string(rnd);
                log::debug!("Renaming export {:?} by {:?}", export, new_name);
                new_name
            };

            match export.kind {
                wasmparser::ExternalKind::Function => {
                    exports.export(new_name.as_str(), Export::Function(export.index));
                }
                wasmparser::ExternalKind::Table => {
                    exports.export(new_name.as_str(), Export::Table(export.index));
                }
                wasmparser::ExternalKind::Memory => {
                    exports.export(new_name.as_str(), Export::Memory(export.index));
                }
                wasmparser::ExternalKind::Global => {
                    exports.export(new_name.as_str(), Export::Global(export.index));
                }
                wasmparser::ExternalKind::Module => {
                    exports.export(new_name.as_str(), Export::Module(export.index));
                }
                wasmparser::ExternalKind::Instance => {
                    exports.export(new_name.as_str(), Export::Instance(export.index));
                }
                _ => {
                    panic!("Unknown export {:?}", export)
                }
            }
        });
        Ok(info.replace_section(info.exports.unwrap(), &exports))
    }

    fn can_mutate<'a>(&self, _: &'a WasmMutate, info: &ModuleInfo) -> Result<bool> {
        Ok(info.has_exports())
    }
}

// Concrete implementations
pub struct ReturnI32SnipMutator;

impl Mutator for ReturnI32SnipMutator {
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        info: &mut ModuleInfo,
    ) -> Result<Module> {
        let mut codes = CodeSection::new();
        let code_section = info.get_code_section();
        let mut reader = CodeSectionReader::new(code_section.data, 0)?;
        let count = reader.get_count();
        let function_to_mutate = rnd.gen_range(0, count);
        let ftype = info.get_functype_idx(function_to_mutate as usize);

        (0..count).for_each(|i| {
            if i == function_to_mutate {
                log::debug!("Snip function idx {:?}", function_to_mutate);
                let locals = vec![];
                let mut f = Function::new(locals);

                match ftype {
                    TypeInfo::Func(t) => {
                        t.returns.iter().for_each(|primitive| match primitive {
                            PrimitiveTypeInfo::I32 => {
                                f.instruction(Instruction::I32Const(0));
                            }
                            PrimitiveTypeInfo::I64 => {
                                f.instruction(Instruction::I64Const(0));
                            }
                            PrimitiveTypeInfo::F32 => {
                                f.instruction(Instruction::F32Const(0.0));
                            }
                            PrimitiveTypeInfo::F64 => {
                                f.instruction(Instruction::F64Const(0.0));
                            }
                        });
                    }
                    _ => panic!("Unconsistent function type"),
                };

                f.instruction(Instruction::End);

                codes.function(&f);
            } else {
                let f = reader.read().unwrap();
                codes.raw(&info.input_wasm[f.range().start..f.range().end]);
            }
        });
        Ok(info.replace_section(info.code.unwrap(), &codes))
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate, info: &ModuleInfo) -> Result<bool> {
        Ok(!config.preserve_semantics && info.has_code())
    }
}

pub struct SetFunction2Unreachable;

impl Mutator for SetFunction2Unreachable {
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        info: &mut ModuleInfo,
    ) -> Result<Module> {
        let mut codes = CodeSection::new();
        let code_section = info.get_code_section();
        let mut reader = CodeSectionReader::new(code_section.data, 0)?;
        let count = reader.get_count();
        let function_to_mutate = rnd.gen_range(0, count);
        (0..count).for_each(|i| {
            if i == function_to_mutate {
                log::debug!("Changing function idx {:?}", i);
                let locals = vec![];
                let mut f = Function::new(locals);
                f.instruction(Instruction::Unreachable);
                f.instruction(Instruction::End);

                codes.function(&f);
            } else {
                let f = reader.read().unwrap();
                codes.raw(&info.input_wasm[f.range().start..f.range().end]);
            }
        });
        Ok(info.replace_section(info.code.unwrap(), &codes))
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate, info: &ModuleInfo) -> Result<bool> {
        Ok(!config.preserve_semantics && info.has_code())
    }
}


pub struct SwapCommutativeOperator<'a>{
    commutative_op_locations: &'a mut Vec<(u32, u32)> // (function offset, instruction offset)
}

impl Mutator for SwapCommutativeOperator<'_> {
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        info: &mut ModuleInfo,
    ) -> Result<Module> {
        let mut codes = CodeSection::new();
        let code_section = info.get_code_section();
        let mut reader = CodeSectionReader::new(code_section.data, 0)?;
        let count = reader.get_count();
        let operation_to_mutate = rnd.gen_range(0, self.commutative_op_locations.len());
        let (fidx, opidx) = self.commutative_op_locations[operation_to_mutate];


        (0..count).for_each(|i| {
            let f = reader.read().unwrap();

            if i == fidx {
                log::debug!("Changing function idx {:?}", i);
                // add two temporary locals, the last two ones
                let mut localreader = f.get_locals_reader().unwrap();

                // Get current locals and map to encoder types
                let mut local_count = 0;
                let mut current_locals = (0..localreader.get_count()).map(|f|{
                    let (count, ty) = localreader.read().unwrap();
                    local_count += count;
                    (count, super::module::map_type(ty).unwrap())
                }).collect::<Vec<(u32, ValType)>>();

                current_locals.push((2, ValType::I32));

                let mut newf = Function::new(current_locals);
                let opreader= f.get_operators_reader().unwrap();
                let mut idx = 0;

                for op in opreader{

                    let borrowed = op.unwrap();
                    if idx == opidx {
                        // Inject new code to swap operands
                        newf.instruction(Instruction::LocalTee(local_count + 1));
                        newf.instruction(Instruction::LocalTee(local_count + 2));
                    } 
                    newf.instruction(super::module::map_operator(borrowed).unwrap());
                    idx += 1
                }

                codes.function(&newf);
            } else {
                codes.raw(&info.input_wasm[f.range().start..f.range().end]);
            }
        });
        Ok(info.replace_section(info.code.unwrap(), &codes))
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate, info: &ModuleInfo) -> Result<bool> {

        // shortcut if there is no code
        if !info.has_code() {
            return Ok(false)
        }
        // Check is some function body has commutative operators
        // +, *, bit and, bit or, bit xor
        // Save where this operators are in the AST ?
        let code_section = info.get_code_section();
        let mut reader = CodeSectionReader::new(code_section.data, 0)?;
        let f = reader.read().unwrap();
        let opcodereader  = f.get_operators_reader()?;
        let mut idx = 0;
        let mut v = Vec::new();

        for op in opcodereader{
            match op.unwrap() {
                wasmparser::Operator::I32Add => {
                    v.push((0, idx))
                },
                _ => {
                    // bypass
                }
            }
            idx += 1;
        }
        
        // At least one commutative op
        Ok(info.has_code()) // && self.commutative_op_locations.len() > 0)
    }
}

#[cfg(test)]
mod tests {
    use crate::{WasmMutate, mutators::{RemoveExportMutator, RenameExportMutator, SetFunction2Unreachable, SwapCommutativeOperator}};
    use rand::{rngs::SmallRng, SeedableRng};

    use super::{Mutator, ReturnI32SnipMutator};

    #[test]
    fn test_code_snip_mutator() {
        let wat = r#"
        (module
            (func (result i64)
                i64.const 42
            )
        )
        "#;
        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(wat).unwrap();

        let mutator = ReturnI32SnipMutator {};

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        assert_eq!(can_mutate, true);

        let mut rnd = SmallRng::seed_from_u64(0);
        let mutation = mutator.mutate(&wasmmutate, &mut rnd, &mut info);

        let mutation_bytes = mutation.unwrap().finish();

        // If it fails, it is probably an invalid
        let text = wasmprinter::print_bytes(mutation_bytes).unwrap();

        assert_eq!("(module\n  (type (;0;) (func (result i64)))\n  (func (;0;) (type 0) (result i64)\n    i64.const 0))", text)
    }

    #[test]
    fn test_code_unreachable_mutator() {
        let wat = r#"
        (module
            (func (result i32)
                i32.const 42
            )
        )
        "#;
        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(wat).unwrap();

        let mutator = SetFunction2Unreachable {};

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        assert_eq!(can_mutate, true);

        let mut rnd = SmallRng::seed_from_u64(0);
        let mutation = mutator.mutate(&wasmmutate, &mut rnd, &mut info);

        let mutation_bytes = mutation.unwrap().finish();

        // If it fails, it is probably an invalid
        let text = wasmprinter::print_bytes(mutation_bytes).unwrap();

        assert_eq!("(module\n  (type (;0;) (func (result i32)))\n  (func (;0;) (type 0) (result i32)\n    unreachable))", text)
    }

    #[test]
    fn test_remove_export_mutator() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
        (module
            (func (export "exported_func") (result i32)
                i32.const 42
            )
        )
        "#;

        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(wat).unwrap();

        let mutator = RemoveExportMutator {};

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        assert_eq!(can_mutate, true);

        let mut rnd = SmallRng::seed_from_u64(0);
        let mutation = mutator.mutate(&wasmmutate, &mut rnd, &mut info);

        let mutation_bytes = mutation.unwrap().finish();

        // If it fails, it is probably an invalid
        let text = wasmprinter::print_bytes(mutation_bytes).unwrap();
        assert_eq!("(module\n  (type (;0;) (func (result i32)))\n  (func (;0;) (type 0) (result i32)\n    i32.const 42))", text)
    }

    #[test]
    fn test_rename_export_mutator() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
        (module
            (func (export "exported_func") (result i32)
                i32.const 42
            )
        )
        "#;

        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(wat).unwrap();

        let mutator = RenameExportMutator { max_name_size: 2 }; // the string is empty

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        assert_eq!(can_mutate, true);

        let mut rnd = SmallRng::seed_from_u64(0);
        let mutation = mutator.mutate(&wasmmutate, &mut rnd, &mut info);

        let mutation_bytes = mutation.unwrap().finish();

        // If it fails, it is probably an invalid
        let text = wasmprinter::print_bytes(mutation_bytes).unwrap();
        assert_eq!("(module\n  (type (;0;) (func (result i32)))\n  (func (;0;) (type 0) (result i32)\n    i32.const 42)\n  (export \"\" (func 0)))", text)
    }


    #[test]
    fn test_commutative_op_mutator() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
                i32.const 76
                i32.add
            )
        )
        "#;

        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(wat).unwrap();

        let mut vec = Vec::new();
        vec.push((
            0, 2 // the second instruction is
        ));
        let mutator = SwapCommutativeOperator { commutative_op_locations: &mut vec}; // the string is empty

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        assert_eq!(can_mutate, true);

        let mut rnd = SmallRng::seed_from_u64(0);
        let mutation = mutator.mutate(&wasmmutate, &mut rnd, &mut info);

        let mutation_bytes = mutation.unwrap().finish();

        // If it fails, it is probably an invalid
        let text = wasmprinter::print_bytes(mutation_bytes).unwrap();
        assert_eq!("(module\n  (type (;0;) (func (result i32)))\n  (func (;0;) (type 0) (result i32)\n    i32.const 42)\n  (export \"\" (func 0)))", text)
    }
}
