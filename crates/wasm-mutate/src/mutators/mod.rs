use std::borrow::Borrow;
use std::io::Write;
use std::primitive;

use crate::module::*;
use crate::{ModuleInfo, WasmMutate};
use rand::{prelude::SmallRng, Rng, RngCore};
use wasm_encoder::{
    encoders, CodeSection, Export, ExportSection, Function, Instruction, Module, RawSection,
    Section, SectionId,
};
use wasmparser::{Chunk, CodeSectionReader, ExportSectionReader, Parser, Payload, Range};

pub trait Mutator {
    /// Method where the mutation happpens
    ///
    /// * `context` instance of WasmMutate
    /// * `chunk` piece of the byte stream corresponding with the payload
    /// * `out_buffer` resulting mutated byte stream
    /// * `mutable` mutable object
    fn mutate(&self, _: &WasmMutate, info: &mut ModuleInfo) -> Module {
        Module::new()
    }

    /// Returns if this mutator can be applied with the info and the byte range in which it can be applied
    fn can_mutate<'a>(&self, _: &'a WasmMutate, info: &ModuleInfo) -> bool {
        false
    }

    /// Provides the name of the mutator, mostly used for debugging purposes
    fn name(&self) -> String {
        return format!("{:?}", std::any::type_name::<Self>());
    }
}

macro_rules! parse_loop {
    ($chunk: expr, $(($pat: pat, $bytes: ident, $todo: tt),)*) => {
        let mut parser = Parser::new(0);
        // Hack !
        parser.parse(b"\0asm\x01\0\0\0", false);
        let mut consumed = 0;
        let sectionsize = $chunk.len();

        loop {
            let (payload, chunksize) = match parser.parse(&$chunk[consumed..], false).unwrap() {
                Chunk::NeedMoreData(_) => {
                    panic!("Invalid Wasm module");
                },
                Chunk::Parsed { consumed, payload } => (payload, consumed),
            };

            match payload {
                $(
                    $pat => {
                        let $bytes = &$chunk[consumed..consumed+chunksize];
                        $todo
                    },
                )*
                _ => panic!("This mutator cannot be applied to this section")
            }

            consumed += chunksize;

            if consumed == sectionsize {
                break
            }
        }
    };
}

pub struct RemoveExportMutator;

impl Mutator for RemoveExportMutator {
    fn mutate(&self, config: &WasmMutate, info: &mut ModuleInfo) -> Module {
        let mut exports = ExportSection::new();
        let mut reader = ExportSectionReader::new(info.get_exports_section().data, 0).unwrap();
        let max_exports = reader.get_count() as u64;
        let skip_at = config.get_rnd().gen_range(0, max_exports);

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
        // Ugly solution
        let mut modu = Module::new();
        info.raw_sections.iter().enumerate().for_each(|(idx, s)| {
            // if section if this one...replace
            if info.exports.unwrap() == idx {
                modu.section(&exports);
            } else {
                modu.section(s);
            }
        });

        modu
    }

    fn can_mutate<'a>(&self, _: &'a WasmMutate, info: &ModuleInfo) -> bool {
        info.has_exports()
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
    fn mutate<'a>(&self, config: &WasmMutate, info: &mut ModuleInfo<'a>) -> Module {
        let mut exports = ExportSection::new();
        let mut reader = ExportSectionReader::new(info.get_exports_section().data, 0).unwrap();
        let max_exports = reader.get_count() as u64;
        let skip_at = config.get_rnd().gen_range(0, max_exports);

        (0..max_exports).for_each(|i| {
            let export = reader.read().unwrap();

            let new_name = if skip_at != i {
                // otherwise bypass
                String::from(export.field)
            } else {
                let new_name = self.limited_string(&mut config.get_rnd());
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

        // Ugly solution
        let mut modu = Module::new();
        info.raw_sections.iter().enumerate().for_each(|(idx, s)| {
            // if section if this one...replace
            if info.exports.unwrap() == idx {
                modu.section(&exports);
            } else {
                modu.section(s);
            }
        });

        modu
    }

    fn can_mutate<'a>(&self, _: &'a WasmMutate, info: &ModuleInfo) -> bool {
        info.has_exports()
    }
}

// Concrete implementations
pub struct ReturnI32SnipMutator;

impl Mutator for ReturnI32SnipMutator {
    fn mutate<'a>(&self, config: &WasmMutate, info: &mut ModuleInfo<'a>) -> Module {
        let mut codes = CodeSection::new();
        let code_section = info.get_code_section();
        let mut reader = CodeSectionReader::new(code_section.data, 0).unwrap();
        let count = reader.get_count();
        let function_to_mutate = config.get_rnd().gen_range(0, count);
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
                let funclone = Function::full_raw(f.get_func_bytes().to_vec());
                codes.function(&funclone);
            }
        });

        let mut modu = Module::new();
        info.raw_sections.iter().enumerate().for_each(|(idx, s)| {
            // if section if this one...replace
            if info.code.unwrap() == idx {
                modu.section(&codes);
            } else {
                modu.section(s);
            }
        });

        modu
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate, info: &ModuleInfo) -> bool {
        !config.preserve_semantics && info.has_code()
    }
}

pub struct SetFunction2Unreachable;

impl Mutator for SetFunction2Unreachable {
    fn mutate<'a>(&self, config: &WasmMutate, info: &mut ModuleInfo<'a>) -> Module {
        let mut codes = CodeSection::new();
        let code_section = info.get_code_section();
        let mut reader = CodeSectionReader::new(code_section.data, 0).unwrap();
        let count = reader.get_count();
        let function_to_mutate = config.get_rnd().gen_range(0, count);
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
                let funclone = Function::full_raw(f.get_func_bytes().to_vec());
                codes.function(&funclone);
            }
        });

        let mut modu = Module::new();
        info.raw_sections.iter().enumerate().for_each(|(idx, s)| {
            // if section if this one...replace
            if info.code.unwrap() == idx {
                modu.section(&codes);
            } else {
                modu.section(s);
            }
        });

        modu
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate, info: &ModuleInfo) -> bool {
        !config.preserve_semantics && info.has_code()
    }
}
