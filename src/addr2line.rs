//! Shared support for `addr2line` and `validate` to parse DWARF sections.

use addr2line::Context;
use anyhow::{bail, Context as _, Result};
use gimli::EndianSlice;
use std::collections::HashMap;
use std::ops::Range;
use wasmparser::{Encoding, Parser, Payload};

pub struct Addr2lineModules<'a> {
    modules: Vec<Module<'a>>,
}

struct Module<'a> {
    range: Range<u64>,
    code_start: Option<u64>,
    custom_sections: HashMap<&'a str, &'a [u8]>,
    context: Option<Context<EndianSlice<'a, gimli::LittleEndian>>>,
}

impl<'a> Addr2lineModules<'a> {
    pub fn parse(wasm: &'a [u8]) -> Result<Self> {
        let mut modules = Vec::new();
        let mut cur_module = None;
        for payload in Parser::new(0).parse_all(wasm) {
            match payload? {
                Payload::Version {
                    encoding: Encoding::Module,
                    range,
                    ..
                } => {
                    assert!(cur_module.is_none());
                    cur_module = Some(Module {
                        range: range.start as u64..0,
                        code_start: None,
                        custom_sections: HashMap::new(),
                        context: None,
                    });
                }

                Payload::CustomSection(s) => {
                    if let Some(cur) = &mut cur_module {
                        cur.custom_sections.insert(s.name(), s.data());
                    }
                }
                Payload::CodeSectionStart { range, .. } => {
                    assert!(cur_module.is_some());
                    cur_module.as_mut().unwrap().code_start = Some(range.start as u64);
                }

                Payload::End(offset) => {
                    if let Some(mut module) = cur_module.take() {
                        module.range.end = offset as u64;
                        modules.push(module);
                    }
                }
                _ => {}
            }
        }
        Ok(Addr2lineModules { modules })
    }

    pub fn context(
        &mut self,
        addr: u64,
        code_section_relative: bool,
    ) -> Result<Option<(&mut Context<EndianSlice<'a, gimli::LittleEndian>>, u64)>> {
        let module = if code_section_relative {
            if self.modules.len() == 1 {
                &mut self.modules[0]
            } else {
                bail!("cannot use `--code-section-relative` with more than one module")
            }
        } else {
            match self
                .modules
                .iter_mut()
                .find(|module| module.range.start <= addr && addr <= module.range.end)
            {
                Some(module) => module,
                None => return Ok(None),
            }
        };

        let dwarf = gimli::Dwarf::load(|id| -> Result<_> {
            let data = module
                .custom_sections
                .get(id.name())
                .copied()
                .unwrap_or(&[]);
            Ok(EndianSlice::new(data, gimli::LittleEndian))
        })?;
        if module.context.is_none() {
            module.context = Some(
                Context::from_dwarf(dwarf)
                    .context("failed to create addr2line dwarf mapping context")?,
            );
        }
        let context = module.context.as_mut().unwrap();

        // Addresses in DWARF are relative to the start of the text section, so
        // factor that in here.
        let text_relative_addr = if code_section_relative {
            addr
        } else {
            match module.code_start.and_then(|start| addr.checked_sub(start)) {
                Some(rel) => rel,
                None => return Ok(None),
            }
        };

        Ok(Some((context, text_relative_addr)))
    }
}
