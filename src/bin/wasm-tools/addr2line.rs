use addr2line::{Context, LookupResult};
use anyhow::{anyhow, bail, Context as _, Result};
use gimli::EndianSlice;
use std::collections::HashMap;
use std::io::Write;
use std::ops::Range;
use std::u64;
use wasmparser::{Encoding, Parser, Payload};

/// Translate a WebAssembly address to a filename and line number using DWARF
/// debugging information.
///
/// WebAssembly binaries compiled with Clang can have DWARF debug information
/// inserted into them to map from WebAssembly instruction offsets to original
/// filenames and line numbers. For example when compiling C the `-g` argument
/// can be used or when compiling Rust the `-Cdebuginfo=1` argument can be used
/// (or the default `dev` profile for Cargo). This subcommand will parse the
/// DWARF debugging information and translate a list of addresses to their
/// original filenames and line numbers.
///
/// Each address may have multiple lines printed for it indicating that the
/// address is an inlined function into another function. Frames are printed
/// innermost or youngest first.
#[derive(clap::Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// Addresses to convert to filenames and line numbers.
    ///
    /// Arguments can be specified as either `0x...` or `@...` in hexadecimal or
    /// are otherwise parsed as a base-10 address. Addresses should be relative
    /// to the beginning of the module unless `--code-section-relative` is
    /// passed in which case they should be relative to the beginning of the
    /// contents of the code section.
    addresses: Vec<String>,

    /// Indicates that addresses are code-section-relative instead of offsets
    /// from the beginning of the module.
    #[clap(long)]
    code_section_relative: bool,
}

struct Module<'a> {
    range: Range<u64>,
    code_start: Option<u64>,
    custom_sections: HashMap<&'a str, &'a [u8]>,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let wasm = self.io.parse_input_wasm()?;

        let modules = self
            .parse_custom_sections(&wasm)
            .context("failed to parse input and read custom sections")?;
        let mut output = self.io.output_writer()?;

        for addr in self.addresses.iter() {
            self.addr2line(&addr, &modules, &mut output)
                .with_context(|| format!("failed to find frames for `{addr}`"))?;
        }

        Ok(())
    }

    fn parse_custom_sections<'a>(&self, wasm: &'a [u8]) -> Result<Vec<Module<'a>>> {
        let mut ret = Vec::new();
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
                        ret.push(module);
                    }
                }
                _ => {}
            }
        }
        Ok(ret)
    }

    fn addr2line(&self, addr: &str, modules: &[Module<'_>], out: &mut dyn Write) -> Result<()> {
        // Support either `0x` or `@` prefixes for hex addresses since 0x is
        // standard and @ is used by wasmprinter (and web browsers I think?)
        let addr = if let Some(hex) = addr.strip_prefix("0x").or_else(|| addr.strip_prefix("@")) {
            u64::from_str_radix(hex, 16)?
        } else {
            addr.parse()?
        };

        let module = modules
            .iter()
            .find(|module| module.range.start <= addr && addr <= module.range.end)
            .ok_or_else(|| anyhow!("no module found which contains this address"))?;

        let dwarf = gimli::Dwarf::load(|id| -> Result<_> {
            let data = module
                .custom_sections
                .get(id.name())
                .copied()
                .unwrap_or(&[]);
            Ok(EndianSlice::new(data, gimli::LittleEndian))
        })?;
        let cx = Context::from_dwarf(dwarf)
            .context("failed to create addr2line dwarf mapping context")?;

        // Addresses in DWARF are relative to the start of the text section, so
        // factor that in here.
        let text_relative_addr = if self.code_section_relative {
            addr
        } else {
            match module.code_start {
                Some(start) => addr
                    .checked_sub(start)
                    .context("address is before the beginning of the text section")?,
                None => bail!("no code section found in module"),
            }
        };

        let mut frames = match cx.find_frames(text_relative_addr) {
            LookupResult::Output(result) => result?,
            LookupResult::Load { .. } => {
                bail!("split-dwarf is not supported yet");
            }
        };

        let mut first = true;
        while let Some(frame) = frames.next()? {
            if first {
                write!(out, "{addr:#x}: ")?;
            } else {
                write!(out, "\t")?;
            }
            first = false;
            if let Some(func) = &frame.function {
                write!(out, "{}", func.demangle()?)?;
            } else {
                write!(out, "<unnamed>")?;
            }

            if let Some(loc) = &frame.location {
                write!(out, " ")?;
                if let Some(file) = loc.file {
                    write!(out, "{file}")?;
                }
                if let Some(line) = loc.line {
                    write!(out, ":{line}")?;
                }
                if let Some(column) = loc.column {
                    write!(out, ":{column}")?;
                }
            }
            writeln!(out, "")?;
        }
        if first {
            writeln!(out, "{addr:#x}: no dwarf frames found for this address")?;
        }
        Ok(())
    }
}
