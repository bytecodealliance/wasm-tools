use addr2line::LookupResult;
use anyhow::{bail, Context as _, Result};
use std::io::Write;
use std::u64;
use wasm_tools::addr2line::Addr2lineModules;

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

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let wasm = self.io.parse_input_wasm()?;

        let mut modules = Addr2lineModules::parse(&wasm)
            .context("failed to parse input and read custom sections")?;
        let mut output = self.io.output_writer()?;

        for addr in self.addresses.iter() {
            self.addr2line(&addr, &mut modules, &mut output)
                .with_context(|| format!("failed to find frames for `{addr}`"))?;
        }

        Ok(())
    }

    fn addr2line(
        &self,
        addr: &str,
        modules: &mut Addr2lineModules<'_>,
        out: &mut dyn Write,
    ) -> Result<()> {
        // Support either `0x` or `@` prefixes for hex addresses since 0x is
        // standard and @ is used by wasmprinter (and web browsers I think?)
        let addr = if let Some(hex) = addr.strip_prefix("0x").or_else(|| addr.strip_prefix("@")) {
            u64::from_str_radix(hex, 16)?
        } else {
            addr.parse()?
        };

        let (cx, text_relative_addr) = match modules.context(addr, self.code_section_relative)? {
            Some(pair) => pair,
            None => bail!("no module found which contains this address"),
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
