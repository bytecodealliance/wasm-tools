use anyhow::{Context, Result};
use clap::Parser;
use std::path::PathBuf;
use wast::Wast;
use wast::lexer::Lexer;
use wast::parser::{self, ParseBuffer};

/// Convert a `*.wast` WebAssembly spec test into a `*.json` file and `*.wasm`
/// files.
///
/// This subcommand will parse a wasm spec test, in the `*.wast` format, and
/// then emit both a JSON file and a number of auxiliary modules. This can
/// perform all the parsing for various implementations of the wasm text format
/// while deferring the actual processing of all commands.
///
/// This subcommand is modelled after WABT's `wast2json` tool, found at
/// <https://github.com/WebAssembly/wabt>. More documentation of the format can
/// be found at
/// <https://github.com/WebAssembly/wabt/blob/main/docs/wast2json.md>.
///
/// Note that this subcommand does not output precisely the same output as
/// `wast2json`, but there should be no major discrepancies.
///
/// This will also emit a number of `*.wasm` and `*.wat` files in the current
/// directory (or in `--wasm-dir`) which the JSON will reference. This command
/// will print the JSON to stdout unless the `-o` flag is given.
#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    general: wasm_tools::GeneralOpts,

    #[clap(flatten)]
    output: wasm_tools::OutputArg,

    /// Where to place binary and text WebAssembly files referenced by tests.
    ///
    /// Defaults to the current directory.
    #[clap(long)]
    wasm_dir: Option<PathBuf>,

    /// Input `*.wast` file that will be parsed and converted to JSON.
    wast: String,

    /// Output pretty-printed JSON instead of compact json.
    #[clap(long)]
    pretty: bool,

    /// Controls the "allow confusing unicode" option which will reject parsing
    /// files that have unusual characters.
    ///
    /// This is defaulted to `true` to enable parsing all upstream spec tests
    /// but can be disabled if desired too.
    #[clap(long, value_name = "true|false")]
    allow_confusing_unicode: Option<bool>,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        &self.general
    }

    pub fn run(&self) -> Result<()> {
        let contents = std::fs::read_to_string(&self.wast)
            .with_context(|| format!("failed to read input wast file: {:?}", self.wast))?;
        let mut lexer = Lexer::new(&contents);
        lexer.allow_confusing_unicode(self.allow_confusing_unicode.unwrap_or(true));
        let adjust_error = |mut err: wast::Error| {
            err.set_path(self.wast.as_ref());
            err.set_text(&contents);
            err
        };
        let buf = ParseBuffer::new_with_lexer(lexer).map_err(&adjust_error)?;
        let wast = parser::parse::<Wast>(&buf).map_err(&adjust_error)?;

        let ret = json_from_wast::Wast::from_ast(&self.wast, &contents, wast)?;

        for (name, wasm) in ret.wasms.iter() {
            let dst = match &self.wasm_dir {
                Some(dir) => dir.join(name),
                None => name.into(),
            };
            std::fs::write(&dst, wasm).with_context(|| format!("failed to write {dst:?}"))?;
        }

        let json = if self.pretty {
            serde_json::to_string_pretty(&ret)?
        } else {
            serde_json::to_string(&ret)?
        };
        self.output
            .output(&self.general, wasm_tools::Output::Json(&json))?;
        Ok(())
    }
}
