use anyhow::Result;
use clap::Parser;

/// Print the textual form of a WebAssembly binary.
#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// Whether or not to print binary offsets intermingled in the text format
    /// as comments for debugging.
    #[clap(short, long)]
    print_offsets: bool,

    /// Indicates that the "skeleton" of a module should be printed.
    ///
    /// Items such as function bodies, data segments, and element segments are
    /// replaced with "..." instead of printing their actual contents.
    #[clap(long)]
    skeleton: bool,

    /// Ensure all wasm items have `$`-based names, even if they don't have an
    /// entry in the `name` section.
    ///
    /// This option, when enabled, will synthesize names for any item which
    /// doesn't previously have a name.
    #[clap(long)]
    name_unnamed: bool,

    /// Print instructions in the folded format.
    #[clap(short, long)]
    fold_instructions: bool,

    /// The string to use when indenting.
    #[clap(long)]
    indent_text: Option<String>,
    /// Number of spaces used for indentation, has lower priority than `--indent-text`
    #[clap(long)]
    indent: Option<usize>,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let wasm = self.io.parse_input_wasm()?;

        let mut config = wasmprinter::Config::new();
        config.print_offsets(self.print_offsets);
        config.print_skeleton(self.skeleton);
        config.name_unnamed(self.name_unnamed);
        config.fold_instructions(self.fold_instructions);
        match self.indent_text.as_ref() {
            Some(s) => {
                config.indent_text(s);
            }
            None => {
                if let Some(s) = self.indent {
                    config.indent_text(&" ".repeat(s));
                }
            }
        }
        self.io.output(wasm_tools::Output::Wat {
            wasm: &wasm,
            config,
        })
    }
}
