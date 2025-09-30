use anyhow::Result;
use clap::Parser;

/// Print the textual form of a WebAssembly binary.
#[derive(Parser)]
#[clap(after_help = "\
Examples:

    # Print the textual form of `foo.wasm` to stdout.
    $ wasm-tools print foo.wasm
(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func $add (;0;) (type 0) (param $lhs i32) (param $rhs i32) (result i32)
    local.get $lhs
    local.get $rhs
    i32.add
 )

    # Print a \"skeleton\" form of `foo.wasm` to stdout.
    $ wasm-tools print foo.wasm --skeleton
(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func $add (;0;) (type 0) (param $lhs i32) (param $rhs i32) (result i32) ...)

    # Print the textual form of `foo.wasm` to stdout, with folded instructions,
    # binary offsets, and indented 6 spaces.
    $ wasm-tools print foo.wasm -p -f --indent 6
(module
(;@b     ;)      (type (;0;) (func (param i32 i32) (result i32)))
(;@37    ;)      (func $add (;0;) (type 0) (param $lhs i32) (param $rhs i32) (result i32)
(;@3c    ;)            (i32.add
(;@38    ;)                  (local.get $lhs)
(;@3a    ;)                  (local.get $rhs))
                 )

    # Print the textual form of `foo.wasm` to stdout, with synthesized names for
    # items without a name in the `name` section. (Notice below that the type
    # denoted by ;0; in the previous examples has been given a name.)
    $ wasm-tools print foo.wasm --name-unnamed
(module
  (type $#type0 (;0;) (func (param i32 i32) (result i32)))
  (func $add (;0;) (type $#type0) (param $lhs i32) (param $rhs i32) (result i32)
...

    # Print the textual form of `foo.wasm` to the file `foo.wat`.
    $ wasm-tools print foo.wasm -o foo.wat

Exit status:
    0 on success,
    nonzero if the input file fails to parse.
")]
pub struct Opts {
    #[clap(flatten)]
    generate_dwarf: wasm_tools::GenerateDwarfArg,

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
    /// (See https://webassembly.github.io/spec/core/text/instructions.html#folded-instructions)
    #[clap(short, long)]
    fold_instructions: bool,

    /// Print the contents of the operand stack within function bodies
    #[clap(long)]
    print_operand_stack: bool,

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
        let wasm = self.io.get_input_wasm(Some(&self.generate_dwarf))?;

        let mut config = wasmprinter::Config::new();
        config.print_offsets(self.print_offsets);
        config.print_skeleton(self.skeleton);
        config.name_unnamed(self.name_unnamed);
        config.fold_instructions(self.fold_instructions);
        config.print_operand_stack(self.print_operand_stack);
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
