//! Shared input/output routines amongst most `wasm-tools` subcommands

use anyhow::{Context, Result, bail};
use std::fs::File;
use std::io::IsTerminal;
use std::io::{BufWriter, Read, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use termcolor::{Ansi, ColorChoice, NoColor, StandardStream, WriteColor};

#[cfg(any(feature = "addr2line", feature = "validate"))]
pub mod addr2line;
#[cfg(any(feature = "component", feature = "wit-dylib"))]
pub mod wit;

#[derive(clap::Parser)]
pub struct GeneralOpts {
    /// Use verbose output (-v info, -vv debug, -vvv trace).
    #[clap(long = "verbose", short = 'v', action = clap::ArgAction::Count)]
    verbose: u8,

    /// Configuration over whether terminal colors are used in output.
    ///
    /// Supports one of `auto|never|always|always-ansi`. The default is to
    /// detect what to do based on the terminal environment, for example by
    /// using `isatty`.
    #[clap(long = "color", default_value = "auto")]
    pub color: ColorChoice,
}

impl GeneralOpts {
    /// Initializes the logger based on the verbosity level.
    pub fn init_logger(&self) {
        let default = match self.verbose {
            0 => "warn",
            1 => "info",
            2 => "debug",
            _ => "trace",
        };

        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or(default))
            .format_target(false)
            .init();
    }
}

// This is intended to be included in a struct as:
//
//      #[clap(flatten)]
//      io: wasm_tools::InputOutput,
//
// and then the methods are used to read the arguments,
#[derive(clap::Parser)]
pub struct InputOutput {
    #[clap(flatten)]
    input: InputArg,

    #[clap(flatten)]
    output: OutputArg,

    #[clap(flatten)]
    general: GeneralOpts,
}

#[derive(clap::Parser)]
pub struct GenerateDwarfArg {
    /// Optionally generate DWARF debugging information from WebAssembly text
    /// files.
    ///
    /// When the input to this command is a WebAssembly text file, such as
    /// `*.wat`, then this option will instruct the text parser to insert DWARF
    /// debugging information to map binary locations back to the original
    /// source locations in the input `*.wat` file. This option has no effect if
    /// the `INPUT` argument is already a WebAssembly binary or if the text
    /// format uses `(module binary ...)`.
    #[clap(
        long,
        value_name = "lines|full",
        conflicts_with = "generate_full_dwarf"
    )]
    generate_dwarf: Option<GenerateDwarf>,

    /// Shorthand for `--generate-dwarf full`
    #[clap(short, conflicts_with = "generate_dwarf")]
    generate_full_dwarf: bool,
}

#[derive(clap::Parser)]
pub struct InputArg {
    /// Input file to process.
    ///
    /// If not provided or if this is `-` then stdin is read entirely and
    /// processed. Note that for most subcommands this input can either be a
    /// binary `*.wasm` file or a textual format `*.wat` file.
    input: Option<PathBuf>,
}

#[derive(Copy, Clone)]
enum GenerateDwarf {
    Lines,
    Full,
}

impl FromStr for GenerateDwarf {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<GenerateDwarf> {
        match s {
            "lines" => Ok(GenerateDwarf::Lines),
            "full" => Ok(GenerateDwarf::Full),
            other => bail!("unknown `--generate-dwarf` setting: {other}"),
        }
    }
}

impl InputArg {
    pub fn get_binary_wasm(
        &self,
        generate_dwarf_optional: Option<&GenerateDwarfArg>,
    ) -> Result<Vec<u8>> {
        let mut parser = wat::Parser::new();
        match generate_dwarf_optional {
            None => {}
            Some(generate_dwarf) => match (
                generate_dwarf.generate_full_dwarf,
                generate_dwarf.generate_dwarf,
            ) {
                (false, Some(GenerateDwarf::Lines)) => {
                    parser.generate_dwarf(wat::GenerateDwarf::Lines);
                }
                (true, _) | (false, Some(GenerateDwarf::Full)) => {
                    parser.generate_dwarf(wat::GenerateDwarf::Full);
                }
                (false, None) => {}
            },
        }
        if let Some(path) = &self.input {
            if path != Path::new("-") {
                let bytes = parser.parse_file(path)?;
                return Ok(bytes);
            }
        }
        let mut stdin = Vec::new();
        std::io::stdin()
            .read_to_end(&mut stdin)
            .context("failed to read <stdin>")?;
        let bytes = parser.parse_bytes(Some("<stdin>".as_ref()), &stdin)?;
        Ok(bytes.into_owned())
    }
}

#[derive(clap::Parser)]
pub struct OutputArg {
    /// Where to place output.
    ///
    /// Required when printing WebAssembly binary output.
    ///
    /// If not provided, then stdout is used.
    #[clap(short, long)]
    output: Option<PathBuf>,
}

pub enum Output<'a> {
    #[cfg(feature = "component")]
    Wit {
        wit: &'a wit_component::DecodedWasm,
        printer: wit_component::WitPrinter,
    },
    Wasm(&'a [u8]),
    Wat {
        wasm: &'a [u8],
        config: wasmprinter::Config,
    },
    Json(&'a str),
}

impl InputOutput {
    pub fn parse_input_wasm(&self, generate_dwarf: Option<&GenerateDwarfArg>) -> Result<Vec<u8>> {
        let ret = self.get_input_wasm(generate_dwarf)?;
        parse_binary_wasm(wasmparser::Parser::new(0), &ret, false)?;
        Ok(ret)
    }

    pub fn get_input_wasm(&self, generate_dwarf: Option<&GenerateDwarfArg>) -> Result<Vec<u8>> {
        self.input.get_binary_wasm(generate_dwarf)
    }

    pub fn output_wasm(&self, wasm: &[u8], wat: bool) -> Result<()> {
        if wat {
            self.output(Output::Wat {
                wasm,
                config: Default::default(),
            })
        } else {
            self.output(Output::Wasm(wasm))
        }
    }

    pub fn output(&self, bytes: Output<'_>) -> Result<()> {
        self.output.output(&self.general, bytes)
    }

    pub fn output_writer(&self) -> Result<Box<dyn WriteColor>> {
        self.output.output_writer(self.general.color)
    }

    pub fn output_path(&self) -> Option<&Path> {
        self.output.output.as_deref()
    }

    pub fn input_path(&self) -> Option<&Path> {
        self.input.input.as_deref()
    }

    pub fn general_opts(&self) -> &GeneralOpts {
        &self.general
    }
}

impl OutputArg {
    pub fn output_wasm(&self, general: &GeneralOpts, wasm: &[u8], wat: bool) -> Result<()> {
        if wat {
            self.output(
                general,
                Output::Wat {
                    wasm,
                    config: Default::default(),
                },
            )
        } else {
            self.output(general, Output::Wasm(wasm))
        }
    }

    pub fn output(&self, general: &GeneralOpts, output: Output<'_>) -> Result<()> {
        match output {
            Output::Wat { wasm, config } => {
                let mut writer = self.output_writer(general.color)?;
                config.print(wasm, &mut wasmprinter::PrintTermcolor(&mut writer))
            }
            Output::Wasm(bytes) => {
                match &self.output {
                    Some(path) => {
                        std::fs::write(path, bytes)
                            .context(format!("failed to write `{}`", path.display()))?;
                    }
                    None => {
                        let mut stdout = std::io::stdout();
                        if stdout.is_terminal() {
                            bail!(
                                "cannot print binary wasm output to a terminal, pass the `-t` flag to print the text format"
                            );
                        }
                        stdout
                            .write_all(bytes)
                            .context("failed to write to stdout")?;
                    }
                }
                Ok(())
            }
            Output::Json(s) => self.output_str(s),
            #[cfg(feature = "component")]
            Output::Wit { wit, mut printer } => {
                let resolve = wit.resolve();
                let ids = resolve
                    .packages
                    .iter()
                    .map(|(id, _)| id)
                    .filter(|id| *id != wit.package())
                    .collect::<Vec<_>>();
                printer.print(resolve, wit.package(), &ids)?;
                let output = printer.output.to_string();
                self.output_str(&output)
            }
        }
    }

    fn output_str(&self, output: &str) -> Result<()> {
        match &self.output {
            Some(path) => {
                std::fs::write(path, output)
                    .context(format!("failed to write `{}`", path.display()))?;
            }
            None => std::io::stdout()
                .write_all(output.as_bytes())
                .context("failed to write to stdout")?,
        }
        Ok(())
    }

    pub fn output_path(&self) -> Option<&Path> {
        self.output.as_deref()
    }

    pub fn output_writer(&self, color: ColorChoice) -> Result<Box<dyn WriteColor>> {
        match &self.output {
            Some(output) => {
                let writer = BufWriter::new(File::create(&output)?);
                if color == ColorChoice::AlwaysAnsi {
                    Ok(Box::new(Ansi::new(writer)))
                } else {
                    Ok(Box::new(NoColor::new(writer)))
                }
            }
            None => {
                let stdout = std::io::stdout();
                if color == ColorChoice::Auto && !stdout.is_terminal() {
                    Ok(Box::new(StandardStream::stdout(ColorChoice::Never)))
                } else {
                    Ok(Box::new(StandardStream::stdout(color)))
                }
            }
        }
    }
}

pub fn parse_binary_wasm(
    parser: wasmparser::Parser,
    bytes: &[u8],
    parse_custom: bool,
) -> Result<()> {
    for payload in parser.parse_all(&bytes) {
        match payload? {
            wasmparser::Payload::TypeSection(s) => s.parse()?,
            wasmparser::Payload::ImportSection(s) => s.parse()?,
            wasmparser::Payload::FunctionSection(s) => s.parse()?,
            wasmparser::Payload::TableSection(s) => s.parse()?,
            wasmparser::Payload::MemorySection(s) => s.parse()?,
            wasmparser::Payload::TagSection(s) => s.parse()?,
            wasmparser::Payload::GlobalSection(s) => s.parse()?,
            wasmparser::Payload::ExportSection(s) => s.parse()?,
            wasmparser::Payload::ElementSection(s) => s.parse()?,
            wasmparser::Payload::DataSection(s) => s.parse()?,
            wasmparser::Payload::CodeSectionEntry(body) => body.parse()?,

            wasmparser::Payload::InstanceSection(s) => s.parse()?,
            wasmparser::Payload::CoreTypeSection(s) => s.parse()?,
            wasmparser::Payload::ComponentInstanceSection(s) => s.parse()?,
            wasmparser::Payload::ComponentAliasSection(s) => s.parse()?,
            wasmparser::Payload::ComponentTypeSection(s) => s.parse()?,
            wasmparser::Payload::ComponentCanonicalSection(s) => s.parse()?,
            wasmparser::Payload::ComponentImportSection(s) => s.parse()?,
            wasmparser::Payload::ComponentExportSection(s) => s.parse()?,

            wasmparser::Payload::UnknownSection { id, .. } => {
                bail!("malformed section id: {id}")
            }

            wasmparser::Payload::CustomSection(s) => {
                if parse_custom {
                    s.parse()?
                }
            }

            _ => (),
        }
    }
    return Ok(());

    trait Parse {
        fn parse(self) -> Result<()>;
    }

    impl<'a, T> Parse for wasmparser::SectionLimited<'a, T>
    where
        T: Parse + wasmparser::FromReader<'a>,
    {
        fn parse(self) -> Result<()> {
            for item in self {
                item?.parse()?;
            }
            Ok(())
        }
    }

    macro_rules! noop {
        ($($t:ty,)*) => ($(
            impl Parse for $t {
                fn parse(self) -> Result<()> {
                    Ok(())
                }
            }
        )*)
    }

    noop! {
        u32,
        &str,
        wasmparser::RecGroup,
        wasmparser::MemoryType,
        wasmparser::TagType,
        wasmparser::Export<'_>,
        wasmparser::Instance<'_>,
        wasmparser::CoreType<'_>,
        wasmparser::ComponentInstance<'_>,
        wasmparser::ComponentAlias<'_>,
        wasmparser::ComponentType<'_>,
        wasmparser::CanonicalFunction,
        wasmparser::ComponentImport<'_>,
        wasmparser::ComponentExport<'_>,
        wasmparser::ImportItemCompact<'_>,
        wasmparser::CoreDumpSection<'_>,
        wasmparser::CoreDumpStackSection<'_>,
        wasmparser::CoreDumpInstancesSection,
        wasmparser::CoreDumpModulesSection<'_>,
        wasmparser::Naming<'_>,
        wasmparser::BranchHint,
        wasmparser::ProducersFieldValue<'_>,
        wasmparser::Dylink0Subsection<'_>,
        wasmparser::Segment<'_>,
        wasmparser::InitFunc,
        wasmparser::SymbolInfo<'_>,
        wasmparser::ComdatSymbol,
        wasmparser::RelocationEntry,
    }

    impl Parse for wasmparser::Imports<'_> {
        fn parse(self) -> Result<()> {
            match self {
                wasmparser::Imports::Single(..) => Ok(()),
                wasmparser::Imports::Compact1 { items, .. } => items.parse(),
                wasmparser::Imports::Compact2 { names, .. } => names.parse(),
            }
        }
    }
    impl Parse for wasmparser::Table<'_> {
        fn parse(self) -> Result<()> {
            self.init.parse()
        }
    }
    impl Parse for wasmparser::Global<'_> {
        fn parse(self) -> Result<()> {
            self.init_expr.parse()
        }
    }
    impl Parse for wasmparser::Element<'_> {
        fn parse(self) -> Result<()> {
            self.kind.parse()?;
            self.items.parse()?;
            Ok(())
        }
    }
    impl Parse for wasmparser::Data<'_> {
        fn parse(self) -> Result<()> {
            self.kind.parse()
        }
    }
    impl Parse for wasmparser::FunctionBody<'_> {
        fn parse(self) -> Result<()> {
            let mut locals = self.get_locals_reader()?.into_iter();
            for item in locals.by_ref() {
                let _ = item?;
            }
            let mut ops = locals.into_operators_reader();
            while !ops.eof() {
                ops.read()?;
            }
            ops.finish()?;
            Ok(())
        }
    }
    impl Parse for wasmparser::TableInit<'_> {
        fn parse(self) -> Result<()> {
            match self {
                wasmparser::TableInit::RefNull => Ok(()),
                wasmparser::TableInit::Expr(e) => e.parse(),
            }
        }
    }
    impl Parse for wasmparser::ConstExpr<'_> {
        fn parse(self) -> Result<()> {
            let mut ops = self.get_operators_reader();
            while !ops.eof() {
                ops.read()?;
            }
            ops.finish()?;
            Ok(())
        }
    }
    impl Parse for wasmparser::ElementKind<'_> {
        fn parse(self) -> Result<()> {
            match self {
                wasmparser::ElementKind::Passive | wasmparser::ElementKind::Declared => Ok(()),
                wasmparser::ElementKind::Active { offset_expr, .. } => offset_expr.parse(),
            }
        }
    }
    impl Parse for wasmparser::ElementItems<'_> {
        fn parse(self) -> Result<()> {
            match self {
                wasmparser::ElementItems::Functions(f) => f.parse(),
                wasmparser::ElementItems::Expressions(_, f) => f.parse(),
            }
        }
    }
    impl Parse for wasmparser::DataKind<'_> {
        fn parse(self) -> Result<()> {
            match self {
                wasmparser::DataKind::Passive => Ok(()),
                wasmparser::DataKind::Active { offset_expr, .. } => offset_expr.parse(),
            }
        }
    }
    impl Parse for wasmparser::CustomSectionReader<'_> {
        fn parse(self) -> Result<()> {
            match self.as_known() {
                wasmparser::KnownCustom::Name(s) => s.parse(),
                wasmparser::KnownCustom::ComponentName(s) => s.parse(),
                wasmparser::KnownCustom::BranchHints(s) => s.parse(),
                wasmparser::KnownCustom::Producers(s) => s.parse(),
                wasmparser::KnownCustom::Dylink0(s) => s.parse(),
                wasmparser::KnownCustom::CoreDump(s) => s.parse(),
                wasmparser::KnownCustom::CoreDumpStack(s) => s.parse(),
                wasmparser::KnownCustom::CoreDumpInstances(s) => s.parse(),
                wasmparser::KnownCustom::CoreDumpModules(s) => s.parse(),
                wasmparser::KnownCustom::Linking(s) => s.parse(),
                wasmparser::KnownCustom::Reloc(s) => s.parse(),
                _ => Ok(()),
            }
        }
    }
    impl<'a, T> Parse for wasmparser::Subsections<'a, T>
    where
        T: Parse + wasmparser::Subsection<'a>,
    {
        fn parse(self) -> Result<()> {
            for s in self {
                s?.parse()?;
            }
            Ok(())
        }
    }
    impl Parse for wasmparser::NameSectionReader<'_> {
        fn parse(self) -> Result<()> {
            for section in self {
                section?.parse()?;
            }
            Ok(())
        }
    }
    impl Parse for wasmparser::Name<'_> {
        fn parse(self) -> Result<()> {
            match self {
                wasmparser::Name::Module { .. } => Ok(()),
                wasmparser::Name::Function(n) => n.parse(),
                wasmparser::Name::Local(n) => n.parse(),
                wasmparser::Name::Label(n) => n.parse(),
                wasmparser::Name::Type(n) => n.parse(),
                wasmparser::Name::Table(n) => n.parse(),
                wasmparser::Name::Memory(n) => n.parse(),
                wasmparser::Name::Global(n) => n.parse(),
                wasmparser::Name::Element(n) => n.parse(),
                wasmparser::Name::Data(n) => n.parse(),
                wasmparser::Name::Field(n) => n.parse(),
                wasmparser::Name::Tag(n) => n.parse(),
                wasmparser::Name::Parameter(n) => n.parse(),
                wasmparser::Name::TagParameter(n) => n.parse(),
                wasmparser::Name::Unknown { .. } => Ok(()),
            }
        }
    }
    impl Parse for wasmparser::IndirectNameMap<'_> {
        fn parse(self) -> Result<()> {
            for item in self {
                item?.parse()?;
            }
            Ok(())
        }
    }
    impl Parse for wasmparser::IndirectNaming<'_> {
        fn parse(self) -> Result<()> {
            self.names.parse()
        }
    }
    impl Parse for wasmparser::NameMap<'_> {
        fn parse(self) -> Result<()> {
            for item in self {
                item?.parse()?;
            }
            Ok(())
        }
    }
    impl Parse for wasmparser::ComponentName<'_> {
        fn parse(self) -> Result<()> {
            match self {
                Self::Component { .. } => Ok(()),
                Self::CoreFuncs(s) => s.parse(),
                Self::CoreGlobals(s) => s.parse(),
                Self::CoreMemories(s) => s.parse(),
                Self::CoreTables(s) => s.parse(),
                Self::CoreTags(s) => s.parse(),
                Self::CoreModules(s) => s.parse(),
                Self::CoreInstances(s) => s.parse(),
                Self::CoreTypes(s) => s.parse(),
                Self::Types(s) => s.parse(),
                Self::Instances(s) => s.parse(),
                Self::Components(s) => s.parse(),
                Self::Funcs(s) => s.parse(),
                Self::Values(s) => s.parse(),
                Self::Unknown { .. } => Ok(()),
            }
        }
    }
    impl Parse for wasmparser::BranchHintFunction<'_> {
        fn parse(self) -> Result<()> {
            self.hints.parse()
        }
    }
    impl Parse for wasmparser::ProducersField<'_> {
        fn parse(self) -> Result<()> {
            self.values.parse()
        }
    }
    impl Parse for wasmparser::LinkingSectionReader<'_> {
        fn parse(self) -> Result<()> {
            self.subsections().parse()
        }
    }
    impl Parse for wasmparser::Linking<'_> {
        fn parse(self) -> Result<()> {
            match self {
                Self::SegmentInfo(s) => s.parse(),
                Self::InitFuncs(s) => s.parse(),
                Self::ComdatInfo(s) => s.parse(),
                Self::SymbolTable(s) => s.parse(),
                Self::Unknown { .. } => Ok(()),
            }
        }
    }
    impl Parse for wasmparser::Comdat<'_> {
        fn parse(self) -> Result<()> {
            self.symbols.parse()
        }
    }
    impl Parse for wasmparser::RelocSectionReader<'_> {
        fn parse(self) -> Result<()> {
            self.entries().parse()
        }
    }
}

#[cfg(feature = "validate")]
pub fn validate(
    features: wasmparser::WasmFeatures,
    validate_custom: bool,
    wasm: &[u8],
) -> Result<()> {
    use rayon::prelude::*;
    use std::mem;
    use std::time::Instant;
    use wasmparser::{
        CustomSectionValidator, FuncValidatorAllocations, Parser, ValidPayload, Validator,
    };

    // Note that here we're mostly copying the contents of
    // `Validator::validate_all`, but there are currently two divergences:
    //
    // * The end of module validation is followed up with a parallel iteration
    //   over the functions to validate instead of a synchronous validation.
    // * Custom sections are optionally validated depending on the
    //   `validate_custom` parameter to this function.
    //
    // The general idea here is that we're going to use `Parser::parse_all`
    // to divvy up the input bytes into chunks. Each chunk is fed serially into
    // a validator which optionally produces functions to validate later. At the
    // end all functions are processed in parallel.
    let mut validator = Validator::new_with_features(features);
    let mut custom_section_validator = if validate_custom {
        Some(CustomSectionValidator::new())
    } else {
        None
    };

    let mut functions_to_validate = Vec::new();

    let start = Instant::now();
    let mut parser = Parser::new(0);
    parser.set_features(features);
    for payload in parser.parse_all(&wasm) {
        let payload = payload?;
        match validator.payload(&payload)? {
            ValidPayload::Ok | ValidPayload::Parser(_) | ValidPayload::End(_) => {}
            ValidPayload::Func(validator, body) => {
                let module_id = custom_section_validator
                    .as_ref()
                    .map(|c| c.current_module_id());
                functions_to_validate.push((validator, body, module_id))
            }
        }
        if let Some(custom) = &mut custom_section_validator {
            custom.payload(&payload, &validator)?;
        }
    }
    log::info!("module structure validated in {:?}", start.elapsed());

    // After we've validate the entire wasm module we'll use `rayon` to
    // iterate over all functions in parallel and perform parallel
    // validation of the input wasm module.
    //
    // Note that validation results for each function are collected into a
    // vector to ensure that in the case of multiple errors the first is
    // always reported. Otherwise `rayon` does not guarantee the order that
    // failures show up in.
    let start = Instant::now();
    functions_to_validate
        .into_par_iter()
        .map_init(
            FuncValidatorAllocations::default,
            |allocs, (to_validate, body, module_id)| -> Result<_> {
                let index = to_validate.index;
                let mut validator = to_validate.into_validator(mem::take(allocs));
                validator
                    .validate(&body)
                    .with_context(|| format!("func {} failed to validate", validator.index()))?;
                if let Some(custom) = &custom_section_validator {
                    if let Some(module_id) = module_id {
                        custom.code_section_entry(module_id, index, &body)?;
                    }
                }
                *allocs = validator.into_allocations();
                Ok(())
            },
        )
        .collect::<Vec<_>>()
        .into_iter()
        .collect::<Result<Vec<_>>>()?;
    log::info!("functions validated in {:?}", start.elapsed());
    Ok(())
}
