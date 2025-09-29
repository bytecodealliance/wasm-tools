use anyhow::{Context, Result};
use clap::Parser;
use wasm_tools::wit::WitResolve;
use wit_component::{StringEncoding, embed_component_metadata};

/// Generate a wasm dynamic library from the input WIT world.
///
/// This command will generate a WebAssembly dynamic shared-everything library
/// which is suitable for linking to other dynamic libraries. The generated
/// dynamic library will have an interface specified by the WIT world provided
/// as an argument to this subcommand. World exports will be implemented in
/// terms of imported `wit_dylib_*` functions. World imports will be made
/// available to call and also interface through `wit_dylib_*` functions.
///
/// This generated module can be used as a low-level detail, for example, of
/// creating a component from an interpreter. The generated dynamic library
/// can be linked against a pre-built interpreter to provide a component that
/// is implemented with a generic interpreter.
///
/// The output of this command is suitable to pass to `wasm-tools component
/// link` to produce a component.
#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    resolve: WitResolve,

    #[clap(flatten)]
    io: wasm_tools::InputOutput,

    /// The string encoding to use for imports/exports, defaults to utf-8.
    #[clap(long, value_name = "ENCODING")]
    encoding: Option<StringEncoding>,

    /// The WIT world to use, if the WIT input has more than one.
    #[clap(short, long)]
    world: Option<String>,

    /// Whether or not to validate the generated dynamic library.
    #[clap(long)]
    validate: bool,

    /// Print the output in the WebAssembly text format instead of binary.
    #[clap(long, short = 't')]
    wat: bool,

    #[clap(flatten)]
    dylib_opts: wit_dylib::DylibOpts,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(mut self) -> Result<()> {
        let (resolve, pkg_id) = self.resolve.load()?;
        let world = resolve.select_world(&[pkg_id], self.world.as_deref())?;

        let mut wasm = wit_dylib::create(&resolve, world, Some(&mut self.dylib_opts));
        self.dylib_opts.async_.ensure_all_used()?;

        embed_component_metadata(
            &mut wasm,
            &resolve,
            world,
            self.encoding.unwrap_or(StringEncoding::UTF8),
        )?;

        if self.validate {
            wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
                .validate_all(&wasm)
                .context("generated adapter was not valid")?;
        }

        self.io.output_wasm(&wasm, self.wat)?;

        Ok(())
    }
}
