use anyhow::Result;
use serde_derive::Serialize;
use wasm_compose::graph::Component;

#[derive(Serialize, Clone, Debug)]
struct ComponentImportsExports {
    pub imports: Vec<String>,
    pub exports: Vec<String>,
}

/// Debugging utility to list component imports and exports.
#[derive(clap::Parser)]
pub struct Opts {
    #[clap(flatten)]
    io: wasm_tools::InputOutput,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(&self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;
        let component = Component::from_bytes("demo", &input)?;

        let imports = component
            .imports()
            .map(|(_idx, name, _ty)| name.to_string())
            .collect();
        let exports = component
            .exports()
            .map(|(_, name, _, _)| name.to_string())
            .collect();
        let world = ComponentImportsExports { exports, imports };

        let output = serde_json::to_string_pretty(&world)?;
        self.io.output(wasm_tools::Output::Json(&output))?;

        Ok(())
    }
}
