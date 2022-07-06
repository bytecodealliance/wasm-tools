use anyhow::Result;
use clap::Parser;
use wasm_compose::cli::WasmComposeApp;

fn main() -> Result<()> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
        .format_target(false)
        .init();

    if let Err(e) = WasmComposeApp::parse().execute() {
        log::error!("{:?}", e);
        std::process::exit(1);
    }

    Ok(())
}
