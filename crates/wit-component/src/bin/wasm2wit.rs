use clap::Parser;
use wit_component::cli::WasmToWitApp;

fn main() {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
        .format_target(false)
        .init();

    if let Err(e) = WasmToWitApp::parse().execute() {
        log::error!("{:?}", e);
        std::process::exit(1);
    }
}
