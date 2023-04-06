#![warn(clippy::pedantic)]

use anyhow::Context;
use camino::{Utf8Path, Utf8PathBuf};
use clap::{Parser, Subcommand};
use tokio::fs::{self, File};
use tokio::io;
use tokio_util::compat::TokioAsyncWriteCompatExt;
use tracing_subscriber::prelude::*;
use wit_deps::Identifier;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Dependency output directory
    #[arg(short, long, default_value = "wit/deps")]
    deps: Utf8PathBuf,

    /// Dependency manifest path
    #[arg(short, long, default_value = "wit/deps.toml")]
    manifest: Utf8PathBuf,

    /// Dependency lock path
    #[arg(short, long, default_value = "wit/deps.lock")]
    lock: Utf8PathBuf,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Lock dependencies
    Lock {
        /// Optional list of packages to lock
        #[arg(short, long)]
        package: Vec<Identifier>,
    },
    /// Write a deterministic tar containing the `wit` subdirectory for a package to stdout
    Tar {
        /// Package to archive
        package: Identifier,

        /// Optional output path, if not specified, the archive will be written to stdout
        #[arg(short, long)]
        output: Option<Utf8PathBuf>,
    },
}

async fn lock(
    manifest_path: impl AsRef<Utf8Path>,
    lock_path: impl AsRef<Utf8Path>,
    deps_path: impl AsRef<Utf8Path>,
    packages: impl IntoIterator<Item = &Identifier>,
) -> anyhow::Result<()> {
    let manifest_path = manifest_path.as_ref();
    let lock_path = lock_path.as_ref();
    let deps_path = deps_path.as_ref();

    let manifest = fs::read_to_string(&manifest_path)
        .await
        .with_context(|| format!("failed to read manifest at `{manifest_path}`"))?;
    wit_deps::lock_path(manifest, lock_path, deps_path, packages)
        .await
        .context("failed to lock dependencies")?;
    Ok(())
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .compact()
                .without_time()
                .with_file(false)
                .with_target(false)
                .with_writer(std::io::stderr),
        )
        .with(
            tracing_subscriber::EnvFilter::builder()
                .with_default_directive(tracing_subscriber::filter::LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();

    let Cli {
        deps: deps_path,
        manifest: manifest_path,
        lock: lock_path,
        command,
    } = Cli::parse();

    match command {
        None => lock(manifest_path, lock_path, deps_path, None).await,
        Some(Command::Lock { package }) => {
            lock(manifest_path, lock_path, deps_path, &package).await
        }
        Some(Command::Tar { package, output }) => {
            lock(manifest_path, lock_path, &deps_path, [&package]).await?;
            let package = deps_path.join(package);
            if let Some(output) = output {
                let output = File::create(&output)
                    .await
                    .with_context(|| format!("failed to create output path `{output}`"))?;
                wit_deps::tar(package, output.compat_write()).await?;
            } else {
                wit_deps::tar(package, io::stdout().compat_write()).await?;
            }
            Ok(())
        }
    }
}
