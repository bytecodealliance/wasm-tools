use anyhow::Context;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    wit_deps::lock!()
        .await
        .context("failed to lock WIT dependencies")?;

    println!("cargo:rerun-if-changed=wit/deps");
    println!("cargo:rerun-if-changed=wit/deps.lock");
    println!("cargo:rerun-if-changed=wit/deps.toml");

    Ok(())
}
