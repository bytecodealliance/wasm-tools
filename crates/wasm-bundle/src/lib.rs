use std::{path::Path, fs};
use anyhow::Result;
use wasm_compose::graph::{Component, CompositionGraph, EncodeOptions, InstanceId};
use warg_crypto::hash::{Hash, Sha256};
use warg_client::{Client, storage::{FileSystemRegistryStorage, FileSystemContentStorage, RegistryStorage}};
use warg_protocol::{package::ReleaseState, registry::{PackageId}};
use wasmprinter::Printer;

/// Bundles Dependencies 
pub struct Bundler {

}

impl Bundler {
  pub fn new() -> Self {
    Self {}
  }

  pub async fn read_component(&self, client: Client<FileSystemRegistryStorage, FileSystemContentStorage>, name: impl Into<String>, path: impl AsRef<Path> + Clone) -> Result<Vec<String>> {
    let name = name.into();
    let path = path.clone();
    let component_a = Component::from_file(name.clone(), path.clone())?;
    let component_b = Component::from_file(name, path)?;
    let mut imports = Vec::new();
    let mut graph = CompositionGraph::new();
    graph.add_component(component_b)?;
    for (name, _) in component_a.imports {
      // imports.push(name);
      let base_name = name.split('/').next().unwrap();
      dbg!(&base_name);
      let packageId = PackageId::new(base_name)?;
      let info = client.registry().load_package(&packageId).await?.unwrap();
      let state = &info.state.releases().last().unwrap().state;
      if let ReleaseState::Released{content} = state {
        let full_digest = content.to_string();
        let digest = full_digest.split(':').last().unwrap();
        let mut content_path = String::from("/Users/interpretations/Library/Caches/warg/content/sha256/");
        content_path.push_str(&digest);
        let kiddo = Component::from_file(name, content_path)?;
        graph.add_component(kiddo)?;
        // let components = bundler.read_component(client, package.id, content_path);
        // dbg!(&components);
      }
      graph.instantiate(0)?;
      graph.instantiate(1)?;
      graph.connect(1, Some(0), 0, 0)?;
      let encoded = graph.encode(EncodeOptions {define_components: true, export: Some(InstanceId(1)), validate: true})?;
      let mut printer = Printer::new();
      let printed = printer.print(&encoded)?;
      dbg!(printed);
      fs::write("./fused.wasm", encoded)?;
      // dbg!(&graph);

      // graph.connect(source, source_export, target, target_import)
      // let base_name = &name.split('/').next().unwrap();
      // dbg!(base_name);
      // let digest = Hash::<Sha256>::of(base_name);
      // dbg!(&digest);
    }
    // dbg!(&component);
    Ok(imports)
  }
}