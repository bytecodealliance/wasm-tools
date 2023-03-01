use crate::{Interface, InterfaceId, Resolve, WorldItem};
use anyhow::{anyhow, Result};
use id_arena::Arena;
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};

pub type Substitutions = HashMap<String, HashMap<String, HashSet<String>>>;

pub fn expand(resolve: &mut Resolve, mut substitutions: Substitutions) -> Result<()> {
    let mut new_interfaces = resolve.interfaces.clone();
    for (_, world) in &mut resolve.worlds {
        let mut subs = substitutions.remove(&world.name).unwrap_or_default();
        expand_interfaces(
            &world.name,
            "imports",
            &mut world.imports,
            &mut new_interfaces,
            &mut subs,
        )?;
        expand_interfaces(
            &world.name,
            "exports",
            &mut world.exports,
            &mut new_interfaces,
            &mut subs,
        )?;
    }

    if !substitutions.is_empty() {
        log::warn!("unused substitutions were provided: {substitutions:?}",);
    }

    resolve.interfaces = new_interfaces;

    Ok(())
}

fn expand_interfaces(
    world_name: &str,
    desc: &str,
    items: &mut IndexMap<String, WorldItem>,
    new_interfaces: &mut Arena<Interface>,
    substitutions: &mut HashMap<String, HashSet<String>>,
) -> Result<()> {
    for (name, item) in items {
        if let WorldItem::Interface(interface) = item {
            if new_interfaces[*interface].wildcard.is_some() {
                let new_interface = expand_interface(
                    *interface,
                    new_interfaces,
                    substitutions.remove(name).ok_or_else(|| {
                        anyhow!(
                            "world {world_name} {desc} item {name} contains wildcards \
                             but no substitutions were provided",
                        )
                    })?,
                );
                *interface = new_interfaces.alloc(new_interface);
            }
        }
    }

    if !substitutions.is_empty() {
        log::warn!("unused substitutions were provided for world {world_name}: {substitutions:?}",);
    }

    Ok(())
}

fn expand_interface(
    interface: InterfaceId,
    new_interfaces: &Arena<Interface>,
    substitutions: HashSet<String>,
) -> Interface {
    let mut new_interface = new_interfaces[interface].clone();
    // Make the expanded interface anonymous; otherwise the generated component type will fail to validate due to
    // the existence of multiple interfaces with the same name.
    //
    // TODO: implement something like
    // https://github.com/WebAssembly/component-model/issues/172#issuecomment-1466939890, which may entail changes
    // to how interfaces are modeled in WIT, `Resolve`, and the component model.
    new_interface.name = None;
    let function = new_interface.wildcard.take().unwrap();
    for var_name in substitutions {
        let mut new_func = function.clone();
        new_func.name = var_name.clone();
        assert!(new_interface.functions.insert(var_name, new_func).is_none());
    }
    new_interface
}
