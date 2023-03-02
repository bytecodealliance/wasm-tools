use anyhow::Result;
use std::collections::{HashMap, HashSet};
use crate::{Resolve, World, WorldId, PackageId, WorldItem};

pub type Substitutions = HashMap<String, HashMap<String, HashSet<String>>>;

pub fn expand(resolve: &mut Resolve, package_id: PackageId, world: Option<&str>, substitutions: Substitutions) -> Result<()> {
    let world_id = resolve.select_world(package_id, world)?;
    expand_world(resolve, world_id, substitutions)
}

fn expand_world(resolve: &mut Resolve, world_id: WorldId, substitutions: Substitutions) -> Result<()> {
    let World { name, imports, exports, .. } = resolve
        .worlds
        .get(world_id)
        .cloned()
        .unwrap();

    match substitutions.get(&name) {
        Some(substitutions) => {
            expand_world_items(resolve, imports, substitutions, "import")?;
            expand_world_items(resolve, exports, substitutions, "export")?;
            Ok(())
        },
        None => {
            // TODO: return error if world contains templated 
            // items yet no substitutions were provided
            Ok(())
        },
    }
}

fn expand_world_items(
    resolve: &mut Resolve, 
    items: impl IntoIterator<Item = (String, WorldItem)>, 
    substitutions: &HashMap<String, HashSet<String>>,
    _direction: &str,
) -> Result<()> {
    for (name, item) in items {
        if let WorldItem::Interface(interface_id) = item {
            let interface = resolve.interfaces.get_mut(interface_id).unwrap();

            if let Some(function) = interface.functions.remove("*") {
                match substitutions.get(&name) {
                    Some(variables) => {
                        for variable_name in variables.iter() {
                            let mut func = function.clone();
                            func.name = variable_name.clone();
                            assert!(interface.functions.insert(variable_name.clone(), func).is_none());
                        }
                    }
                    None => {
                        // TODO: return error if interface contains `*` yet no substitutions were provided
                    }
                }
            }
        }
    }
    Ok(())
}