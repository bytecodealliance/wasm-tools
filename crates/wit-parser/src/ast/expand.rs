use anyhow::{anyhow, Result};
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};
use crate::{Resolve, World, WorldId, PackageId, WorldItem, InterfaceId};

pub type Substitutions = HashMap<String, HashMap<String, HashSet<String>>>;

pub fn expand(
    resolve: &mut Resolve,
    package_id: PackageId,
    world: Option<&str>,
    substitutions: Substitutions
) -> Result<()> {
    let world_id = resolve.select_world(package_id, world)?;
    resolve.expand_wildcards(world_id, substitutions)
}

impl Resolve {
    pub fn expand_wildcards(&mut self, world_id: WorldId, substitutions: Substitutions) -> Result<()> {
        let mut world = self.worlds.get(world_id).cloned().ok_or(anyhow!("invalid world id"))?;
        self.expand_world(&mut world, substitutions)?;
        self.worlds[world_id] = world;
        Ok(())
    }

    fn has_wildcards(&self, interface_id: InterfaceId) -> bool {
        self
            .interfaces[interface_id]
            .wildcard
            .is_some()
    }

    fn expand_world(&mut self, world: &mut World, mut substitutions: Substitutions) -> Result<()> {
        let mut world_subs = substitutions.remove(&world.name).unwrap_or_default();

        self.expand_imports(world, &mut world_subs)?;
        self.expand_exports(world, &mut world_subs)?;

        if !world_subs.is_empty() {
            log::warn!(
                "unused substitutions were provided for expanding world {name:?}",
                name = world.name,
            );
        }

        Ok(())
    }

    fn expand_imports(
        &mut self,
        world: &mut World,
        substitutions: &mut HashMap<String, HashSet<String>>
    ) -> Result<()> {
        self.expand_items(&world.name, "import", &mut world.imports, substitutions)
    }

    fn expand_exports(
        &mut self,
        world: &mut World,
        substitutions: &mut HashMap<String, HashSet<String>>
    ) -> Result<()> {
        self.expand_items(&world.name, "export", &mut world.imports, substitutions)
    }

    fn expand_items<'a>(
        &mut self,
        world_name: &str,
        desc: &str,
        items: &mut IndexMap<String, WorldItem>,
        substitutions: &mut HashMap<String, HashSet<String>>
    ) -> Result<()> {
        for (name, item) in items {
            if let WorldItem::Interface(interface_id) = item {
                if self.has_wildcards(*interface_id) {
                    let iface_subs = substitutions
                        .remove(name)
                        .ok_or_else(|| anyhow!(
                            "world {world_name} {desc} item {name} contains wildcards but no substitutions were provided",
                        ))?;
                    *interface_id = self.expand_interface(*interface_id, iface_subs); 
                }
            }
        }
        Ok(())
    }

    fn expand_interface(&mut self, interface_id: InterfaceId, subnames: HashSet<String>) -> InterfaceId {
        let mut new_interface = self.interfaces[interface_id].clone();
        let function = new_interface.wildcard.take().unwrap();
        for var_name in subnames.into_iter() {
            let mut new_func = function.clone();
            new_func.name = var_name.clone();
            assert!(new_interface
                .functions
                .insert(var_name, new_func)
                .is_none());
        }
        self.interfaces.alloc(new_interface)
    }
}