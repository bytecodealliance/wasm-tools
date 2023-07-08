use anyhow::{Context, Result};
use pretty_assertions::assert_eq;
use std::collections::HashSet;
use std::{fs, path::Path};
use wit_component::WitPrinter;
use wit_parser::{Resolve, TypeOwner, WorldItem};

/// This is a test which iterates over the `tests/merge` directory and treats
/// each subdirectory as its own test. Each subdirectory has an `into` and a
/// `from` folder which represent two different `Resolve` sets, each with their
/// own set of dependencies as well.
///
/// Each test will merge the `from` into the `into` and assert that everything
/// is valid along the way. On successful merge the resulting documents
/// are printed and asserted against expectations. Failures assert the
/// correct error message.
#[test]
fn merging() -> Result<()> {
    drop(env_logger::try_init());

    for entry in fs::read_dir("tests/merge")? {
        let path = entry?.path();
        if !path.is_dir() {
            continue;
        }

        let test_case = path.file_stem().unwrap().to_str().unwrap();
        println!("testing {test_case}");

        let mut from = Resolve::default();
        from.push_dir(&path.join("from"))?;
        let mut into = Resolve::default();
        into.push_dir(&path.join("into"))?;

        assert_valid_resolve(&from);
        assert_valid_resolve(&into);

        match into.merge(from) {
            Ok(_) => {
                assert!(
                    !test_case.starts_with("bad-"),
                    "should have failed to merge"
                );
                assert_valid_resolve(&into);
                for (id, pkg) in into.packages.iter() {
                    let expected = path
                        .join("merge")
                        .join(&pkg.name.name)
                        .with_extension("wit");
                    let output = WitPrinter::default().print(&into, id)?;
                    assert_output(&expected, &output)?;
                }
            }
            Err(e) => {
                assert!(test_case.starts_with("bad-"), "failed to merge with {e:?}");
                assert_output(&path.join("error.txt"), &format!("{e:?}"))?;
            }
        }
    }

    Ok(())
}

fn assert_output(expected: &Path, actual: &str) -> Result<()> {
    if std::env::var_os("BLESS").is_some() {
        fs::create_dir_all(expected.parent().unwrap())?;
        fs::write(expected, actual).with_context(|| format!("failed to write {expected:?}"))?;
    } else {
        assert_eq!(
            fs::read_to_string(expected)
                .with_context(|| format!("failed to read {expected:?}"))?
                .replace("\r\n", "\n"),
            actual,
            "expectation `{}` did not match actual",
            expected.display(),
        );
    }
    Ok(())
}

fn assert_valid_resolve(resolve: &Resolve) {
    let mut package_interfaces = Vec::new();
    let mut package_worlds = Vec::new();
    for (id, pkg) in resolve.packages.iter() {
        let mut interfaces = HashSet::new();
        for (name, iface) in pkg.interfaces.iter() {
            assert!(interfaces.insert(*iface));
            let iface = &resolve.interfaces[*iface];
            assert_eq!(name, iface.name.as_ref().unwrap());
            assert_eq!(iface.package.unwrap(), id);
        }
        package_interfaces.push(pkg.interfaces.values().copied().collect::<HashSet<_>>());
        let mut worlds = HashSet::new();
        for (name, world) in pkg.worlds.iter() {
            assert!(worlds.insert(*world));
            let world = &resolve.worlds[*world];
            assert_eq!(*name, world.name);
            assert_eq!(world.package.unwrap(), id);
        }
        package_worlds.push(pkg.worlds.values().copied().collect::<HashSet<_>>());
    }

    let mut interface_types = Vec::new();
    for (id, iface) in resolve.interfaces.iter() {
        assert!(resolve.packages.get(iface.package.unwrap()).is_some());
        if iface.name.is_some() {
            assert!(package_interfaces[iface.package.unwrap().index()].contains(&id));
        }

        for (name, ty) in iface.types.iter() {
            let ty = &resolve.types[*ty];
            assert_eq!(ty.name.as_ref(), Some(name));
            assert_eq!(ty.owner, TypeOwner::Interface(id));
        }
        interface_types.push(iface.types.values().copied().collect::<HashSet<_>>());
        for (name, f) in iface.functions.iter() {
            assert_eq!(*name, f.name);
        }
    }

    let mut world_types = Vec::new();
    for (id, world) in resolve.worlds.iter() {
        assert!(resolve.packages.get(world.package.unwrap()).is_some());
        assert!(package_worlds[world.package.unwrap().index()].contains(&id));

        let mut types = HashSet::new();
        for (name, item) in world.imports.iter().chain(world.exports.iter()) {
            match item {
                WorldItem::Interface(_) => {}
                WorldItem::Function(f) => {
                    assert_eq!(f.name, name.clone().unwrap_name());
                }
                WorldItem::Type(ty) => {
                    assert!(types.insert(*ty));
                    let ty = &resolve.types[*ty];
                    assert_eq!(ty.name, Some(name.clone().unwrap_name()));
                    assert_eq!(ty.owner, TypeOwner::World(id));
                }
            }
        }
        world_types.push(types);
    }

    for (ty_id, ty) in resolve.types.iter() {
        match ty.owner {
            TypeOwner::Interface(id) => {
                assert!(resolve.interfaces.get(id).is_some());
                assert!(interface_types[id.index()].contains(&ty_id));
            }
            TypeOwner::World(id) => {
                assert!(resolve.worlds.get(id).is_some());
                assert!(world_types[id.index()].contains(&ty_id));
            }
            TypeOwner::None => {}
        }
    }
}
