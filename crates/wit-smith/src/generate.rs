use crate::config::Config;
use arbitrary::{Arbitrary, Result, Unstructured};
use indexmap::{IndexMap, IndexSet};
use semver::Version;
use std::collections::hash_map::{Entry, HashMap};
use std::collections::HashSet;
use std::fmt::Write;
use std::mem;
use std::rc::Rc;
use std::str;
use wit_parser::*;

pub struct Generator {
    config: Config,
    packages: Packages,
    next_interface_id: u32,
}

struct InterfaceGenerator<'a> {
    generator: &'a Generator,
    file: &'a mut File,
    config: &'a Config,
    unique_names: HashSet<String>,
    types_in_interface: Vec<Type>,
}

#[derive(Clone)]
struct Type {
    name: String,
    size: usize,
    is_resource: bool,
}

#[derive(Default)]
struct Packages {
    list: Vec<Package>,
    packages_with_interfaces: Vec<usize>,
    packages_with_worlds: Vec<usize>,
}

pub struct Package {
    pub name: PackageName,
    pub sources: SourceMap,
    file: File,
}

#[derive(Clone)]
pub struct PackageName {
    pub namespace: String,
    pub name: String,
    pub version: Option<Version>,
}

impl Generator {
    pub fn new(config: Config) -> Generator {
        Generator {
            config,
            packages: Default::default(),
            next_interface_id: 0,
        }
    }

    pub fn generate(&mut self, u: &mut Unstructured<'_>) -> Result<Vec<Package>> {
        let mut names = HashSet::new();
        while self.packages.list.len() < self.config.max_packages && self.packages.list.is_empty() {
            let pkg = self.gen_package(u, &mut names)?;
            let i = self.packages.list.len();
            if pkg.file.interfaces.len() > 0 {
                self.packages.packages_with_interfaces.push(i);
            }
            if pkg.file.worlds.len() > 0 {
                self.packages.packages_with_worlds.push(i);
            }
            self.packages.list.push(pkg);
        }
        Ok(mem::take(&mut self.packages.list))
    }

    fn gen_package(
        &mut self,
        u: &mut Unstructured<'_>,
        names: &mut HashSet<String>,
    ) -> Result<Package> {
        let namespace = gen_unique_name(u, names)?;
        let name = gen_unique_name(u, names)?;
        let version = if u.arbitrary()? {
            Some(gen_version(u)?)
        } else {
            None
        };
        let mut ret = Package {
            name: PackageName {
                namespace,
                name,
                version,
            },
            file: File::default(),
            sources: SourceMap::new(),
        };

        #[derive(Arbitrary, Clone)]
        enum Generate {
            Interface,
            Use,
            World,
            Done,
        }

        let mut items = 0;
        let mut empty = true;
        let mut files = vec![File::default()];
        let mut package_names = HashSet::new();
        log::debug!("===================== new package ====================");
        while items < self.config.max_pkg_items {
            items += 1;
            let max = if files.len() < self.config.max_files_per_package {
                files.len() + 1
            } else {
                files.len()
            };
            let i = u.int_in_range(0..=max)?;
            let file = match files.get_mut(i) {
                Some(file) => file,
                None => {
                    files.push(ret.file.clone());
                    files.last_mut().unwrap()
                }
            };

            // Only generate Use/Done if we've already generated a world or interface. This ensures
            // that we never generate empty packages, which aren't representable.
            let generate = if empty {
                u.choose(&[Generate::World, Generate::Interface])?.clone()
            } else {
                u.arbitrary()?
            };

            match generate {
                Generate::World => {
                    let name =
                        file.gen_unique_package_name(u, &mut package_names, DefinitionKind::World)?;
                    log::debug!("new world `{name}` in {i}");
                    let world = self.gen_world(u, &name, file)?;
                    file.items.push(world);

                    // Insert the world at the package and file level, asserting
                    // uniqueness.
                    assert!(ret.file.worlds.insert(name.clone()));
                    assert!(file.worlds.insert(name.clone()));
                    let prev = ret.file.namespace.insert(
                        name.clone(),
                        (DefinitionLevel::Package, DefinitionKind::World),
                    );
                    assert!(prev.is_none());

                    // Insert the definition into all other files as well.
                    for file in files.iter_mut() {
                        if file.insert_definition(&name, DefinitionKind::World) {
                            assert!(file.worlds.insert(name.clone()));
                        }
                    }

                    empty = false;
                }
                Generate::Interface => {
                    let name = file.gen_unique_package_name(
                        u,
                        &mut package_names,
                        DefinitionKind::Interface,
                    )?;
                    log::debug!("new interface `{name}` in {i}");
                    let id = self.next_interface_id;
                    self.next_interface_id += 1;
                    let (src, types) = self.gen_interface(u, Some(&name), file)?;
                    file.items.push(src);
                    if types.is_empty() {
                        continue;
                    }
                    let interface = FileInterface {
                        name,
                        id,
                        types: Rc::new(types),
                    };

                    // This interface is defined at the package level, and it
                    // must be unique.
                    let prev = ret
                        .file
                        .interfaces
                        .insert(interface.name.clone(), interface.clone());
                    assert!(prev.is_none());
                    let prev = ret.file.namespace.insert(
                        interface.name.clone(),
                        (DefinitionLevel::Package, DefinitionKind::Interface),
                    );
                    assert!(prev.is_none());

                    // This is also defined at the file level, and it must be
                    // unique here too.
                    let prev = file
                        .interfaces
                        .insert(interface.name.clone(), interface.clone());
                    assert!(prev.is_none());

                    // Insert the definition into all other files as well.
                    for file in files.iter_mut() {
                        if file.insert_definition(&interface.name, DefinitionKind::Interface) {
                            let prev = file
                                .interfaces
                                .insert(interface.name.clone(), interface.clone());
                            assert!(prev.is_none());
                        }
                    }

                    empty = false;
                }
                Generate::Use => {
                    let mut piece = String::new();
                    piece.push_str("use ");
                    let (name, id, types) =
                        match self.gen_interface_path(u, &mut ret.file, &mut piece)? {
                            Some(i) => i,
                            None => continue,
                        };
                    let name = name.to_string();
                    let types = types.clone();
                    // If this interface's name already exist within this `file`
                    // then this must be renamed with `as`. If the name exists
                    // only at the package level then it's ok to replace it with
                    // something else.
                    //
                    // If the name doesn't exist then use the fuzz input to
                    // determine whether a rename should happen.
                    let name =
                        if matches!(file.namespace.get(&name), Some((DefinitionLevel::File, _)))
                            || u.arbitrary()?
                        {
                            let name = file.gen_unique_file_name(u, DefinitionKind::Interface)?;
                            piece.push_str(" as %");
                            piece.push_str(&name);
                            name
                        } else {
                            file.namespace.insert(
                                name.clone(),
                                (DefinitionLevel::File, DefinitionKind::Interface),
                            );
                            name
                        };
                    piece.push_str(";");
                    log::debug!("new use `{name}` in {i}");
                    file.worlds.swap_remove(&name);
                    file.interfaces
                        .insert(name.clone(), FileInterface { name, id, types });
                    file.items.push(piece)
                }
                Generate::Done => break,
            };
        }

        shuffle(u, &mut files)?;
        for file in files.iter_mut() {
            shuffle(u, &mut file.items)?;
        }

        let mut has_name = false;
        let len = files.len();
        for (i, file) in files.iter_mut().enumerate() {
            let mut s = String::new();
            if u.arbitrary()? || (!has_name && i == len - 1) {
                has_name = true;
                s.push_str("package ");
                s.push_str("%");
                s.push_str(&ret.name.namespace);
                s.push_str(":");
                s.push_str("%");
                s.push_str(&ret.name.name);
                if let Some(version) = &ret.name.version {
                    s.push_str(&format!("@{version}"));
                }
                s.push_str(";\n\n");
            }
            for piece in file.items.iter() {
                s.push_str(&piece);
                s.push_str("\n\n");
            }
            log::trace!("===============================================");
            log::trace!("{s}");
            ret.sources.push(format!("wit{i}.wit").as_ref(), &s);
        }
        Ok(ret)
    }

    fn gen_world(
        &mut self,
        u: &mut Unstructured<'_>,
        name: &str,
        file: &mut File,
    ) -> Result<String> {
        InterfaceGenerator::new(self, file).gen_world(u, name)
    }

    fn gen_interface(
        &mut self,
        u: &mut Unstructured<'_>,
        name: Option<&str>,
        file: &mut File,
    ) -> Result<(String, Vec<Type>)> {
        let mut generator = InterfaceGenerator::new(self, file);
        let ret = generator.gen_interface(u, name)?;
        Ok((ret, generator.types_in_interface))
    }

    fn gen_interface_path<'a>(
        &'a self,
        u: &mut Unstructured<'_>,
        file: &'a mut File,
        dst: &mut String,
    ) -> Result<Option<(&'a str, u32, &'a Rc<Vec<Type>>)>> {
        enum Choice {
            Interfaces,
            Packages,
        }
        let mut choices = Vec::new();
        if !file.interfaces.is_empty() {
            choices.push(Choice::Interfaces);
        }
        if !self.packages.packages_with_interfaces.is_empty() {
            choices.push(Choice::Packages);
        }

        if choices.is_empty() {
            return Ok(None);
        }
        Ok(match u.choose(&choices)? {
            Choice::Interfaces => {
                let i = u.int_in_range(0..=file.interfaces.len() - 1)?;
                let (name, i) = file.interfaces.get_index(i).unwrap();
                // Once a name is used from a file's local namespace then it
                // can't be overridden in that namespace so switch it to a file
                // definition from whatever it previously was.
                file.namespace.insert(
                    name.clone(),
                    (DefinitionLevel::File, DefinitionKind::Interface),
                );
                file.worlds.swap_remove(name);
                dst.push_str("%");
                dst.push_str(&i.name);
                Some((&i.name, i.id, &i.types))
            }
            Choice::Packages => {
                let pkg = u.choose(&self.packages.packages_with_interfaces)?;
                let pkg = &self.packages.list[*pkg];
                dst.push_str("%");
                dst.push_str(&pkg.name.namespace);
                dst.push_str(":");
                dst.push_str("%");
                dst.push_str(&pkg.name.name);
                dst.push_str("/");
                let i = u.int_in_range(0..=pkg.file.interfaces.len() - 1)?;
                let i = &pkg.file.interfaces[i];
                dst.push_str("%");
                dst.push_str(&i.name);
                if let Some(version) = &pkg.name.version {
                    dst.push_str(&format!("@{version}"));
                }
                Some((&i.name, i.id, &i.types))
            }
        })
    }

    fn gen_world_path<'a>(
        &'a self,
        u: &mut Unstructured<'_>,
        file: &'a mut File,
        dst: &mut String,
    ) -> Result<Option<&'a str>> {
        enum Choice {
            Worlds,
            Packages,
        }
        let mut choices = Vec::new();
        if !file.worlds.is_empty() {
            choices.push(Choice::Worlds);
        }
        if !self.packages.packages_with_worlds.is_empty() {
            choices.push(Choice::Packages);
        }

        if choices.is_empty() {
            return Ok(None);
        }
        Ok(match u.choose(&choices)? {
            Choice::Worlds => {
                let i = u.int_in_range(0..=file.worlds.len() - 1)?;
                let name = &file.worlds[i];
                dst.push_str("%");
                dst.push_str(&name);
                // Same as `gen_interface_path`, once a name is used as a world
                // it's forced to always be a world so update its definition to
                // be a file-level world.
                file.namespace
                    .insert(name.clone(), (DefinitionLevel::File, DefinitionKind::World));
                Some(name)
            }
            Choice::Packages => {
                let pkg = u.choose(&self.packages.packages_with_worlds)?;
                let pkg = &self.packages.list[*pkg];
                dst.push_str("%");
                dst.push_str(&pkg.name.namespace);
                dst.push_str(":");
                dst.push_str("%");
                dst.push_str(&pkg.name.name);
                dst.push_str("/");
                let i = u.int_in_range(0..=pkg.file.worlds.len() - 1)?;
                let w = &pkg.file.worlds[i];
                dst.push_str("%");
                dst.push_str(&w);
                if let Some(version) = &pkg.name.version {
                    dst.push_str(&format!("@{version}"));
                }
                Some(w)
            }
        })
    }
}

impl<'a> InterfaceGenerator<'a> {
    fn new(generator: &'a Generator, file: &'a mut File) -> InterfaceGenerator<'a> {
        InterfaceGenerator {
            generator,
            file,
            config: &generator.config,
            types_in_interface: Vec::new(),
            // Claim the name `memory` to avoid conflicting with the canonical
            // ABI always using a linear memory named `memory`.
            unique_names: HashSet::from_iter(["memory".to_string()]),
        }
    }

    fn gen_interface(&mut self, u: &mut Unstructured<'_>, name: Option<&str>) -> Result<String> {
        let mut ret = String::new();
        ret.push_str("interface ");
        if let Some(name) = name {
            ret.push_str("%");
            ret.push_str(name);
            ret.push_str(" ");
        }
        ret.push_str("{\n");

        #[derive(Arbitrary)]
        enum Generate {
            Use,
            Type,
            Function,
        }

        let mut parts = Vec::new();
        while parts.len() < self.config.max_interface_items && u.arbitrary()? {
            match u.arbitrary()? {
                Generate::Use => {
                    let mut part = String::new();
                    if self.gen_use(u, &mut part)? {
                        parts.push(part);
                    }
                }
                Generate::Type => {
                    let name = self.gen_unique_name(u)?;
                    let (ty, mut typedef) = self.gen_typedef(u, &name)?;
                    let is_resource = ty.is_resource;
                    self.types_in_interface.push(ty);
                    if is_resource {
                        if u.arbitrary()? {
                            typedef.push_str(" {\n");
                            self.gen_resource_funcs(&name, u, &mut typedef)?;
                            typedef.push_str("}");
                        } else {
                            typedef.push_str(";");
                        }
                    }
                    parts.push(typedef);
                }
                Generate::Function => {
                    parts.push(self.gen_func(u)?);
                }
            }
        }

        shuffle(u, &mut parts)?;
        for part in parts {
            ret.push_str(&part);
            ret.push_str("\n\n");
        }

        ret.push_str("}");
        Ok(ret)
    }

    fn gen_world(&mut self, u: &mut Unstructured<'_>, name: &str) -> Result<String> {
        let mut ret = String::new();
        ret.push_str("world %");
        ret.push_str(name);
        ret.push_str(" {\n");

        #[derive(Arbitrary, Copy, Clone)]
        enum Direction {
            Import,
            Export,
        }

        #[derive(Arbitrary)]
        enum ItemKind {
            Func(Direction),
            Interface(Direction),
            AnonInterface(Direction),
            Type,
            Use,
            Include,
        }

        let mut parts = Vec::new();
        let mut imported_interfaces = HashSet::new();
        let mut exported_interfaces = HashSet::new();

        // Claim the name `memory` to avoid conflicting with the canonical
        // ABI always using a linear memory named `memory`.
        let mut export_names = HashSet::from_iter(["memory".to_string()]);

        while parts.len() < self.config.max_world_items && !u.is_empty() && u.arbitrary()? {
            let kind = u.arbitrary::<ItemKind>()?;
            let (direction, named) = match kind {
                ItemKind::Func(dir) | ItemKind::AnonInterface(dir) => (Some(dir), true),
                ItemKind::Interface(dir) => (Some(dir), false),
                ItemKind::Type => (None, true),
                ItemKind::Use => (None, false),
                ItemKind::Include => (None, false),
            };

            let mut part = String::new();
            if let Some(dir) = direction {
                part.push_str(match dir {
                    Direction::Import => "import ",
                    Direction::Export => "export ",
                });
            }

            let name = if named {
                let names = match direction {
                    Some(Direction::Import) | None => &mut self.unique_names,
                    Some(Direction::Export) => &mut export_names,
                };
                let name = gen_unique_name(u, names)?;
                if direction.is_some() {
                    part.push_str("%");
                    part.push_str(&name);
                    part.push_str(": ");
                }
                Some(name)
            } else {
                None
            };

            match kind {
                ItemKind::Func(_) => {
                    self.gen_func_sig(u, &mut part, false)?;
                }
                ItemKind::Interface(dir) => {
                    let id = match self.generator.gen_interface_path(u, self.file, &mut part)? {
                        Some((_name, id, _types)) => id,
                        // If an interface couldn't be chosen or wasn't
                        // chosen then skip this import. A unique name was
                        // selecteed above but we just sort of leave that
                        // floating in the wild to get handled by some other
                        // test case.
                        None => continue,
                    };

                    // If this interface has already been imported or
                    // exported this document can't do so again. Throw out
                    // this item in that situation.
                    let unique = match dir {
                        Direction::Import => imported_interfaces.insert(id),
                        Direction::Export => exported_interfaces.insert(id),
                    };
                    if !unique {
                        continue;
                    }
                    part.push_str(";");
                }
                ItemKind::AnonInterface(_) => {
                    let iface = InterfaceGenerator::new(self.generator, self.file)
                        .gen_interface(u, None)?;
                    part.push_str(&iface);
                }

                ItemKind::Type => {
                    let name = name.unwrap();
                    let (ty, typedef) = self.gen_typedef(u, &name)?;
                    assert!(part.is_empty());
                    part = typedef;
                    let is_resource = ty.is_resource;
                    self.types_in_interface.push(ty);

                    if is_resource {
                        if u.arbitrary()? {
                            part.push_str(" {\n");
                            self.gen_resource_funcs(&name, u, &mut part)?;
                            part.push_str("}");
                        } else {
                            part.push_str(";");
                        }
                    }
                }

                ItemKind::Use => {
                    if !self.gen_use(u, &mut part)? {
                        continue;
                    }
                }

                ItemKind::Include => {
                    part.push_str("include ");
                    if self
                        .generator
                        .gen_world_path(u, self.file, &mut part)?
                        .is_none()
                    {
                        // If an interface couldn't be chosen or wasn't
                        // chosen then skip this include.
                        continue;
                    }
                    part.push_str(";");
                }
            }
            parts.push(part);
        }

        shuffle(u, &mut parts)?;

        for part in parts {
            ret.push_str(&part);
            ret.push_str("\n");
        }

        ret.push_str("}");

        Ok(ret)
    }

    fn gen_resource_funcs(
        &mut self,
        resource_name: &str,
        u: &mut Unstructured<'_>,
        ret: &mut String,
    ) -> Result<()> {
        let mut parts = Vec::new();

        #[derive(Arbitrary)]
        enum Item {
            Constructor,
            Static,
            Method,
        }

        let mut has_constructor = false;
        let mut names = HashSet::new();
        names.insert(resource_name.to_string());
        while parts.len() < self.config.max_resource_items && !u.is_empty() && u.arbitrary()? {
            match u.arbitrary()? {
                Item::Constructor if has_constructor => {}
                Item::Constructor => {
                    has_constructor = true;
                    let mut part = format!("constructor");
                    self.gen_params(u, &mut part, false)?;
                    part.push_str(";");
                    parts.push(part);
                }
                Item::Static => {
                    let mut part = format!("%");
                    part.push_str(&gen_unique_name(u, &mut names)?);
                    part.push_str(": static ");
                    self.gen_func_sig(u, &mut part, false)?;
                    parts.push(part);
                }
                Item::Method => {
                    let mut part = format!("%");
                    part.push_str(&gen_unique_name(u, &mut names)?);
                    part.push_str(": ");
                    self.gen_func_sig(u, &mut part, true)?;
                    parts.push(part);
                }
            }
        }

        shuffle(u, &mut parts)?;

        for part in parts {
            ret.push_str(&part);
            ret.push_str("\n");
        }
        Ok(())
    }

    fn gen_use(&mut self, u: &mut Unstructured<'_>, part: &mut String) -> Result<bool> {
        let mut path = String::new();
        let (_name, _id, types) =
            match self.generator.gen_interface_path(u, self.file, &mut path)? {
                Some(types) => types,
                None => return Ok(false),
            };
        part.push_str("use ");
        part.push_str(&path);
        part.push_str(".{");
        let ty = u.choose(types)?;
        part.push_str("%");
        part.push_str(&ty.name);
        let size = ty.size;
        let is_resource = ty.is_resource;
        let name = if self.unique_names.contains(&ty.name) || u.arbitrary()? {
            part.push_str(" as %");
            let name = self.gen_unique_name(u)?;
            part.push_str(&name);
            name
        } else {
            assert!(self.unique_names.insert(ty.name.clone()));
            ty.name.clone()
        };
        self.types_in_interface.push(Type {
            name,
            size,
            is_resource,
        });
        part.push_str("};");
        Ok(true)
    }

    fn gen_typedef(&mut self, u: &mut Unstructured<'_>, name: &str) -> Result<(Type, String)> {
        #[derive(Arbitrary)]
        pub enum Kind {
            Record,
            Flags,
            Variant,
            Enum,
            Anonymous,
            Resource,
        }

        let mut fuel = self.config.max_type_size;
        let mut ret = String::new();
        let mut is_resource = false;
        match u.arbitrary()? {
            Kind::Record => {
                ret.push_str("record %");
                ret.push_str(name);
                ret.push_str(" {\n");
                for _ in 0..u.int_in_range(1..=self.config.max_type_parts)? {
                    ret.push_str("  %");
                    ret.push_str(&self.gen_unique_name(u)?);
                    ret.push_str(": ");
                    self.gen_type(u, &mut fuel, &mut ret)?;
                    ret.push_str(",\n");
                }
                ret.push_str("}");
            }
            Kind::Variant => {
                ret.push_str("variant %");
                ret.push_str(name);
                ret.push_str(" {\n");
                for _ in 0..u.int_in_range(1..=self.config.max_type_parts)? {
                    ret.push_str("  %");
                    ret.push_str(&self.gen_unique_name(u)?);
                    if u.arbitrary()? {
                        ret.push_str("(");
                        self.gen_type(u, &mut fuel, &mut ret)?;
                        ret.push_str(")");
                    }
                    ret.push_str(",\n");
                }
                ret.push_str("}");
            }
            Kind::Enum => {
                ret.push_str("enum %");
                ret.push_str(name);
                ret.push_str(" {\n");
                for _ in 0..u.int_in_range(1..=self.config.max_type_parts)? {
                    ret.push_str("  %");
                    ret.push_str(&self.gen_unique_name(u)?);
                    ret.push_str(",\n");
                }
                ret.push_str("}");
            }
            Kind::Flags => {
                ret.push_str("flags %");
                ret.push_str(name);
                ret.push_str(" {\n");
                for _ in 0..u.int_in_range(1..=self.config.max_type_parts)? {
                    ret.push_str("  %");
                    ret.push_str(&self.gen_unique_name(u)?);
                    ret.push_str(",\n");
                }
                ret.push_str("}");
            }
            Kind::Anonymous => {
                ret.push_str("type %");
                ret.push_str(name);
                ret.push_str(" = ");
                self.gen_type(u, &mut fuel, &mut ret)?;
                ret.push_str(";");
            }
            Kind::Resource => {
                is_resource = true;
                ret.push_str("resource %");
                ret.push_str(name);
            }
        }

        let ty = Type {
            size: self.config.max_type_size - fuel,
            is_resource,
            name: name.to_string(),
        };
        Ok((ty, ret))
    }

    fn gen_type(
        &mut self,
        u: &mut Unstructured<'_>,
        fuel: &mut usize,
        dst: &mut String,
    ) -> Result<()> {
        #[derive(Arbitrary)]
        enum Kind {
            Bool,
            U8,
            U16,
            U32,
            U64,
            S8,
            S16,
            S32,
            S64,
            F32,
            F64,
            Char,
            String,
            Id,
            Tuple,
            Option,
            Result,
            List,
            Stream,
            Future,
            ErrorContext,
        }

        *fuel = match fuel.checked_sub(1) {
            Some(fuel) => fuel,
            None => {
                dst.push_str("bool");
                return Ok(());
            }
        };
        loop {
            break match u.arbitrary()? {
                Kind::Bool => dst.push_str("bool"),
                Kind::U8 => dst.push_str("u8"),
                Kind::S8 => dst.push_str("s8"),
                Kind::U16 => dst.push_str("u16"),
                Kind::S16 => dst.push_str("s16"),
                Kind::U32 => dst.push_str("u32"),
                Kind::S32 => dst.push_str("s32"),
                Kind::U64 => dst.push_str("u64"),
                Kind::S64 => dst.push_str("s64"),
                Kind::F32 => dst.push_str("f32"),
                Kind::F64 => dst.push_str("f64"),
                Kind::Char => dst.push_str("char"),
                Kind::String => dst.push_str("string"),
                Kind::Id => {
                    if self.types_in_interface.is_empty() {
                        continue;
                    }
                    let ty = u.choose(&self.types_in_interface)?;
                    *fuel = match fuel.checked_sub(ty.size) {
                        Some(fuel) => fuel,
                        None => continue,
                    };
                    let own_wrapper = if ty.is_resource && u.arbitrary()? {
                        dst.push_str("own<");
                        true
                    } else {
                        false
                    };
                    dst.push_str("%");
                    dst.push_str(&ty.name);
                    if own_wrapper {
                        dst.push_str(">");
                    }
                }
                Kind::Tuple => {
                    let fields = u.int_in_range(1..=self.config.max_type_parts)?;
                    *fuel = match fuel.checked_sub(fields) {
                        Some(fuel) => fuel,
                        None => continue,
                    };
                    dst.push_str("tuple<");
                    for i in 0..fields {
                        if i > 0 {
                            dst.push_str(", ");
                        }
                        self.gen_type(u, fuel, dst)?;
                    }
                    dst.push_str(">");
                }
                Kind::Option => {
                    *fuel = match fuel.checked_sub(1) {
                        Some(fuel) => fuel,
                        None => continue,
                    };
                    dst.push_str("option<");
                    self.gen_type(u, fuel, dst)?;
                    dst.push_str(">");
                }
                Kind::List => {
                    *fuel = match fuel.checked_sub(1) {
                        Some(fuel) => fuel,
                        None => continue,
                    };
                    dst.push_str("list<");
                    self.gen_type(u, fuel, dst)?;
                    dst.push_str(">");
                }
                Kind::Result => {
                    *fuel = match fuel.checked_sub(2) {
                        Some(fuel) => fuel,
                        None => continue,
                    };
                    dst.push_str("result");
                    let ok = u.arbitrary()?;
                    let err = u.arbitrary()?;
                    match (ok, err) {
                        (true, true) => {
                            dst.push_str("<");
                            self.gen_type(u, fuel, dst)?;
                            dst.push_str(", ");
                            self.gen_type(u, fuel, dst)?;
                            dst.push_str(">");
                        }
                        (true, false) => {
                            dst.push_str("<");
                            self.gen_type(u, fuel, dst)?;
                            dst.push_str(">");
                        }
                        (false, true) => {
                            dst.push_str("<_, ");
                            self.gen_type(u, fuel, dst)?;
                            dst.push_str(">");
                        }
                        (false, false) => {}
                    }
                }
                Kind::Stream => {
                    *fuel = match fuel.checked_sub(1) {
                        Some(fuel) => fuel,
                        None => continue,
                    };
                    dst.push_str("stream<");
                    self.gen_type(u, fuel, dst)?;
                    dst.push_str(">");
                }
                Kind::Future => {
                    *fuel = match fuel.checked_sub(1) {
                        Some(fuel) => fuel,
                        None => continue,
                    };
                    if u.arbitrary()? {
                        dst.push_str("future<");
                        self.gen_type(u, fuel, dst)?;
                        dst.push_str(">");
                    } else {
                        dst.push_str("future");
                    }
                }
                Kind::ErrorContext => {
                    dst.push_str("error-context");
                }
            };
        }

        Ok(())
    }

    fn gen_func(&mut self, u: &mut Unstructured<'_>) -> Result<String> {
        let mut ret = "%".to_string();
        ret.push_str(&self.gen_unique_name(u)?);
        ret.push_str(": ");
        self.gen_func_sig(u, &mut ret, false)?;
        Ok(ret)
    }

    fn gen_func_sig(
        &mut self,
        u: &mut Unstructured<'_>,
        dst: &mut String,
        method: bool,
    ) -> Result<()> {
        if u.arbitrary()? {
            dst.push_str("async ");
        }
        dst.push_str("func");
        self.gen_params(u, dst, method)?;
        if u.arbitrary()? {
            dst.push_str(" -> ");
            let mut fuel = self.config.max_type_size;
            self.gen_type(u, &mut fuel, dst)?;
        }
        dst.push_str(";");
        Ok(())
    }

    fn gen_params(
        &mut self,
        u: &mut Unstructured<'_>,
        dst: &mut String,
        method: bool,
    ) -> Result<()> {
        dst.push_str("(");
        let mut names = HashSet::new();
        if method {
            names.insert("self".to_string());
        }
        let mut fuel = self.config.max_type_size;
        for i in 0..u.int_in_range(0..=self.config.max_type_parts)? {
            if i > 0 {
                dst.push_str(", ");
            }
            dst.push_str("%");
            dst.push_str(&gen_unique_name(u, &mut names)?);
            dst.push_str(": ");
            self.gen_type(u, &mut fuel, dst)?;
        }
        dst.push_str(")");
        Ok(())
    }

    fn gen_unique_name(&mut self, u: &mut Unstructured<'_>) -> Result<String> {
        gen_unique_name(u, &mut self.unique_names)
    }
}

fn gen_unique_name(u: &mut Unstructured<'_>, set: &mut HashSet<String>) -> Result<String> {
    let mut name = gen_name(u)?;
    while !set.insert(name.clone()) {
        write!(&mut name, "{}", set.len()).unwrap();
    }
    Ok(name)
}

fn gen_name(u: &mut Unstructured<'_>) -> Result<String> {
    let size = u.arbitrary_len::<u8>()?;
    let size = std::cmp::min(size, 20);
    let name = match str::from_utf8(u.peek_bytes(size).unwrap()) {
        Ok(s) => {
            u.bytes(size).unwrap();
            s.to_string()
        }
        Err(e) => {
            let i = e.valid_up_to();
            let valid = u.bytes(i).unwrap();
            str::from_utf8(valid).unwrap().to_string()
        }
    };
    let name = name
        .chars()
        .map(|x| if x.is_ascii_lowercase() { x } else { 'x' })
        .collect::<String>();
    Ok(if name.is_empty() {
        "name".to_string()
    } else {
        name
    })
}

fn shuffle<T>(u: &mut Unstructured<'_>, mut slice: &mut [T]) -> Result<()> {
    while slice.len() > 0 {
        let pos = u.int_in_range(0..=slice.len() - 1)?;
        slice.swap(0, pos);
        slice = &mut slice[1..];
    }
    Ok(())
}

#[derive(Clone, Default)]
struct File {
    items: Vec<String>,
    namespace: HashMap<String, (DefinitionLevel, DefinitionKind)>,
    interfaces: IndexMap<String, FileInterface>,
    worlds: IndexSet<String>,
}

#[derive(Clone)]
struct FileInterface {
    name: String,
    id: u32,
    types: Rc<Vec<Type>>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum DefinitionLevel {
    Package,
    File,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum DefinitionKind {
    World,
    Interface,
}

impl File {
    fn gen_unique_package_name(
        &mut self,
        u: &mut Unstructured<'_>,
        names: &mut HashSet<String>,
        kind: DefinitionKind,
    ) -> Result<String> {
        let mut name = gen_name(u)?;
        loop {
            // Find a package-unique name first
            if !names.insert(name.clone()) {
                write!(&mut name, "{}", names.len()).unwrap();
                continue;
            }

            // Then make sure it's file-unique too
            if self.claim_file_name(&mut name, kind) {
                break;
            }
        }
        Ok(name)
    }

    fn gen_unique_file_name(
        &mut self,
        u: &mut Unstructured<'_>,
        kind: DefinitionKind,
    ) -> Result<String> {
        let mut name = gen_name(u)?;
        while !self.claim_file_name(&mut name, kind) {
            // try again on the next iteration
        }
        Ok(name)
    }

    fn claim_file_name(&mut self, name: &mut String, kind: DefinitionKind) -> bool {
        match self.namespace.entry(name.clone()) {
            Entry::Occupied(mut e) => match e.get().0 {
                // If this name is already claimed elsewhere in the package
                // then that's ok as we're going to shadow it, so switch it
                // to a file definition.
                DefinitionLevel::Package => *e.get_mut() = (DefinitionLevel::File, kind),

                // If it's already defined in the file try to add more stuff
                // to the name to make the next try not collide.
                DefinitionLevel::File => {
                    name.push_str("y");
                    write!(name, "{}", self.namespace.len()).unwrap();
                    return false;
                }
            },

            // Not defined? Claim it.
            Entry::Vacant(v) => {
                v.insert((DefinitionLevel::File, kind));
            }
        }
        true
    }

    fn insert_definition(&mut self, name: &str, kind: DefinitionKind) -> bool {
        match self.namespace.get(name) {
            // This name is already defined, so it can't be inserted.
            Some((DefinitionLevel::File, _)) => return false,
            Some(other) => {
                panic!("found duplicate definition when should be package-unique: {other:?}")
            }
            None => {}
        }
        let prev = self
            .namespace
            .insert(name.to_string(), (DefinitionLevel::Package, kind));
        assert!(prev.is_none());
        true
    }
}

fn gen_version(u: &mut Unstructured<'_>) -> Result<Version> {
    Ok(Version {
        major: u.int_in_range(0..=10)?,
        minor: u.int_in_range(0..=10)?,
        patch: u.int_in_range(0..=10)?,
        pre: if u.arbitrary()? {
            semver::Prerelease::new("alpha.0").unwrap()
        } else {
            semver::Prerelease::EMPTY
        },
        build: if u.arbitrary()? {
            semver::BuildMetadata::new("1.2.0").unwrap()
        } else {
            semver::BuildMetadata::EMPTY
        },
    })
}
