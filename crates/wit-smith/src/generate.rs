use crate::config::Config;
use arbitrary::{Arbitrary, Result, Unstructured};
use indexmap::IndexMap;
use semver::Version;
use std::collections::hash_map::{Entry, HashMap};
use std::collections::HashSet;
use std::fmt::Write;
use std::rc::Rc;
use std::str;
use wit_parser::*;

pub struct Generator {
    config: Config,
    packages: PackageList,
    next_interface_id: u32,
}

type TypeList = Vec<Type>;
type InterfaceList = IndexMap<String, FileInterface>;
type PackageList = Vec<(PackageName, InterfaceList)>;

struct InterfaceGenerator<'a> {
    gen: &'a Generator,
    file: &'a mut File,
    config: &'a Config,
    unique_names: HashSet<String>,
    types_in_interface: TypeList,
}

#[derive(Clone)]
struct Type {
    name: String,
    size: usize,
    is_resource: bool,
}

pub struct Package {
    pub name: PackageName,
    pub sources: SourceMap,
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

    pub fn gen(&mut self, u: &mut Unstructured<'_>) -> Result<Vec<Package>> {
        let mut packages = Vec::new();
        let mut names = HashSet::new();
        while packages.len() < self.config.max_packages && (packages.is_empty() || u.arbitrary()?) {
            let (pkg, interfaces) = self.gen_package(u, &mut names)?;
            if interfaces.len() > 0 {
                self.packages.push((pkg.name.clone(), interfaces));
            }
            packages.push(pkg);
        }
        Ok(packages)
    }

    fn gen_package(
        &mut self,
        u: &mut Unstructured<'_>,
        names: &mut HashSet<String>,
    ) -> Result<(Package, InterfaceList)> {
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
            sources: SourceMap::new(),
        };

        #[derive(Arbitrary)]
        enum Generate {
            Interface,
            Use,
            World,
            Done,
        }

        let mut items = 0;
        let mut files = vec![File::default()];
        let mut package_names = HashSet::new();
        let mut package = File::default();
        log::debug!("===================== new package ====================");
        while items < self.config.max_pkg_items && !u.is_empty() {
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
                    files.push(File {
                        items: Vec::new(),
                        namespace: package
                            .interfaces
                            .iter()
                            .map(|(k, _)| (k.clone(), Definition::Package))
                            .collect(),
                        interfaces: package.interfaces.clone(),
                    });
                    files.last_mut().unwrap()
                }
            };
            match u.arbitrary()? {
                Generate::World => {
                    let name = file.gen_unique_package_name(u, &mut package_names)?;
                    log::debug!("new world `{name}` in {i}");
                    let world = self.gen_world(u, &name, file)?;
                    file.items.push(world);
                }
                Generate::Interface => {
                    let name = file.gen_unique_package_name(u, &mut package_names)?;
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
                    let prev = package
                        .interfaces
                        .insert(interface.name.clone(), interface.clone());
                    assert!(prev.is_none());

                    // This is also defined at the file level, and it must be
                    // unique here too.
                    let prev = file
                        .interfaces
                        .insert(interface.name.clone(), interface.clone());
                    assert!(prev.is_none());

                    // Insert the definition into all other files as well.
                    for file in files.iter_mut() {
                        file.insert_definition(interface.clone());
                    }
                }
                Generate::Use => {
                    let mut piece = String::new();
                    piece.push_str("use ");
                    let (name, id, types) = match self.gen_path(u, &mut package, &mut piece)? {
                        Some(i) => i,
                        None => continue,
                    };
                    let name = name.to_string();
                    let types = types.clone();
                    let name =
                        if file.namespace.get(&name) == Some(&Definition::File) || u.arbitrary()? {
                            let name = file.gen_unique_file_name(u)?;
                            piece.push_str(" as %");
                            piece.push_str(&name);
                            name
                        } else {
                            file.namespace.insert(name.clone(), Definition::File);
                            name
                        };
                    log::debug!("new use `{name}` in {i}");
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
                s.push_str("\n\n");
            }
            for piece in file.items.iter() {
                s.push_str(&piece);
                s.push_str("\n\n");
            }
            log::trace!("===============================================");
            log::trace!("{s}");
            ret.sources.push(format!("wit{i}.wit").as_ref(), &s);
        }
        Ok((ret, package.interfaces))
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
    ) -> Result<(String, TypeList)> {
        let mut gen = InterfaceGenerator::new(self, file);
        let ret = gen.gen_interface(u, name)?;
        Ok((ret, gen.types_in_interface))
    }

    fn gen_path<'a>(
        &'a self,
        u: &mut Unstructured<'_>,
        file: &'a mut File,
        dst: &mut String,
    ) -> Result<Option<(&'a str, u32, &'a Rc<TypeList>)>> {
        enum Choice {
            Interfaces,
            Packages,
        }
        let mut choices = Vec::new();
        if !file.interfaces.is_empty() {
            choices.push(Choice::Interfaces);
        }
        if !self.packages.is_empty() {
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
                file.namespace.insert(name.clone(), Definition::File);
                dst.push_str("%");
                dst.push_str(&i.name);
                Some((&i.name, i.id, &i.types))
            }
            Choice::Packages => {
                let (pkg, ifaces) = u.choose(&self.packages)?;
                dst.push_str("%");
                dst.push_str(&pkg.namespace);
                dst.push_str(":");
                dst.push_str("%");
                dst.push_str(&pkg.name);
                dst.push_str("/");
                let i = u.int_in_range(0..=ifaces.len() - 1)?;
                let i = &ifaces[i];
                dst.push_str("%");
                dst.push_str(&i.name);
                if let Some(version) = &pkg.version {
                    dst.push_str(&format!("@{version}"));
                }
                Some((&i.name, i.id, &i.types))
            }
        })
    }
}

impl<'a> InterfaceGenerator<'a> {
    fn new(gen: &'a Generator, file: &'a mut File) -> InterfaceGenerator<'a> {
        // Claim the name `memory` to avoid conflicting with the canonical ABI
        // always using a linear memory named `memory`.
        let mut unique_names = HashSet::new();
        unique_names.insert("memory".to_string());
        InterfaceGenerator {
            gen,
            file,
            config: &gen.config,
            types_in_interface: Vec::new(),
            unique_names,
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
                    if is_resource && u.arbitrary()? {
                        typedef.push_str(" {\n");
                        self.gen_resource_funcs(u, &mut typedef)?;
                        typedef.push_str("}");
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
        }

        let mut parts = Vec::new();
        let mut imported_interfaces = HashSet::new();
        let mut exported_interfaces = HashSet::new();

        while parts.len() < self.config.max_world_items && !u.is_empty() && u.arbitrary()? {
            let kind = u.arbitrary::<ItemKind>()?;
            let (direction, named) = match kind {
                ItemKind::Func(dir) | ItemKind::AnonInterface(dir) => (Some(dir), true),
                ItemKind::Interface(dir) => (Some(dir), false),
                ItemKind::Type => (None, true),
                ItemKind::Use => (None, false),
            };

            let mut part = String::new();
            if let Some(dir) = direction {
                part.push_str(match dir {
                    Direction::Import => "import ",
                    Direction::Export => "export ",
                });
            }

            let name = if named {
                let name = gen_unique_name(u, &mut self.unique_names)?;
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
                    let id = match self.gen.gen_path(u, self.file, &mut part)? {
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
                }
                ItemKind::AnonInterface(_) => {
                    let iface =
                        InterfaceGenerator::new(self.gen, self.file).gen_interface(u, None)?;
                    part.push_str(&iface);
                }

                ItemKind::Type => {
                    let name = name.unwrap();
                    let (ty, typedef) = self.gen_typedef(u, &name)?;
                    assert!(part.is_empty());
                    part = typedef;
                    let is_resource = ty.is_resource;
                    self.types_in_interface.push(ty);

                    if is_resource && u.arbitrary()? {
                        part.push_str(" {\n");
                        self.gen_resource_funcs(u, &mut part)?;
                        part.push_str("}");
                    }
                }

                ItemKind::Use => {
                    if !self.gen_use(u, &mut part)? {
                        continue;
                    }
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

    fn gen_resource_funcs(&mut self, u: &mut Unstructured<'_>, ret: &mut String) -> Result<()> {
        let mut parts = Vec::new();

        #[derive(Arbitrary)]
        enum Item {
            Constructor,
            Static,
            Method,
        }

        let mut has_constructor = false;
        let mut names = HashSet::new();
        while parts.len() < self.config.max_resource_items && !u.is_empty() && u.arbitrary()? {
            match u.arbitrary()? {
                Item::Constructor if has_constructor => {}
                Item::Constructor => {
                    has_constructor = true;
                    let mut part = format!("constructor");
                    self.gen_params(u, &mut part, false)?;
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
        let (_name, _id, types) = match self.gen.gen_path(u, self.file, &mut path)? {
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
        part.push_str("}");
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
            Float32,
            Float64,
            Char,
            String,
            Id,
            Tuple,
            Option,
            Result,
            List,
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
                Kind::Float32 => dst.push_str("float32"),
                Kind::Float64 => dst.push_str("float64"),
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
        dst.push_str("func");
        self.gen_params(u, dst, method)?;
        if u.arbitrary()? {
            dst.push_str(" -> ");
            self.gen_params(u, dst, false)?;
        } else if u.arbitrary()? {
            dst.push_str(" -> ");
            let mut fuel = self.config.max_type_size;
            self.gen_type(u, &mut fuel, dst)?;
        }
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

#[derive(Default)]
struct File {
    items: Vec<String>,
    namespace: HashMap<String, Definition>,
    interfaces: IndexMap<String, FileInterface>,
}

#[derive(Clone)]
struct FileInterface {
    name: String,
    id: u32,
    types: Rc<TypeList>,
}

#[derive(PartialEq)]
enum Definition {
    Package,
    File,
}

impl File {
    fn gen_unique_package_name(
        &mut self,
        u: &mut Unstructured<'_>,
        names: &mut HashSet<String>,
    ) -> Result<String> {
        let mut name = gen_name(u)?;
        loop {
            // Find a package-unique name first
            if !names.insert(name.clone()) {
                write!(&mut name, "{}", names.len()).unwrap();
                continue;
            }

            // Then make sure it's file-unique too
            if self.claim_file_name(&mut name) {
                break;
            }
        }
        Ok(name)
    }

    fn gen_unique_file_name(&mut self, u: &mut Unstructured<'_>) -> Result<String> {
        let mut name = gen_name(u)?;
        while !self.claim_file_name(&mut name) {
            // try again on the next iteration
        }
        Ok(name)
    }

    fn claim_file_name(&mut self, name: &mut String) -> bool {
        match self.namespace.entry(name.clone()) {
            Entry::Occupied(mut e) => match e.get() {
                // If this name is already claimed elsewhere in the package
                // then that's ok as we're going to shadow it, so switch it
                // to a file definition.
                Definition::Package => *e.get_mut() = Definition::File,

                // If it's already defined in the file try to add more stuff
                // to the name to make the next try not collide.
                Definition::File => {
                    name.push_str("y");
                    write!(name, "{}", self.namespace.len()).unwrap();
                    return false;
                }
            },

            // Not defined? Claim it.
            Entry::Vacant(v) => {
                v.insert(Definition::File);
            }
        }
        true
    }

    fn insert_definition(&mut self, def: FileInterface) {
        match self.namespace.get(&def.name) {
            Some(Definition::File) => return,
            Some(Definition::Package) => unreachable!(),
            None => {}
        }
        let prev = self.namespace.insert(def.name.clone(), Definition::Package);
        assert!(prev.is_none());
        let prev = self.interfaces.insert(def.name.clone(), def);
        assert!(prev.is_none());
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
