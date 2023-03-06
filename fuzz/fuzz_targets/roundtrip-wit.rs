#![no_main]

use libfuzzer_sys::fuzz_target;
use std::collections::HashMap;
use std::path::Path;
use wasm_encoder::{CustomSection, Encode, Section};
use wit_component::*;
use wit_parser::{PackageId, Resolve, SourceMap};

fuzz_target!(|data: &[u8]| {
    drop(env_logger::try_init());

    let mut u = arbitrary::Unstructured::new(data);
    let pkgs = match generate::packages(&mut u) {
        Ok(doc) => doc,
        Err(_) => return,
    };
    let mut resolve = Resolve::default();
    let mut deps = HashMap::new();
    let mut last = None;
    for pkg in pkgs {
        let url = format!("my-scheme:/{}", pkg.name);
        let unresolved = pkg.sources.parse(&pkg.name, Some(&url)).unwrap();
        let id = match resolve.push(unresolved, &deps) {
            Ok(id) => id,
            Err(e) => {
                let err = e.to_string();
                if err.contains("conflicts with a previous")
                    || err.contains("shadows previously imported")
                {
                    return;
                }
                panic!("bad wit parse: {e:?}")
            }
        };
        let prev = deps.insert(pkg.name, id);
        assert!(prev.is_none());
        last = Some(id);
    }
    let pkg = last.unwrap();
    let wasm = roundtrip_through_printing("doc1", &resolve, pkg);

    let name = &resolve.packages[pkg].name;
    let (resolve2, pkg2) = match wit_component::decode(&name, &wasm).unwrap() {
        DecodedWasm::WitPackage(resolve, pkg) => (resolve, pkg),
        DecodedWasm::Component(..) => unreachable!(),
    };

    let wasm2 = roundtrip_through_printing("doc2", &resolve2, pkg2);

    if wasm != wasm2 {
        panic!("roundtrip wasm didn't match");
    }

    for (id, _world) in resolve.worlds.iter() {
        let mut dummy = wit_component::dummy_module(&resolve, id);
        let metadata =
            wit_component::metadata::encode(&resolve, id, StringEncoding::UTF8, None).unwrap();
        let section = CustomSection {
            name: "component-type",
            data: &metadata,
        };
        dummy.push(section.id());
        section.encode(&mut dummy);

        write_file("dummy.wasm", &dummy);
        let wasm = wit_component::ComponentEncoder::default()
            .module(&dummy)
            .unwrap()
            .encode()
            .unwrap();
        write_file("dummy.component.wasm", &wasm);
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
            component_model: true,
            ..Default::default()
        })
        .validate_all(&wasm)
        .unwrap();

        wit_component::decode(&name, &wasm).unwrap();
    }
});

fn roundtrip_through_printing(file: &str, resolve: &Resolve, pkg: PackageId) -> Vec<u8> {
    // Encode `resolve` to wasm as the baseline expectation
    let wasm = wit_component::encode(resolve, pkg).unwrap();
    write_file(&format!("{file}.wasm"), &wasm);
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
        component_model: true,
        ..Default::default()
    })
    .validate_all(&wasm)
    .unwrap();

    // For all packages in `resolve` print them all to a string, then re-parse
    // them and insert them into a `new_resolve`.
    let mut new_deps = HashMap::new();
    let mut new_resolve = Resolve::default();
    let mut last = None;
    for (_, pkg) in resolve.packages.iter() {
        let mut map = SourceMap::new();
        let pkg_name = &pkg.name;
        for (name, doc) in pkg.documents.iter() {
            let doc = DocumentPrinter::default().print(resolve, *doc).unwrap();
            write_file(&format!("{file}-{pkg_name}-{name}.wit"), &doc);
            map.push(format!("{name}.wit").as_ref(), &name, doc);
        }
        let unresolved = map.parse(&pkg.name, pkg.url.as_deref()).unwrap();
        let id = new_resolve.push(unresolved, &new_deps).unwrap();
        new_deps.insert(pkg.name.clone(), id);
        last = Some(id);
    }

    // Finally encode the `new_resolve` which should be the exact same as
    // before.
    let wasm2 = wit_component::encode(&new_resolve, last.unwrap()).unwrap();
    write_file(&format!("{file}-reencoded.wasm"), &wasm2);
    if wasm != wasm2 {
        panic!("failed to roundtrip through text printing");
    }

    wasm
}

fn write_file(path: &str, contents: impl AsRef<[u8]>) {
    if !log::log_enabled!(log::Level::Debug) {
        return;
    }
    log::debug!("writing file {path}");
    let contents = contents.as_ref();
    let path = Path::new(path);
    std::fs::write(path, contents).unwrap();
    if path.extension().and_then(|s| s.to_str()) == Some("wasm") {
        let path = path.with_extension("wat");
        log::debug!("writing file {}", path.display());
        std::fs::write(path, wasmprinter::print_bytes(&contents).unwrap()).unwrap();
    }
}

mod generate {
    use arbitrary::{Arbitrary, Result, Unstructured};
    use std::collections::HashSet;
    use std::mem;
    use std::str;
    use wit_parser::*;

    const MAX_PARTS: usize = 5;
    const MAX_PACKAGES: usize = 10;
    const MAX_DOCUMENTS: usize = 10;
    const MAX_DOC_ITEMS: usize = 10;
    const MAX_WORLD_ITEMS: usize = 10;
    const MAX_INTERFACE_ITEMS: usize = 10;
    const MAX_TYPE_SIZE: usize = 100;

    #[derive(Default)]
    struct Generator {
        packages: PackageList,
        documents: DocumentList,
        interfaces: InterfaceList,
        next_interface_id: u32,
    }

    type TypeList = Vec<(String, usize)>;
    type InterfaceList = Vec<(String, u32, bool, TypeList)>;
    type DocumentList = Vec<(String, InterfaceList)>;
    type PackageList = Vec<(String, DocumentList)>;

    struct InterfaceGenerator<'a> {
        gen: &'a Generator,
        unique_names: HashSet<String>,
        types_in_interface: Vec<(String, usize)>,
    }

    pub struct Package {
        pub name: String,
        pub sources: SourceMap,
    }

    pub fn packages(u: &mut Unstructured<'_>) -> Result<Vec<Package>> {
        Generator::default().gen(u)
    }

    impl Generator {
        fn gen(&mut self, u: &mut Unstructured<'_>) -> Result<Vec<Package>> {
            let mut packages = Vec::new();
            let mut names = HashSet::new();
            while packages.len() < MAX_PACKAGES && (packages.is_empty() || u.arbitrary()?) {
                let name = gen_unique_name(u, &mut names)?;
                let (sources, documents) = self.gen_package(&name, u)?;
                if documents.len() > 0 {
                    self.packages.push((name.clone(), documents));
                }
                packages.push(Package { name, sources });
            }
            Ok(packages)
        }

        fn gen_package(
            &mut self,
            pkg: &str,
            u: &mut Unstructured<'_>,
        ) -> Result<(SourceMap, DocumentList)> {
            let mut map = SourceMap::new();
            let mut count = 0;
            let mut names = HashSet::new();

            while count < MAX_DOCUMENTS && (count == 0 || u.arbitrary()?) {
                let name = gen_unique_name(u, &mut names)?;
                let (doc, interfaces) = self.gen_document(u)?;
                super::write_file(format!("orig-{pkg}-{name}.wit").as_ref(), &doc);
                map.push(format!("{name}.wit").as_ref(), &name, doc);
                count += 1;
                if interfaces.len() > 0 {
                    self.documents.push((name, interfaces));
                }
            }

            Ok((map, mem::take(&mut self.documents)))
        }

        fn gen_document(&mut self, u: &mut Unstructured<'_>) -> Result<(String, InterfaceList)> {
            #[derive(Arbitrary)]
            enum Generate {
                World,
                Interface,
                Done,
            }

            let mut pieces = Vec::new();
            let mut has_default_interface = false;
            let mut has_default_world = false;
            let mut names = HashSet::new();
            while pieces.len() < MAX_DOC_ITEMS && !u.is_empty() {
                let name = gen_unique_name(u, &mut names)?;
                match u.arbitrary()? {
                    Generate::World => {
                        pieces.push(self.gen_world(u, &name, &mut has_default_world)?)
                    }
                    Generate::Interface => {
                        let id = self.next_interface_id;
                        self.next_interface_id += 1;
                        let (src, types) =
                            self.gen_interface(u, Some(&name), &mut has_default_interface)?;
                        if types.len() > 0 {
                            self.interfaces
                                .push((name, id, src.starts_with("default"), types));
                        }
                        pieces.push(src);
                    }
                    Generate::Done => break,
                }
            }
            shuffle(u, &mut pieces)?;
            let mut ret = String::new();
            for piece in pieces {
                ret.push_str(&piece);
                ret.push_str("\n\n");
            }
            Ok((ret, mem::take(&mut self.interfaces)))
        }

        fn gen_world(
            &mut self,
            u: &mut Unstructured<'_>,
            name: &str,
            has_default: &mut bool,
        ) -> Result<String> {
            InterfaceGenerator::new(self).gen_world(u, name, has_default)
        }

        fn gen_interface(
            &mut self,
            u: &mut Unstructured<'_>,
            name: Option<&str>,
            has_default: &mut bool,
        ) -> Result<(String, TypeList)> {
            let mut gen = InterfaceGenerator::new(self);
            let ret = gen.gen_interface(u, name, has_default)?;
            Ok((ret, gen.types_in_interface))
        }

        fn gen_path(
            &self,
            u: &mut Unstructured<'_>,
            dst: &mut String,
        ) -> Result<Option<(u32, &TypeList)>> {
            enum Choice {
                Interfaces,
                Documents,
                Packages,
            }
            let mut choices = Vec::new();
            if !self.interfaces.is_empty() {
                choices.push(Choice::Interfaces);
            }
            if !self.documents.is_empty() {
                choices.push(Choice::Documents);
            }
            if !self.packages.is_empty() {
                choices.push(Choice::Packages);
            }
            if choices.is_empty() {
                return Ok(None);
            }
            Ok(match u.choose(&choices)? {
                Choice::Interfaces => {
                    dst.push_str("self.");
                    let (name, id, _default, types) = u.choose(&self.interfaces)?;
                    dst.push_str("%");
                    dst.push_str(name);
                    Some((*id, types))
                }
                Choice::Documents => {
                    dst.push_str("pkg.");
                    let (name, ifaces) = u.choose(&self.documents)?;
                    dst.push_str("%");
                    dst.push_str(name);
                    let (name, id, default, types) = u.choose(ifaces)?;
                    if !*default || !u.arbitrary()? {
                        dst.push_str(".");
                        dst.push_str("%");
                        dst.push_str(name);
                    }
                    Some((*id, types))
                }
                Choice::Packages => {
                    let (name, docs) = u.choose(&self.packages)?;
                    dst.push_str("%");
                    dst.push_str(name);
                    dst.push_str(".");
                    let (name, ifaces) = u.choose(docs)?;
                    dst.push_str("%");
                    dst.push_str(name);
                    let (name, id, default, types) = u.choose(ifaces)?;
                    if !*default || !u.arbitrary()? {
                        dst.push_str(".");
                        dst.push_str("%");
                        dst.push_str(name);
                    }
                    Some((*id, types))
                }
            })
        }
    }

    impl<'a> InterfaceGenerator<'a> {
        fn new(gen: &'a Generator) -> InterfaceGenerator<'a> {
            InterfaceGenerator {
                gen,
                types_in_interface: Vec::new(),
                unique_names: HashSet::new(),
            }
        }

        fn gen_interface(
            &mut self,
            u: &mut Unstructured<'_>,
            name: Option<&str>,
            has_default: &mut bool,
        ) -> Result<String> {
            let mut ret = String::new();
            if !*has_default && u.arbitrary()? {
                *has_default = true;
                ret.push_str("default ");
            }
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
            while parts.len() < MAX_INTERFACE_ITEMS && u.arbitrary()? {
                match u.arbitrary()? {
                    Generate::Use => {
                        let mut part = String::new();
                        if self.gen_use(u, &mut part)? {
                            parts.push(part);
                        }
                    }
                    Generate::Type => {
                        let name = self.gen_unique_name(u)?;
                        let (size, typedef) = self.gen_typedef(u, &name)?;
                        parts.push(typedef);
                        self.types_in_interface.push((name, size));
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

        fn gen_world(
            &mut self,
            u: &mut Unstructured<'_>,
            name: &str,
            has_default: &mut bool,
        ) -> Result<String> {
            let mut ret = String::new();
            if !*has_default && u.arbitrary()? {
                *has_default = true;
                ret.push_str("default ");
            }
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

            while parts.len() < MAX_WORLD_ITEMS && !u.is_empty() && u.arbitrary()? {
                let kind = u.arbitrary::<ItemKind>()?;
                let direction = match kind {
                    ItemKind::Func(dir)
                    | ItemKind::Interface(dir)
                    | ItemKind::AnonInterface(dir) => Some(dir),
                    ItemKind::Type | ItemKind::Use => None,
                };

                let mut part = String::new();
                let name = match direction {
                    Some(Direction::Import) | None => gen_unique_name(u, &mut self.unique_names)?,
                    Some(Direction::Export) => gen_unique_name(u, &mut self.unique_names)?,
                };
                if let Some(dir) = direction {
                    part.push_str(match dir {
                        Direction::Import => "import",
                        Direction::Export => "export",
                    });
                    part.push_str(" %");
                    part.push_str(&name);
                    part.push_str(": ");
                }

                match kind {
                    ItemKind::Func(_) => {
                        self.gen_func_sig(u, &mut part)?;
                    }
                    ItemKind::Interface(dir) => {
                        let id = match self.gen.gen_path(u, &mut part)? {
                            Some((id, _types)) => id,
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
                            InterfaceGenerator::new(self.gen).gen_interface(u, None, &mut true)?;
                        part.push_str(&iface);
                    }

                    ItemKind::Type => {
                        let (size, typedef) = self.gen_typedef(u, &name)?;
                        assert!(part.is_empty());
                        part = typedef;
                        self.types_in_interface.push((name, size));
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

        fn gen_use(&mut self, u: &mut Unstructured<'_>, part: &mut String) -> Result<bool> {
            let mut path = String::new();
            let (_id, types) = match self.gen.gen_path(u, &mut path)? {
                Some(types) => types,
                None => return Ok(false),
            };
            part.push_str("use ");
            part.push_str(&path);
            part.push_str(".{");
            let (name, size) = u.choose(types)?;
            part.push_str("%");
            part.push_str(name);
            let name = if self.unique_names.contains(name) || u.arbitrary()? {
                part.push_str(" as %");
                let name = self.gen_unique_name(u)?;
                part.push_str(&name);
                name
            } else {
                assert!(self.unique_names.insert(name.clone()));
                name.clone()
            };
            self.types_in_interface.push((name, *size));
            part.push_str("}");
            Ok(true)
        }

        fn gen_typedef(&mut self, u: &mut Unstructured<'_>, name: &str) -> Result<(usize, String)> {
            #[derive(Arbitrary)]
            pub enum Kind {
                Record,
                Flags,
                Variant,
                Enum,
                Union,
                Anonymous,
            }

            let mut fuel = MAX_TYPE_SIZE;
            let mut ret = String::new();
            match u.arbitrary()? {
                Kind::Record => {
                    ret.push_str("record %");
                    ret.push_str(name);
                    ret.push_str(" {\n");
                    for _ in 0..u.int_in_range(0..=MAX_PARTS)? {
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
                    for _ in 0..u.int_in_range(1..=MAX_PARTS)? {
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
                Kind::Union => {
                    ret.push_str("union %");
                    ret.push_str(name);
                    ret.push_str(" {\n");
                    for _ in 0..u.int_in_range(1..=MAX_PARTS)? {
                        ret.push_str("  ");
                        self.gen_type(u, &mut fuel, &mut ret)?;
                        ret.push_str(",\n");
                    }
                    ret.push_str("}");
                }
                Kind::Enum => {
                    ret.push_str("enum %");
                    ret.push_str(name);
                    ret.push_str(" {\n");
                    for _ in 0..u.int_in_range(1..=MAX_PARTS)? {
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
                    for _ in 0..u.int_in_range(0..=MAX_PARTS)? {
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
            }

            Ok((MAX_TYPE_SIZE - fuel, ret))
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
                        let (name, type_size) = u.choose(&self.types_in_interface)?;
                        *fuel = match fuel.checked_sub(*type_size) {
                            Some(fuel) => fuel,
                            None => continue,
                        };
                        dst.push_str("%");
                        dst.push_str(name);
                    }
                    Kind::Tuple => {
                        let fields = u.int_in_range(0..=MAX_PARTS)?;
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
            self.gen_func_sig(u, &mut ret)?;
            Ok(ret)
        }

        fn gen_func_sig(&mut self, u: &mut Unstructured<'_>, dst: &mut String) -> Result<()> {
            dst.push_str("func");
            self.gen_params(u, dst)?;
            if u.arbitrary()? {
                dst.push_str(" -> ");
                self.gen_params(u, dst)?;
            } else if u.arbitrary()? {
                dst.push_str(" -> ");
                let mut fuel = MAX_TYPE_SIZE;
                self.gen_type(u, &mut fuel, dst)?;
            }
            Ok(())
        }

        fn gen_params(&mut self, u: &mut Unstructured<'_>, dst: &mut String) -> Result<()> {
            dst.push_str("(");
            let mut fuel = MAX_TYPE_SIZE;
            for i in 0..u.int_in_range(0..=MAX_PARTS)? {
                if i > 0 {
                    dst.push_str(", ");
                }
                dst.push_str("%");
                dst.push_str(&self.gen_unique_name(u)?);
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
        use std::fmt::Write;
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
}
