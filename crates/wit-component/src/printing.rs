use anyhow::{anyhow, bail, Result};
use std::fmt::{self, Write};
use wit_parser::*;

/// A utility for printing WebAssembly interface definitions to a string.
#[derive(Default)]
pub struct WitPrinter {
    output: Output,
}

impl WitPrinter {
    /// Print the given WIT package to a string.
    pub fn print(&mut self, resolve: &Resolve, pkgid: PackageId) -> Result<String> {
        let pkg = &resolve.packages[pkgid];
        self.output.push_str("package ");
        self.print_name(&pkg.name.namespace);
        self.output.push_str(":");
        self.print_name(&pkg.name.name);
        if let Some(version) = &pkg.name.version {
            self.output.push_str(&format!("@{version}"));
        }
        self.output.push_str("\n\n");
        for (name, id) in pkg.interfaces.iter() {
            self.output.push_str("interface ");
            self.print_name(name);
            self.output.push_str(" {\n");
            self.print_interface(resolve, *id)?;
            writeln!(&mut self.output, "}}\n")?;
        }

        for (name, id) in pkg.worlds.iter() {
            self.output.push_str("world ");
            self.print_name(name);
            self.output.push_str(" {\n");
            self.print_world(resolve, *id)?;
            writeln!(&mut self.output, "}}")?;
        }

        Ok(std::mem::take(&mut self.output).into())
    }

    /// Print the given WebAssembly interface to a string.
    fn print_interface(&mut self, resolve: &Resolve, id: InterfaceId) -> Result<()> {
        let interface = &resolve.interfaces[id];

        self.print_types(
            resolve,
            TypeOwner::Interface(id),
            interface
                .types
                .iter()
                .map(|(name, id)| (name.as_str(), *id)),
        )?;

        for (i, (name, func)) in interface.functions.iter().enumerate() {
            if i > 0 {
                self.output.push_str("\n");
            }
            self.print_name(name);
            self.output.push_str(": ");
            self.print_function(resolve, func)?;
            self.output.push_str("\n");
        }

        Ok(())
    }

    fn print_types<'a>(
        &mut self,
        resolve: &Resolve,
        owner: TypeOwner,
        types: impl Iterator<Item = (&'a str, TypeId)>,
    ) -> Result<()> {
        // Partition types defined in this interface into either those imported
        // from foreign interfaces or those defined locally.
        let mut types_to_declare = Vec::new();
        let mut types_to_import: Vec<(_, Vec<_>)> = Vec::new();
        for (name, ty_id) in types {
            let ty = &resolve.types[ty_id];
            if let TypeDefKind::Type(Type::Id(other)) = ty.kind {
                let other = &resolve.types[other];
                match other.owner {
                    TypeOwner::None => {}
                    other_owner if owner != other_owner => {
                        let other_name = other
                            .name
                            .as_ref()
                            .ok_or_else(|| anyhow!("cannot import unnamed type"))?;
                        if let Some((owner, list)) = types_to_import.last_mut() {
                            if *owner == other_owner {
                                list.push((name, other_name));
                                continue;
                            }
                        }
                        types_to_import.push((other_owner, vec![(name, other_name)]));
                        continue;
                    }
                    _ => {}
                }
            }

            types_to_declare.push(ty_id);
        }

        // Generate a `use` statement for all imported types.
        let my_pkg = match owner {
            TypeOwner::Interface(id) => resolve.interfaces[id].package.unwrap(),
            TypeOwner::World(id) => resolve.worlds[id].package.unwrap(),
            TypeOwner::None => unreachable!(),
        };
        let amt_to_import = types_to_import.len();
        for (owner, tys) in types_to_import {
            write!(&mut self.output, "use ")?;
            let id = match owner {
                TypeOwner::Interface(id) => id,
                // it's only possible to import types from interfaces at
                // this time.
                _ => unreachable!(),
            };
            self.print_path_to_interface(resolve, id, my_pkg)?;
            write!(&mut self.output, ".{{")?;
            for (i, (my_name, other_name)) in tys.into_iter().enumerate() {
                if i > 0 {
                    write!(&mut self.output, ", ")?;
                }
                if my_name == other_name {
                    self.print_name(my_name);
                } else {
                    self.print_name(other_name);
                    self.output.push_str(" as ");
                    self.print_name(my_name);
                }
            }
            writeln!(&mut self.output, "}}")?;
        }

        if amt_to_import > 0 && types_to_declare.len() > 0 {
            self.output.push_str("\n");
        }

        for id in types_to_declare {
            self.declare_type(resolve, &Type::Id(id))?;
        }

        Ok(())
    }

    fn print_function(&mut self, resolve: &Resolve, func: &Function) -> Result<()> {
        self.output.push_str("func(");
        for (i, (name, ty)) in func.params.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.print_name(name);
            self.output.push_str(": ");
            self.print_type_name(resolve, ty)?;
        }
        self.output.push_str(")");

        match &func.results {
            Results::Named(rs) => match rs.len() {
                0 => (),
                _ => {
                    self.output.push_str(" -> (");
                    for (i, (name, ty)) in rs.iter().enumerate() {
                        if i > 0 {
                            self.output.push_str(", ");
                        }
                        self.print_name(name);
                        self.output.push_str(": ");
                        self.print_type_name(resolve, ty)?;
                    }
                    self.output.push_str(")");
                }
            },
            Results::Anon(ty) => {
                self.output.push_str(" -> ");
                self.print_type_name(resolve, ty)?;
            }
        }
        Ok(())
    }

    fn print_world(&mut self, resolve: &Resolve, id: WorldId) -> Result<()> {
        let world = &resolve.worlds[id];
        let pkgid = world.package.unwrap();
        let mut types = Vec::new();
        for (name, import) in world.imports.iter() {
            match import {
                WorldItem::Type(t) => match name {
                    WorldKey::Name(s) => types.push((s.as_str(), *t)),
                    WorldKey::Interface(_) => unreachable!(),
                },
                _ => {
                    self.print_world_item(resolve, name, import, pkgid, "import")?;
                }
            }
        }
        self.print_types(resolve, TypeOwner::World(id), types.into_iter())?;
        for (name, export) in world.exports.iter() {
            self.print_world_item(resolve, name, export, pkgid, "export")?;
        }
        Ok(())
    }

    fn print_world_item(
        &mut self,
        resolve: &Resolve,
        name: &WorldKey,
        item: &WorldItem,
        cur_pkg: PackageId,
        desc: &str,
    ) -> Result<()> {
        self.output.push_str(desc);
        self.output.push_str(" ");
        match name {
            WorldKey::Name(name) => {
                self.print_name(name);
                self.output.push_str(": ");
                match item {
                    WorldItem::Interface(id) => {
                        assert!(resolve.interfaces[*id].name.is_none());
                        writeln!(self.output, "interface {{")?;
                        self.print_interface(resolve, *id)?;
                        writeln!(self.output, "}}")?;
                    }
                    WorldItem::Function(f) => {
                        self.print_function(resolve, f)?;
                        self.output.push_str("\n");
                    }
                    // Types are handled separately
                    WorldItem::Type(_) => unreachable!(),
                }
            }
            WorldKey::Interface(id) => {
                match item {
                    WorldItem::Interface(id2) => assert_eq!(id, id2),
                    _ => unreachable!(),
                }
                self.print_path_to_interface(resolve, *id, cur_pkg)?;
                self.output.push_str("\n");
            }
        }
        Ok(())
    }

    fn print_path_to_interface(
        &mut self,
        resolve: &Resolve,
        interface: InterfaceId,
        cur_pkg: PackageId,
    ) -> Result<()> {
        let iface = &resolve.interfaces[interface];
        if iface.package == Some(cur_pkg) {
            self.print_name(iface.name.as_ref().unwrap());
        } else {
            let pkg = &resolve.packages[iface.package.unwrap()].name;
            self.print_name(&pkg.namespace);
            self.output.push_str(":");
            self.print_name(&pkg.name);
            self.output.push_str("/");
            self.print_name(iface.name.as_ref().unwrap());
            if let Some(version) = &pkg.version {
                self.output.push_str(&format!("@{version}"));
            }
        }
        Ok(())
    }

    fn print_type_name(&mut self, resolve: &Resolve, ty: &Type) -> Result<()> {
        match ty {
            Type::Bool => self.output.push_str("bool"),
            Type::U8 => self.output.push_str("u8"),
            Type::U16 => self.output.push_str("u16"),
            Type::U32 => self.output.push_str("u32"),
            Type::U64 => self.output.push_str("u64"),
            Type::S8 => self.output.push_str("s8"),
            Type::S16 => self.output.push_str("s16"),
            Type::S32 => self.output.push_str("s32"),
            Type::S64 => self.output.push_str("s64"),
            Type::Float32 => self.output.push_str("float32"),
            Type::Float64 => self.output.push_str("float64"),
            Type::Char => self.output.push_str("char"),
            Type::String => self.output.push_str("string"),

            Type::Id(id) => {
                let ty = &resolve.types[*id];
                if let Some(name) = &ty.name {
                    self.print_name(name);
                    return Ok(());
                }

                match &ty.kind {
                    TypeDefKind::Handle(h) => {
                        self.print_handle_type(resolve, h)?;
                    }
                    TypeDefKind::Resource(_) => {
                        bail!("resolve has an unnamed resource type");
                    }
                    TypeDefKind::Tuple(t) => {
                        self.print_tuple_type(resolve, t)?;
                    }
                    TypeDefKind::Option(t) => {
                        self.print_option_type(resolve, t)?;
                    }
                    TypeDefKind::Result(t) => {
                        self.print_result_type(resolve, t)?;
                    }
                    TypeDefKind::Record(_) => {
                        bail!("resolve has an unnamed record type");
                    }
                    TypeDefKind::Flags(_) => {
                        bail!("resolve has unnamed flags type")
                    }
                    TypeDefKind::Enum(_) => {
                        bail!("resolve has unnamed enum type")
                    }
                    TypeDefKind::Variant(_) => {
                        bail!("resolve has unnamed variant type")
                    }
                    TypeDefKind::Union(_) => {
                        bail!("document has unnamed union type")
                    }
                    TypeDefKind::List(ty) => {
                        self.output.push_str("list<");
                        self.print_type_name(resolve, ty)?;
                        self.output.push_str(">");
                    }
                    TypeDefKind::Type(ty) => self.print_type_name(resolve, ty)?,
                    TypeDefKind::Future(_) => {
                        todo!("document has an unnamed future type")
                    }
                    TypeDefKind::Stream(_) => {
                        todo!("document has an unnamed stream type")
                    }
                    TypeDefKind::Unknown => unreachable!(),
                }
            }
        }

        Ok(())
    }

    fn print_handle_type(&mut self, resolve: &Resolve, handle: &Handle) -> Result<()> {
        match handle {
            Handle::Shared(ty) => {
                self.output.push_str("shared<");
                self.print_type_name(resolve, ty)?;
                self.output.push_str(">");
            }
        }

        Ok(())
    }

    fn print_tuple_type(&mut self, resolve: &Resolve, tuple: &Tuple) -> Result<()> {
        self.output.push_str("tuple<");
        for (i, ty) in tuple.types.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.print_type_name(resolve, ty)?;
        }
        self.output.push_str(">");

        Ok(())
    }

    fn print_option_type(&mut self, resolve: &Resolve, payload: &Type) -> Result<()> {
        self.output.push_str("option<");
        self.print_type_name(resolve, payload)?;
        self.output.push_str(">");
        Ok(())
    }

    fn print_result_type(&mut self, resolve: &Resolve, result: &Result_) -> Result<()> {
        match result {
            Result_ {
                ok: Some(ok),
                err: Some(err),
            } => {
                self.output.push_str("result<");
                self.print_type_name(resolve, ok)?;
                self.output.push_str(", ");
                self.print_type_name(resolve, err)?;
                self.output.push_str(">");
            }
            Result_ {
                ok: None,
                err: Some(err),
            } => {
                self.output.push_str("result<_, ");
                self.print_type_name(resolve, err)?;
                self.output.push_str(">");
            }
            Result_ {
                ok: Some(ok),
                err: None,
            } => {
                self.output.push_str("result<");
                self.print_type_name(resolve, ok)?;
                self.output.push_str(">");
            }
            Result_ {
                ok: None,
                err: None,
            } => {
                self.output.push_str("result");
            }
        }
        Ok(())
    }

    fn declare_type(&mut self, resolve: &Resolve, ty: &Type) -> Result<()> {
        match ty {
            Type::Bool
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::S8
            | Type::S16
            | Type::S32
            | Type::S64
            | Type::Float32
            | Type::Float64
            | Type::Char
            | Type::String => return Ok(()),

            Type::Id(id) => {
                let ty = &resolve.types[*id];
                match &ty.kind {
                    TypeDefKind::Handle(h) => {
                        self.declare_handle(resolve, ty.name.as_deref(), h)?
                    }
                    TypeDefKind::Resource(r) => {
                        self.declare_resource(resolve, ty.name.as_deref(), r)?
                    }
                    TypeDefKind::Record(r) => {
                        self.declare_record(resolve, ty.name.as_deref(), r)?
                    }
                    TypeDefKind::Tuple(t) => self.declare_tuple(resolve, ty.name.as_deref(), t)?,
                    TypeDefKind::Flags(f) => self.declare_flags(ty.name.as_deref(), f)?,
                    TypeDefKind::Variant(v) => {
                        self.declare_variant(resolve, ty.name.as_deref(), v)?
                    }
                    TypeDefKind::Union(u) => self.declare_union(resolve, ty.name.as_deref(), u)?,
                    TypeDefKind::Option(t) => {
                        self.declare_option(resolve, ty.name.as_deref(), t)?
                    }
                    TypeDefKind::Result(r) => {
                        self.declare_result(resolve, ty.name.as_deref(), r)?
                    }
                    TypeDefKind::Enum(e) => self.declare_enum(ty.name.as_deref(), e)?,
                    TypeDefKind::List(inner) => {
                        self.declare_list(resolve, ty.name.as_deref(), inner)?
                    }
                    TypeDefKind::Type(inner) => match ty.name.as_deref() {
                        Some(name) => {
                            self.output.push_str("type ");
                            self.print_name(name);
                            self.output.push_str(" = ");
                            self.print_type_name(resolve, inner)?;
                            self.output.push_str("\n\n");
                        }
                        None => bail!("unnamed type in document"),
                    },
                    TypeDefKind::Future(_) => todo!("declare future"),
                    TypeDefKind::Stream(_) => todo!("declare stream"),
                    TypeDefKind::Unknown => unreachable!(),
                }
            }
        }
        Ok(())
    }

    fn declare_handle(
        &mut self,
        resolve: &Resolve,
        name: Option<&str>,
        handle: &Handle,
    ) -> Result<()> {
        match name {
            Some(name) => {
                self.print_name(name);
                self.output.push_str(" = ");

                match handle {
                    Handle::Shared(ty) => {
                        self.output.push_str("shared<");
                        self.print_type_name(resolve, ty)?;
                        self.output.push_str(">");
                    }
                }

                self.output.push_str("\n\n");

                Ok(())
            }
            None => bail!("document has unnamed handle type"),
        }
    }

    fn declare_resource(
        &mut self,
        resolve: &Resolve,
        name: Option<&str>,
        resource: &Resource,
    ) -> Result<()> {
        match name {
            Some(name) => {
                self.output.push_str("resource ");
                self.print_name(name);
                self.output.push_str(" {\n");
                for function in &resource.methods {
                    self.declare_function(resolve, Some(name), function)?;
                }
                self.output.push_str(" }\n\n");
                Ok(())
            }
            None => bail!("document has unnamed resource type"),
        }
    }

    fn declare_function(
        &mut self,
        resolve: &Resolve,
        name: Option<&str>,
        function: &Function,
    ) -> Result<()> {
        match function.kind {
            FunctionKind::Static => {
                self.output.push_str("static ");
            }
            _ => {}
        }

        self.print_name(&function.name);
        self.output.push_str(": func(");

        match function.kind {
            FunctionKind::Method => match name {
                Some(name) => {
                    self.output.push_str(&format!("self: shared<{name}>"));
                }
                None => bail!("document has unnamed resource type"),
            },
            _ => {}
        }

        for (name, ty) in &function.params {
            self.print_name(&name);
            self.output.push_str(": ");
            self.print_type_name(resolve, &ty)?;
            self.output.push_str(", ");
        }

        self.output.push_str(") ");

        self.output.push_str("-> ");

        match &function.results {
            Results::Named(results) => {
                self.output.push_str("(");

                if results.len() > 0 {
                    for (name, ty) in results {
                        self.print_name(&name);
                        self.output.push_str(": ");
                        self.print_type_name(resolve, &ty)?;
                        self.output.push_str(", ");
                    }
                }
                self.output.push_str(")");
            }
            Results::Anon(ty) => {
                self.print_type_name(resolve, &ty)?;
            }
        }

        self.output.push_str(";\n");

        Ok(())
    }

    fn declare_record(
        &mut self,
        resolve: &Resolve,
        name: Option<&str>,
        record: &Record,
    ) -> Result<()> {
        match name {
            Some(name) => {
                self.output.push_str("record ");
                self.print_name(name);
                self.output.push_str(" {\n");
                for field in &record.fields {
                    self.print_name(&field.name);
                    self.output.push_str(": ");
                    self.print_type_name(resolve, &field.ty)?;
                    self.output.push_str(",\n");
                }
                self.output.push_str("}\n\n");
                Ok(())
            }
            None => bail!("document has unnamed record type"),
        }
    }

    fn declare_tuple(
        &mut self,
        resolve: &Resolve,
        name: Option<&str>,
        tuple: &Tuple,
    ) -> Result<()> {
        if let Some(name) = name {
            self.output.push_str("type ");
            self.print_name(name);
            self.output.push_str(" = ");
            self.print_tuple_type(resolve, tuple)?;
            self.output.push_str("\n\n");
        }
        Ok(())
    }

    fn declare_flags(&mut self, name: Option<&str>, flags: &Flags) -> Result<()> {
        match name {
            Some(name) => {
                self.output.push_str("flags ");
                self.print_name(name);
                self.output.push_str(" {\n");
                for flag in &flags.flags {
                    self.print_name(&flag.name);
                    self.output.push_str(",\n");
                }
                self.output.push_str("}\n\n");
            }
            None => bail!("document has unnamed flags type"),
        }
        Ok(())
    }

    fn declare_variant(
        &mut self,
        resolve: &Resolve,
        name: Option<&str>,
        variant: &Variant,
    ) -> Result<()> {
        let name = match name {
            Some(name) => name,
            None => bail!("document has unnamed union type"),
        };
        self.output.push_str("variant ");
        self.print_name(name);
        self.output.push_str(" {\n");
        for case in &variant.cases {
            self.print_name(&case.name);
            if let Some(ty) = case.ty {
                self.output.push_str("(");
                self.print_type_name(resolve, &ty)?;
                self.output.push_str(")");
            }
            self.output.push_str(",\n");
        }
        self.output.push_str("}\n\n");
        Ok(())
    }

    fn declare_union(
        &mut self,
        resolve: &Resolve,
        name: Option<&str>,
        union: &Union,
    ) -> Result<()> {
        let name = match name {
            Some(name) => name,
            None => bail!("document has unnamed union type"),
        };
        self.output.push_str("union ");
        self.print_name(name);
        self.output.push_str(" {\n");
        for case in &union.cases {
            self.output.push_str("");
            self.print_type_name(resolve, &case.ty)?;
            self.output.push_str(",\n");
        }
        self.output.push_str("}\n\n");
        Ok(())
    }

    fn declare_option(
        &mut self,
        resolve: &Resolve,
        name: Option<&str>,
        payload: &Type,
    ) -> Result<()> {
        if let Some(name) = name {
            self.output.push_str("type ");
            self.print_name(name);
            self.output.push_str(" = ");
            self.print_option_type(resolve, payload)?;
            self.output.push_str("\n\n");
        }
        Ok(())
    }

    fn declare_result(
        &mut self,
        resolve: &Resolve,
        name: Option<&str>,
        result: &Result_,
    ) -> Result<()> {
        if let Some(name) = name {
            self.output.push_str("type ");
            self.print_name(name);
            self.output.push_str(" = ");
            self.print_result_type(resolve, result)?;
            self.output.push_str("\n\n");
        }
        Ok(())
    }

    fn declare_enum(&mut self, name: Option<&str>, enum_: &Enum) -> Result<()> {
        let name = match name {
            Some(name) => name,
            None => bail!("document has unnamed enum type"),
        };
        self.output.push_str("enum ");
        self.print_name(name);
        self.output.push_str(" {\n");
        for case in &enum_.cases {
            self.print_name(&case.name);
            self.output.push_str(",\n");
        }
        self.output.push_str("}\n\n");
        Ok(())
    }

    fn declare_list(&mut self, resolve: &Resolve, name: Option<&str>, ty: &Type) -> Result<()> {
        if let Some(name) = name {
            self.output.push_str("type ");
            self.print_name(name);
            self.output.push_str(" = list<");
            self.print_type_name(resolve, ty)?;
            self.output.push_str(">\n\n");
            return Ok(());
        }

        Ok(())
    }

    fn print_name(&mut self, name: &str) {
        if is_keyword(name) {
            self.output.push_str("%");
        }
        self.output.push_str(name);
    }
}

fn is_keyword(name: &str) -> bool {
    match name {
        "use" | "type" | "func" | "u8" | "u16" | "u32" | "u64" | "s8" | "s16" | "s32" | "s64"
        | "float32" | "float64" | "char" | "resource" | "record" | "flags" | "variant" | "enum"
        | "union" | "bool" | "string" | "option" | "result" | "future" | "stream" | "list"
        | "shared" | "_" | "as" | "from" | "static" | "interface" | "tuple" | "implements"
        | "world" | "import" | "export" | "default" | "pkg" | "self" | "package" => true,
        _ => false,
    }
}

/// Helper structure to help maintain an indentation level when printing source,
/// modeled after the support in `wit-bindgen-core`.
#[derive(Default)]
struct Output {
    indent: usize,
    output: String,
}

impl Output {
    fn push_str(&mut self, src: &str) {
        let lines = src.lines().collect::<Vec<_>>();
        for (i, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with('}') && self.output.ends_with("  ") {
                self.output.pop();
                self.output.pop();
            }
            self.output.push_str(if lines.len() == 1 {
                line
            } else {
                line.trim_start()
            });
            if trimmed.ends_with('{') {
                self.indent += 1;
            }
            if trimmed.starts_with('}') {
                // Note that a `saturating_sub` is used here to prevent a panic
                // here in the case of invalid code being generated in debug
                // mode. It's typically easier to debug those issues through
                // looking at the source code rather than getting a panic.
                self.indent = self.indent.saturating_sub(1);
            }
            if i != lines.len() - 1 || src.ends_with('\n') {
                // Trim trailing whitespace, if any, then push an indented
                // newline
                while let Some(c) = self.output.chars().next_back() {
                    if c.is_whitespace() && c != '\n' {
                        self.output.pop();
                    } else {
                        break;
                    }
                }
                self.output.push('\n');
                for _ in 0..self.indent {
                    self.output.push_str("  ");
                }
            }
        }
    }
}

impl Write for Output {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_str(s);
        Ok(())
    }
}

impl From<Output> for String {
    fn from(output: Output) -> String {
        output.output
    }
}
