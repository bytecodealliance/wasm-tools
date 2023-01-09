use anyhow::{anyhow, bail, Result};
use indexmap::{IndexMap, IndexSet};
use std::fmt::{self, Write};
use wit_parser::*;

/// A utility for printing WebAssembly interface definitions to a string.
#[derive(Default)]
pub struct DocumentPrinter {
    output: Output,
    declared: IndexSet<TypeId>,
}

impl DocumentPrinter {
    /// Print the given `*.wit` document to a string.
    pub fn print(&mut self, resolve: &Resolve, docid: DocumentId) -> Result<String> {
        let doc = &resolve.documents[docid];
        for (name, id) in doc.interfaces.iter() {
            writeln!(&mut self.output, "interface {name} {{")?;
            self.print_interface(resolve, *id)?;
            writeln!(&mut self.output, "}}\n")?;
        }

        for (name, id) in doc.worlds.iter() {
            let world = &resolve.worlds[*id];
            writeln!(&mut self.output, "world {name} {{")?;
            for (name, import) in world.imports.iter() {
                self.print_world_item(resolve, name, import, docid, "import")?;
            }
            for (name, export) in world.exports.iter() {
                self.print_world_item(resolve, name, export, docid, "export")?;
            }
            writeln!(&mut self.output, "}}")?;
        }

        self.declared.clear();
        Ok(std::mem::take(&mut self.output).into())
    }

    /// Print the given WebAssembly interface to a string.
    fn print_interface(&mut self, resolve: &Resolve, id: InterfaceId) -> Result<()> {
        let interface = &resolve.interfaces[id];

        // Partition types defined in this interface into either those imported
        // from foreign interfaces or those defined locally.
        let mut types_to_declare = Vec::new();
        let mut types_to_import = IndexMap::new();
        for (name, ty_id) in &interface.types {
            let ty = &resolve.types[*ty_id];
            if let TypeDefKind::Type(Type::Id(other)) = ty.kind {
                let other = &resolve.types[other];
                if let TypeOwner::Interface(other_iface) = other.owner {
                    if other_iface != id {
                        let other_name = other
                            .name
                            .as_ref()
                            .ok_or_else(|| anyhow!("cannot import unnamed type"))?;
                        types_to_import
                            .entry(other_iface)
                            .or_insert(Vec::new())
                            .push((name, other_name));
                        continue;
                    }
                }
            }

            types_to_declare.push(*ty_id);
        }

        // Generate a `use` statement for all imported types.
        let my_doc = resolve.interfaces[id].document;
        for (id, tys) in types_to_import {
            write!(&mut self.output, "use ")?;
            self.print_path_to_interface(resolve, id, my_doc)?;
            write!(&mut self.output, ".{{")?;
            for (i, (my_name, other_name)) in tys.into_iter().enumerate() {
                if i > 0 {
                    write!(&mut self.output, ", ")?;
                }
                if my_name == other_name {
                    write!(&mut self.output, "{my_name}")?;
                } else {
                    write!(&mut self.output, "{other_name} as {my_name}")?;
                }
            }
            writeln!(&mut self.output, "}}")?;
        }

        // Declare all local types
        for id in types_to_declare {
            self.declare_type(resolve, &Type::Id(id))?;
        }

        for (i, (name, func)) in interface.functions.iter().enumerate() {
            if i > 0 {
                self.output.push_str("\n");
            }
            write!(&mut self.output, "{name}: ")?;
            self.print_function(resolve, func)?;
            self.output.push_str("\n");
        }

        Ok(())
    }

    fn print_function(&mut self, resolve: &Resolve, func: &Function) -> Result<()> {
        self.output.push_str("func(");
        for (i, (name, ty)) in func.params.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            write!(&mut self.output, "{}: ", name)?;
            self.print_type_name(resolve, ty)?;
        }
        self.output.push_str(")");

        match &func.results {
            Results::Named(rs) => match rs.len() {
                0 => (),
                1 => {
                    self.output.push_str(" -> ");
                    self.print_type_name(resolve, &rs[0].1)?;
                }
                _ => {
                    self.output.push_str(" -> (");
                    for (i, (name, ty)) in rs.iter().enumerate() {
                        if i > 0 {
                            self.output.push_str(", ");
                        }
                        write!(&mut self.output, "{name}: ")?;
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

    fn print_world_item(
        &mut self,
        resolve: &Resolve,
        name: &str,
        item: &WorldItem,
        cur_doc: DocumentId,
        desc: &str,
    ) -> Result<()> {
        write!(&mut self.output, "{desc} {name}: ")?;
        match item {
            WorldItem::Interface(id) => {
                if resolve.interfaces[*id].name.is_some() {
                    self.print_path_to_interface(resolve, *id, cur_doc)?;
                    self.output.push_str("\n");
                } else {
                    writeln!(self.output, "interface {{")?;
                    self.print_interface(resolve, *id)?;
                    writeln!(self.output, "}}")?;
                }
            }
            WorldItem::Function(f) => {
                self.print_function(resolve, f)?;
                self.output.push_str("\n");
            }
        }
        Ok(())
    }

    fn print_path_to_interface(
        &mut self,
        resolve: &Resolve,
        interface: InterfaceId,
        cur_doc: DocumentId,
    ) -> Result<()> {
        let cur_pkg = resolve.documents[cur_doc].package;
        let iface = &resolve.interfaces[interface];
        let iface_doc = &resolve.documents[iface.document];
        if iface.document == cur_doc {
            write!(&mut self.output, "self")?;
        } else if cur_pkg == iface_doc.package {
            write!(&mut self.output, "pkg.{}", iface_doc.name)?;
        } else {
            let iface_pkg = &resolve.packages[iface_doc.package.unwrap()];
            write!(&mut self.output, "{}.{}", iface_pkg.name, iface_doc.name)?;
        }
        write!(&mut self.output, ".{}", iface.name.as_ref().unwrap())?;
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
                    self.output.push_str(name);
                    return Ok(());
                }

                match &ty.kind {
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
                if !self.declared.insert(*id) {
                    return Ok(());
                }

                let ty = &resolve.types[*id];
                match &ty.kind {
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
                            write!(&mut self.output, "type {} = ", name)?;
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

    fn declare_record(
        &mut self,
        resolve: &Resolve,
        name: Option<&str>,
        record: &Record,
    ) -> Result<()> {
        for field in record.fields.iter() {
            self.declare_type(resolve, &field.ty)?;
        }

        match name {
            Some(name) => {
                writeln!(&mut self.output, "record {} {{", name)?;
                for field in &record.fields {
                    write!(&mut self.output, "{}: ", field.name)?;
                    self.declare_type(resolve, &field.ty)?;
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
        for ty in tuple.types.iter() {
            self.declare_type(resolve, ty)?;
        }

        if let Some(name) = name {
            write!(&mut self.output, "type {} = ", name)?;
            self.print_tuple_type(resolve, tuple)?;
            self.output.push_str("\n\n");
        }
        Ok(())
    }

    fn declare_flags(&mut self, name: Option<&str>, flags: &Flags) -> Result<()> {
        match name {
            Some(name) => {
                writeln!(&mut self.output, "flags {} {{", name)?;
                for flag in &flags.flags {
                    writeln!(&mut self.output, "{},", flag.name)?;
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
        for case in variant.cases.iter() {
            if let Some(ty) = case.ty {
                self.declare_type(resolve, &ty)?;
            }
        }

        let name = match name {
            Some(name) => name,
            None => bail!("document has unnamed union type"),
        };
        writeln!(&mut self.output, "variant {} {{", name)?;
        for case in &variant.cases {
            write!(&mut self.output, "{}", case.name)?;
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
        for case in union.cases.iter() {
            self.declare_type(resolve, &case.ty)?;
        }

        let name = match name {
            Some(name) => name,
            None => bail!("document has unnamed union type"),
        };
        writeln!(&mut self.output, "union {} {{", name)?;
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
        self.declare_type(resolve, payload)?;

        if let Some(name) = name {
            write!(&mut self.output, "type {} = ", name)?;
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
        if let Some(ok) = result.ok {
            self.declare_type(resolve, &ok)?;
        }
        if let Some(err) = result.err {
            self.declare_type(resolve, &err)?;
        }

        if let Some(name) = name {
            write!(&mut self.output, "type {} = ", name)?;
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
        writeln!(&mut self.output, "enum {} {{", name)?;
        for case in &enum_.cases {
            writeln!(&mut self.output, "{},", case.name)?;
        }
        self.output.push_str("}\n\n");
        Ok(())
    }

    fn declare_list(&mut self, resolve: &Resolve, name: Option<&str>, ty: &Type) -> Result<()> {
        self.declare_type(resolve, ty)?;

        if let Some(name) = name {
            write!(&mut self.output, "type {} = list<", name)?;
            self.print_type_name(resolve, ty)?;
            self.output.push_str(">\n\n");
            return Ok(());
        }

        Ok(())
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
