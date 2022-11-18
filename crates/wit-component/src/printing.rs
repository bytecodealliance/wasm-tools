use anyhow::{bail, Result};
use indexmap::IndexSet;
use std::fmt::{self, Write};
use wit_parser::{
    Enum, Flags, Interface, Record, Result_, Results, Tuple, Type, TypeDefKind, TypeId, Union,
    Variant, World,
};

/// A utility for printing WebAssembly interface definitions to a string.
#[derive(Default)]
pub struct WorldPrinter {
    output: Output,
    declared: IndexSet<TypeId>,
}

impl WorldPrinter {
    /// Print the given WebAssembly interface to a string.
    pub fn print(&mut self, world: &World) -> Result<String> {
        for (name, import) in world.imports.iter() {
            writeln!(&mut self.output, "interface {name} {{")?;
            self.print_interface(import)?;
            writeln!(&mut self.output, "}}\n")?;
        }

        for (name, export) in world.exports.iter() {
            if world.imports.contains_key(name) {
                writeln!(&mut self.output, "interface {name}-exports {{")?;
            } else {
                writeln!(&mut self.output, "interface {name} {{")?;
            }
            self.print_interface(export)?;
            writeln!(&mut self.output, "}}\n")?;
        }

        writeln!(&mut self.output, "world {} {{", world.name)?;
        for (name, _import) in world.imports.iter() {
            writeln!(&mut self.output, "import {name}: {name}")?;
        }
        for (name, _export) in world.exports.iter() {
            if world.imports.contains_key(name) {
                writeln!(&mut self.output, "export {name}: {name}-exports")?;
            } else {
                writeln!(&mut self.output, "export {name}: {name}")?;
            }
        }
        if let Some(default) = &world.default {
            writeln!(&mut self.output, "default export interface {{")?;
            self.print_interface(default)?;
            writeln!(&mut self.output, "}}")?;
        }
        writeln!(&mut self.output, "}}")?;

        self.declared.clear();
        Ok(std::mem::take(&mut self.output).into())
    }

    /// Print the given WebAssembly interface to a string.
    fn print_interface(&mut self, interface: &Interface) -> Result<()> {
        for (id, _) in &interface.types {
            self.declare_type(interface, &Type::Id(id))?;
        }

        for func in &interface.functions {
            write!(&mut self.output, "{}: func(", func.name)?;
            for (i, (name, ty)) in func.params.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                write!(&mut self.output, "{}: ", name)?;
                self.print_type_name(interface, ty)?;
            }
            self.output.push_str(")");

            match &func.results {
                Results::Named(rs) => match rs.len() {
                    0 => (),
                    1 => {
                        self.output.push_str(" -> ");
                        self.print_type_name(interface, &rs[0].1)?;
                    }
                    _ => {
                        self.output.push_str(" -> (");
                        for (i, (name, ty)) in rs.iter().enumerate() {
                            if i > 0 {
                                self.output.push_str(", ");
                            }
                            write!(&mut self.output, "{name}: ")?;
                            self.print_type_name(interface, ty)?;
                        }
                        self.output.push_str(")");
                    }
                },
                Results::Anon(ty) => {
                    self.output.push_str(" -> ");
                    self.print_type_name(interface, ty)?;
                }
            }

            self.output.push_str("\n\n");
        }

        Ok(())
    }

    fn print_type_name(&mut self, interface: &Interface, ty: &Type) -> Result<()> {
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
                let ty = &interface.types[*id];
                if let Some(name) = &ty.name {
                    self.output.push_str(name);
                    return Ok(());
                }

                match &ty.kind {
                    TypeDefKind::Tuple(t) => {
                        self.print_tuple_type(interface, t)?;
                    }
                    TypeDefKind::Option(t) => {
                        self.print_option_type(interface, t)?;
                    }
                    TypeDefKind::Result(t) => {
                        self.print_result_type(interface, t)?;
                    }
                    TypeDefKind::Record(_) => {
                        bail!("interface has an unnamed record type");
                    }
                    TypeDefKind::Flags(_) => {
                        bail!("interface has unnamed flags type")
                    }
                    TypeDefKind::Enum(_) => {
                        bail!("interface has unnamed enum type")
                    }
                    TypeDefKind::Variant(_) => {
                        bail!("interface has unnamed variant type")
                    }
                    TypeDefKind::Union(_) => {
                        bail!("interface has unnamed union type")
                    }
                    TypeDefKind::List(ty) => {
                        self.output.push_str("list<");
                        self.print_type_name(interface, ty)?;
                        self.output.push_str(">");
                    }
                    TypeDefKind::Type(ty) => self.print_type_name(interface, ty)?,
                    TypeDefKind::Future(_) => {
                        todo!("interface has an unnamed future type")
                    }
                    TypeDefKind::Stream(_) => {
                        todo!("interface has an unnamed stream type")
                    }
                }
            }
        }

        Ok(())
    }

    fn print_tuple_type(&mut self, interface: &Interface, tuple: &Tuple) -> Result<()> {
        self.output.push_str("tuple<");
        for (i, ty) in tuple.types.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.print_type_name(interface, ty)?;
        }
        self.output.push_str(">");

        Ok(())
    }

    fn print_option_type(&mut self, interface: &Interface, payload: &Type) -> Result<()> {
        self.output.push_str("option<");
        self.print_type_name(interface, payload)?;
        self.output.push_str(">");
        Ok(())
    }

    fn print_result_type(&mut self, interface: &Interface, result: &Result_) -> Result<()> {
        match result {
            Result_ {
                ok: Some(ok),
                err: Some(err),
            } => {
                self.output.push_str("result<");
                self.print_type_name(interface, ok)?;
                self.output.push_str(", ");
                self.print_type_name(interface, err)?;
                self.output.push_str(">");
            }
            Result_ {
                ok: None,
                err: Some(err),
            } => {
                self.output.push_str("result<_, ");
                self.print_type_name(interface, err)?;
                self.output.push_str(">");
            }
            Result_ {
                ok: Some(ok),
                err: None,
            } => {
                self.output.push_str("result<");
                self.print_type_name(interface, ok)?;
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

    fn declare_type(&mut self, interface: &Interface, ty: &Type) -> Result<()> {
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

                let ty = &interface.types[*id];
                match &ty.kind {
                    TypeDefKind::Record(r) => {
                        self.declare_record(interface, ty.name.as_deref(), r)?
                    }
                    TypeDefKind::Tuple(t) => {
                        self.declare_tuple(interface, ty.name.as_deref(), t)?
                    }
                    TypeDefKind::Flags(f) => self.declare_flags(ty.name.as_deref(), f)?,
                    TypeDefKind::Variant(v) => {
                        self.declare_variant(interface, ty.name.as_deref(), v)?
                    }
                    TypeDefKind::Union(u) => {
                        self.declare_union(interface, ty.name.as_deref(), u)?
                    }
                    TypeDefKind::Option(t) => {
                        self.declare_option(interface, ty.name.as_deref(), t)?
                    }
                    TypeDefKind::Result(r) => {
                        self.declare_result(interface, ty.name.as_deref(), r)?
                    }
                    TypeDefKind::Enum(e) => self.declare_enum(ty.name.as_deref(), e)?,
                    TypeDefKind::List(inner) => {
                        self.declare_list(interface, ty.name.as_deref(), inner)?
                    }
                    TypeDefKind::Type(inner) => match ty.name.as_deref() {
                        Some(name) => {
                            write!(&mut self.output, "type {} = ", name)?;
                            self.print_type_name(interface, inner)?;
                            self.output.push_str("\n\n");
                        }
                        None => bail!("unnamed type in interface"),
                    },
                    TypeDefKind::Future(_) => todo!("declare future"),
                    TypeDefKind::Stream(_) => todo!("declare stream"),
                }
            }
        }
        Ok(())
    }

    fn declare_record(
        &mut self,
        interface: &Interface,
        name: Option<&str>,
        record: &Record,
    ) -> Result<()> {
        for field in record.fields.iter() {
            self.declare_type(interface, &field.ty)?;
        }

        match name {
            Some(name) => {
                writeln!(&mut self.output, "record {} {{", name)?;
                for field in &record.fields {
                    write!(&mut self.output, "{}: ", field.name)?;
                    self.declare_type(interface, &field.ty)?;
                    self.print_type_name(interface, &field.ty)?;
                    self.output.push_str(",\n");
                }
                self.output.push_str("}\n\n");
                Ok(())
            }
            None => bail!("interface has unnamed record type"),
        }
    }

    fn declare_tuple(
        &mut self,
        interface: &Interface,
        name: Option<&str>,
        tuple: &Tuple,
    ) -> Result<()> {
        for ty in tuple.types.iter() {
            self.declare_type(interface, ty)?;
        }

        if let Some(name) = name {
            write!(&mut self.output, "type {} = ", name)?;
            self.print_tuple_type(interface, tuple)?;
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
            None => bail!("interface has unnamed flags type"),
        }
        Ok(())
    }

    fn declare_variant(
        &mut self,
        interface: &Interface,
        name: Option<&str>,
        variant: &Variant,
    ) -> Result<()> {
        for case in variant.cases.iter() {
            if let Some(ty) = case.ty {
                self.declare_type(interface, &ty)?;
            }
        }

        let name = match name {
            Some(name) => name,
            None => bail!("interface has unnamed union type"),
        };
        writeln!(&mut self.output, "variant {} {{", name)?;
        for case in &variant.cases {
            write!(&mut self.output, "{}", case.name)?;
            if let Some(ty) = case.ty {
                self.output.push_str("(");
                self.print_type_name(interface, &ty)?;
                self.output.push_str(")");
            }
            self.output.push_str(",\n");
        }
        self.output.push_str("}\n\n");
        Ok(())
    }

    fn declare_union(
        &mut self,
        interface: &Interface,
        name: Option<&str>,
        union: &Union,
    ) -> Result<()> {
        for case in union.cases.iter() {
            self.declare_type(interface, &case.ty)?;
        }

        let name = match name {
            Some(name) => name,
            None => bail!("interface has unnamed union type"),
        };
        writeln!(&mut self.output, "union {} {{", name)?;
        for case in &union.cases {
            self.output.push_str("");
            self.print_type_name(interface, &case.ty)?;
            self.output.push_str(",\n");
        }
        self.output.push_str("}\n\n");
        Ok(())
    }

    fn declare_option(
        &mut self,
        interface: &Interface,
        name: Option<&str>,
        payload: &Type,
    ) -> Result<()> {
        self.declare_type(interface, payload)?;

        if let Some(name) = name {
            write!(&mut self.output, "type {} = ", name)?;
            self.print_option_type(interface, payload)?;
            self.output.push_str("\n\n");
        }
        Ok(())
    }

    fn declare_result(
        &mut self,
        interface: &Interface,
        name: Option<&str>,
        result: &Result_,
    ) -> Result<()> {
        if let Some(ok) = result.ok {
            self.declare_type(interface, &ok)?;
        }
        if let Some(err) = result.err {
            self.declare_type(interface, &err)?;
        }

        if let Some(name) = name {
            write!(&mut self.output, "type {} = ", name)?;
            self.print_result_type(interface, result)?;
            self.output.push_str("\n\n");
        }
        Ok(())
    }

    fn declare_enum(&mut self, name: Option<&str>, enum_: &Enum) -> Result<()> {
        let name = match name {
            Some(name) => name,
            None => bail!("interface has unnamed enum type"),
        };
        writeln!(&mut self.output, "enum {} {{", name)?;
        for case in &enum_.cases {
            writeln!(&mut self.output, "{},", case.name)?;
        }
        self.output.push_str("}\n\n");
        Ok(())
    }

    fn declare_list(&mut self, interface: &Interface, name: Option<&str>, ty: &Type) -> Result<()> {
        self.declare_type(interface, ty)?;

        if let Some(name) = name {
            write!(&mut self.output, "type {} = list<", name)?;
            self.print_type_name(interface, ty)?;
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
