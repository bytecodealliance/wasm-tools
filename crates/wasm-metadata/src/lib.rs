use anyhow::Result;
use indexmap::{map::Entry, IndexMap};
use serde::Serialize;
use std::fmt;
use std::mem;
use std::ops::Range;
use wasm_encoder::{ComponentSection as _, ComponentSectionId, Encode, Section};
use wasmparser::{
    ComponentNameSectionReader, NameSectionReader, Parser, Payload::*, ProducersSectionReader,
};

/// A representation of a WebAssembly producers section.
///
/// Spec: https://github.com/WebAssembly/tool-conventions/blob/main/ProducersSection.md
#[derive(Debug, Serialize)]
pub struct Producers(
    #[serde(serialize_with = "indexmap::map::serde_seq::serialize")]
    IndexMap<String, IndexMap<String, String>>,
);

impl Default for Producers {
    fn default() -> Self {
        Self::empty()
    }
}

impl Producers {
    /// Creates an empty producers section
    pub fn empty() -> Self {
        Producers(IndexMap::new())
    }

    /// Indicates if section is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Read the producers section from a Wasm binary. Supports both core
    /// Modules and Components. In the component case, only returns the
    /// producers section in the outer component, ignoring all interior
    /// components and modules.
    pub fn from_wasm(bytes: &[u8]) -> Result<Option<Self>> {
        let mut depth = 0;
        for payload in Parser::new(0).parse_all(bytes) {
            let payload = payload?;
            use wasmparser::Payload::*;
            match payload {
                ModuleSection { .. } | ComponentSection { .. } => depth += 1,
                End { .. } => depth -= 1,
                CustomSection(c) if c.name() == "producers" && depth == 0 => {
                    let section = ProducersSectionReader::new(c.data(), c.data_offset())?;
                    let producers = Self::from_reader(section)?;
                    return Ok(Some(producers));
                }
                _ => {}
            }
        }
        Ok(None)
    }
    /// Read the producers section from a Wasm binary.
    pub fn from_reader(section: ProducersSectionReader) -> Result<Self> {
        let mut fields = IndexMap::new();
        for field in section.into_iter() {
            let field = field?;
            let mut values = IndexMap::new();
            for value in field.values.into_iter() {
                let value = value?;
                values.insert(value.name.to_owned(), value.version.to_owned());
            }
            fields.insert(field.name.to_owned(), values);
        }
        Ok(Producers(fields))
    }
    /// Add a name & version value to a field.
    ///
    /// The spec says expected field names are "language", "processed-by", and "sdk".
    /// The version value should be left blank for languages.
    pub fn add(&mut self, field: &str, name: &str, version: &str) {
        match self.0.entry(field.to_string()) {
            Entry::Occupied(e) => {
                e.into_mut().insert(name.to_owned(), version.to_owned());
            }
            Entry::Vacant(e) => {
                let mut m = IndexMap::new();
                m.insert(name.to_owned(), version.to_owned());
                e.insert(m);
            }
        }
    }

    /// Add all values found in another `Producers` section. Values in `other` take
    /// precedence.
    pub fn merge(&mut self, other: &Self) {
        for (field, values) in other.iter() {
            for (name, version) in values.iter() {
                self.add(field, name, version);
            }
        }
    }

    /// Get the contents of a field
    pub fn get<'a>(&'a self, field: &str) -> Option<ProducersField<'a>> {
        self.0.get(&field.to_owned()).map(ProducersField)
    }

    /// Iterate through all fields
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (&'a String, ProducersField<'a>)> + 'a {
        self.0
            .iter()
            .map(|(name, field)| (name, ProducersField(field)))
    }

    /// Construct the fields specified by [`AddMetadata`]
    fn from_meta(add: &AddMetadata) -> Self {
        let mut s = Self::empty();
        for lang in add.language.iter() {
            s.add("language", &lang, "");
        }
        for (name, version) in add.processed_by.iter() {
            s.add("processed-by", &name, &version);
        }
        for (name, version) in add.sdk.iter() {
            s.add("sdk", &name, &version);
        }
        s
    }

    /// Serialize into [`wasm_encoder::ProducersSection`].
    pub fn section(&self) -> wasm_encoder::ProducersSection {
        let mut section = wasm_encoder::ProducersSection::new();
        for (fieldname, fieldvalues) in self.0.iter() {
            let mut field = wasm_encoder::ProducersField::new();
            for (name, version) in fieldvalues {
                field.value(&name, &version);
            }
            section.field(&fieldname, &field);
        }
        section
    }

    /// Merge into an existing wasm module. Rewrites the module with this producers section
    /// merged into its existing one, or adds this producers section if none is present.
    pub fn add_to_wasm(&self, input: &[u8]) -> Result<Vec<u8>> {
        rewrite_wasm(&None, self, input)
    }

    fn display(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let indent = std::iter::repeat(" ").take(indent).collect::<String>();
        for (fieldname, fieldvalues) in self.0.iter() {
            writeln!(f, "{indent}{fieldname}:")?;
            for (name, version) in fieldvalues {
                if version.is_empty() {
                    writeln!(f, "{indent}    {name}")?;
                } else {
                    writeln!(f, "{indent}    {name}: {version}")?;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Display for Producers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display(f, 0)
    }
}

/// Contents of a producers field
pub struct ProducersField<'a>(&'a IndexMap<String, String>);

impl<'a> ProducersField<'a> {
    /// Get the version associated with a name in the field
    pub fn get(&self, name: &str) -> Option<&'a String> {
        self.0.get(&name.to_owned())
    }
    /// Iterate through all name-version pairs in the field
    pub fn iter(&self) -> impl Iterator<Item = (&'a String, &'a String)> + 'a {
        self.0.iter()
    }
}

/// Add metadata (module name, producers) to a WebAssembly file.
///
/// Supports both core WebAssembly modules and components. In components,
/// metadata will be added to the outermost component.
#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[derive(Debug, Clone, Default)]
pub struct AddMetadata {
    /// Add a module or component name to the names section
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME"))]
    pub name: Option<String>,

    /// Add a programming language to the producers section
    #[cfg_attr(feature = "clap", clap(long, value_name = "NAME"))]
    pub language: Vec<String>,

    /// Add a tool and its version to the producers section
    #[cfg_attr(feature = "clap", clap(long = "processed-by", value_parser = parse_key_value, value_name="NAME=VERSION"))]
    pub processed_by: Vec<(String, String)>,

    /// Add an SDK and its version to the producers section
    #[cfg_attr(feature="clap", clap(long, value_parser = parse_key_value, value_name="NAME=VERSION"))]
    pub sdk: Vec<(String, String)>,
}

#[cfg(feature = "clap")]
fn parse_key_value(s: &str) -> Result<(String, String)> {
    s.split_once('=')
        .map(|(k, v)| (k.to_owned(), v.to_owned()))
        .ok_or_else(|| anyhow::anyhow!("expected KEY=VALUE"))
}

impl AddMetadata {
    /// Process a WebAssembly binary. Supports both core WebAssembly modules, and WebAssembly
    /// components. The module and component will have, at very least, an empty name and producers
    /// section created.
    pub fn to_wasm(&self, input: &[u8]) -> Result<Vec<u8>> {
        rewrite_wasm(&self.name, &Producers::from_meta(self), input)
    }
}

fn rewrite_wasm(
    add_name: &Option<String>,
    add_producers: &Producers,
    input: &[u8],
) -> Result<Vec<u8>> {
    let mut producers_found = false;
    let mut names_found = false;
    let mut stack = Vec::new();
    let mut output = Vec::new();
    for payload in Parser::new(0).parse_all(&input) {
        let payload = payload?;

        // Track nesting depth, so that we don't mess with inner producer sections:
        match payload {
            Version { encoding, .. } => {
                output.extend_from_slice(match encoding {
                    wasmparser::Encoding::Component => &wasm_encoder::Component::HEADER,
                    wasmparser::Encoding::Module => &wasm_encoder::Module::HEADER,
                });
            }
            ModuleSection { .. } | ComponentSection { .. } => {
                stack.push(mem::take(&mut output));
                continue;
            }
            End { .. } => {
                let mut parent = match stack.pop() {
                    Some(c) => c,
                    None => break,
                };
                if output.starts_with(&wasm_encoder::Component::HEADER) {
                    parent.push(ComponentSectionId::Component as u8);
                    output.encode(&mut parent);
                } else {
                    parent.push(ComponentSectionId::CoreModule as u8);
                    output.encode(&mut parent);
                }
                output = parent;
            }
            _ => {}
        }

        // Process the wasm sections:
        match payload {
            // Only rewrite the outermost producers section:
            CustomSection(c) if c.name() == "producers" && stack.len() == 0 => {
                producers_found = true;
                let section = ProducersSectionReader::new(c.data(), c.data_offset())?;
                let mut producers = Producers::from_reader(section)?;
                // Add to the section according to the command line flags:
                producers.merge(&add_producers);
                // Encode into output:
                producers.section().append_to(&mut output);
            }

            CustomSection(c) if c.name() == "name" && stack.len() == 0 => {
                names_found = true;
                let section = NameSectionReader::new(c.data(), c.data_offset());
                let mut names = ModuleNames::from_reader(section)?;
                names.merge(&ModuleNames::from_name(add_name));

                names.section()?.as_custom().append_to(&mut output);
            }

            CustomSection(c) if c.name() == "component-name" && stack.len() == 0 => {
                names_found = true;
                let section = ComponentNameSectionReader::new(c.data(), c.data_offset());
                let mut names = ComponentNames::from_reader(section)?;
                names.merge(&ComponentNames::from_name(add_name));
                names.section()?.as_custom().append_to(&mut output);
            }

            // All other sections get passed through unmodified:
            _ => {
                if let Some((id, range)) = payload.as_section() {
                    wasm_encoder::RawSection {
                        id,
                        data: &input[range],
                    }
                    .append_to(&mut output);
                }
            }
        }
    }
    if !names_found && add_name.is_some() {
        if output.starts_with(&wasm_encoder::Component::HEADER) {
            let names = ComponentNames::from_name(add_name);
            names.section()?.append_to_component(&mut output);
        } else {
            let names = ModuleNames::from_name(add_name);
            names.section()?.append_to(&mut output)
        }
    }
    if !producers_found && !add_producers.is_empty() {
        let mut producers = Producers::empty();
        // Add to the section according to the command line flags:
        producers.merge(add_producers);
        // Encode into output:
        producers.section().append_to(&mut output);
    }
    Ok(output)
}

/// A tree of the metadata found in a WebAssembly binary.
#[derive(Debug, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Metadata {
    /// Metadata found inside a WebAssembly component.
    Component {
        /// The component name, if any. Found in the component-name section.
        name: Option<String>,
        /// The component's producers section, if any.
        producers: Option<Producers>,
        /// All child modules and components inside the component.
        children: Vec<Box<Metadata>>,
        /// Byte range of the module in the parent binary
        range: Range<usize>,
    },
    /// Metadata found inside a WebAssembly module.
    Module {
        /// The module name, if any. Found in the name section.
        name: Option<String>,
        /// The module's producers section, if any.
        producers: Option<Producers>,
        /// Byte range of the module in the parent binary
        range: Range<usize>,
    },
}

impl Metadata {
    /// Parse metadata from a WebAssembly binary. Supports both core WebAssembly modules, and
    /// WebAssembly components.
    pub fn from_binary(input: &[u8]) -> Result<Self> {
        let mut metadata = Vec::new();

        for payload in Parser::new(0).parse_all(&input) {
            match payload? {
                Version { encoding, .. } => {
                    if metadata.is_empty() {
                        match encoding {
                            wasmparser::Encoding::Module => {
                                metadata.push(Metadata::empty_module(0..input.len()))
                            }
                            wasmparser::Encoding::Component => {
                                metadata.push(Metadata::empty_component(0..input.len()))
                            }
                        }
                    }
                }
                ModuleSection { range, .. } => metadata.push(Metadata::empty_module(range)),
                ComponentSection { range, .. } => metadata.push(Metadata::empty_component(range)),
                End { .. } => {
                    let finished = metadata.pop().expect("non-empty metadata stack");
                    if metadata.is_empty() {
                        return Ok(finished);
                    } else {
                        metadata.last_mut().unwrap().push_child(finished);
                    }
                }
                CustomSection(c) if c.name() == "name" => {
                    let section = NameSectionReader::new(c.data(), c.data_offset());
                    let names = ModuleNames::from_reader(section)?;
                    if let Some(name) = names.get_name() {
                        metadata
                            .last_mut()
                            .expect("non-empty metadata stack")
                            .set_name(&name);
                    }
                }
                CustomSection(c) if c.name() == "component-name" => {
                    let section = ComponentNameSectionReader::new(c.data(), c.data_offset());
                    let names = ComponentNames::from_reader(section)?;
                    if let Some(name) = names.get_name() {
                        metadata
                            .last_mut()
                            .expect("non-empty metadata stack")
                            .set_name(name);
                    }
                }
                CustomSection(c) if c.name() == "producers" => {
                    let section = ProducersSectionReader::new(c.data(), c.data_offset())?;
                    let producers = Producers::from_reader(section)?;
                    metadata
                        .last_mut()
                        .expect("non-empty metadata stack")
                        .set_producers(producers);
                }

                _ => {}
            }
        }
        Err(anyhow::anyhow!(
            "malformed wasm binary, should have reached end"
        ))
    }

    fn empty_component(range: Range<usize>) -> Self {
        Metadata::Component {
            name: None,
            producers: None,
            children: Vec::new(),
            range,
        }
    }

    fn empty_module(range: Range<usize>) -> Self {
        Metadata::Module {
            name: None,
            producers: None,
            range,
        }
    }
    fn set_name(&mut self, n: &str) {
        match self {
            Metadata::Module { name, .. } => *name = Some(n.to_owned()),
            Metadata::Component { name, .. } => *name = Some(n.to_owned()),
        }
    }
    fn set_producers(&mut self, p: Producers) {
        match self {
            Metadata::Module { producers, .. } => *producers = Some(p),
            Metadata::Component { producers, .. } => *producers = Some(p),
        }
    }
    fn push_child(&mut self, child: Self) {
        match self {
            Metadata::Module { .. } => panic!("module shouldnt have children"),
            Metadata::Component { children, .. } => children.push(Box::new(child)),
        }
    }

    fn display(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let spaces = std::iter::repeat(" ").take(indent).collect::<String>();
        match self {
            Metadata::Module {
                name, producers, ..
            } => {
                if let Some(name) = name {
                    writeln!(f, "{spaces}module {name}:")?;
                } else {
                    writeln!(f, "{spaces}module:")?;
                }
                if let Some(producers) = producers {
                    producers.display(f, indent + 4)?;
                }
                Ok(())
            }
            Metadata::Component {
                name,
                producers,
                children,
                ..
            } => {
                if let Some(name) = name {
                    writeln!(f, "{spaces}component {name}:")?;
                } else {
                    writeln!(f, "{spaces}component:")?;
                }
                if let Some(producers) = producers {
                    producers.display(f, indent + 4)?;
                }
                for c in children {
                    c.display(f, indent + 4)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Metadata {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display(f, 0)
    }
}

/// Helper for rewriting a module's name section with a new module name.
pub struct ModuleNames<'a> {
    module_name: Option<String>,
    names: Vec<wasmparser::Name<'a>>,
}

impl<'a> ModuleNames<'a> {
    /// Create an empty name section.
    pub fn empty() -> Self {
        ModuleNames {
            module_name: None,
            names: Vec::new(),
        }
    }
    /// Read a name section from a WebAssembly binary. Records the module name, and all other
    /// contents of name section, for later serialization.
    pub fn from_reader(section: NameSectionReader<'a>) -> Result<ModuleNames<'a>> {
        let mut s = Self::empty();
        for name in section.into_iter() {
            let name = name?;
            match name {
                wasmparser::Name::Module { name, .. } => s.module_name = Some(name.to_owned()),
                _ => s.names.push(name),
            }
        }
        Ok(s)
    }
    /// Update module section according to [`AddMetadata`]
    fn from_name(name: &Option<String>) -> Self {
        let mut s = Self::empty();
        s.module_name = name.clone();
        s
    }

    /// Merge with another section
    fn merge(&mut self, other: &Self) {
        if other.module_name.is_some() {
            self.module_name = other.module_name.clone();
        }
        self.names.extend_from_slice(&other.names);
    }

    /// Set module name
    pub fn set_name(&mut self, name: &str) {
        self.module_name = Some(name.to_owned())
    }
    /// Get module name
    pub fn get_name(&self) -> Option<&String> {
        self.module_name.as_ref()
    }
    /// Serialize into [`wasm_encoder::NameSection`].
    pub fn section(&self) -> Result<wasm_encoder::NameSection> {
        let mut section = wasm_encoder::NameSection::new();
        if let Some(module_name) = &self.module_name {
            section.module(&module_name);
        }
        for n in self.names.iter() {
            match n {
                wasmparser::Name::Module { .. } => unreachable!(),
                wasmparser::Name::Function(m) => section.functions(&name_map(&m)?),
                wasmparser::Name::Local(m) => section.locals(&indirect_name_map(&m)?),
                wasmparser::Name::Label(m) => section.labels(&indirect_name_map(&m)?),
                wasmparser::Name::Type(m) => section.types(&name_map(&m)?),
                wasmparser::Name::Table(m) => section.tables(&name_map(&m)?),
                wasmparser::Name::Memory(m) => section.memories(&name_map(&m)?),
                wasmparser::Name::Global(m) => section.globals(&name_map(&m)?),
                wasmparser::Name::Element(m) => section.elements(&name_map(&m)?),
                wasmparser::Name::Data(m) => section.types(&name_map(&m)?),
                wasmparser::Name::Unknown { .. } => {} // wasm-encoder doesn't support it
            }
        }
        Ok(section)
    }
}

/// Helper for rewriting a component's component-name section with a new component name.
pub struct ComponentNames<'a> {
    component_name: Option<String>,
    names: Vec<wasmparser::ComponentName<'a>>,
}

impl<'a> ComponentNames<'a> {
    /// Create an empty component-name section.
    pub fn empty() -> Self {
        ComponentNames {
            component_name: None,
            names: Vec::new(),
        }
    }
    /// Read a component-name section from a WebAssembly binary. Records the component name, as
    /// well as all other component name fields for later serialization.
    pub fn from_reader(section: ComponentNameSectionReader<'a>) -> Result<ComponentNames<'a>> {
        let mut s = Self::empty();
        for name in section.into_iter() {
            let name = name?;
            match name {
                wasmparser::ComponentName::Component { name, .. } => {
                    s.component_name = Some(name.to_owned())
                }
                _ => s.names.push(name),
            }
        }
        Ok(s)
    }
    /// Set component name according to [`AddMetadata`]
    fn from_name(name: &Option<String>) -> Self {
        let mut s = Self::empty();
        s.component_name = name.clone();
        s
    }

    /// Merge with another section
    fn merge(&mut self, other: &Self) {
        if other.component_name.is_some() {
            self.component_name = other.component_name.clone();
        }
        self.names.extend_from_slice(&other.names);
    }

    /// Set component name
    pub fn set_name(&mut self, name: &str) {
        self.component_name = Some(name.to_owned())
    }
    /// Get component name
    pub fn get_name(&self) -> Option<&String> {
        self.component_name.as_ref()
    }
    /// Serialize into [`wasm_encoder::ComponentNameSection`]
    pub fn section(&self) -> Result<wasm_encoder::ComponentNameSection> {
        let mut section = wasm_encoder::ComponentNameSection::new();
        if let Some(component_name) = &self.component_name {
            section.component(&component_name);
        }
        for n in self.names.iter() {
            match n {
                wasmparser::ComponentName::Component { .. } => unreachable!(),
                wasmparser::ComponentName::CoreFuncs(m) => section.core_funcs(&name_map(&m)?),
                wasmparser::ComponentName::CoreGlobals(m) => section.core_globals(&name_map(&m)?),
                wasmparser::ComponentName::CoreMemories(m) => section.core_memories(&name_map(&m)?),
                wasmparser::ComponentName::CoreTables(m) => section.core_tables(&name_map(&m)?),
                wasmparser::ComponentName::CoreModules(m) => section.core_modules(&name_map(&m)?),
                wasmparser::ComponentName::CoreInstances(m) => {
                    section.core_instances(&name_map(&m)?)
                }
                wasmparser::ComponentName::CoreTypes(m) => section.core_types(&name_map(&m)?),
                wasmparser::ComponentName::Types(m) => section.types(&name_map(&m)?),
                wasmparser::ComponentName::Instances(m) => section.instances(&name_map(&m)?),
                wasmparser::ComponentName::Components(m) => section.components(&name_map(&m)?),
                wasmparser::ComponentName::Funcs(m) => section.funcs(&name_map(&m)?),
                wasmparser::ComponentName::Values(m) => section.values(&name_map(&m)?),
                wasmparser::ComponentName::Unknown { .. } => {} // wasm-encoder doesn't support it
            }
        }
        Ok(section)
    }
}

fn name_map(map: &wasmparser::NameMap<'_>) -> Result<wasm_encoder::NameMap> {
    let mut out = wasm_encoder::NameMap::new();
    for m in map.clone().into_iter() {
        let m = m?;
        out.append(m.index, m.name);
    }
    Ok(out)
}

fn indirect_name_map(
    map: &wasmparser::IndirectNameMap<'_>,
) -> Result<wasm_encoder::IndirectNameMap> {
    let mut out = wasm_encoder::IndirectNameMap::new();
    for m in map.clone().into_iter() {
        let m = m?;
        out.append(m.index, &name_map(&m.names)?);
    }
    Ok(out)
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn add_to_empty_module() {
        let wat = "(module)";
        let module = wat::parse_str(wat).unwrap();
        let add = AddMetadata {
            name: Some("foo".to_owned()),
            language: vec!["bar".to_owned()],
            processed_by: vec![("baz".to_owned(), "1.0".to_owned())],
            sdk: vec![],
        };
        let module = add.to_wasm(&module).unwrap();

        let metadata = Metadata::from_binary(&module).unwrap();
        match metadata {
            Metadata::Module {
                name,
                producers,
                range,
            } => {
                assert_eq!(name, Some("foo".to_owned()));
                let producers = producers.expect("some producers");
                assert_eq!(producers.get("language").unwrap().get("bar").unwrap(), "");
                assert_eq!(
                    producers.get("processed-by").unwrap().get("baz").unwrap(),
                    "1.0"
                );
                assert_eq!(range.start, 0);
                assert_eq!(range.end, 71);
            }
            _ => panic!("metadata should be module"),
        }
    }

    #[test]
    fn add_to_empty_component() {
        let wat = "(component)";
        let component = wat::parse_str(wat).unwrap();
        let add = AddMetadata {
            name: Some("foo".to_owned()),
            language: vec!["bar".to_owned()],
            processed_by: vec![("baz".to_owned(), "1.0".to_owned())],
            sdk: vec![],
        };
        let component = add.to_wasm(&component).unwrap();

        let metadata = Metadata::from_binary(&component).unwrap();
        match metadata {
            Metadata::Component {
                name,
                producers,
                children,
                range,
            } => {
                assert!(children.is_empty());
                assert_eq!(name, Some("foo".to_owned()));
                let producers = producers.expect("some producers");
                assert_eq!(producers.get("language").unwrap().get("bar").unwrap(), "");
                assert_eq!(
                    producers.get("processed-by").unwrap().get("baz").unwrap(),
                    "1.0"
                );
                assert_eq!(range.start, 0);
                assert_eq!(range.end, 81);
            }
            _ => panic!("metadata should be component"),
        }
    }

    #[test]
    fn add_to_nested_component() {
        // Create the same old module, stick some metadata into it
        let wat = "(module)";
        let module = wat::parse_str(wat).unwrap();
        let add = AddMetadata {
            name: Some("foo".to_owned()),
            language: vec!["bar".to_owned()],
            processed_by: vec![("baz".to_owned(), "1.0".to_owned())],
            sdk: vec![],
        };
        let module = add.to_wasm(&module).unwrap();

        // Stick that module inside a component.
        let mut component = wasm_encoder::Component::new();
        component.section(&wasm_encoder::RawSection {
            id: wasm_encoder::ComponentSectionId::CoreModule.into(),
            data: &module,
        });
        let component = component.finish();

        // Add some different metadata to the component.
        let add = AddMetadata {
            name: Some("gussie".to_owned()),
            sdk: vec![("willa".to_owned(), "sparky".to_owned())],
            ..Default::default()
        };
        let component = add.to_wasm(&component).unwrap();

        let metadata = Metadata::from_binary(&component).unwrap();
        match metadata {
            Metadata::Component {
                name,
                producers,
                children,
                ..
            } => {
                // Check that the component metadata is in the component
                assert_eq!(name, Some("gussie".to_owned()));
                let producers = producers.as_ref().expect("some producers");
                assert_eq!(
                    producers.get("sdk").unwrap().get("willa").unwrap(),
                    &"sparky".to_owned()
                );
                // Check that there is a single child with the metadata set for the module
                assert_eq!(children.len(), 1);
                let child = children.get(0).unwrap();
                match &**child {
                    Metadata::Module {
                        name,
                        producers,
                        range,
                    } => {
                        assert_eq!(name, &Some("foo".to_owned()));
                        let producers = producers.as_ref().expect("some producers");
                        assert_eq!(producers.get("language").unwrap().get("bar").unwrap(), "");
                        assert_eq!(
                            producers.get("processed-by").unwrap().get("baz").unwrap(),
                            "1.0"
                        );
                        assert_eq!(range.start, 10);
                        assert_eq!(range.end, 81);
                    }
                    _ => panic!("child is a module"),
                }
            }
            _ => panic!("root should be component"),
        }
    }

    #[test]
    fn producers_empty_module() {
        let wat = "(module)";
        let module = wat::parse_str(wat).unwrap();
        let mut producers = Producers::empty();
        producers.add("language", "bar", "");
        producers.add("processed-by", "baz", "1.0");

        let module = producers.add_to_wasm(&module).unwrap();

        let metadata = Metadata::from_binary(&module).unwrap();
        match metadata {
            Metadata::Module {
                name, producers, ..
            } => {
                assert_eq!(name, None);
                let producers = producers.expect("some producers");
                assert_eq!(producers.get("language").unwrap().get("bar").unwrap(), "");
                assert_eq!(
                    producers.get("processed-by").unwrap().get("baz").unwrap(),
                    "1.0"
                );
            }
            _ => panic!("metadata should be module"),
        }
    }

    #[test]
    fn producers_add_another_field() {
        let wat = "(module)";
        let module = wat::parse_str(wat).unwrap();
        let mut producers = Producers::empty();
        producers.add("language", "bar", "");
        producers.add("processed-by", "baz", "1.0");
        let module = producers.add_to_wasm(&module).unwrap();

        let mut producers = Producers::empty();
        producers.add("language", "waaat", "");
        let module = producers.add_to_wasm(&module).unwrap();

        let metadata = Metadata::from_binary(&module).unwrap();
        match metadata {
            Metadata::Module {
                name, producers, ..
            } => {
                assert_eq!(name, None);
                let producers = producers.expect("some producers");
                assert_eq!(producers.get("language").unwrap().get("bar").unwrap(), "");
                assert_eq!(producers.get("language").unwrap().get("waaat").unwrap(), "");
                assert_eq!(
                    producers.get("processed-by").unwrap().get("baz").unwrap(),
                    "1.0"
                );
            }
            _ => panic!("metadata should be module"),
        }
    }

    #[test]
    fn producers_overwrite_field() {
        let wat = "(module)";
        let module = wat::parse_str(wat).unwrap();
        let mut producers = Producers::empty();
        producers.add("processed-by", "baz", "1.0");
        let module = producers.add_to_wasm(&module).unwrap();

        let mut producers = Producers::empty();
        producers.add("processed-by", "baz", "420");
        let module = producers.add_to_wasm(&module).unwrap();

        let metadata = Metadata::from_binary(&module).unwrap();
        match metadata {
            Metadata::Module { producers, .. } => {
                let producers = producers.expect("some producers");
                assert_eq!(
                    producers.get("processed-by").unwrap().get("baz").unwrap(),
                    "420"
                );
            }
            _ => panic!("metadata should be module"),
        }
    }
}
