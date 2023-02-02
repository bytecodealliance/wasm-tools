use anyhow::Result;
use indexmap::{map::Entry, IndexMap};
use std::fmt;
use wasmparser::{
    ComponentNameSectionReader, NameSectionReader, Parser, Payload::*, ProducersSectionReader,
};

/// A representation of a WebAssembly producers section.
///
/// Spec: https://github.com/WebAssembly/tool-conventions/blob/main/ProducersSection.md
#[derive(Debug)]
pub struct Producers(IndexMap<String, IndexMap<String, String>>);

impl Producers {
    /// Creates an empty producers section
    pub fn empty() -> Self {
        Producers(IndexMap::new())
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
    /// Add the fields specified by [`AddMetadata`]
    pub fn add_meta(&mut self, add: &AddMetadata) {
        for lang in add.language.iter() {
            self.add("language", &lang, "");
        }
        for (name, version) in add.processed_by.iter() {
            self.add("processed-by", &name, &version);
        }
        for (name, version) in add.sdk.iter() {
            self.add("sdk", &name, &version);
        }
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
        let mut parser = Parser::new(0).parse_all(&input);

        enum Output {
            Component(wasm_encoder::Component),
            Module(wasm_encoder::Module),
        }
        impl Output {
            fn section(
                &mut self,
                section: &(impl wasm_encoder::Section + wasm_encoder::ComponentSection),
            ) {
                match self {
                    Output::Component(c) => {
                        c.section(section);
                    }
                    Output::Module(m) => {
                        m.section(section);
                    }
                }
            }
            fn finish(self) -> Vec<u8> {
                match self {
                    Output::Component(c) => c.finish(),
                    Output::Module(m) => m.finish(),
                }
            }
        }

        let mut output = match parser
            .next()
            .ok_or_else(|| anyhow::anyhow!("at least a version tag on binary"))??
        {
            Version {
                encoding: wasmparser::Encoding::Component,
                ..
            } => Output::Component(wasm_encoder::Component::new()),
            Version {
                encoding: wasmparser::Encoding::Module,
                ..
            } => Output::Module(wasm_encoder::Module::new()),
            _ => {
                panic!("first item from parser must be a Version tag")
            }
        };

        let mut producers_found = false;
        let mut names_found = false;
        let mut depth = 0;
        for payload in parser {
            let payload = payload?;

            // Track nesting depth, so that we don't mess with inner producer sections:
            match payload {
                ModuleSection { .. } | ComponentSection { .. } => depth += 1,
                End { .. } => depth -= 1,
                _ => {}
            }

            // Process the wasm sections:
            match payload {
                // Only rewrite the outermost producers section:
                CustomSection(c) if c.name() == "producers" && depth == 0 => {
                    producers_found = true;
                    let section = ProducersSectionReader::new(c.data(), c.data_offset())?;
                    let mut producers = Producers::from_reader(section)?;
                    // Add to the section according to the command line flags:
                    producers.add_meta(&self);
                    // Encode into output:
                    output.section(&producers.section());
                }

                CustomSection(c) if c.name() == "name" && depth == 0 => {
                    names_found = true;
                    let section = NameSectionReader::new(c.data(), c.data_offset());
                    let mut names = ModuleNames::from_reader(section)?;
                    names.add_meta(&self);

                    output.section(&names.section()?.as_custom());
                }

                CustomSection(c) if c.name() == "component-name" && depth == 0 => {
                    names_found = true;
                    let section = ComponentNameSectionReader::new(c.data(), c.data_offset());
                    let mut names = ComponentNames::from_reader(section)?;
                    names.add_meta(&self);
                    output.section(&names.section()?.as_custom());
                }

                // All other sections get passed through unmodified:
                _ => {
                    if let Some((id, range)) = payload.as_section() {
                        output.section(&wasm_encoder::RawSection {
                            id,
                            data: &input[range],
                        });
                    }
                }
            }
        }
        if !names_found && self.name.is_some() {
            match &mut output {
                Output::Component(c) => {
                    let mut names = ComponentNames::empty();
                    names.add_meta(&self);
                    c.section(&names.section()?);
                }
                Output::Module(m) => {
                    let mut names = ModuleNames::empty();
                    names.add_meta(&self);
                    m.section(&names.section()?);
                }
            }
        }
        if !producers_found
            && (!self.language.is_empty() || !self.processed_by.is_empty() || !self.sdk.is_empty())
        {
            let mut producers = Producers::empty();
            // Add to the section according to the command line flags:
            producers.add_meta(&self);
            // Encode into output:
            output.section(&producers.section());
        }
        Ok(output.finish())
    }
}

/// A tree of the metadata found in a WebAssembly binary.
#[derive(Debug)]
pub enum Metadata {
    /// Metadata found inside a WebAssembly component.
    Component {
        /// The component name, if any. Found in the component-name section.
        name: Option<String>,
        /// The component's producers section, if any.
        producers: Option<Producers>,
        /// All child modules and components inside the component.
        children: Vec<Box<Metadata>>,
    },
    /// Metadata found inside a WebAssembly module.
    Module {
        /// The module name, if any. Found in the name section.
        name: Option<String>,
        /// The module's producers section, if any.
        producers: Option<Producers>,
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
                            wasmparser::Encoding::Module => metadata.push(Metadata::empty_module()),
                            wasmparser::Encoding::Component => {
                                metadata.push(Metadata::empty_component())
                            }
                        }
                    }
                }
                ModuleSection { .. } => metadata.push(Metadata::empty_module()),
                ComponentSection { .. } => metadata.push(Metadata::empty_component()),
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

    fn empty_component() -> Self {
        Metadata::Component {
            name: None,
            producers: None,
            children: Vec::new(),
        }
    }

    fn empty_module() -> Self {
        Metadata::Module {
            name: None,
            producers: None,
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
            Metadata::Module { name, producers } => {
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
    fn add_meta(&mut self, add: &AddMetadata) {
        self.module_name = add.name.clone();
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
    fn add_meta(&mut self, add: &AddMetadata) {
        self.component_name = add.name.clone();
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
