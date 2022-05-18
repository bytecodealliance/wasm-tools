use crate::{
    encode_section, encoders, ComponentExport, ComponentSection, ComponentSectionId, Encode,
};

/// Represents an argument to instantiating a WebAssembly module.
#[derive(Debug, Clone)]
pub enum ModuleArg {
    /// The argument is an instance.
    Instance(u32),
}

impl Encode for ModuleArg {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Self::Instance(index) => {
                sink.push(0x02);
                sink.extend(encoders::u32(*index));
            }
        }
    }
}

/// Represents an argument to instantiating a WebAssembly component.
#[derive(Debug, Clone)]
pub enum ComponentArg {
    /// The argument is a module.
    Module(u32),
    /// The argument is a component.
    Component(u32),
    /// The argument is an instance.
    Instance(u32),
    /// The argument is a function.
    Function(u32),
    /// The argument is a value.
    Value(u32),
    /// The argument is a type.
    Type(u32),
}

impl Encode for ComponentArg {
    fn encode(&self, sink: &mut Vec<u8>) {
        let (kind, index) = match self {
            Self::Module(index) => (0x00, *index),
            Self::Component(index) => (0x01, *index),
            Self::Instance(index) => (0x02, *index),
            Self::Function(index) => (0x03, *index),
            Self::Value(index) => (0x04, *index),
            Self::Type(index) => (0x05, *index),
        };

        sink.push(kind);
        sink.extend(encoders::u32(index));
    }
}

/// An encoder for the instance section of WebAssembly components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, InstanceSection, ModuleArg, Export};
///
/// let mut instances = InstanceSection::new();
/// instances.export_core_items([("foo", Export::Function(0))]);
/// instances.instantiate_module(1, [("foo", ModuleArg::Instance(0))]);
///
/// let mut component = Component::new();
/// component.section(&instances);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct InstanceSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl InstanceSection {
    /// Create a new instance section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of instances in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define an instance by instantiating a module.
    pub fn instantiate_module<'a, Args, Arg>(&mut self, module_index: u32, args: Args) -> &mut Self
    where
        Args: IntoIterator<Item = (&'a str, Arg)>,
        Args::IntoIter: ExactSizeIterator,
        Arg: Into<ModuleArg>,
    {
        let args = args.into_iter();
        self.bytes.push(0x00);
        self.bytes.push(0x00);
        self.bytes.extend(encoders::u32(module_index));
        self.bytes
            .extend(encoders::u32(u32::try_from(args.len()).unwrap()));
        for (name, arg) in args {
            self.bytes.extend(encoders::str(name));
            arg.into().encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Define an instance by exporting core WebAssembly items.
    pub fn export_core_items<'a, Exports, Export>(&mut self, exports: Exports) -> &mut Self
    where
        Exports: IntoIterator<Item = (&'a str, Export)>,
        Exports::IntoIter: ExactSizeIterator,
        Export: Into<crate::Export>,
    {
        let exports = exports.into_iter();
        self.bytes.push(0x02);
        self.bytes
            .extend(encoders::u32(u32::try_from(exports.len()).unwrap()));
        for (name, export) in exports {
            self.bytes.extend(encoders::str(name));
            export.into().encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Define an instance by instantiating a component.
    pub fn instantiate_component<'a, Args, Arg>(
        &mut self,
        component_index: u32,
        args: Args,
    ) -> &mut Self
    where
        Args: IntoIterator<Item = (&'a str, Arg)>,
        Args::IntoIter: ExactSizeIterator,
        Arg: Into<ComponentArg>,
    {
        let args = args.into_iter();
        self.bytes.push(0x00);
        self.bytes.push(0x01);
        self.bytes.extend(encoders::u32(component_index));
        self.bytes
            .extend(encoders::u32(u32::try_from(args.len()).unwrap()));
        for (name, arg) in args {
            self.bytes.extend(encoders::str(name));
            arg.into().encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }

    /// Define an instance by exporting items.
    pub fn export_items<'a, Exports, Export>(&mut self, exports: Exports) -> &mut Self
    where
        Exports: IntoIterator<Item = (&'a str, Export)>,
        Exports::IntoIter: ExactSizeIterator,
        Export: Into<ComponentExport>,
    {
        let exports = exports.into_iter();
        self.bytes.push(0x01);
        self.bytes
            .extend(encoders::u32(u32::try_from(exports.len()).unwrap()));
        for (name, export) in exports {
            self.bytes.extend(encoders::str(name));
            export.into().encode(&mut self.bytes);
        }
        self.num_added += 1;
        self
    }
}

impl Encode for InstanceSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(
            sink,
            ComponentSectionId::Instance,
            self.num_added,
            &self.bytes,
        );
    }
}

impl ComponentSection for InstanceSection {}
