use crate::{
    encoders, AdapterModuleSectionId, ComponentSectionId, Export, Section, SectionEncodingFormat,
};

const INSTANCE_KIND_INSTANTIATION: u8 = 0x00;
const INSTANCE_KIND_EXPORTS: u8 = 0x01;

/// An encoder for the instance section.
///
/// Instance sections are only supported for adapter modules and components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, InstanceSection, SectionEncodingFormat, Export};
///
/// // This assumes there is a module with index 0, a function with index 0,
/// // a module with index 2, and a global with index 0.
/// let mut instances = InstanceSection::new(SectionEncodingFormat::Component);
/// instances.instantiate(0, [
///     ("x", Export::Function(0)),
///     ("", Export::Module(2)),
///     ("foo", Export::Global(0)),
/// ]);
///
/// let mut component = Component::new();
/// component.section(&instances);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug)]
pub struct InstanceSection {
    bytes: Vec<u8>,
    num_added: u32,
    format: SectionEncodingFormat,
}

impl InstanceSection {
    /// Create a new instance section encoder.
    pub fn new(format: SectionEncodingFormat) -> Self {
        if format == SectionEncodingFormat::Module {
            panic!("instance sections are not supported for module encoding");
        }

        Self {
            bytes: Vec::new(),
            num_added: 0,
            format,
        }
    }

    /// The number of instances in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define an instantiation of the given module with the given
    /// arguments to the instantiation.
    pub fn instantiate<'a, I>(&mut self, module: u32, args: I) -> &mut Self
    where
        I: IntoIterator<Item = (&'a str, Export)>,
        I::IntoIter: ExactSizeIterator,
    {
        let args = args.into_iter();

        self.bytes.push(INSTANCE_KIND_INSTANTIATION);
        self.bytes.extend(encoders::u32(module));

        self.bytes
            .extend(encoders::u32(u32::try_from(args.len()).unwrap()));
        for (name, export) in args {
            self.bytes.extend(encoders::str(name));
            export.encode(self.format, &mut self.bytes);
        }

        self
    }

    /// Define an instance by exporting the given exports.
    pub fn exports<'a, E>(&mut self, exports: E) -> &mut Self
    where
        E: IntoIterator<Item = (&'a str, Export)>,
        E::IntoIter: ExactSizeIterator,
    {
        let exports = exports.into_iter();

        self.bytes.push(INSTANCE_KIND_EXPORTS);

        self.bytes
            .extend(encoders::u32(u32::try_from(exports.len()).unwrap()));
        for (name, export) in exports {
            self.bytes.extend(encoders::str(name));
            export.encode(self.format, &mut self.bytes);
        }

        self
    }

    fn encode(&self, format: SectionEncodingFormat, sink: &mut impl Extend<u8>) {
        assert_eq!(self.format, format, "instance section format mismatch");
        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}

impl Section<AdapterModuleSectionId> for InstanceSection {
    fn id(&self) -> AdapterModuleSectionId {
        AdapterModuleSectionId::Instance
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::AdapterModule, sink);
    }
}

impl Section<ComponentSectionId> for InstanceSection {
    fn id(&self) -> ComponentSectionId {
        ComponentSectionId::Instance
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        self.encode(SectionEncodingFormat::Component, sink);
    }
}
