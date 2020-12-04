use super::*;

/// An encoder for the alias section.
pub struct AliasSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl AliasSection {
    /// Construct a new alias section encoder.
    pub fn new() -> AliasSection {
        AliasSection {
            bytes: vec![],
            num_added: 0,
        }
    }

    /// Define an alias that references the export of a defined instance.
    pub fn instance_export(&mut self, instance: u32, export: crate::Export) -> &mut Self {
        self.bytes.push(0x00);
        self.bytes.extend(encoders::u32(instance));
        export.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Define an alias that references a parent's type.
    pub fn parent_type(&mut self, ty: u32) -> &mut Self {
        self.bytes.push(0x01);
        self.bytes.push(0x07);
        self.bytes.extend(encoders::u32(ty));
        self.num_added += 1;
        self
    }

    /// Define an alias that references a parent's module.
    pub fn parent_module(&mut self, module: u32) -> &mut Self {
        self.bytes.push(0x01);
        self.bytes.push(0x05);
        self.bytes.extend(encoders::u32(module));
        self.num_added += 1;
        self
    }
}

impl Section for AliasSection {
    fn id(&self) -> u8 {
        SectionId::Alias.into()
    }

    fn encode<S>(&self, sink: &mut S)
    where
        S: Extend<u8>,
    {
        let num_added = encoders::u32(self.num_added);
        let n = num_added.len();
        sink.extend(
            encoders::u32(u32::try_from(n + self.bytes.len()).unwrap())
                .chain(num_added)
                .chain(self.bytes.iter().copied()),
        );
    }
}
