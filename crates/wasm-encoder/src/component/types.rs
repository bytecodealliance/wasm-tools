use crate::{
    encode_functype, encode_section, encoders, ComponentSection, ComponentSectionId, Encode,
    EntityType, ValType,
};

/// Represents a module type.
#[derive(Debug, Clone, Default)]
pub struct ModuleType {
    bytes: Vec<u8>,
    num_added: u32,
    types_added: u32,
}

impl ModuleType {
    /// Creates a new module type.
    pub fn new() -> Self {
        Self::default()
    }

    /// Define a function in this module type.
    pub fn function<P, R>(&mut self, params: P, results: R) -> &mut Self
    where
        P: IntoIterator<Item = ValType>,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = ValType>,
        R::IntoIter: ExactSizeIterator,
    {
        self.bytes.push(0x01);
        encode_functype(&mut self.bytes, params, results);
        self.num_added += 1;
        self.types_added += 1;
        self
    }

    /// Defines an import in this module type.
    pub fn import(&mut self, module: &str, name: &str, ty: EntityType) -> &mut Self {
        self.bytes.push(0x02);
        self.bytes.extend(encoders::str(module));
        self.bytes.extend(encoders::str(name));
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Defines an export in this module type.
    pub fn export(&mut self, name: &str, ty: EntityType) -> &mut Self {
        self.bytes.push(0x07);
        self.bytes.extend(encoders::str(name));
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }

    /// Gets the number of types that have been added to this module type.
    pub fn type_count(&self) -> u32 {
        self.types_added
    }
}

impl Encode for ModuleType {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.extend(encoders::u32(self.num_added));
        sink.extend(&self.bytes);
    }
}

/// Represents a component type.
#[derive(Debug, Clone, Default)]
pub struct ComponentType {
    bytes: Vec<u8>,
    num_added: u32,
    types_added: u32,
}

impl ComponentType {
    /// Creates a new component type.
    pub fn new() -> Self {
        Self::default()
    }

    /// Define a type in this component type.
    ///
    /// The returned encoder must be finished before adding another definition.
    #[must_use = "the encoder must be used to encode the type"]
    pub fn ty(&mut self) -> TypeEncoder {
        self.bytes.push(0x01);
        self.num_added += 1;
        self.types_added += 1;
        TypeEncoder(&mut self.bytes)
    }

    /// Defines an import in this component type.
    ///
    /// The type is expected to be an index to a previously defined or aliased type.
    pub fn import(&mut self, name: &str, ty: u32) -> &mut Self {
        self.bytes.push(0x02);
        self.bytes.extend(encoders::str(name));
        self.bytes.extend(encoders::u32(ty));
        self.num_added += 1;
        self
    }

    /// Defines an export in this component type.
    ///
    /// The type is expected to be an index to a previously defined or aliased type.
    pub fn export(&mut self, name: &str, ty: u32) -> &mut Self {
        self.bytes.push(0x07);
        self.bytes.extend(encoders::str(name));
        self.bytes.extend(encoders::u32(ty));
        self.num_added += 1;
        self
    }

    /// Defines an alias to an outer type in this component type.
    pub fn alias_outer_type(&mut self, count: u32, index: u32) -> &mut Self {
        self.bytes.push(0x09);
        self.bytes.push(0x02);
        self.bytes.push(0x05);
        self.bytes.extend(encoders::u32(count));
        self.bytes.extend(encoders::u32(index));
        self.num_added += 1;
        self.types_added += 1;
        self
    }

    /// Gets the number of types that have been added or aliased in this component type.
    pub fn type_count(&self) -> u32 {
        self.types_added
    }
}

impl Encode for ComponentType {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.extend(encoders::u32(self.num_added));
        sink.extend(&self.bytes);
    }
}

/// Represents an instance type.
#[derive(Debug, Clone, Default)]
pub struct InstanceType {
    bytes: Vec<u8>,
    num_added: u32,
    types_added: u32,
}

impl InstanceType {
    /// Creates a new instance type.
    pub fn new() -> Self {
        Self::default()
    }

    /// Define a type in this instance type.
    ///
    /// The returned encoder must be finished before adding another definition.
    #[must_use = "the encoder must be used to encode the type"]
    pub fn ty(&mut self) -> TypeEncoder {
        self.bytes.push(0x01);
        self.num_added += 1;
        self.types_added += 1;
        TypeEncoder(&mut self.bytes)
    }

    /// Defines an export in this instance type.
    ///
    /// The type is expected to be an index to a previously defined or aliased type.
    pub fn export(&mut self, name: &str, ty: u32) -> &mut Self {
        self.bytes.push(0x07);
        self.bytes.extend(encoders::str(name));
        self.bytes.extend(encoders::u32(ty));
        self.num_added += 1;
        self
    }

    /// Defines an alias to an outer type in this instance type.
    pub fn alias_outer_type(&mut self, count: u32, index: u32) -> &mut Self {
        self.bytes.push(0x09);
        self.bytes.push(0x02);
        self.bytes.push(0x05);
        self.bytes.extend(encoders::u32(count));
        self.bytes.extend(encoders::u32(index));
        self.num_added += 1;
        self.types_added += 1;
        self
    }

    /// Gets the number of types that have been added or aliased in this instance type.
    pub fn type_count(&self) -> u32 {
        self.types_added
    }
}

impl Encode for InstanceType {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.extend(encoders::u32(self.num_added));
        sink.extend(&self.bytes);
    }
}

/// Used to encode types.
#[derive(Debug)]
pub struct TypeEncoder<'a>(&'a mut Vec<u8>);

impl<'a> TypeEncoder<'a> {
    /// Define a module type.
    pub fn module(self, ty: &ModuleType) {
        self.0.push(0x4f);
        ty.encode(self.0);
    }

    /// Define a component type.
    pub fn component(self, ty: &ComponentType) {
        self.0.push(0x4e);
        ty.encode(self.0);
    }

    /// Define an instance type.
    pub fn instance(self, ty: &InstanceType) {
        self.0.push(0x4d);
        ty.encode(self.0);
    }

    /// Define a function type.
    pub fn function<'b, P, T>(self, params: P, result: impl Into<InterfaceTypeRef>)
    where
        P: IntoIterator<Item = (Option<&'b str>, T)>,
        P::IntoIter: ExactSizeIterator,
        T: Into<InterfaceTypeRef>,
    {
        let params = params.into_iter();
        self.0.push(0x4c);

        self.0
            .extend(encoders::u32(u32::try_from(params.len()).unwrap()));
        for (name, ty) in params {
            match name {
                Some(name) => {
                    self.0.push(0x01);
                    self.0.extend(encoders::str(name));
                }
                None => self.0.push(0x00),
            }
            ty.into().encode(self.0);
        }

        result.into().encode(self.0);
    }

    /// Define a value type.
    pub fn value(self, ty: impl Into<InterfaceTypeRef>) {
        self.0.push(0x4b);
        ty.into().encode(self.0);
    }

    /// Define an interface type.
    ///
    /// The returned encoder must be finished before adding another type.
    #[must_use = "the encoder must be used to encode the type"]
    pub fn interface_type(self) -> InterfaceTypeEncoder<'a> {
        InterfaceTypeEncoder(self.0)
    }
}

/// Represents a primitive interface type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveInterfaceType {
    /// The type is the unit type.
    Unit,
    /// The type is a boolean.
    Bool,
    /// The type is a signed 8-bit integer.
    S8,
    /// The type is an unsigned 8-bit integer.
    U8,
    /// The type is a signed 16-bit integer.
    S16,
    /// The type is an unsigned 16-bit integer.
    U16,
    /// The type is a signed 32-bit integer.
    S32,
    /// The type is an unsigned 32-bit integer.
    U32,
    /// The type is a signed 64-bit integer.
    S64,
    /// The type is an unsigned 64-bit integer.
    U64,
    /// The type is a 32-bit floating point number.
    Float32,
    /// The type is a 64-bit floating point number.
    Float64,
    /// The type is a Unicode character.
    Char,
    /// The type is a string.
    String,
}

impl Encode for PrimitiveInterfaceType {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(match self {
            Self::Unit => 0x7f,
            Self::Bool => 0x7e,
            Self::S8 => 0x7d,
            Self::U8 => 0x7c,
            Self::S16 => 0x7b,
            Self::U16 => 0x7a,
            Self::S32 => 0x79,
            Self::U32 => 0x78,
            Self::S64 => 0x77,
            Self::U64 => 0x76,
            Self::Float32 => 0x75,
            Self::Float64 => 0x74,
            Self::Char => 0x73,
            Self::String => 0x72,
        });
    }
}

/// Represents a reference to an interface type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InterfaceTypeRef {
    /// The reference is to a primitive type.
    Primitive(PrimitiveInterfaceType),
    /// The reference is to a type index.
    ///
    /// The type index must be to an interface type.
    Type(u32),
}

impl Encode for InterfaceTypeRef {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Self::Primitive(ty) => ty.encode(sink),
            Self::Type(index) => sink.extend(encoders::s33(*index as i64)),
        }
    }
}

impl From<PrimitiveInterfaceType> for InterfaceTypeRef {
    fn from(ty: PrimitiveInterfaceType) -> Self {
        Self::Primitive(ty)
    }
}

/// Used for encoding interface types.
#[derive(Debug)]
pub struct InterfaceTypeEncoder<'a>(&'a mut Vec<u8>);

impl InterfaceTypeEncoder<'_> {
    /// Define a primitive interface type.
    pub fn primitive(self, ty: PrimitiveInterfaceType) {
        ty.encode(self.0);
    }

    /// Define a record type.
    pub fn record<'a, F, T>(self, fields: F)
    where
        F: IntoIterator<Item = (&'a str, T)>,
        F::IntoIter: ExactSizeIterator,
        T: Into<InterfaceTypeRef>,
    {
        let fields = fields.into_iter();
        self.0.push(0x71);
        self.0
            .extend(encoders::u32(fields.len().try_into().unwrap()));
        for (name, ty) in fields {
            self.0.extend(encoders::str(name));
            ty.into().encode(self.0);
        }
    }

    /// Define a variant type.
    pub fn variant<'a, C, T>(self, cases: C)
    where
        C: IntoIterator<Item = (&'a str, T, Option<u32>)>,
        C::IntoIter: ExactSizeIterator,
        T: Into<InterfaceTypeRef>,
    {
        let cases = cases.into_iter();
        self.0.push(0x70);
        self.0
            .extend(encoders::u32(cases.len().try_into().unwrap()));
        for (name, ty, default_to) in cases {
            self.0.extend(encoders::str(name));
            ty.into().encode(self.0);
            if let Some(default) = default_to {
                self.0.push(0x01);
                self.0.extend(encoders::u32(default));
            } else {
                self.0.push(0x00);
            }
        }
    }

    /// Define a list type.
    pub fn list(self, ty: impl Into<InterfaceTypeRef>) {
        self.0.push(0x6f);
        ty.into().encode(self.0);
    }

    /// Define a tuple type.
    pub fn tuple<I, T>(self, types: I)
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
        T: Into<InterfaceTypeRef>,
    {
        let types = types.into_iter();
        self.0.push(0x6E);
        self.0
            .extend(encoders::u32(types.len().try_into().unwrap()));
        for ty in types {
            ty.into().encode(self.0);
        }
    }

    /// Define a flags type.
    pub fn flags<'a, I>(self, names: I)
    where
        I: IntoIterator<Item = &'a str>,
        I::IntoIter: ExactSizeIterator,
    {
        let names = names.into_iter();
        self.0.push(0x6D);
        self.0
            .extend(encoders::u32(names.len().try_into().unwrap()));
        for name in names {
            self.0.extend(encoders::str(name));
        }
    }

    /// Define an enum type.
    pub fn enum_type<'a, I>(self, tags: I)
    where
        I: IntoIterator<Item = &'a str>,
        I::IntoIter: ExactSizeIterator,
    {
        let tags = tags.into_iter();
        self.0.push(0x6C);
        self.0.extend(encoders::u32(tags.len().try_into().unwrap()));
        for tag in tags {
            self.0.extend(encoders::str(tag));
        }
    }

    /// Define a union type.
    pub fn union<I, T>(self, types: I)
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
        T: Into<InterfaceTypeRef>,
    {
        let types = types.into_iter();
        self.0.push(0x6B);
        self.0
            .extend(encoders::u32(types.len().try_into().unwrap()));
        for ty in types {
            ty.into().encode(self.0);
        }
    }

    /// Define an option type.
    pub fn option(self, ty: impl Into<InterfaceTypeRef>) {
        self.0.push(0x6A);
        ty.into().encode(self.0);
    }

    /// Define an expected type.
    pub fn expected(self, ok: impl Into<InterfaceTypeRef>, error: impl Into<InterfaceTypeRef>) {
        self.0.push(0x69);
        ok.into().encode(self.0);
        error.into().encode(self.0);
    }
}

/// An encoder for the type section of WebAssembly components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, ComponentTypeSection, PrimitiveInterfaceType};
///
/// let mut types = ComponentTypeSection::new();
///
/// types.function(
///   [
///     (Some("a"), PrimitiveInterfaceType::String),
///     (Some("b"), PrimitiveInterfaceType::String)
///   ],
///   PrimitiveInterfaceType::String
/// );
///
/// let mut component = Component::new();
/// component.section(&types);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct ComponentTypeSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ComponentTypeSection {
    /// Create a new component type section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of types in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Encode a type into this section.
    ///
    /// The returned encoder must be finished before adding another type.
    #[must_use = "the encoder must be used to encode the type"]
    pub fn ty(&mut self) -> TypeEncoder<'_> {
        self.num_added += 1;
        TypeEncoder(&mut self.bytes)
    }

    /// Define a module type in this type section.
    pub fn module(&mut self, ty: &ModuleType) -> &mut Self {
        self.ty().module(ty);
        self
    }

    /// Define a component type in this type section.
    pub fn component(&mut self, ty: &ComponentType) -> &mut Self {
        self.ty().component(ty);
        self
    }

    /// Define an instance type in this type section.
    pub fn instance(&mut self, ty: &InstanceType) -> &mut Self {
        self.ty().instance(ty);
        self
    }

    /// Define a function type in this type section.
    pub fn function<'a, P, T>(
        &mut self,
        params: P,
        result: impl Into<InterfaceTypeRef>,
    ) -> &mut Self
    where
        P: IntoIterator<Item = (Option<&'a str>, T)>,
        P::IntoIter: ExactSizeIterator,
        T: Into<InterfaceTypeRef>,
    {
        self.ty().function(params, result);
        self
    }

    /// Define a value type in this type section.
    pub fn value(&mut self, ty: impl Into<InterfaceTypeRef>) -> &mut Self {
        self.ty().value(ty);
        self
    }

    /// Define an interface type in this type section.
    ///
    /// The returned encoder must be finished before adding another type.
    #[must_use = "the encoder must be used to encode the type"]
    pub fn interface_type(&mut self) -> InterfaceTypeEncoder<'_> {
        self.ty().interface_type()
    }
}

impl Encode for ComponentTypeSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, ComponentSectionId::Type, self.num_added, &self.bytes);
    }
}

impl ComponentSection for ComponentTypeSection {}
