use crate::component::*;
use crate::kw;
use crate::parser::{Cursor, Parse, Parser, Peek, Result};
use crate::token::{Id, Index, NameAnnotation, Span};

/// A definition of a type.
///
/// typeexpr          ::= <deftype>
///                     | <intertype>
#[derive(Debug)]
pub enum ComponentTypeDef<'a> {
    /// The type of an entity.
    DefType(DefType<'a>),
    /// The type of a value.
    InterType(InterType<'a>),
}

/// The type of an exported item from an component or instance.
#[derive(Debug)]
pub struct ComponentExportType<'a> {
    /// Where this export was defined.
    pub span: Span,
    /// The name of this export.
    pub name: &'a str,
    /// The signature of the item that's exported.
    pub item: ItemSig<'a>,
}

impl<'a> Parse<'a> for ComponentExportType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::export>()?.0;
        let name = parser.parse()?;
        let item = parser.parens(|p| p.parse())?;
        Ok(ComponentExportType { span, name, item })
    }
}
/// A type declaration in a component.
///
/// type              ::= (type <id>? <typeexpr>)
#[derive(Debug)]
pub struct TypeField<'a> {
    /// Where this type was defined.
    pub span: Span,
    /// An optional identifer to refer to this `type` by as part of name
    /// resolution.
    pub id: Option<Id<'a>>,
    /// An optional name for this function stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// The type that we're declaring.
    pub def: ComponentTypeDef<'a>,
}

impl<'a> Parse<'a> for TypeField<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::r#type>()?.0;
        let id = parser.parse()?;
        let name = parser.parse()?;
        let def = if parser.peek::<InterType>() {
            ComponentTypeDef::InterType(parser.parse()?)
        } else {
            ComponentTypeDef::DefType(parser.parens(|p| p.parse())?)
        };
        Ok(TypeField {
            span,
            id,
            name,
            def,
        })
    }
}
/// A reference to a type defined in this component.
///
/// This is the same as `TypeUse`, but accepts `$T` as shorthand for
/// `(type $T)`.
#[derive(Debug, Clone)]
pub enum ComponentTypeUse<'a, T> {
    /// The type that we're referencing.
    Ref(ItemRef<'a, kw::r#type>),
    /// The inline type.
    Inline(T),
}

impl<'a, T> ComponentTypeUse<'a, T> {
    /// Constructs a new instance of `ComponentTypeUse` without an inline definition but
    /// with an index specified.
    pub fn new_with_index(idx: Index<'a>) -> ComponentTypeUse<'a, T> {
        ComponentTypeUse::Ref(ItemRef {
            idx,
            kind: kw::r#type::default(),
            export_names: Vec::new(),
        })
    }
}

impl<'a, T: Peek + Parse<'a>> Parse<'a> for ComponentTypeUse<'a, T> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<IndexOrRef<'a, kw::r#type>>() {
            Ok(ComponentTypeUse::Ref(
                parser.parse::<IndexOrRef<kw::r#type>>()?.0,
            ))
        } else if parser.peek::<Index>() {
            Ok(ComponentTypeUse::Ref(parser.parse()?))
        } else {
            Ok(ComponentTypeUse::Inline(parser.parse()?))
        }
    }
}

impl<'a, T: Peek + Parse<'a>> Peek for ComponentTypeUse<'a, T> {
    fn peek(cursor: Cursor<'_>) -> bool {
        kw::r#type::peek(cursor) || T::peek(cursor)
    }
    fn display() -> &'static str {
        T::display()
    }
}
