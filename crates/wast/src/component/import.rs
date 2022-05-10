use crate::component::*;
use crate::kw;
use crate::parser::{Parse, Parser, Result};
use crate::token::Span;

/// An `import` statement and entry in a WebAssembly component.
#[derive(Debug, Clone)]
pub struct ComponentImport<'a> {
    /// Where this `import` was defined
    pub span: Span,
    /// The name of the item to import.
    pub name: &'a str,
    /// The type of the import.
    pub type_: ComponentTypeUse<'a, DefType<'a>>,
}

impl<'a> Parse<'a> for ComponentImport<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::import>()?.0;
        let name = parser.parse()?;
        let type_ = parser.parse()?;
        Ok(ComponentImport { span, name, type_ })
    }
}
