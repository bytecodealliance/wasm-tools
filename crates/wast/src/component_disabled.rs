//! Disabled support for the component model.

use crate::parser::{Parse, Parser, Result};
use crate::token::{Id, Span};

/// Empty definition of a component that cannot be created.
#[derive(Debug)]
pub struct Component<'a> {
    /// Where this `component` was defined
    pub span: Span,
    /// An optional identifier this component is known by
    pub id: Option<Id<'a>>,
}

impl<'a> Parse<'a> for Component<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Err(parser.error("support for parsing components disabled at compile time"))
    }
}
