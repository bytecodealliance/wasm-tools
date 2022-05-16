use crate::core::{Module, ModuleField, ModuleKind};
use crate::kw;
use crate::parser::{Parse, Parser, Result};
use crate::token::Span;

/// A `*.wat` file parser, or a parser for one parenthesized module.
///
/// This is the top-level type which you'll frequently parse when working with
/// this crate. A `*.wat` file is either one `module` s-expression or a sequence
/// of s-expressions that are module fields.
#[derive(Debug)]
pub struct Wat<'a> {
    #[allow(missing_docs)]
    pub module: Module<'a>,
}

impl Wat<'_> {
    /// Encodes this `Wat` to binary form.
    pub fn encode(&mut self) -> std::result::Result<Vec<u8>, crate::Error> {
        self.module.encode()
    }
}

impl<'a> Parse<'a> for Wat<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if !parser.has_meaningful_tokens() {
            return Err(parser.error("expected at least one module field"));
        }
        let _r = parser.register_annotation("custom");
        let module = if !parser.peek2::<kw::module>() {
            let fields = ModuleField::parse_remaining(parser)?;
            Module {
                span: Span { offset: 0 },
                id: None,
                name: None,
                kind: ModuleKind::Text(fields),
            }
        } else {
            parser.parens(|parser| parser.parse())?
        };
        module.validate(parser)?;
        Ok(Wat { module })
    }
}
