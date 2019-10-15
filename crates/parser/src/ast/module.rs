use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

pub struct Wat<'a> {
    pub module: Module<'a>,
}

impl<'a> Parse<'a> for Wat<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let module = if !parser.peek2::<kw::module>() {
            let mut fields = Vec::new();
            while !parser.is_empty() {
                fields.push(parser.parens(ModuleField::parse)?);
            }
            Module {
                name: None,
                kind: ModuleKind::Text(fields),
            }
        } else {
            parser.parens(|parser| parser.parse())?
        };
        Ok(Wat { module })
    }
}

pub struct Module<'a> {
    pub name: Option<ast::Id<'a>>,
    pub kind: ModuleKind<'a>,
}

pub enum ModuleKind<'a> {
    /// A module defined in the textual s-expression format.
    Text(Vec<ModuleField<'a>>),
    /// A module that had its raw binary bytes defined via the `binary`
    /// directive.
    Binary(Vec<&'a [u8]>),
}

impl<'a> Parse<'a> for Module<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::module>()?;
        let name = parser.parse()?;

        let kind = if parser.peek::<kw::binary>() {
            parser.parse::<kw::binary>()?;
            let mut data = Vec::new();
            while !parser.is_empty() {
                data.push(parser.parse()?);
            }
            ModuleKind::Binary(data)
        } else {
            let mut fields = Vec::new();
            while !parser.is_empty() {
                fields.push(parser.parens(ModuleField::parse)?);
            }
            ModuleKind::Text(fields)
        };
        Ok(Module { name, kind })
    }
}

#[derive(PartialEq, Debug)]
pub enum ModuleField<'a> {
    Type(ast::Type<'a>),
    Import(ast::Import<'a>),
    Func(ast::Func<'a>),
    Table(ast::Table<'a>),
    Memory(ast::Memory<'a>),
    Global(ast::Global<'a>),
    Export(ast::Export<'a>),
    Start(ast::Index<'a>),
    Elem(ast::Elem<'a>),
    Data(ast::Data<'a>),
}

impl<'a> Parse<'a> for ModuleField<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<kw::r#type>() {
            return Ok(ModuleField::Type(parser.parse()?));
        }
        if parser.peek::<kw::import>() {
            return Ok(ModuleField::Import(parser.parse()?));
        }
        if parser.peek::<kw::func>() {
            return Ok(ModuleField::Func(parser.parse()?));
        }
        if parser.peek::<kw::table>() {
            return Ok(ModuleField::Table(parser.parse()?));
        }
        if parser.peek::<kw::memory>() {
            return Ok(ModuleField::Memory(parser.parse()?));
        }
        if parser.peek::<kw::global>() {
            return Ok(ModuleField::Global(parser.parse()?));
        }
        if parser.peek::<kw::export>() {
            return Ok(ModuleField::Export(parser.parse()?));
        }
        if parser.peek::<kw::start>() {
            parser.parse::<kw::start>()?;
            return Ok(ModuleField::Start(parser.parse()?));
        }
        if parser.peek::<kw::elem>() {
            return Ok(ModuleField::Elem(parser.parse()?));
        }
        if parser.peek::<kw::data>() {
            return Ok(ModuleField::Data(parser.parse()?));
        }
        Err(parser.error("expected valid module field"))
    }
}
