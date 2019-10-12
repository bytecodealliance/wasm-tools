use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

#[derive(PartialEq, Debug)]
pub struct File<'a> {
    pub module: Module<'a>,
}

impl<'a> Parse<'a> for File<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek2::<kw::module>() {
            parser.parens(Module::parse).map(|module| File { module })
        } else {
            let mut fields = Vec::new();
            while !parser.is_empty() {
                fields.push(parser.parens(ModuleField::parse)?);
            }
            Ok(File {
                module: Module { name: None, fields },
            })
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Module<'a> {
    pub name: Option<ast::Id<'a>>,
    pub fields: Vec<ModuleField<'a>>,
}

impl<'a> Parse<'a> for Module<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::module>()?;
        let name = parser.parse()?;
        let mut fields = Vec::new();
        while !parser.is_empty() {
            fields.push(parser.parens(ModuleField::parse)?);
        }
        Ok(Module { name, fields })
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
