use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

pub use crate::resolve::Names;

/// A `*.wat` file parser, or a parser for one parenthesized module.
///
/// This is the top-level type which you'll frequently parse when working with
/// this crate. A `*.wat` file is either one `module` s-expression or a sequence
/// of s-expressions that are module fields.
pub struct Wat<'a> {
    #[allow(missing_docs)]
    pub module: Module<'a>,
}

impl<'a> Parse<'a> for Wat<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let module = if !parser.peek2::<kw::module>() {
            let mut fields = Vec::new();
            // must have at least one field
            fields.push(parser.parens(ModuleField::parse)?);
            while !parser.is_empty() {
                fields.push(parser.parens(ModuleField::parse)?);
            }
            Module {
                span: ast::Span { offset: 0 },
                name: None,
                kind: ModuleKind::Text(fields),
            }
        } else {
            parser.parens(|parser| parser.parse())?
        };
        Ok(Wat { module })
    }
}

/// A parsed WebAssembly module.
pub struct Module<'a> {
    /// Where this `module` was defined
    pub span: ast::Span,
    /// An optional name to refer to this module by.
    pub name: Option<ast::Id<'a>>,
    /// What kind of module this was parsed as.
    pub kind: ModuleKind<'a>,
}

/// The different kinds of ways to define a module.
pub enum ModuleKind<'a> {
    /// A module defined in the textual s-expression format.
    Text(Vec<ModuleField<'a>>),
    /// A module that had its raw binary bytes defined via the `binary`
    /// directive.
    Binary(Vec<&'a [u8]>),
}

impl<'a> Module<'a> {
    /// Performs a name resolution pass on this [`Module`], resolving all
    /// symbolic names to indices.
    ///
    /// The WAT format contains a number of shorthands to make it easier to
    /// write, such as inline exports, inline imports, inline type definitions,
    /// etc. Additionally it allows using symbolic names such as `$foo` instead
    /// of using indices. This module will postprocess an AST to remove all of
    /// this syntactic sugar, preparing the AST for binary emission.  This is
    /// where expansion and name resolution happens.
    ///
    /// This function will mutate the AST of this [`Module`] and replace all
    /// [`Index`] arguments with `Index::Num`. This will also expand inline
    /// exports/imports listed on fields and handle various other shorthands of
    /// the text format.
    ///
    /// If successful the AST was modified to be ready for binary encoding. A
    /// [`Names`] structure is also returned so if you'd like to do your own
    /// name lookups on the result you can do so as well.
    ///
    /// # Errors
    ///
    /// If an error happens during resolution, such a name resolution error or
    /// items are found in the wrong order, then an error is returned.
    pub fn resolve(&mut self) -> std::result::Result<Names<'a>, crate::Error> {
        crate::resolve::resolve(self)
    }

    /// Encodes this [`Module`] to its binary form.
    ///
    /// This function will take the textual representation in [`Module`] and
    /// perform all steps necessary to convert it to a binary WebAssembly
    /// module, suitable for writing to a `*.wasm` file. This function may
    /// internally modify the [`Module`], for example:
    ///
    /// * Name resolution is performed to ensure that `Index::Id` isn't present
    ///   anywhere in the AST.
    ///
    /// * Inline shorthands such as imports/exports/types are all expanded to be
    ///   dedicated fields of the module.
    ///
    /// * Module fields may be shuffled around to preserve index ordering from
    ///   expansions.
    ///
    /// After all of this expansion has happened the module will be converted to
    /// its binary form and returned as a `Vec<u8>`. This is then suitable to
    /// hand off to other wasm runtimes and such.
    ///
    /// # Errors
    ///
    /// This function can return an error for name resolution errors and other
    /// expansion-related errors.
    pub fn encode(&mut self) -> std::result::Result<Vec<u8>, crate::Error> {
        self.resolve()?;
        Ok(crate::binary::encode(self))
    }
}

impl<'a> Parse<'a> for Module<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::module>()?.0;
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
        Ok(Module { span, name, kind })
    }
}

/// A listing of all possible fields that can make up a WebAssembly module.
#[allow(missing_docs)]
#[derive(Debug)]
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
