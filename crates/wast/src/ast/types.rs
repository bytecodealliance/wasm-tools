use crate::ast::{self, kw};
use crate::parser::{Cursor, Parse, Parser, Peek, Result};

/// The value types for a wasm module.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum ValType<'a> {
    I32,
    I64,
    F32,
    F64,
    V128,
    I8,
    I16,
    Ref(RefType<'a>),
    Rtt(ast::Index<'a>),
}

impl<'a> Parse<'a> for ValType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::i32>() {
            parser.parse::<kw::i32>()?;
            Ok(ValType::I32)
        } else if l.peek::<kw::i64>() {
            parser.parse::<kw::i64>()?;
            Ok(ValType::I64)
        } else if l.peek::<kw::f32>() {
            parser.parse::<kw::f32>()?;
            Ok(ValType::F32)
        } else if l.peek::<kw::f64>() {
            parser.parse::<kw::f64>()?;
            Ok(ValType::F64)
        } else if l.peek::<kw::v128>() {
            parser.parse::<kw::v128>()?;
            Ok(ValType::V128)
        } else if l.peek::<kw::i8>() {
            parser.parse::<kw::i8>()?;
            Ok(ValType::I8)
        } else if l.peek::<kw::i16>() {
            parser.parse::<kw::i16>()?;
            Ok(ValType::I16)
        } else if l.peek::<kw::funcref>() {
            parser.parse::<kw::funcref>()?;
            Ok(ValType::Ref(RefType::Func))
        } else if l.peek::<kw::anyfunc>() {
            parser.parse::<kw::anyfunc>()?;
            Ok(ValType::Ref(RefType::Func))
        } else if l.peek::<kw::externref>() {
            parser.parse::<kw::externref>()?;
            Ok(ValType::Ref(RefType::Extern))
        } else if l.peek::<kw::anyref>() {
            // Parse `anyref` as an alias of `externref` until all tests are
            // ported to use the new name
            parser.parse::<kw::anyref>()?;
            Ok(ValType::Ref(RefType::Extern))
        } else if l.peek::<ast::LParen>() {
            parser.parens(|p| {
                let mut l = parser.lookahead1();
                if l.peek::<kw::r#ref>() {
                    p.parse::<kw::r#ref>()?;
                    Ok(ValType::Ref(p.parse()?))
                } else if l.peek::<kw::optref>() {
                    p.parse::<kw::optref>()?;
                    Ok(ValType::Ref(RefType::OptType(parser.parse()?)))
                } else if l.peek::<kw::rtt>() {
                    p.parse::<kw::rtt>()?;
                    Ok(ValType::Rtt(parser.parse()?))
                } else {
                    Err(l.error())
                }
            })
        } else if l.peek::<kw::exnref>() {
            parser.parse::<kw::exnref>()?;
            Ok(ValType::Ref(RefType::Exn))
        } else if l.peek::<kw::eqref>() {
            parser.parse::<kw::eqref>()?;
            Ok(ValType::Ref(RefType::Eq))
        } else if l.peek::<kw::i31ref>() {
            parser.parse::<kw::i31ref>()?;
            Ok(ValType::Ref(RefType::I31))
        } else {
            Err(l.error())
        }
    }
}

/// The reference value types for a wasm module.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum RefType<'a> {
    /// An untyped function reference: funcref. This is part of the reference
    /// types proposal.
    Func,
    /// A reference to any host value: externref. This was originally known as
    /// anyref when it was the supertype of all reference value types. This is
    /// part of the reference types proposal.
    Extern,
    /// A reference to an exception: exnref. This is part of the exception
    /// handling proposal.
    Exn,
    /// A reference that has an identity that can be compared: eqref. This is
    /// part of the GC proposal.
    Eq,
    /// An unboxed 31-bit integer: i31ref. This may be going away if there is no common
    /// supertype of all reference types. Part of the GC proposal.
    I31,
    /// A reference to a function, struct, or array: ref T. This is part of the
    /// GC proposal.
    Type(ast::Index<'a>),
    /// A nullable reference to a function, struct, or array: optref T. This is
    /// part of the GC proposal.
    OptType(ast::Index<'a>),
}

impl<'a> From<TableElemType> for RefType<'a> {
    fn from(elem: TableElemType) -> Self {
        match elem {
            TableElemType::Funcref => RefType::Func,
            TableElemType::Externref => RefType::Extern,
            TableElemType::Exnref => RefType::Exn,
        }
    }
}

impl<'a> Parse<'a> for RefType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::func>() {
            parser.parse::<kw::func>()?;
            Ok(RefType::Func)
        } else if l.peek::<kw::r#extern>() {
            parser.parse::<kw::r#extern>()?;
            Ok(RefType::Extern)
        } else if l.peek::<kw::exn>() {
            parser.parse::<kw::exn>()?;
            Ok(RefType::Exn)
        } else if l.peek::<kw::eq>() {
            parser.parse::<kw::eq>()?;
            Ok(RefType::Eq)
        } else if l.peek::<kw::i31>() {
            parser.parse::<kw::i31>()?;
            Ok(RefType::I31)
        } else if l.peek::<kw::opt>() {
            parser.parse::<kw::opt>()?;
            Ok(RefType::OptType(parser.parse()?))
        } else if l.peek::<ast::Index>() {
            Ok(RefType::Type(parser.parse()?))
        } else {
            Err(l.error())
        }
    }
}

/// Type for a `global` in a wasm module
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GlobalType<'a> {
    /// The element type of this `global`
    pub ty: ValType<'a>,
    /// Whether or not the global is mutable or not.
    pub mutable: bool,
}

impl<'a> Parse<'a> for GlobalType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek2::<kw::r#mut>() {
            parser.parens(|p| {
                p.parse::<kw::r#mut>()?;
                Ok(GlobalType {
                    ty: parser.parse()?,
                    mutable: true,
                })
            })
        } else {
            Ok(GlobalType {
                ty: parser.parse()?,
                mutable: false,
            })
        }
    }
}

/// List of different kinds of table types we can have.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TableElemType {
    /// An element for a table that is a list of functions.
    Funcref,
    /// An element for a table that is a list of `externref` values.
    Externref,
    /// An element for a table that is a list of `exnref` values.
    Exnref,
}

impl<'a> Parse<'a> for TableElemType {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        // legacy support for `anyfunc`
        if parser.peek::<kw::anyfunc>() {
            parser.parse::<kw::anyfunc>()?;
            return Ok(TableElemType::Funcref);
        }
        let mut l = parser.lookahead1();
        if l.peek::<kw::funcref>() {
            parser.parse::<kw::funcref>()?;
            Ok(TableElemType::Funcref)
        } else if l.peek::<kw::anyref>() {
            // Parse `anyref` as an alias of `externref` until all tests are
            // ported to use the new name
            parser.parse::<kw::anyref>()?;
            Ok(TableElemType::Externref)
        } else if l.peek::<kw::externref>() {
            parser.parse::<kw::externref>()?;
            Ok(TableElemType::Externref)
        } else if l.peek::<kw::exnref>() {
            parser.parse::<kw::exnref>()?;
            Ok(TableElemType::Exnref)
        } else {
            Err(l.error())
        }
    }
}

impl Peek for TableElemType {
    fn peek(cursor: Cursor<'_>) -> bool {
        kw::funcref::peek(cursor)
            || kw::anyref::peek(cursor)
            || kw::externref::peek(cursor)
            || /* legacy */ kw::anyfunc::peek(cursor)
            || kw::exnref::peek(cursor)
    }
    fn display() -> &'static str {
        "table element type"
    }
}

/// Min/max limits used for tables/memories.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Limits {
    /// The minimum number of units for this type.
    pub min: u32,
    /// An optional maximum number of units for this type.
    pub max: Option<u32>,
}

impl<'a> Parse<'a> for Limits {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let min = parser.parse()?;
        let max = if parser.peek::<u32>() {
            Some(parser.parse()?)
        } else {
            None
        };
        Ok(Limits { min, max })
    }
}

/// Configuration for a table of a wasm mdoule
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TableType {
    /// Limits on the element sizes of this table
    pub limits: Limits,
    /// The type of element stored in this table
    pub elem: TableElemType,
}

impl<'a> Parse<'a> for TableType {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(TableType {
            limits: parser.parse()?,
            elem: parser.parse()?,
        })
    }
}

/// Configuration for a memory of a wasm module
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct MemoryType {
    /// Limits on the page sizes of this memory
    pub limits: Limits,
    /// Whether or not this is a shared (atomic) memory type
    pub shared: bool,
}

impl<'a> Parse<'a> for MemoryType {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let limits: Limits = parser.parse()?;
        let shared = parser.parse::<Option<kw::shared>>()?.is_some();
        Ok(MemoryType { limits, shared })
    }
}

/// A function type with parameters and results.
#[derive(Clone, Debug, Default)]
pub struct FunctionType<'a> {
    /// The parameters of a function, optionally each having an identifier for
    /// name resolution and a name for the custom `name` section.
    pub params: Vec<(
        Option<ast::Id<'a>>,
        Option<ast::NameAnnotation<'a>>,
        ValType<'a>,
    )>,
    /// The results types of a function.
    pub results: Vec<ValType<'a>>,
}

impl<'a> FunctionType<'a> {
    fn finish_parse(&mut self, allow_names: bool, parser: Parser<'a>) -> Result<()> {
        while parser.peek2::<kw::param>() || parser.peek2::<kw::result>() {
            parser.parens(|p| {
                let mut l = p.lookahead1();
                if l.peek::<kw::param>() {
                    if self.results.len() > 0 {
                        return Err(p.error(
                            "result before parameter (or unexpected token): \
                             cannot list params after results",
                        ));
                    }
                    p.parse::<kw::param>()?;
                    if p.is_empty() {
                        return Ok(());
                    }
                    let (id, name) = if allow_names {
                        (p.parse::<Option<_>>()?, p.parse::<Option<_>>()?)
                    } else {
                        (None, None)
                    };
                    let parse_more = id.is_none() && name.is_none();
                    let ty = p.parse()?;
                    self.params.push((id, name, ty));
                    while parse_more && !p.is_empty() {
                        self.params.push((None, None, p.parse()?));
                    }
                } else if l.peek::<kw::result>() {
                    p.parse::<kw::result>()?;
                    while !p.is_empty() {
                        self.results.push(p.parse()?);
                    }
                } else {
                    return Err(l.error());
                }
                Ok(())
            })?;
        }
        Ok(())
    }
}

impl<'a> Parse<'a> for FunctionType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut ret = FunctionType {
            params: Vec::new(),
            results: Vec::new(),
        };
        ret.finish_parse(true, parser)?;
        Ok(ret)
    }
}

impl<'a> Peek for FunctionType<'a> {
    fn peek(cursor: Cursor<'_>) -> bool {
        if let Some(next) = cursor.lparen() {
            match next.keyword() {
                Some(("param", _)) | Some(("result", _)) => return true,
                _ => {}
            }
        }

        false
    }

    fn display() -> &'static str {
        "function type"
    }
}

/// A function type with parameters and results.
#[derive(Clone, Debug, Default)]
pub struct FunctionTypeNoNames<'a>(pub FunctionType<'a>);

impl<'a> Parse<'a> for FunctionTypeNoNames<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut ret = FunctionType {
            params: Vec::new(),
            results: Vec::new(),
        };
        ret.finish_parse(false, parser)?;
        Ok(FunctionTypeNoNames(ret))
    }
}

impl<'a> Peek for FunctionTypeNoNames<'a> {
    fn peek(cursor: Cursor<'_>) -> bool {
        FunctionType::peek(cursor)
    }

    fn display() -> &'static str {
        FunctionType::display()
    }
}

impl<'a> From<FunctionTypeNoNames<'a>> for FunctionType<'a> {
    fn from(ty: FunctionTypeNoNames<'a>) -> FunctionType<'a> {
        ty.0
    }
}

/// A struct type with fields.
#[derive(Clone, Debug)]
pub struct StructType<'a> {
    /// The fields of the struct
    pub fields: Vec<StructField<'a>>,
}

impl<'a> Parse<'a> for StructType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut ret = StructType { fields: Vec::new() };
        while !parser.is_empty() {
            let field = if parser.peek2::<kw::field>() {
                parser.parens(|parser| {
                    parser.parse::<kw::field>()?;
                    StructField::parse(parser, true)
                })
            } else {
                StructField::parse(parser, false)
            };
            ret.fields.push(field?);
        }
        Ok(ret)
    }
}

/// A field of a struct type.
#[derive(Clone, Debug)]
pub struct StructField<'a> {
    /// An optional identifier for name resolution.
    pub id: Option<ast::Id<'a>>,
    /// Whether this field may be mutated or not.
    pub mutable: bool,
    /// The value type stored in this field.
    pub ty: ValType<'a>,
}

impl<'a> StructField<'a> {
    fn parse(parser: Parser<'a>, with_id: bool) -> Result<Self> {
        let id = if with_id { parser.parse()? } else { None };
        let (ty, mutable) = if parser.peek2::<kw::r#mut>() {
            let ty = parser.parens(|parser| {
                parser.parse::<kw::r#mut>()?;
                parser.parse()
            })?;
            (ty, true)
        } else {
            (parser.parse::<ValType<'a>>()?, false)
        };
        Ok(StructField { id, mutable, ty })
    }
}

/// An array type with fields.
#[derive(Clone, Debug)]
pub struct ArrayType<'a> {
    /// Whether this field may be mutated or not.
    pub mutable: bool,
    /// The value type stored in this field.
    pub ty: ValType<'a>,
}

impl<'a> Parse<'a> for ArrayType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let (ty, mutable) = if parser.peek2::<kw::r#mut>() {
            let ty = parser.parens(|parser| {
                parser.parse::<kw::r#mut>()?;
                parser.parse()
            })?;
            (ty, true)
        } else {
            (parser.parse::<ValType<'a>>()?, false)
        };
        Ok(ArrayType { mutable, ty })
    }
}

/// A type for a nested module
#[derive(Clone, Debug, Default)]
pub struct ModuleType<'a> {
    /// The imports that are expected for this module type.
    pub imports: Vec<ast::Import<'a>>,
    /// The exports that this module type is expected to have.
    pub exports: Vec<ExportType<'a>>,
    /// Instances within this module which are entirely exported.
    pub instance_exports: Vec<(ast::Span, ast::Id<'a>)>,
}

impl<'a> Parse<'a> for ModuleType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut imports = Vec::new();
        while parser.peek2::<kw::import>() {
            imports.push(parser.parens(|p| p.parse())?);
        }
        let mut exports = Vec::new();
        let mut instance_exports = Vec::new();
        while parser.peek2::<kw::export>() {
            parser.parens(|p| {
                if p.peek2::<ast::Index>() {
                    let span = p.parse::<kw::export>()?.0;
                    instance_exports.push((span, p.parse()?));
                } else {
                    exports.push(p.parse()?);
                }
                Ok(())
            })?;
        }
        Ok(ModuleType {
            imports,
            exports,
            instance_exports,
        })
    }
}

impl<'a> Peek for ModuleType<'a> {
    fn peek(cursor: Cursor<'_>) -> bool {
        if let Some(next) = cursor.lparen() {
            match next.keyword() {
                Some(("import", _)) | Some(("export", _)) => return true,
                _ => {}
            }
        }

        false
    }

    fn display() -> &'static str {
        "module type"
    }
}

/// A type for a nested instance
#[derive(Clone, Debug, Default)]
pub struct InstanceType<'a> {
    /// The exported types from this instance
    pub exports: Vec<ExportType<'a>>,
}

impl<'a> Parse<'a> for InstanceType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut exports = Vec::new();
        while !parser.is_empty() {
            exports.push(parser.parens(|p| p.parse())?);
        }
        Ok(InstanceType { exports })
    }
}

impl<'a> Peek for InstanceType<'a> {
    fn peek(cursor: Cursor<'_>) -> bool {
        if let Some(next) = cursor.lparen() {
            match next.keyword() {
                Some(("export", _)) => return true,
                _ => {}
            }
        }

        false
    }

    fn display() -> &'static str {
        "instance type"
    }
}

/// The type of an exported item from a module or instance.
#[derive(Debug, Clone)]
pub struct ExportType<'a> {
    /// Where this export was defined.
    pub span: ast::Span,
    /// The name of this export.
    pub name: &'a str,
    /// The signature of the item that's exported.
    pub item: ast::ItemSig<'a>,
}

impl<'a> Parse<'a> for ExportType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::export>()?.0;
        let name = parser.parse()?;
        let item = parser.parens(|p| p.parse())?;
        Ok(ExportType { span, name, item })
    }
}

/// A definition of a type.
#[derive(Debug)]
pub enum TypeDef<'a> {
    /// A function type definition.
    Func(FunctionType<'a>),
    /// A struct type definition.
    Struct(StructType<'a>),
    /// An array type definition.
    Array(ArrayType<'a>),
    /// A module type definition.
    Module(ModuleType<'a>),
    /// An instance type definition.
    Instance(InstanceType<'a>),
}

/// A type declaration in a module
#[derive(Debug)]
pub struct Type<'a> {
    /// Where this type was defined.
    pub span: ast::Span,
    /// An optional identifer to refer to this `type` by as part of name
    /// resolution.
    pub id: Option<ast::Id<'a>>,
    /// The type that we're declaring.
    pub def: TypeDef<'a>,
}

impl<'a> Parse<'a> for Type<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::r#type>()?.0;
        let id = parser.parse()?;
        let def = parser.parens(|parser| {
            let mut l = parser.lookahead1();
            if l.peek::<kw::func>() {
                parser.parse::<kw::func>()?;
                Ok(TypeDef::Func(parser.parse()?))
            } else if l.peek::<kw::r#struct>() {
                parser.parse::<kw::r#struct>()?;
                Ok(TypeDef::Struct(parser.parse()?))
            } else if l.peek::<kw::array>() {
                parser.parse::<kw::array>()?;
                Ok(TypeDef::Array(parser.parse()?))
            } else if l.peek::<kw::module>() {
                parser.parse::<kw::module>()?;
                Ok(TypeDef::Module(parser.parse()?))
            } else if l.peek::<kw::instance>() {
                parser.parse::<kw::instance>()?;
                Ok(TypeDef::Instance(parser.parse()?))
            } else {
                Err(l.error())
            }
        })?;
        Ok(Type { span, id, def })
    }
}

/// A reference to a type defined in this module.
#[derive(Clone, Debug)]
pub struct TypeUse<'a, T> {
    /// The type that we're referencing, if it was present.
    pub index: Option<ast::Index<'a>>,
    /// The inline type, if present.
    pub inline: Option<T>,
}

impl<'a, T> TypeUse<'a, T> {
    /// Constructs a new instance of `TypeUse` without an inline definition but
    /// with an index specified.
    pub fn new_with_index(index: ast::Index<'a>) -> TypeUse<'a, T> {
        TypeUse {
            index: Some(index),
            inline: None,
        }
    }
}

impl<'a, T: Peek + Parse<'a>> Parse<'a> for TypeUse<'a, T> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let index = if parser.peek2::<kw::r#type>() {
            Some(parser.parens(|parser| {
                parser.parse::<kw::r#type>()?;
                Ok(parser.parse()?)
            })?)
        } else {
            None
        };
        let inline = parser.parse()?;

        Ok(TypeUse { index, inline })
    }
}

impl<'a> From<TypeUse<'a, FunctionTypeNoNames<'a>>> for TypeUse<'a, FunctionType<'a>> {
    fn from(src: TypeUse<'a, FunctionTypeNoNames<'a>>) -> TypeUse<'a, FunctionType<'a>> {
        TypeUse {
            index: src.index,
            inline: src.inline.map(|x| x.into()),
        }
    }
}
