use crate::ast::{self, kw};
use crate::parser::{Cursor, Parse, Parser, Peek, Result};

/// An interface-types type.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum InterType<'a> {
    Unit,
    Bool,
    S8,
    U8,
    S16,
    U16,
    S32,
    U32,
    S64,
    U64,
    Float32,
    Float64,
    Char,
    String,
    Record(Record<'a>),
    Variant(Variant<'a>),
    List(List<'a>),
    Tuple(Tuple<'a>),
    Flags(Flags<'a>),
    Enum(Enum<'a>),
    Union(Union<'a>),
    Option(OptionType<'a>),
    Expected(Expected<'a>),
}

impl<'a> Parse<'a> for InterType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::unit>() {
            parser.parse::<kw::unit>()?;
            Ok(InterType::Unit)
        } else if l.peek::<kw::bool_>() {
            parser.parse::<kw::bool_>()?;
            Ok(InterType::Bool)
        } else if l.peek::<kw::s8>() {
            parser.parse::<kw::s8>()?;
            Ok(InterType::S8)
        } else if l.peek::<kw::r#u8>() {
            parser.parse::<kw::r#u8>()?;
            Ok(InterType::U8)
        } else if l.peek::<kw::s16>() {
            parser.parse::<kw::s16>()?;
            Ok(InterType::S16)
        } else if l.peek::<kw::r#u16>() {
            parser.parse::<kw::r#u16>()?;
            Ok(InterType::U16)
        } else if l.peek::<kw::s32>() {
            parser.parse::<kw::s32>()?;
            Ok(InterType::S32)
        } else if l.peek::<kw::r#u32>() {
            parser.parse::<kw::r#u32>()?;
            Ok(InterType::U32)
        } else if l.peek::<kw::s64>() {
            parser.parse::<kw::s64>()?;
            Ok(InterType::S64)
        } else if l.peek::<kw::r#u64>() {
            parser.parse::<kw::r#u64>()?;
            Ok(InterType::U64)
        } else if l.peek::<kw::float32>() {
            parser.parse::<kw::float32>()?;
            Ok(InterType::Float32)
        } else if l.peek::<kw::float64>() {
            parser.parse::<kw::float64>()?;
            Ok(InterType::Float64)
        } else if l.peek::<kw::r#char>() {
            parser.parse::<kw::r#char>()?;
            Ok(InterType::Char)
        } else if l.peek::<kw::string>() {
            parser.parse::<kw::string>()?;
            Ok(InterType::String)
        } else if l.peek::<ast::LParen>() {
            if parser.peek2::<kw::record>() {
                let record = parser.parse()?;
                Ok(InterType::Record(record))
            } else if parser.peek2::<kw::variant>() {
                let variant = parser.parse()?;
                Ok(InterType::Variant(variant))
            } else if parser.peek2::<kw::list>() {
                let list = parser.parse()?;
                Ok(InterType::List(list))
            } else if parser.peek2::<kw::tuple>() {
                let tuple = parser.parse()?;
                Ok(InterType::Tuple(tuple))
            } else if parser.peek2::<kw::flags>() {
                let flags = parser.parse()?;
                Ok(InterType::Flags(flags))
            } else if parser.peek2::<kw::enum_>() {
                let enum_ = parser.parse()?;
                Ok(InterType::Enum(enum_))
            } else if parser.peek2::<kw::union>() {
                let union = parser.parse()?;
                Ok(InterType::Union(union))
            } else if parser.peek2::<kw::option>() {
                let optional = parser.parse()?;
                Ok(InterType::Option(optional))
            } else if parser.peek2::<kw::expected>() {
                let expected = parser.parse()?;
                Ok(InterType::Expected(expected))
            } else {
                Err(parser.error("expected derived intertype"))
            }
        } else {
            Err(l.error())
        }
    }
}

impl<'a> Peek for InterType<'a> {
    fn peek(cursor: Cursor<'_>) -> bool {
        kw::unit::peek(cursor)
            || kw::bool_::peek(cursor)
            || kw::s8::peek(cursor)
            || kw::r#u8::peek(cursor)
            || kw::s16::peek(cursor)
            || kw::r#u16::peek(cursor)
            || kw::s32::peek(cursor)
            || kw::r#u32::peek(cursor)
            || kw::s64::peek(cursor)
            || kw::r#u64::peek(cursor)
            || kw::float32::peek(cursor)
            || kw::float64::peek(cursor)
            || kw::r#char::peek(cursor)
            || kw::string::peek(cursor)
            || (ast::LParen::peek(cursor)
                && (kw::record::peek2(cursor)
                    || kw::record::peek2(cursor)
                    || kw::variant::peek2(cursor)
                    || kw::list::peek2(cursor)
                    || kw::tuple::peek2(cursor)
                    || kw::flags::peek2(cursor)
                    || kw::enum_::peek2(cursor)
                    || kw::union::peek2(cursor)
                    || kw::option::peek2(cursor)
                    || kw::expected::peek2(cursor)))
    }
    fn display() -> &'static str {
        "intertype"
    }
}

/// An interface-types record, aka a struct.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Record<'a> {
    fields: Vec<Field<'a>>,
}

impl<'a> Parse<'a> for Record<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::record>()?;
            let mut fields = Vec::new();
            while !parser.is_empty() {
                fields.push(parser.parse()?);
            }
            Ok(Record { fields })
        })
    }
}

/// An interface-types record field.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Field<'a> {
    /// The name of the field.
    name: ast::Id<'a>,
    /// The type of the field.
    type_: ast::ComponentTypeUse<'a, InterType<'a>>,
}

impl<'a> Parse<'a> for Field<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::field>()?;
            Ok(Field {
                name: parser.parse()?,
                type_: parser.parse()?,
            })
        })
    }
}

/// An interface-types variant, aka a discriminated union with named arms.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Variant<'a> {
    cases: Vec<Case<'a>>,
}

impl<'a> Parse<'a> for Variant<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::variant>()?;
            let mut cases = Vec::new();
            while !parser.is_empty() {
                cases.push(parser.parse()?);
            }
            Ok(Variant { cases })
        })
    }
}

/// An interface-types variant case.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Case<'a> {
    /// The name of the case.
    name: ast::Id<'a>,
    /// The type of the case.
    type_: ast::ComponentTypeUse<'a, InterType<'a>>,
    /// The optional defaults-to name.
    defaults_to: Option<ast::Id<'a>>,
}

impl<'a> Parse<'a> for Case<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::case>()?;
            let name = parser.parse()?;
            let type_ = parser.parse()?;
            let defaults_to = if !parser.is_empty() {
                Some(parser.parens(|parser| {
                    parser.parse::<kw::defaults_to>()?;
                    parser.parse()
                })?)
            } else {
                None
            };
            Ok(Case {
                name,
                type_,
                defaults_to,
            })
        })
    }
}

/// An interface-types list, aka a fixed-size array.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct List<'a> {
    element: Box<ast::ComponentTypeUse<'a, InterType<'a>>>,
}

impl<'a> Parse<'a> for List<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::list>()?;
            let ty = parser.parse()?;
            Ok(List {
                element: Box::new(ty),
            })
        })
    }
}

/// An interface-types tuple, aka a record with anonymous fields.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Tuple<'a> {
    fields: Vec<ast::ComponentTypeUse<'a, InterType<'a>>>,
}

impl<'a> Parse<'a> for Tuple<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::tuple>()?;
            let mut fields = Vec::new();
            while !parser.is_empty() {
                fields.push(parser.parse()?);
            }
            Ok(Tuple { fields })
        })
    }
}

/// An interface-types flags, aka a fixed-sized bitfield with named fields.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Flags<'a> {
    flag_names: Vec<ast::Id<'a>>,
}

impl<'a> Parse<'a> for Flags<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::flags>()?;
            let mut flag_names = Vec::new();
            while !parser.is_empty() {
                flag_names.push(parser.parse()?);
            }
            Ok(Flags { flag_names })
        })
    }
}

/// An interface-types enum, aka a discriminated union with unit arms.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Enum<'a> {
    arms: Vec<ast::Id<'a>>,
}

impl<'a> Parse<'a> for Enum<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::enum_>()?;
            let mut arms = Vec::new();
            while !parser.is_empty() {
                arms.push(parser.parse()?);
            }
            Ok(Enum { arms })
        })
    }
}

/// An interface-types union, aka a discriminated union with anonymous arms.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Union<'a> {
    arms: Vec<ast::ComponentTypeUse<'a, InterType<'a>>>,
}

impl<'a> Parse<'a> for Union<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::union>()?;
            let mut arms = Vec::new();
            while !parser.is_empty() {
                arms.push(parser.parse()?);
            }
            Ok(Union { arms })
        })
    }
}

/// An interface-types optional, aka an option.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct OptionType<'a> {
    element: Box<ast::ComponentTypeUse<'a, InterType<'a>>>,
}

impl<'a> Parse<'a> for OptionType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::option>()?;
            let ty = parser.parse()?;
            Ok(OptionType {
                element: Box::new(ty),
            })
        })
    }
}

/// An interface-types expected, aka an result.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Expected<'a> {
    ok: Box<ast::ComponentTypeUse<'a, InterType<'a>>>,
    err: Box<ast::ComponentTypeUse<'a, InterType<'a>>>,
}

impl<'a> Parse<'a> for Expected<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::expected>()?;
            let ok = parser.parse()?;
            let err = parser.parse()?;
            Ok(Expected {
                ok: Box::new(ok),
                err: Box::new(err),
            })
        })
    }
}
