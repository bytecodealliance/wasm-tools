/// The `deftype` production in the component-model AST, and its children.

use crate::ast::{self, kw};
use crate::parser::{Cursor, Parse, Parser, Peek, Result};

/// Different kinds of elements that can be exported from a WebAssembly component,
/// contained in a [`ComponentExport`].
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum DefTypeKind {
    Func,
    Module,
    Component,
    Instance,
    Value,
}

impl<'a> Parse<'a> for DefTypeKind {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::func>() {
            parser.parse::<kw::func>()?;
            Ok(DefTypeKind::Func)
        } else if l.peek::<kw::module>() {
            parser.parse::<kw::module>()?;
            Ok(DefTypeKind::Module)
        } else if l.peek::<kw::component>() {
            parser.parse::<kw::component>()?;
            Ok(DefTypeKind::Component)
        } else if l.peek::<kw::instance>() {
            parser.parse::<kw::instance>()?;
            Ok(DefTypeKind::Instance)
        } else if l.peek::<kw::value>() {
            parser.parse::<kw::value>()?;
            Ok(DefTypeKind::Value)
        } else {
            Err(l.error())
        }
    }
}

impl Peek for DefTypeKind {
    fn peek(cursor: Cursor<'_>) -> bool {
        kw::func::peek(cursor)
            || kw::module::peek(cursor)
            || kw::component::peek(cursor)
            || kw::instance::peek(cursor)
            || kw::value::peek(cursor)
    }
    fn display() -> &'static str {
        "deftype kind"
    }
}

/// deftype           ::= <moduletype>
///                     | <componenttype>
///                     | <instancetype>
///                     | <functype>
///                     | <valuetype>
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum DefType<'a> {
    Func(ast::ComponentFunctionType<'a>),
    Module(ast::ModuleType<'a>),
    Component(ast::ComponentModuleType<'a>),
    Instance(ast::InstanceType<'a>),
    Value(ast::ValueType<'a>),
}

impl<'a> Parse<'a> for DefType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<ast::ComponentFunctionType>() {
            let ty = parser.parse()?;
            Ok(DefType::Func(ty))
        } else if parser.peek::<ast::ModuleType>() {
            let ty = parser.parse()?;
            Ok(DefType::Module(ty))
        } else if parser.peek::<ast::ComponentModuleType>() {
            let ty = parser.parse()?;
            Ok(DefType::Component(ty))
        } else if parser.peek::<ast::InstanceType>() {
            let ty = parser.parse()?;
            Ok(DefType::Instance(ty))
        } else if parser.peek::<ast::ValueType>() {
            let ty = parser.parse()?;
            Ok(DefType::Value(ty))
        } else {
            Err(parser.error("expected a deftype"))
        }
    }
}

impl Peek for DefType<'_> {
    fn peek(cursor: Cursor<'_>) -> bool {
        ast::LParen::peek(cursor) && (
        kw::module::peek2(cursor) ||
        kw::component::peek2(cursor) ||
        kw::instance::peek2(cursor) ||
        kw::func::peek2(cursor) ||
        kw::value::peek2(cursor))
    }

    fn display() -> &'static str {
        "deftype"
    }
}

/// A component function type with parameters and results.
///
/// functype          ::= (func <id>? (param <name>? <intertype>)* (result <intertype>)?)
#[derive(Clone, Debug)]
pub struct ComponentFunctionType<'a> {
    /// An optional name.
    pub id: Option<ast::Id<'a>>,
    /// The parameters of a function, optionally each having an identifier for
    /// name resolution and a name for the custom `name` section.
    pub params: Box<
        [(
            Option<ast::Id<'a>>,
            Option<ast::NameAnnotation<'a>>,
            ast::ComponentTypeUse<'a, ast::InterType<'a>>,
        )],
    >,
    /// The result type of a function.
    pub result: ast::ComponentTypeUse<'a, ast::InterType<'a>>,
}

impl<'a> Parse<'a> for ComponentFunctionType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::func>()?;
            let id = parser.parse::<Option<ast::Id>>()?;
            let mut params = Vec::new();
            while parser.peek2::<kw::param>() {
                parser.parens(|p| {
                    let mut l = p.lookahead1();
                    if l.peek::<kw::param>() {
                        p.parse::<kw::param>()?;
                        if p.is_empty() {
                            return Ok(());
                        }
                        // If we just saw `(param` and we're looking at `$X`, it
                        // could be a parameter name or a type name. Peek ahead to
                        // see if we're at the closing `)`, if so, parse it as a
                        // type.
                        if p.peek2_empty() {
                            let ty = p.parse()?;
                            params.push((None, None, ty));
                        } else {
                            let (id, name) = (p.parse::<Option<_>>()?, p.parse::<Option<_>>()?);
                            let ty = p.parse()?;
                            params.push((id, name, ty));
                        }
                    } else {
                        return Err(l.error());
                    }
                    Ok(())
                })?;
            }
            let result = if parser.peek::<ast::LParen>() {
                // Parse a `(result ...)`.
                parser.parens(|parser| {
                    parser.parse::<kw::result>()?;
                    parser.parse()
                })?
            } else {
                // If the result is omitted, use `unit`.
                ast::ComponentTypeUse {
                    index: None,
                    inline: Some(ast::InterType::Unit),
                }
            };
            Ok(Self {
                id,
                params: params.into(),
                result,
            })
        })
    }
}

impl<'a> Peek for ComponentFunctionType<'a> {
    fn peek(cursor: Cursor<'_>) -> bool {
        if let Some(next) = cursor.lparen() {
            match next.keyword() {
                Some(("func", _)) => return true,
                _ => {}
            }
        }

        false
    }

    fn display() -> &'static str {
        "component function type"
    }
}

/// A type for a nested module
#[derive(Clone, Debug, Default)]
pub struct ModuleType<'a> {
    /// An optional identifer to refer to this `module` type by as part of
    /// name resolution.
    pub id: Option<ast::Id<'a>>,
    /// The imports that are expected for this module type.
    pub imports: Vec<ast::Import<'a>>,
    /// The exports that this module type is expected to have.
    pub exports: Vec<ast::ExportType<'a>>,
}

impl<'a> Parse<'a> for ModuleType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        // See comments in `nested_module.rs` for why this is tested here.
        if parser.parens_depth() > 100 {
            return Err(parser.error("module type nesting too deep"));
        }

        parser.parens(|parser| {
            parser.parse::<kw::module>()?;
            let id = parser.parse()?;
            let mut imports = Vec::new();
            let mut exports = Vec::new();
            while !parser.is_empty() {
                if parser.peek2::<kw::import>() {
                    imports.push(parser.parens(|p| p.parse())?);
                } else if parser.peek2::<kw::export>() {
                    parser.parens(|p| {
                        exports.push(p.parse()?);
                        Ok(())
                    })?;
                } else {
                    return Err(parser.error("Expected a moduletype-def"));
                }
            }
            Ok(ModuleType {
                id,
                imports,
                exports,
            })
        })
    }
}

impl<'a> Peek for ModuleType<'a> {
    fn peek(cursor: Cursor<'_>) -> bool {
        if let Some(next) = cursor.lparen() {
            match next.keyword() {
                Some(("module", _)) => return true,
                _ => {}
            }
        }

        false
    }

    fn display() -> &'static str {
        "module type"
    }
}

/// A type for a nested component
#[derive(Clone, Debug, Default)]
pub struct ComponentModuleType<'a> {
    /// An optional identifer to refer to this `component` type by as part of
    /// name resolution.
    pub id: Option<ast::Id<'a>>,
    /// The public types for this component.
    pub types: Vec<ast::ComponentType<'a>>,
    /// The public type relationships for this component.
    pub aliases: Vec<ast::Alias<'a>>,
    /// The imports that are expected for this component type.
    pub imports: Vec<ast::ComponentImport<'a>>,
    /// The exports that this component type is expected to have.
    pub exports: Vec<ast::ComponentExportType<'a>>,
}

impl<'a> Parse<'a> for ComponentModuleType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.parens_depth() > 100 {
            return Err(parser.error("component type nesting too deep"));
        }

        parser.parens(|parser| {
            parser.parse::<kw::component>()?;
            let id = parser.parse()?;

            let mut types = Vec::new();
            let mut aliases = Vec::new();
            let mut imports = Vec::new();
            let mut exports = Vec::new();
            while parser.peek::<ast::LParen>() {
                if parser.peek2::<kw::import>() {
                    imports.push(parser.parse()?);
                } else if parser.peek2::<kw::export>() {
                    exports.push(parser.parse()?);
                } else if parser.peek2::<kw::r#type>() {
                    types.push(parser.parse()?);
                } else if parser.peek2::<kw::alias>() {
                    aliases.push(parser.parse()?);
                }
            }
            Ok(ComponentModuleType {
                id,
                types,
                aliases,
                imports,
                exports,
            })
        })
    }
}

impl<'a> Peek for ComponentModuleType<'a> {
    fn peek(cursor: Cursor<'_>) -> bool {
        if let Some(next) = cursor.lparen() {
            matches!(next.keyword(), Some(("component", _)))
        } else {
            false
        }
    }

    fn display() -> &'static str {
        "component type"
    }
}

/// A type for a nested instance
#[derive(Clone, Debug, Default)]
pub struct InstanceType<'a> {
    /// An optional identifer to refer to this `instance` type by as part of
    /// name resolution.
    pub id: Option<ast::Id<'a>>,
    /// The public types for this instance.
    pub types: Vec<ast::ComponentType<'a>>,
    /// The public type relationships for this instance.
    pub aliases: Vec<ast::ComponentExportType<'a>>,
    /// The exports that this instance type is expected to have.
    pub exports: Vec<ast::ComponentExportType<'a>>,
}

impl<'a> Parse<'a> for InstanceType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        // See comments in `nested_module.rs` for why this is tested here.
        if parser.parens_depth() > 100 {
            return Err(parser.error("instance type nesting too deep"));
        }

        parser.parens(|parser| {
            parser.parse::<kw::instance>()?;
            let id = parser.parse()?;
            let mut types = Vec::new();
            let mut aliases = Vec::new();
            let mut exports = Vec::new();
            while parser.peek::<ast::LParen>() {
                if parser.peek2::<kw::export>() {
                    exports.push(parser.parse()?);
                } else if parser.peek2::<kw::r#type>() {
                    types.push(parser.parse()?);
                } else if parser.peek2::<kw::alias>() {
                    aliases.push(parser.parse()?);
                }
            }
            Ok(InstanceType {
                id,
                types,
                aliases,
                exports,
            })
        })
    }
}

impl<'a> Peek for InstanceType<'a> {
    fn peek(cursor: Cursor<'_>) -> bool {
        if let Some(next) = cursor.lparen() {
            match next.keyword() {
                Some(("instance", _)) => return true,
                _ => {}
            }
        }

        false
    }

    fn display() -> &'static str {
        "instance type"
    }
}

/// A value type.
#[derive(Debug, Clone)]
pub struct ValueType<'a> {
    /// An optional name.
    pub id: Option<ast::Id<'a>>,
    /// The type of the value.
    pub value_type: ast::ComponentTypeUse<'a, ast::InterType<'a>>,
}

impl<'a> Parse<'a> for ValueType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parens(|parser| {
            parser.parse::<kw::value>()?;
            Ok(ValueType {
                id: parser.parse()?,
                value_type: parser.parse()?,
            })
        })
    }
}

impl<'a> Peek for ValueType<'a> {
    fn peek(cursor: Cursor<'_>) -> bool {
        ast::LParen::peek(cursor) && kw::value::peek2(cursor)
    }

    fn display() -> &'static str {
        "valuetype"
    }
}
