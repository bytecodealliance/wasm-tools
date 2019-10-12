use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

pub struct Wast<'a> {
    pub directives: Vec<WastDirective<'a>>,
}

impl<'a> Parse<'a> for Wast<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut directives = Vec::new();
        while !parser.is_empty() {
            directives.push(parser.parens(|p| p.parse())?);
        }
        Ok(Wast { directives })
    }
}

pub enum WastDirective<'a> {
    Module(ast::Module<'a>),
    AssertMalformed {
        module: ast::Module<'a>,
        message: &'a str,
    },
    AssertInvalid {
        module: ast::Module<'a>,
        message: &'a str,
    },
    Register {
        name: &'a str,
        module: Option<ast::Id<'a>>,
    },
    Invoke(WastInvoke<'a>),
    AssertTrap {
        exec: WastExecute<'a>,
        message: &'a str,
    },
    AssertReturn {
        exec: WastExecute<'a>,
        results: Vec<ast::Expression<'a>>,
    },
    AssertReturnCanonicalNan(WastInvoke<'a>),
    AssertReturnArithmeticNan(WastInvoke<'a>),
    AssertReturnFunc(WastInvoke<'a>),
    AssertExhaustion {
        call: WastInvoke<'a>,
        message: &'a str,
    },
    AssertUnlinkable {
        module: ast::Module<'a>,
        message: &'a str,
    },
}

impl<'a> Parse<'a> for WastDirective<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::module>() {
            Ok(WastDirective::Module(parser.parse()?))
        } else if l.peek::<kw::assert_malformed>() {
            parser.parse::<kw::assert_malformed>()?;
            Ok(WastDirective::AssertMalformed {
                module: parser.parens(|p| p.parse())?,
                message: parser.parse()?,
            })
        } else if l.peek::<kw::assert_invalid>() {
            parser.parse::<kw::assert_invalid>()?;
            Ok(WastDirective::AssertInvalid {
                module: parser.parens(|p| p.parse())?,
                message: parser.parse()?,
            })
        } else if l.peek::<kw::register>() {
            parser.parse::<kw::register>()?;
            Ok(WastDirective::Register {
                name: parser.parse()?,
                module: parser.parse()?,
            })
        } else if l.peek::<kw::invoke>() {
            Ok(WastDirective::Invoke(parser.parse()?))
        } else if l.peek::<kw::assert_trap>() {
            parser.parse::<kw::assert_trap>()?;
            Ok(WastDirective::AssertTrap {
                exec: parser.parens(|p| p.parse())?,
                message: parser.parse()?,
            })
        } else if l.peek::<kw::assert_return>() {
            parser.parse::<kw::assert_return>()?;
            let exec = parser.parens(|p| p.parse())?;
            let mut results = Vec::new();
            while !parser.is_empty() {
                results.push(parser.parens(|p| p.parse())?);
            }
            Ok(WastDirective::AssertReturn { exec, results })
        } else if l.peek::<kw::assert_return_canonical_nan>() {
            parser.parse::<kw::assert_return_canonical_nan>()?;
            Ok(WastDirective::AssertReturnCanonicalNan(
                parser.parens(|p| p.parse())?,
            ))
        } else if l.peek::<kw::assert_return_arithmetic_nan>() {
            parser.parse::<kw::assert_return_arithmetic_nan>()?;
            Ok(WastDirective::AssertReturnArithmeticNan(
                parser.parens(|p| p.parse())?,
            ))
        } else if l.peek::<kw::assert_return_func>() {
            parser.parse::<kw::assert_return_func>()?;
            Ok(WastDirective::AssertReturnFunc(
                parser.parens(|p| p.parse())?,
            ))
        } else if l.peek::<kw::assert_exhaustion>() {
            parser.parse::<kw::assert_exhaustion>()?;
            Ok(WastDirective::AssertExhaustion {
                call: parser.parens(|p| p.parse())?,
                message: parser.parse()?,
            })
        } else if l.peek::<kw::assert_unlinkable>() {
            parser.parse::<kw::assert_unlinkable>()?;
            Ok(WastDirective::AssertUnlinkable {
                module: parser.parens(|p| p.parse())?,
                message: parser.parse()?,
            })
        } else {
            Err(l.error())
        }
    }
}

pub enum WastExecute<'a> {
    Invoke(WastInvoke<'a>),
    Module(ast::Module<'a>),
    Get {
        module: Option<ast::Id<'a>>,
        global: &'a str,
    },
}

impl<'a> Parse<'a> for WastExecute<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::invoke>() {
            Ok(WastExecute::Invoke(parser.parse()?))
        } else if l.peek::<kw::module>() {
            Ok(WastExecute::Module(parser.parse()?))
        } else if l.peek::<kw::get>() {
            parser.parse::<kw::get>()?;
            Ok(WastExecute::Get {
                module: parser.parse()?,
                global: parser.parse()?,
            })
        } else {
            Err(l.error())
        }
    }
}

pub struct WastInvoke<'a> {
    pub module: Option<ast::Id<'a>>,
    pub name: &'a str,
    pub args: Vec<ast::Expression<'a>>,
}

impl<'a> Parse<'a> for WastInvoke<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        parser.parse::<kw::invoke>()?;
        let module = parser.parse()?;
        let name = parser.parse()?;
        let mut args = Vec::new();
        while !parser.is_empty() {
            args.push(parser.parens(|p| p.parse())?);
        }
        Ok(WastInvoke { module, name, args })
    }
}
