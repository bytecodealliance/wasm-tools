//! Untyped value

use std::borrow::Cow;

use crate::{ast::Node, lex::Keyword, parser::ParserError, Parser, WasmValue};

/// An UntypedValue is a parsed but not type-checked WAVE value.
#[derive(Clone, Debug)]
pub struct UntypedValue<'source> {
    source: Cow<'source, str>,
    node: Node,
}

impl<'source> UntypedValue<'source> {
    pub(crate) fn new(source: impl Into<Cow<'source, str>>, node: Node) -> Self {
        Self {
            source: source.into(),
            node,
        }
    }

    /// Parses an untyped value from WAVE.
    pub fn parse(source: &'source str) -> Result<Self, ParserError> {
        let mut parser = Parser::new(source);
        let val = parser.parse_raw_value()?;
        parser.finish()?;
        Ok(val)
    }

    /// Creates an owned value, copying the entire source string if necessary.
    pub fn into_owned(self) -> UntypedValue<'static> {
        UntypedValue::new(self.source.into_owned(), self.node)
    }

    /// Returns the source this value was parsed from.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Returns this value's root node.
    pub fn node(&self) -> &Node {
        &self.node
    }

    /// Converts this untyped value into the given typed value.
    pub fn to_wasm_value<V: WasmValue>(&self, ty: &V::Type) -> Result<V, ParserError> {
        self.node.to_wasm_value(ty, &self.source)
    }
}

impl std::fmt::Display for UntypedValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_node(f, &self.node, &self.source)
    }
}

/// An UntypedFuncCall is a parsed but not type-checked WAVE function call.
///
/// WAVE function calls have the form `<name>(<params...>)`.
pub struct UntypedFuncCall<'source> {
    source: Cow<'source, str>,
    name: Node,
    params: Option<Node>,
}

impl<'source> UntypedFuncCall<'source> {
    pub(crate) fn new(
        source: impl Into<Cow<'source, str>>,
        name: Node,
        params: Option<Node>,
    ) -> Self {
        Self {
            source: source.into(),
            name,
            params,
        }
    }

    /// Parses an untyped function call from WAVE.
    pub fn parse(source: &'source str) -> Result<Self, ParserError> {
        let mut parser = Parser::new(source);
        let call = parser.parse_raw_func_call()?;
        parser.finish()?;
        Ok(call)
    }

    /// Creates an owned function call, copying the entire source string if necessary.
    pub fn into_owned(self) -> UntypedFuncCall<'static> {
        UntypedFuncCall::new(
            self.source.into_owned(),
            self.name.clone(),
            self.params.clone(),
        )
    }

    /// Returns the source this function call was parsed from.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Returns the function name node.
    pub fn name_node(&self) -> &Node {
        &self.name
    }

    /// Returns the function parameters node.
    ///
    /// Returns `None` if the function call has no parameters.
    pub fn params_node(&self) -> Option<&Node> {
        self.params.as_ref()
    }

    /// Returns the function name.
    pub fn name(&self) -> &str {
        self.name.slice(&self.source)
    }

    /// Converts the untyped parameters into the given types.
    ///
    /// Any number of trailing option-typed values may be omitted; those will
    /// be returned as `none` values.
    pub fn to_wasm_params<'types, V: WasmValue + 'static>(
        &self,
        types: impl IntoIterator<Item = &'types V::Type>,
    ) -> Result<Vec<V>, ParserError> {
        match &self.params {
            Some(params) => params.to_wasm_params(types, self.source()),
            None => Ok(vec![]),
        }
    }
}

impl std::fmt::Display for UntypedFuncCall<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name.slice(&self.source))?;
        match &self.params {
            Some(params) => fmt_node(f, params, &self.source),
            None => f.write_str("()"),
        }
    }
}

fn fmt_node(f: &mut impl std::fmt::Write, node: &Node, src: &str) -> std::fmt::Result {
    use crate::ast::NodeType::*;
    match node.ty() {
        BoolTrue | BoolFalse | Number | Char | String | MultilineString | Label => {
            f.write_str(node.slice(src))
        }
        Tuple => fmt_sequence(f, '(', ')', node.as_tuple()?, src),
        List => fmt_sequence(f, '[', ']', node.as_list()?, src),
        Record => {
            let fields = node.as_record(src)?;
            if fields.len() == 0 {
                return f.write_str("{:}");
            }
            f.write_char('{')?;
            for (idx, (name, value)) in node.as_record(src)?.enumerate() {
                if idx != 0 {
                    f.write_str(", ")?;
                }
                write!(f, "{name}: ")?;
                fmt_node(f, value, src)?;
            }
            f.write_char('}')
        }
        VariantWithPayload => {
            let (label, payload) = node.as_variant(src)?;
            if Keyword::decode(label).is_some() {
                f.write_char('%')?;
            }
            fmt_variant(f, label, payload, src)
        }
        OptionSome => fmt_variant(f, "some", node.as_option()?, src),
        OptionNone => fmt_variant(f, "none", None, src),
        ResultOk => fmt_variant(f, "ok", node.as_result()?.unwrap(), src),
        ResultErr => fmt_variant(f, "err", node.as_result()?.unwrap_err(), src),
        Flags => {
            f.write_char('{')?;
            for (idx, flag) in node.as_flags(src)?.enumerate() {
                if idx != 0 {
                    f.write_str(", ")?;
                }
                f.write_str(flag)?;
            }
            f.write_char('}')
        }
    }
}

fn fmt_sequence<'a>(
    f: &mut impl std::fmt::Write,
    open: char,
    close: char,
    nodes: impl Iterator<Item = &'a Node>,
    src: &str,
) -> std::fmt::Result {
    f.write_char(open)?;
    for (idx, node) in nodes.enumerate() {
        if idx != 0 {
            f.write_str(", ")?;
        }
        fmt_node(f, node, src)?;
    }
    f.write_char(close)
}

fn fmt_variant(
    f: &mut impl std::fmt::Write,
    case: &str,
    payload: Option<&Node>,
    src: &str,
) -> std::fmt::Result {
    f.write_str(case)?;
    if let Some(node) = payload {
        f.write_char('(')?;
        fmt_node(f, node, src)?;
        f.write_char(')')?;
    }
    Ok(())
}

impl From<ParserError> for std::fmt::Error {
    fn from(_: ParserError) -> Self {
        Self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips() {
        for src in [
            "true",
            "18446744073709551616",
            "-9223372036854775808",
            "[-3.1415, 0, inf, nan, -inf]",
            "['☃', '\\n']",
            r#""☃☃☃""#,
            "(1, false)",
            "{:}",
            "{code: red}",
            "left(1)",
            "[some(1), none]",
            "[ok(1), err(2)]",
            "[ok, err]",
            "%inf(inf)",
            "%some",
            "%none(none)",
        ] {
            let val = UntypedValue::parse(src).unwrap();
            let encoded = val.to_string();
            assert_eq!(encoded, src);
        }
    }
}
