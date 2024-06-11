//! Parsing types

use std::{collections::HashSet, error::Error, fmt::Display};

use indexmap::IndexMap;

use crate::{
    ast::{Node, NodeType},
    lex::{Keyword, Lexer, Span, Token},
    untyped::{UntypedFuncCall, UntypedValue},
    WasmValue,
};

/// A Web Assembly Value Encoding parser.
pub struct Parser<'source> {
    lex: Lexer<'source>,
    curr: Option<Token>,
}

impl<'source> Parser<'source> {
    /// Returns a new Parser of the given source.
    pub fn new(source: &'source str) -> Self {
        Self::with_lexer(Lexer::new(source))
    }

    /// Returns a new Parser with the given [`Lexer`].
    pub fn with_lexer(lexer: Lexer<'source>) -> Self {
        Self {
            lex: lexer,
            curr: None,
        }
    }

    /// Parses a WAVE-encoded value of the given [`crate::wasm::WasmType`] into a
    /// corresponding [`WasmValue`].
    pub fn parse_value<V: WasmValue>(&mut self, ty: &V::Type) -> Result<V, ParserError> {
        let node = self.parse_node()?;
        node.to_wasm_value(ty, self.lex.source())
    }

    /// Parses a WAVE-encoded value into an [`UntypedValue`].
    pub fn parse_raw_value(&mut self) -> Result<UntypedValue<'source>, ParserError> {
        let node = self.parse_node()?;
        Ok(UntypedValue::new(self.lex.source(), node))
    }

    /// Parses a function name followed by a WAVE-encoded, parenthesized,
    /// comma-separated sequence of values into an [`UntypedFuncCall`].
    pub fn parse_raw_func_call(&mut self) -> Result<UntypedFuncCall<'source>, ParserError> {
        self.advance()?;
        let name = self.parse_label()?;
        self.advance()?;
        self.expect_token(Token::ParenOpen)?;
        let params = self.parse_tuple()?;
        Ok(UntypedFuncCall::new(self.lex.source(), name, params))
    }

    /// Returns an error if any significant input remains unparsed.
    pub fn finish(&mut self) -> Result<(), ParserError> {
        match self.lex.clone().spanned().next() {
            None => Ok(()),
            Some((_, span)) => Err(ParserError::new(
                ParserErrorKind::TrailingCharacters,
                span.clone(),
            )),
        }
    }

    fn parse_node(&mut self) -> Result<Node, ParserError> {
        Ok(match self.advance()? {
            Token::Number => self.leaf_node(NodeType::Number),
            Token::Char => self.leaf_node(NodeType::Char),
            Token::String => self.leaf_node(NodeType::String),
            Token::MultilineString => self.leaf_node(NodeType::MultilineString),
            Token::ParenOpen => self.parse_tuple()?,
            Token::BracketOpen => self.parse_list()?,
            Token::BraceOpen => self.parse_record_or_flags()?,
            Token::LabelOrKeyword => match Keyword::decode(self.slice()) {
                Some(Keyword::True) => self.leaf_node(NodeType::BoolTrue),
                Some(Keyword::False) => self.leaf_node(NodeType::BoolFalse),
                Some(Keyword::Some) => self.parse_option(NodeType::OptionSome)?,
                Some(Keyword::None) => self.parse_option(NodeType::OptionNone)?,
                Some(Keyword::Ok) => self.parse_result(NodeType::ResultOk)?,
                Some(Keyword::Err) => self.parse_result(NodeType::ResultErr)?,
                Some(Keyword::Inf | Keyword::Nan) => self.leaf_node(NodeType::Number),
                None => self.parse_label_maybe_payload()?,
            },
            Token::BraceClose
            | Token::ParenClose
            | Token::BracketClose
            | Token::Colon
            | Token::Comma => return Err(self.unexpected_token()),
        })
    }

    fn parse_tuple(&mut self) -> Result<Node, ParserError> {
        let start = self.span().start;
        let children = self.parse_comma_separated_nodes(Token::ParenClose)?;
        let span = start..self.span().end;
        if children.is_empty() {
            return Err(ParserError::new(ParserErrorKind::EmptyTuple, span));
        }
        Ok(Node::new(NodeType::Tuple, span, children))
    }

    fn parse_list(&mut self) -> Result<Node, ParserError> {
        let start = self.span().start;
        let children = self.parse_comma_separated_nodes(Token::BracketClose)?;
        Ok(Node::new(NodeType::List, start..self.span().end, children))
    }

    fn parse_record_or_flags(&mut self) -> Result<Node, ParserError> {
        let start = self.span().start;
        self.advance()?;

        match self.token() {
            // Handle empty record (`{:}`)
            Token::Colon => {
                self.advance()?; // Advance to `}`
                self.expect_token(Token::BraceClose)?;
                return Ok(Node::new(NodeType::Record, start..self.span().end, []));
            }
            // Handle empty flags (`{}`)
            Token::BraceClose => return Ok(Node::new(NodeType::Flags, start..self.span().end, [])),
            _ => (),
        }

        // Check for a following `:` to distinguish records from flags
        if self.next_is(Token::Colon) {
            self.finish_record(start)
        } else {
            self.finish_flags(start)
        }
    }

    fn finish_record(&mut self, start: usize) -> Result<Node, ParserError> {
        let mut seen = HashSet::with_capacity(1);
        let mut children = Vec::with_capacity(2);
        loop {
            // Parse field label
            let label = self.parse_label()?;
            // Check for duplicate fields
            let field = self.slice().trim_start_matches('%');
            if !seen.insert(field) {
                return Err(ParserError::with_detail(
                    ParserErrorKind::DuplicateField,
                    label.span(),
                    format!("{field:?}"),
                ));
            }
            // Parse colon
            self.advance()?;
            self.expect_token(Token::Colon)?;
            // Parse value
            let value = self.parse_node()?;
            children.extend([label, value]);
            // Parse comma and/or end of record
            if self.advance()? == Token::Comma {
                self.advance()?;
            }
            if self.token() == Token::BraceClose {
                break;
            }
        }
        Ok(Node::new(
            NodeType::Record,
            start..self.span().end,
            children,
        ))
    }

    fn finish_flags(&mut self, start: usize) -> Result<Node, ParserError> {
        let mut flags = IndexMap::with_capacity(1);
        loop {
            // Parse flag label
            let label = self.parse_label()?;
            // Insert and check for duplicate flags
            let span = label.span();
            let flag = self.slice().trim_start_matches('%');
            if flags.insert(flag, label).is_some() {
                return Err(ParserError::with_detail(
                    ParserErrorKind::DuplicateFlag,
                    span,
                    format!("{flag:?}"),
                ));
            }
            // Parse comma and/or end of flags
            if self.advance()? == Token::Comma {
                self.advance()?;
            }
            if self.token() == Token::BraceClose {
                break;
            }
        }
        Ok(Node::new(
            NodeType::Flags,
            start..self.span().end,
            flags.into_values(),
        ))
    }

    fn parse_label_maybe_payload(&mut self) -> Result<Node, ParserError> {
        let start = self.span().start;
        let label = self.parse_label()?;
        if self.next_is(Token::ParenOpen) {
            self.advance()?;
            let payload = self.parse_node()?;
            self.advance()?;
            self.expect_token(Token::ParenClose)?;
            Ok(Node::new(
                NodeType::VariantWithPayload,
                start..self.span().end,
                [label, payload],
            ))
        } else {
            Ok(label)
        }
    }

    fn parse_option(&mut self, ty: NodeType) -> Result<Node, ParserError> {
        let start = self.span().start;
        let payload = match ty {
            NodeType::OptionSome => {
                self.advance()?;
                self.expect_token(Token::ParenOpen)?;
                let payload = self.parse_node()?;
                self.advance()?;
                self.expect_token(Token::ParenClose)?;
                Some(payload)
            }
            NodeType::OptionNone => None,
            _ => unreachable!(),
        };
        Ok(Node::new(ty, start..self.span().end, payload))
    }

    fn parse_result(&mut self, ty: NodeType) -> Result<Node, ParserError> {
        let start = self.span().start;
        let mut payload = None;
        if self.next_is(Token::ParenOpen) {
            self.advance()?;
            self.expect_token(Token::ParenOpen)?;
            payload = Some(self.parse_node()?);
            self.advance()?;
            self.expect_token(Token::ParenClose)?;
        }
        Ok(Node::new(ty, start..self.span().end, payload))
    }

    fn parse_label(&mut self) -> Result<Node, ParserError> {
        self.expect_token(Token::LabelOrKeyword)?;
        Ok(self.leaf_node(NodeType::Label))
    }

    fn advance(&mut self) -> Result<Token, ParserError> {
        let token = match self.lex.next() {
            Some(Ok(token)) => token,
            Some(Err(span)) => {
                let span = span.unwrap_or_else(|| self.lex.span());
                return Err(ParserError::new(ParserErrorKind::InvalidToken, span));
            }
            None => {
                return Err(ParserError::new(
                    ParserErrorKind::UnexpectedEnd,
                    self.lex.span(),
                ));
            }
        };
        self.curr = Some(token);
        Ok(token)
    }

    fn token(&self) -> Token {
        self.curr.unwrap()
    }

    fn span(&self) -> Span {
        self.lex.span()
    }

    fn slice(&self) -> &'source str {
        &self.lex.source()[self.span()]
    }

    fn next_is(&mut self, token: Token) -> bool {
        self.lex.clone().next().and_then(|res| res.ok()) == Some(token)
    }

    fn expect_token(&self, token: Token) -> Result<(), ParserError> {
        if self.token() == token {
            Ok(())
        } else {
            Err(self.unexpected_token())
        }
    }

    fn unexpected_token(&self) -> ParserError {
        ParserError::with_detail(ParserErrorKind::UnexpectedToken, self.span(), self.token())
    }

    fn parse_comma_separated_nodes(&mut self, end_token: Token) -> Result<Vec<Node>, ParserError> {
        let mut nodes = vec![];
        if self.next_is(end_token) {
            self.advance()?;
            return Ok(nodes);
        }
        loop {
            nodes.push(self.parse_node()?);

            match self.advance()? {
                Token::Comma => {
                    if self.next_is(end_token) {
                        self.advance()?;
                        break;
                    }
                }
                _ => {
                    self.expect_token(end_token)?;
                    break;
                }
            }
        }
        Ok(nodes)
    }

    fn leaf_node(&self, ty: NodeType) -> Node {
        Node::new(ty, self.span(), [])
    }
}

/// A WAVE parsing error.
#[derive(Debug)]
pub struct ParserError {
    kind: ParserErrorKind,
    span: Span,
    detail: Option<String>,
    source: Option<Box<dyn Error + Send + Sync + 'static>>,
}

impl ParserError {
    pub(crate) fn new(kind: ParserErrorKind, span: Span) -> Self {
        Self {
            kind,
            span,
            detail: None,
            source: None,
        }
    }

    pub(crate) fn with_detail(kind: ParserErrorKind, span: Span, detail: impl Display) -> Self {
        Self {
            kind,
            span,
            detail: Some(detail.to_string()),
            source: None,
        }
    }

    pub(crate) fn with_source(
        kind: ParserErrorKind,
        span: Span,
        source: impl Into<Box<dyn Error + Send + Sync>>,
    ) -> Self {
        Self {
            kind,
            span,
            detail: None,
            source: Some(source.into()),
        }
    }

    /// Returns the [`ParserErrorKind`] of this error.
    pub fn kind(&self) -> ParserErrorKind {
        self.kind
    }

    /// Returns the [`Span`] of this error.
    pub fn span(&self) -> Span {
        self.span.clone()
    }

    /// Returns any detail string for this error.
    pub fn detail(&self) -> Option<&str> {
        self.detail.as_deref()
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(source) = &self.source {
            write!(f, "{}: {} at {:?}", self.kind, source, self.span)
        } else if let Some(detail) = &self.detail {
            write!(f, "{}: {} at {:?}", self.kind, detail, self.span)
        } else {
            write!(f, "{} at {:?}", self.kind, self.span)
        }
    }
}

impl Error for ParserError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(self.source.as_deref()? as _)
    }
}

/// The kind of a WAVE parsing error.
#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
#[allow(missing_docs)]
pub enum ParserErrorKind {
    EmptyTuple,
    MultipleChars,
    InvalidEscape,
    InvalidMultilineString,
    InvalidParams,
    InvalidToken,
    InvalidType,
    InvalidValue,
    TrailingCharacters,
    UnexpectedEnd,
    UnexpectedToken,
    DuplicateField,
    DuplicateFlag,
    WasmValueError,
}

impl Display for ParserErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = match self {
            ParserErrorKind::EmptyTuple => "empty tuple",
            ParserErrorKind::MultipleChars => "multiple characters in char value",
            ParserErrorKind::InvalidEscape => "invalid character escape",
            ParserErrorKind::InvalidMultilineString => "invalid multiline string",
            ParserErrorKind::InvalidParams => "invalid params",
            ParserErrorKind::InvalidToken => "invalid token",
            ParserErrorKind::InvalidType => "invalid value type",
            ParserErrorKind::InvalidValue => "invalid value",
            ParserErrorKind::TrailingCharacters => "trailing characters after value",
            ParserErrorKind::UnexpectedEnd => "unexpected end of input",
            ParserErrorKind::UnexpectedToken => "unexpected token",
            ParserErrorKind::DuplicateField => "duplicate field",
            ParserErrorKind::DuplicateFlag => "duplicate flag",
            ParserErrorKind::WasmValueError => "error converting Wasm value",
        };
        write!(f, "{msg}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::{Type, Value};

    #[test]
    fn parse_option_or_result() {
        let ty = Type::option(Type::BOOL);
        assert_eq!(
            parse_value("some(true)", &ty),
            Value::make_option(&ty, Some(Value::make_bool(true))).unwrap()
        );
        let ty = Type::result(Some(Type::BOOL), None);
        assert_eq!(
            parse_value("ok(false)", &ty),
            Value::make_result(&ty, Ok(Some(Value::make_bool(false)))).unwrap()
        );
    }

    #[test]
    fn parse_flat_option_or_result() {
        let ty = Type::option(Type::BOOL);
        assert_eq!(
            parse_value("true", &ty),
            Value::make_option(&ty, Some(Value::make_bool(true))).unwrap()
        );
        let ty = Type::result(Some(Type::BOOL), None);
        assert_eq!(
            parse_value("false", &ty),
            Value::make_result(&ty, Ok(Some(Value::make_bool(false)))).unwrap()
        );
    }

    #[test]
    fn parse_record_reordering() {
        let ty = Type::record([("red", Type::S32), ("green", Type::CHAR)]).unwrap();
        // Parse the fields in the order they appear in the type.
        assert_eq!(
            parse_value("{red: 0, green: 'a'}", &ty),
            Value::make_record(
                &ty,
                [
                    ("red", Value::make_s32(0)),
                    ("green", Value::make_char('a'))
                ]
            )
            .unwrap()
        );
        // Parse the fields in reverse order.
        assert_eq!(
            parse_value("{green: 'a', red: 0}", &ty),
            Value::make_record(
                &ty,
                [
                    ("red", Value::make_s32(0)),
                    ("green", Value::make_char('a'))
                ]
            )
            .unwrap()
        );
    }

    #[test]
    fn parse_record_with_optional_fields() {
        let field_ty = Type::option(Type::CHAR);
        let ty = Type::record([("red", Type::S32), ("green", field_ty.clone())]).unwrap();
        // Explicit `some`.
        assert_eq!(
            parse_value("{red: 0, green: some('a')}", &ty),
            Value::make_record(
                &ty,
                [
                    ("red", Value::make_s32(0)),
                    (
                        "green",
                        Value::make_option(&field_ty, Some(Value::make_char('a'))).unwrap()
                    )
                ]
            )
            .unwrap()
        );
        // Flattened `some`.
        assert_eq!(
            parse_value("{red: 0, green: 'a'}", &ty),
            Value::make_record(
                &ty,
                [
                    ("red", Value::make_s32(0)),
                    (
                        "green",
                        Value::make_option(&field_ty, Some(Value::make_char('a'))).unwrap()
                    )
                ]
            )
            .unwrap()
        );
        // Explicit `none`.
        assert_eq!(
            parse_value("{red: 0, green: none}", &ty),
            Value::make_record(
                &ty,
                [
                    ("red", Value::make_s32(0)),
                    ("green", Value::make_option(&field_ty, None).unwrap())
                ]
            )
            .unwrap()
        );
        // Implied `none`.
        assert_eq!(
            parse_value("{red: 0}", &ty),
            Value::make_record(
                &ty,
                [
                    ("red", Value::make_s32(0)),
                    ("green", Value::make_option(&field_ty, None).unwrap())
                ]
            )
            .unwrap()
        );
    }

    #[test]
    fn parse_flag_reordering() {
        let ty = Type::flags(["hot", "cold"]).unwrap();
        // Parse the flags in the order they appear in the type.
        assert_eq!(
            parse_value("{hot, cold}", &ty),
            Value::make_flags(&ty, ["hot", "cold"]).unwrap()
        );
        // Parse the flags in reverse order.
        assert_eq!(
            parse_value("{cold, hot}", &ty),
            Value::make_flags(&ty, ["hot", "cold"]).unwrap()
        );
    }

    #[test]
    fn parse_percent_identifiers() {
        let ty = Type::record([("red", Type::S32), ("green", Type::CHAR)]).unwrap();
        // Test identifiers with '%' prefixes.
        assert_eq!(
            parse_value("{ %red: 0, %green: 'a' }", &ty),
            Value::make_record(
                &ty,
                [
                    ("red", Value::make_s32(0)),
                    ("green", Value::make_char('a'))
                ]
            )
            .unwrap()
        );
    }

    #[test]
    fn parse_prefixed_keyword_variant_cases() {
        let ty = Type::list(
            Type::variant([
                ("true", Some(Type::U8)),
                ("false", None),
                ("inf", Some(Type::U8)),
                ("nan", None),
                ("some", Some(Type::U8)),
                ("none", None),
                ("ok", Some(Type::U8)),
                ("err", None),
            ])
            .unwrap(),
        );
        parse_value(
            "[%true(1), %false, %inf(1), %nan, %some(1), %none, %ok(1), %err]",
            &ty,
        );
    }

    #[test]
    fn reject_unprefixed_keyword_enum_cases() {
        let cases = ["true", "false", "inf", "nan", "none", "ok", "err"];
        let ty = Type::enum_ty(cases).unwrap();
        for case in cases {
            let err = Parser::new(case).parse_value::<Value>(&ty).unwrap_err();
            assert_eq!(err.kind(), ParserErrorKind::InvalidType);
        }
    }

    #[test]
    fn parse_unprefixed_keyword_fields() {
        let ty = Type::record([
            ("true", Type::U8),
            ("false", Type::U8),
            ("inf", Type::U8),
            ("nan", Type::U8),
            ("some", Type::U8),
            ("none", Type::U8),
            ("ok", Type::U8),
            ("err", Type::U8),
        ])
        .unwrap();
        parse_value(
            "{true: 1, false: 1, inf: 1, nan: 1, some: 1, none: 1, ok: 1, err: 1}",
            &ty,
        );
    }

    #[test]
    fn parse_unprefixed_keyword_flags() {
        let ty = Type::flags(["true", "false", "inf", "nan", "some", "none", "ok", "err"]).unwrap();
        parse_value("{true, false, inf, nan, some, none, ok, err}", &ty);
    }

    #[test]
    fn reject_unprefixed_some_variant_case() {
        let ty = Type::variant([("some", Some(Type::U8))]).unwrap();
        let err = Parser::new("some(1)")
            .parse_value::<Value>(&ty)
            .unwrap_err();
        assert_eq!(err.kind(), ParserErrorKind::InvalidType);
    }

    fn parse_value(input: &str, ty: &Type) -> Value {
        Parser::new(input)
            .parse_value(ty)
            .unwrap_or_else(|err| panic!("error decoding {input:?}: {err}"))
    }
}
