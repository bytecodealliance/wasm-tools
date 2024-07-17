//! Abstract syntax tree types

use std::{borrow::Cow, collections::HashMap, str::FromStr};

use crate::{
    lex::Span,
    parser::{ParserError, ParserErrorKind},
    strings::{unescape, StringPartsIter},
    wasm::{WasmType, WasmTypeKind, WasmValue, WasmValueError},
};

/// A WAVE AST node.
#[derive(Clone, Debug)]
pub struct Node {
    ty: NodeType,
    span: Span,
    children: Vec<Node>,
}

impl Node {
    pub(crate) fn new(
        ty: NodeType,
        span: impl Into<Span>,
        children: impl IntoIterator<Item = Node>,
    ) -> Self {
        Self {
            ty,
            span: span.into(),
            children: Vec::from_iter(children),
        }
    }

    /// Returns this node's type.
    pub fn ty(&self) -> NodeType {
        self.ty
    }

    /// Returns this node's span.
    pub fn span(&self) -> Span {
        self.span.clone()
    }

    /// Returns a bool value if this node represents a bool.
    pub fn as_bool(&self) -> Result<bool, ParserError> {
        match self.ty {
            NodeType::BoolTrue => Ok(true),
            NodeType::BoolFalse => Ok(false),
            _ => Err(self.error(ParserErrorKind::InvalidType)),
        }
    }

    /// Returns a number value of the given type (integer or float) if this node
    /// can represent a number of that type.
    pub fn as_number<T: FromStr>(&self, src: &str) -> Result<T, ParserError> {
        self.ensure_type(NodeType::Number)?;
        self.slice(src)
            .parse()
            .map_err(|_| self.error(ParserErrorKind::InvalidValue))
    }

    /// Returns a char value if this node represents a valid char.
    pub fn as_char(&self, src: &str) -> Result<char, ParserError> {
        self.ensure_type(NodeType::Char)?;
        let inner = &src[self.span.start + 1..self.span.end - 1];
        let (ch, len) = if inner.starts_with('\\') {
            unescape(inner).ok_or_else(|| self.error(ParserErrorKind::InvalidEscape))?
        } else {
            let ch = inner.chars().next().unwrap();
            (ch, ch.len_utf8())
        };
        // Verify length
        if len != inner.len() {
            return Err(self.error(ParserErrorKind::MultipleChars));
        }
        Ok(ch)
    }

    /// Returns a str value if this node represents a valid string.
    pub fn as_str<'src>(&self, src: &'src str) -> Result<Cow<'src, str>, ParserError> {
        let mut parts = self.iter_str(src)?;
        let Some(first) = parts.next().transpose()? else {
            return Ok("".into());
        };
        match parts.next().transpose()? {
            // Single part may be borrowed
            None => Ok(first),
            // Multiple parts must be collected into a single owned String
            Some(second) => {
                let s: String = [Ok(first), Ok(second)]
                    .into_iter()
                    .chain(parts)
                    .collect::<Result<_, _>>()?;
                Ok(s.into())
            }
        }
    }

    /// Returns an iterator of string "parts" which together form a decoded
    /// string value if this node represents a valid string.
    pub fn iter_str<'src>(
        &self,
        src: &'src str,
    ) -> Result<impl Iterator<Item = Result<Cow<'src, str>, ParserError>>, ParserError> {
        match self.ty {
            NodeType::String => {
                let span = self.span.start + 1..self.span.end - 1;
                Ok(StringPartsIter::new(&src[span.clone()], span.start))
            }
            NodeType::MultilineString => {
                let span = self.span.start + 3..self.span.end - 3;
                Ok(StringPartsIter::new_multiline(
                    &src[span.clone()],
                    span.start,
                )?)
            }
            _ => Err(self.error(ParserErrorKind::InvalidType)),
        }
    }

    /// Returns an iterator of value nodes if this node represents a tuple.
    pub fn as_tuple(&self) -> Result<impl ExactSizeIterator<Item = &Node>, ParserError> {
        self.ensure_type(NodeType::Tuple)?;
        Ok(self.children.iter())
    }

    /// Returns an iterator of value nodes if this node represents a list.
    pub fn as_list(&self) -> Result<impl ExactSizeIterator<Item = &Node>, ParserError> {
        self.ensure_type(NodeType::List)?;
        Ok(self.children.iter())
    }

    /// Returns an iterator of field name and value node pairs if this node
    /// represents a record value.
    pub fn as_record<'this, 'src>(
        &'this self,
        src: &'src str,
    ) -> Result<impl ExactSizeIterator<Item = (&'src str, &'this Node)>, ParserError> {
        self.ensure_type(NodeType::Record)?;
        Ok(self
            .children
            .chunks(2)
            .map(|chunk| (chunk[0].as_label(src).unwrap(), &chunk[1])))
    }

    /// Returns a variant label and optional payload if this node can represent
    /// a variant value.
    pub fn as_variant<'this, 'src>(
        &'this self,
        src: &'src str,
    ) -> Result<(&'src str, Option<&'this Node>), ParserError> {
        match self.ty {
            NodeType::Label => Ok((self.as_label(src)?, None)),
            NodeType::VariantWithPayload => {
                let label = self.children[0].as_label(src)?;
                let value = &self.children[1];
                Ok((label, Some(value)))
            }
            _ => Err(self.error(ParserErrorKind::InvalidType)),
        }
    }

    /// Returns an enum value label if this node represents a label.
    pub fn as_enum<'src>(&self, src: &'src str) -> Result<&'src str, ParserError> {
        self.as_label(src)
    }

    /// Returns an option value if this node represents an option.
    pub fn as_option(&self) -> Result<Option<&Node>, ParserError> {
        match self.ty {
            NodeType::OptionSome => Ok(Some(&self.children[0])),
            NodeType::OptionNone => Ok(None),
            _ => Err(self.error(ParserErrorKind::InvalidType)),
        }
    }

    /// Returns a result value with optional payload value if this node
    /// represents a result.
    pub fn as_result(&self) -> Result<Result<Option<&Node>, Option<&Node>>, ParserError> {
        let payload = self.children.first();
        match self.ty {
            NodeType::ResultOk => Ok(Ok(payload)),
            NodeType::ResultErr => Ok(Err(payload)),
            _ => Err(self.error(ParserErrorKind::InvalidType)),
        }
    }

    /// Returns an iterator of flag labels if this node represents flags.
    pub fn as_flags<'this, 'src: 'this>(
        &'this self,
        src: &'src str,
    ) -> Result<impl Iterator<Item = &'src str> + 'this, ParserError> {
        self.ensure_type(NodeType::Flags)?;
        Ok(self.children.iter().map(|node| {
            debug_assert_eq!(node.ty, NodeType::Label);
            node.slice(src)
        }))
    }

    fn as_label<'src>(&self, src: &'src str) -> Result<&'src str, ParserError> {
        self.ensure_type(NodeType::Label)?;
        let label = self.slice(src);
        let label = label.strip_prefix('%').unwrap_or(label);
        Ok(label)
    }

    /// Converts this node into the given typed value from the given input source.
    pub fn to_wasm_value<V: WasmValue>(&self, ty: &V::Type, src: &str) -> Result<V, ParserError> {
        Ok(match ty.kind() {
            WasmTypeKind::Bool => V::make_bool(self.as_bool()?),
            WasmTypeKind::S8 => V::make_s8(self.as_number(src)?),
            WasmTypeKind::S16 => V::make_s16(self.as_number(src)?),
            WasmTypeKind::S32 => V::make_s32(self.as_number(src)?),
            WasmTypeKind::S64 => V::make_s64(self.as_number(src)?),
            WasmTypeKind::U8 => V::make_u8(self.as_number(src)?),
            WasmTypeKind::U16 => V::make_u16(self.as_number(src)?),
            WasmTypeKind::U32 => V::make_u32(self.as_number(src)?),
            WasmTypeKind::U64 => V::make_u64(self.as_number(src)?),
            WasmTypeKind::Float32 => V::make_float32(self.as_number(src)?),
            WasmTypeKind::Float64 => V::make_float64(self.as_number(src)?),
            WasmTypeKind::Char => V::make_char(self.as_char(src)?),
            WasmTypeKind::String => V::make_string(self.as_str(src)?),
            WasmTypeKind::List => self.to_wasm_list(ty, src)?,
            WasmTypeKind::Record => self.to_wasm_record(ty, src)?,
            WasmTypeKind::Tuple => self.to_wasm_tuple(ty, src)?,
            WasmTypeKind::Variant => self.to_wasm_variant(ty, src)?,
            WasmTypeKind::Enum => self.to_wasm_enum(ty, src)?,
            WasmTypeKind::Option => self.to_wasm_option(ty, src)?,
            WasmTypeKind::Result => self.to_wasm_result(ty, src)?,
            WasmTypeKind::Flags => self.to_wasm_flags(ty, src)?,
            other => {
                return Err(
                    self.wasm_value_error(WasmValueError::UnsupportedType(other.to_string()))
                )
            }
        })
    }

    /// Converts this node into the given types.
    /// See [`crate::untyped::UntypedFuncCall::to_wasm_params`].
    pub fn to_wasm_params<'types, V: WasmValue + 'static>(
        &self,
        types: impl IntoIterator<Item = &'types V::Type>,
        src: &str,
    ) -> Result<Vec<V>, ParserError> {
        let mut types = types.into_iter();
        let mut values = self
            .as_tuple()?
            .map(|node| {
                let ty = types.next().ok_or_else(|| {
                    ParserError::with_detail(
                        ParserErrorKind::InvalidParams,
                        node.span().clone(),
                        "more param(s) than expected",
                    )
                })?;
                node.to_wasm_value::<V>(ty, src)
            })
            .collect::<Result<Vec<_>, _>>()?;
        // Handle trailing optional fields
        for ty in types {
            if ty.kind() == WasmTypeKind::Option {
                values.push(V::make_option(ty, None).map_err(|err| self.wasm_value_error(err))?);
            } else {
                return Err(ParserError::with_detail(
                    ParserErrorKind::InvalidParams,
                    self.span.end - 1..self.span.end,
                    "missing required param(s)",
                ));
            }
        }
        Ok(values)
    }

    fn to_wasm_list<V: WasmValue>(&self, ty: &V::Type, src: &str) -> Result<V, ParserError> {
        let element_type = ty.list_element_type().unwrap();
        let elements = self
            .as_list()?
            .map(|node| node.to_wasm_value(&element_type, src))
            .collect::<Result<Vec<_>, _>>()?;
        V::make_list(ty, elements).map_err(|err| self.wasm_value_error(err))
    }

    fn to_wasm_record<V: WasmValue>(&self, ty: &V::Type, src: &str) -> Result<V, ParserError> {
        let values = self.as_record(src)?.collect::<HashMap<_, _>>();
        let record_fields = ty.record_fields().collect::<Vec<_>>();
        let fields = record_fields
            .iter()
            .map(|(name, field_type)| {
                let value = match (values.get(name.as_ref()), field_type.kind()) {
                    (Some(node), _) => node.to_wasm_value(field_type, src)?,
                    (None, WasmTypeKind::Option) => V::make_option(field_type, None)
                        .map_err(|err| self.wasm_value_error(err))?,
                    _ => {
                        return Err(
                            self.wasm_value_error(WasmValueError::MissingField(name.to_string()))
                        );
                    }
                };
                Ok((name.as_ref(), value))
            })
            .collect::<Result<Vec<_>, _>>()?;
        V::make_record(ty, fields).map_err(|err| self.wasm_value_error(err))
    }

    fn to_wasm_tuple<V: WasmValue>(&self, ty: &V::Type, src: &str) -> Result<V, ParserError> {
        let types = ty.tuple_element_types().collect::<Vec<_>>();
        let values = self.as_tuple()?;
        if types.len() != values.len() {
            return Err(
                self.wasm_value_error(WasmValueError::WrongNumberOfTupleValues {
                    want: types.len(),
                    got: values.len(),
                }),
            );
        }
        let values = ty
            .tuple_element_types()
            .zip(self.as_tuple()?)
            .map(|(ty, node)| node.to_wasm_value(&ty, src))
            .collect::<Result<Vec<_>, _>>()?;
        V::make_tuple(ty, values).map_err(|err| self.wasm_value_error(err))
    }

    fn to_wasm_variant<V: WasmValue>(&self, ty: &V::Type, src: &str) -> Result<V, ParserError> {
        let (label, payload) = self.as_variant(src)?;
        let payload_type = ty
            .variant_cases()
            .find_map(|(case, payload)| (case == label).then_some(payload))
            .ok_or_else(|| self.wasm_value_error(WasmValueError::UnknownCase(label.into())))?;
        let payload_value = self.to_wasm_maybe_payload(label, &payload_type, payload, src)?;
        V::make_variant(ty, label, payload_value).map_err(|err| self.wasm_value_error(err))
    }

    fn to_wasm_enum<V: WasmValue>(&self, ty: &V::Type, src: &str) -> Result<V, ParserError> {
        V::make_enum(ty, self.as_enum(src)?).map_err(|err| self.wasm_value_error(err))
    }

    fn to_wasm_option<V: WasmValue>(&self, ty: &V::Type, src: &str) -> Result<V, ParserError> {
        let payload_type = ty.option_some_type().unwrap();
        let value = match self.ty {
            NodeType::OptionSome => {
                self.to_wasm_maybe_payload("some", &Some(payload_type), self.as_option()?, src)?
            }
            NodeType::OptionNone => {
                self.to_wasm_maybe_payload("none", &None, self.as_option()?, src)?
            }
            _ if flattenable(payload_type.kind()) => Some(self.to_wasm_value(&payload_type, src)?),
            _ => {
                return Err(self.error(ParserErrorKind::InvalidType));
            }
        };
        V::make_option(ty, value).map_err(|err| self.wasm_value_error(err))
    }

    fn to_wasm_result<V: WasmValue>(&self, ty: &V::Type, src: &str) -> Result<V, ParserError> {
        let (ok_type, err_type) = ty.result_types().unwrap();
        let value = match self.ty {
            NodeType::ResultOk => {
                Ok(self.to_wasm_maybe_payload("ok", &ok_type, self.as_result()?.unwrap(), src)?)
            }
            NodeType::ResultErr => Err(self.to_wasm_maybe_payload(
                "err",
                &err_type,
                self.as_result()?.unwrap_err(),
                src,
            )?),
            _ => match ok_type {
                Some(ty) if flattenable(ty.kind()) => Ok(Some(self.to_wasm_value(&ty, src)?)),
                _ => return Err(self.error(ParserErrorKind::InvalidType)),
            },
        };
        V::make_result(ty, value).map_err(|err| self.wasm_value_error(err))
    }

    fn to_wasm_flags<V: WasmValue>(&self, ty: &V::Type, src: &str) -> Result<V, ParserError> {
        V::make_flags(ty, self.as_flags(src)?).map_err(|err| self.wasm_value_error(err))
    }

    fn to_wasm_maybe_payload<V: WasmValue>(
        &self,
        case: &str,
        payload_type: &Option<V::Type>,
        payload: Option<&Node>,
        src: &str,
    ) -> Result<Option<V>, ParserError> {
        match (payload_type.as_ref(), payload) {
            (Some(ty), Some(node)) => Ok(Some(node.to_wasm_value(ty, src)?)),
            (None, None) => Ok(None),
            (Some(_), None) => {
                Err(self.wasm_value_error(WasmValueError::MissingPayload(case.into())))
            }
            (None, Some(_)) => {
                Err(self.wasm_value_error(WasmValueError::UnexpectedPayload(case.into())))
            }
        }
    }

    fn wasm_value_error(&self, err: WasmValueError) -> ParserError {
        ParserError::with_source(ParserErrorKind::WasmValueError, self.span(), err)
    }

    pub(crate) fn slice<'src>(&self, src: &'src str) -> &'src str {
        &src[self.span()]
    }

    fn ensure_type(&self, ty: NodeType) -> Result<(), ParserError> {
        if self.ty == ty {
            Ok(())
        } else {
            Err(self.error(ParserErrorKind::InvalidType))
        }
    }

    fn error(&self, kind: ParserErrorKind) -> ParserError {
        ParserError::new(kind, self.span())
    }
}

fn flattenable(kind: WasmTypeKind) -> bool {
    // TODO: Consider wither to allow flattening an option in a result or vice-versa.
    !matches!(kind, WasmTypeKind::Option | WasmTypeKind::Result)
}

/// The type of a WAVE AST [`Node`].
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NodeType {
    /// Boolean `true`
    BoolTrue,
    /// Boolean `false`
    BoolFalse,
    /// Number
    /// May be an integer or float, including `nan`, `inf`, `-inf`
    Number,
    /// Char
    /// Span includes delimiters.
    Char,
    /// String
    /// Span includes delimiters.
    String,
    /// Multiline String
    /// Span includes delimiters.
    MultilineString,
    /// Tuple
    /// Child nodes are the tuple values.
    Tuple,
    /// List
    /// Child nodes are the list values.
    List,
    /// Record
    /// Child nodes are field Label, value pairs, e.g.
    /// `[<field 1>, <value 1>, <field 2>, <value 2>, ...]`
    Record,
    /// Label
    /// In value position may represent an enum value or variant case (without payload).
    Label,
    /// Variant case with payload
    /// Child nodes are variant case Label and payload value.
    VariantWithPayload,
    /// Option `some`
    /// Child node is the payload value.
    OptionSome,
    /// Option `none`
    OptionNone,
    /// Result `ok`
    /// Has zero or one child node for the payload value.
    ResultOk,
    /// Result `err`
    /// Has zero or one child node for the payload value.
    ResultErr,
    /// Flags
    /// Child nodes are flag Labels.
    Flags,
}
