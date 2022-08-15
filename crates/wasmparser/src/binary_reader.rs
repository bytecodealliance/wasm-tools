/* Copyright 2018 Mozilla Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use crate::{limits::*, *};
use std::convert::TryInto;
use std::error::Error;
use std::fmt;
use std::ops::Range;
use std::str;

fn is_name(name: &str, expected: &'static str) -> bool {
    name == expected
}

fn is_name_prefix(name: &str, prefix: &'static str) -> bool {
    name.starts_with(prefix)
}

const WASM_MAGIC_NUMBER: &[u8; 4] = b"\0asm";

/// A binary reader for WebAssembly modules.
#[derive(Debug, Clone)]
pub struct BinaryReaderError {
    // Wrap the actual error data in a `Box` so that the error is just one
    // word. This means that we can continue returning small `Result`s in
    // registers.
    pub(crate) inner: Box<BinaryReaderErrorInner>,
}

#[derive(Debug, Clone)]
pub(crate) struct BinaryReaderErrorInner {
    pub(crate) message: String,
    pub(crate) offset: usize,
    pub(crate) needed_hint: Option<usize>,
}

/// The result for `BinaryReader` operations.
pub type Result<T, E = BinaryReaderError> = std::result::Result<T, E>;

impl Error for BinaryReaderError {}

impl fmt::Display for BinaryReaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} (at offset 0x{:x})",
            self.inner.message, self.inner.offset
        )
    }
}

impl BinaryReaderError {
    #[cold]
    pub(crate) fn new(message: impl Into<String>, offset: usize) -> Self {
        let message = message.into();
        BinaryReaderError {
            inner: Box::new(BinaryReaderErrorInner {
                message,
                offset,
                needed_hint: None,
            }),
        }
    }

    #[cold]
    pub(crate) fn eof(offset: usize, needed_hint: usize) -> Self {
        BinaryReaderError {
            inner: Box::new(BinaryReaderErrorInner {
                message: "unexpected end-of-file".to_string(),
                offset,
                needed_hint: Some(needed_hint),
            }),
        }
    }

    /// Get this error's message.
    pub fn message(&self) -> &str {
        &self.inner.message
    }

    /// Get the offset within the Wasm binary where the error occurred.
    pub fn offset(&self) -> usize {
        self.inner.offset
    }
}

/// A binary reader of the WebAssembly structures and types.
#[derive(Clone, Debug, Hash)]
pub struct BinaryReader<'a> {
    pub(crate) buffer: &'a [u8],
    pub(crate) position: usize,
    pub(crate) original_offset: usize,
    allow_memarg64: bool,
}

impl<'a> BinaryReader<'a> {
    /// Constructs `BinaryReader` type.
    ///
    /// # Examples
    /// ```
    /// let fn_body = &vec![0x41, 0x00, 0x10, 0x00, 0x0B];
    /// let mut reader = wasmparser::BinaryReader::new(fn_body);
    /// while !reader.eof() {
    ///     let op = reader.read_operator();
    ///     println!("{:?}", op)
    /// }
    /// ```
    pub fn new(data: &[u8]) -> BinaryReader {
        BinaryReader {
            buffer: data,
            position: 0,
            original_offset: 0,
            allow_memarg64: false,
        }
    }

    /// Constructs a `BinaryReader` with an explicit starting offset.
    pub fn new_with_offset(data: &[u8], original_offset: usize) -> BinaryReader {
        BinaryReader {
            buffer: data,
            position: 0,
            original_offset,
            allow_memarg64: false,
        }
    }

    /// Gets the original position of the binary reader.
    pub fn original_position(&self) -> usize {
        self.original_offset + self.position
    }

    /// Whether or not to allow 64-bit memory arguments in functions.
    ///
    /// This is intended to be `true` when support for the memory64
    /// WebAssembly proposal is also enabled.
    pub fn allow_memarg64(&mut self, allow: bool) {
        self.allow_memarg64 = allow;
    }

    /// Returns a range from the starting offset to the end of the buffer.
    pub fn range(&self) -> Range<usize> {
        self.original_offset..self.original_offset + self.buffer.len()
    }

    pub(crate) fn remaining_buffer(&self) -> &'a [u8] {
        &self.buffer[self.position..]
    }

    fn ensure_has_byte(&self) -> Result<()> {
        if self.position < self.buffer.len() {
            Ok(())
        } else {
            Err(BinaryReaderError::eof(self.original_position(), 1))
        }
    }

    pub(crate) fn ensure_has_bytes(&self, len: usize) -> Result<()> {
        if self.position + len <= self.buffer.len() {
            Ok(())
        } else {
            let hint = self.position + len - self.buffer.len();
            Err(BinaryReaderError::eof(self.original_position(), hint))
        }
    }

    pub(crate) fn read_u7(&mut self) -> Result<u8> {
        let b = self.read_u8()?;
        if (b & 0x80) != 0 {
            return Err(BinaryReaderError::new(
                "invalid u7",
                self.original_position() - 1,
            ));
        }
        Ok(b)
    }

    /// Reads a core WebAssembly value type from the binary reader.
    pub fn read_val_type(&mut self) -> Result<ValType> {
        match Self::val_type_from_byte(self.peek()?) {
            Some(ty) => {
                self.position += 1;
                Ok(ty)
            }
            None => Err(BinaryReaderError::new(
                "invalid value type",
                self.original_position(),
            )),
        }
    }

    pub(crate) fn read_component_start(&mut self) -> Result<ComponentStartFunction> {
        let func_index = self.read_var_u32()?;
        let size = self.read_size(MAX_WASM_START_ARGS, "start function arguments")?;
        Ok(ComponentStartFunction {
            func_index,
            arguments: (0..size)
                .map(|_| self.read_var_u32())
                .collect::<Result<_>>()?,
        })
    }

    fn external_kind_from_byte(byte: u8, offset: usize) -> Result<ExternalKind> {
        match byte {
            0x00 => Ok(ExternalKind::Func),
            0x01 => Ok(ExternalKind::Table),
            0x02 => Ok(ExternalKind::Memory),
            0x03 => Ok(ExternalKind::Global),
            0x04 => Ok(ExternalKind::Tag),
            x => Err(Self::invalid_leading_byte_error(x, "external kind", offset)),
        }
    }

    pub(crate) fn read_external_kind(&mut self) -> Result<ExternalKind> {
        let offset = self.original_position();
        Self::external_kind_from_byte(self.read_u8()?, offset)
    }

    fn component_external_kind_from_bytes(
        byte1: u8,
        byte2: Option<u8>,
        offset: usize,
    ) -> Result<ComponentExternalKind> {
        Ok(match byte1 {
            0x00 => match byte2.unwrap() {
                0x11 => ComponentExternalKind::Module,
                x => {
                    return Err(Self::invalid_leading_byte_error(
                        x,
                        "component external kind",
                        offset + 1,
                    ))
                }
            },
            0x01 => ComponentExternalKind::Func,
            0x02 => ComponentExternalKind::Value,
            0x03 => ComponentExternalKind::Type,
            0x04 => ComponentExternalKind::Component,
            0x05 => ComponentExternalKind::Instance,
            x => {
                return Err(Self::invalid_leading_byte_error(
                    x,
                    "component external kind",
                    offset,
                ))
            }
        })
    }

    pub(crate) fn read_component_external_kind(&mut self) -> Result<ComponentExternalKind> {
        let offset = self.original_position();
        let byte1 = self.read_u8()?;
        let byte2 = if byte1 == 0x00 {
            Some(self.read_u8()?)
        } else {
            None
        };

        Self::component_external_kind_from_bytes(byte1, byte2, offset)
    }

    pub(crate) fn read_func_type(&mut self) -> Result<FuncType> {
        let params_len = self.read_size(MAX_WASM_FUNCTION_PARAMS, "function params")?;
        let mut params = Vec::with_capacity(params_len);
        for _ in 0..params_len {
            params.push(self.read_val_type()?);
        }
        let returns_len = self.read_size(MAX_WASM_FUNCTION_RETURNS, "function returns")?;
        let mut returns = Vec::with_capacity(returns_len);
        for _ in 0..returns_len {
            returns.push(self.read_val_type()?);
        }
        Ok(FuncType {
            params: params.into_boxed_slice(),
            returns: returns.into_boxed_slice(),
        })
    }

    pub(crate) fn read_type(&mut self) -> Result<Type> {
        Ok(match self.read_u8()? {
            0x60 => Type::Func(self.read_func_type()?),
            x => return self.invalid_leading_byte(x, "type"),
        })
    }

    pub(crate) fn read_core_type(&mut self) -> Result<CoreType<'a>> {
        Ok(match self.read_u8()? {
            0x60 => CoreType::Func(self.read_func_type()?),
            0x50 => {
                let size = self.read_size(MAX_WASM_MODULE_TYPE_DECLS, "module type declaration")?;
                CoreType::Module(
                    (0..size)
                        .map(|_| self.read_module_type_decl())
                        .collect::<Result<_>>()?,
                )
            }
            x => return self.invalid_leading_byte(x, "core type"),
        })
    }

    pub(crate) fn read_component_type(&mut self) -> Result<ComponentType<'a>> {
        Ok(match self.read_u8()? {
            0x40 => ComponentType::Func(ComponentFuncType {
                params: self
                    .read_type_vec(MAX_WASM_FUNCTION_PARAMS, "component function parameters")?,
                results: self
                    .read_type_vec(MAX_WASM_FUNCTION_RETURNS, "component function results")?,
            }),
            0x41 => {
                let size =
                    self.read_size(MAX_WASM_COMPONENT_TYPE_DECLS, "component type declaration")?;
                ComponentType::Component(
                    (0..size)
                        .map(|_| self.read_component_type_decl())
                        .collect::<Result<_>>()?,
                )
            }
            0x42 => {
                let size =
                    self.read_size(MAX_WASM_INSTANCE_TYPE_DECLS, "instance type declaration")?;
                ComponentType::Instance(
                    (0..size)
                        .map(|_| self.read_instance_type_decl())
                        .collect::<Result<_>>()?,
                )
            }
            x => {
                if let Some(ty) = Self::primitive_val_type_from_byte(x) {
                    ComponentType::Defined(ComponentDefinedType::Primitive(ty))
                } else {
                    ComponentType::Defined(self.read_component_defined_type(x)?)
                }
            }
        })
    }

    pub(crate) fn read_type_vec(&mut self, max: usize, desc: &str) -> Result<TypeVec<'a>> {
        Ok(match self.read_u8()? {
            0x00 => TypeVec::Unnamed(self.read_component_val_type()?),
            0x01 => {
                let size = self.read_size(max, desc)?;
                TypeVec::Named(
                    (0..size)
                        .map(|_| Ok((self.read_string()?, self.read_component_val_type()?)))
                        .collect::<Result<_>>()?,
                )
            }
            x => return self.invalid_leading_byte(x, desc),
        })
    }

    pub(crate) fn read_module_type_decl(&mut self) -> Result<ModuleTypeDeclaration<'a>> {
        Ok(match self.read_u8()? {
            0x00 => ModuleTypeDeclaration::Import(self.read_import()?),
            0x01 => ModuleTypeDeclaration::Type(self.read_type()?),
            0x02 => ModuleTypeDeclaration::Alias(self.read_alias()?),
            0x03 => ModuleTypeDeclaration::Export {
                name: self.read_string()?,
                ty: self.read_type_ref()?,
            },
            x => return self.invalid_leading_byte(x, "type definition"),
        })
    }

    pub(crate) fn read_component_type_decl(&mut self) -> Result<ComponentTypeDeclaration<'a>> {
        // Component types are effectively instance types with the additional
        // variant of imports; check for imports here or delegate to
        // `read_instance_type_decl` with the appropriate conversions.
        if self.peek()? == 0x03 {
            self.position += 1;
            return Ok(ComponentTypeDeclaration::Import(
                self.read_component_import()?,
            ));
        }

        Ok(match self.read_instance_type_decl()? {
            InstanceTypeDeclaration::CoreType(t) => ComponentTypeDeclaration::CoreType(t),
            InstanceTypeDeclaration::Type(t) => ComponentTypeDeclaration::Type(t),
            InstanceTypeDeclaration::Alias(a) => ComponentTypeDeclaration::Alias(a),
            InstanceTypeDeclaration::Export { name, ty } => {
                ComponentTypeDeclaration::Export { name, ty }
            }
        })
    }

    pub(crate) fn read_instance_type_decl(&mut self) -> Result<InstanceTypeDeclaration<'a>> {
        Ok(match self.read_u8()? {
            0x00 => InstanceTypeDeclaration::CoreType(self.read_core_type()?),
            0x01 => InstanceTypeDeclaration::Type(self.read_component_type()?),
            0x02 => InstanceTypeDeclaration::Alias(self.read_component_alias()?),
            0x04 => InstanceTypeDeclaration::Export {
                name: self.read_string()?,
                ty: self.read_component_type_ref()?,
            },
            x => return self.invalid_leading_byte(x, "component or instance type declaration"),
        })
    }

    fn primitive_val_type_from_byte(byte: u8) -> Option<PrimitiveValType> {
        Some(match byte {
            0x7f => PrimitiveValType::Bool,
            0x7e => PrimitiveValType::S8,
            0x7d => PrimitiveValType::U8,
            0x7c => PrimitiveValType::S16,
            0x7b => PrimitiveValType::U16,
            0x7a => PrimitiveValType::S32,
            0x79 => PrimitiveValType::U32,
            0x78 => PrimitiveValType::S64,
            0x77 => PrimitiveValType::U64,
            0x76 => PrimitiveValType::Float32,
            0x75 => PrimitiveValType::Float64,
            0x74 => PrimitiveValType::Char,
            0x73 => PrimitiveValType::String,
            _ => return None,
        })
    }

    fn read_variant_case(&mut self) -> Result<VariantCase<'a>> {
        Ok(VariantCase {
            name: self.read_string()?,
            ty: self.read_optional_val_type()?,
            refines: match self.read_u8()? {
                0x0 => None,
                0x1 => Some(self.read_var_u32()?),
                x => return self.invalid_leading_byte(x, "variant case refines"),
            },
        })
    }

    fn read_component_val_type(&mut self) -> Result<ComponentValType> {
        if let Some(ty) = Self::primitive_val_type_from_byte(self.peek()?) {
            self.position += 1;
            return Ok(ComponentValType::Primitive(ty));
        }

        Ok(ComponentValType::Type(self.read_var_s33()? as u32))
    }

    fn read_component_defined_type(&mut self, byte: u8) -> Result<ComponentDefinedType<'a>> {
        Ok(match byte {
            0x72 => {
                let size = self.read_size(MAX_WASM_RECORD_FIELDS, "record field")?;
                ComponentDefinedType::Record(
                    (0..size)
                        .map(|_| Ok((self.read_string()?, self.read_component_val_type()?)))
                        .collect::<Result<_>>()?,
                )
            }
            0x71 => {
                let size = self.read_size(MAX_WASM_VARIANT_CASES, "variant cases")?;
                ComponentDefinedType::Variant(
                    (0..size)
                        .map(|_| self.read_variant_case())
                        .collect::<Result<_>>()?,
                )
            }
            0x70 => ComponentDefinedType::List(self.read_component_val_type()?),
            0x6f => {
                let size = self.read_size(MAX_WASM_TUPLE_TYPES, "tuple types")?;
                ComponentDefinedType::Tuple(
                    (0..size)
                        .map(|_| self.read_component_val_type())
                        .collect::<Result<_>>()?,
                )
            }
            0x6e => {
                let size = self.read_size(MAX_WASM_FLAG_NAMES, "flag names")?;
                ComponentDefinedType::Flags(
                    (0..size)
                        .map(|_| self.read_string())
                        .collect::<Result<_>>()?,
                )
            }
            0x6d => {
                let size = self.read_size(MAX_WASM_ENUM_CASES, "enum cases")?;
                ComponentDefinedType::Enum(
                    (0..size)
                        .map(|_| self.read_string())
                        .collect::<Result<_>>()?,
                )
            }
            0x6c => {
                let size = self.read_size(MAX_WASM_UNION_TYPES, "union types")?;
                ComponentDefinedType::Union(
                    (0..size)
                        .map(|_| self.read_component_val_type())
                        .collect::<Result<_>>()?,
                )
            }
            0x6b => ComponentDefinedType::Option(self.read_component_val_type()?),
            0x6a => ComponentDefinedType::Result {
                ok: self.read_optional_val_type()?,
                err: self.read_optional_val_type()?,
            },
            x => return self.invalid_leading_byte(x, "component defined type"),
        })
    }

    pub(crate) fn read_export(&mut self) -> Result<Export<'a>> {
        Ok(Export {
            name: self.read_string()?,
            kind: self.read_external_kind()?,
            index: self.read_var_u32()?,
        })
    }

    pub(crate) fn read_component_export(&mut self) -> Result<ComponentExport<'a>> {
        Ok(ComponentExport {
            name: self.read_string()?,
            kind: self.read_component_external_kind()?,
            index: self.read_var_u32()?,
        })
    }

    pub(crate) fn read_import(&mut self) -> Result<Import<'a>> {
        Ok(Import {
            module: self.read_string()?,
            name: self.read_string()?,
            ty: self.read_type_ref()?,
        })
    }

    pub(crate) fn read_component_import(&mut self) -> Result<ComponentImport<'a>> {
        Ok(ComponentImport {
            name: self.read_string()?,
            ty: self.read_component_type_ref()?,
        })
    }

    pub(crate) fn read_component_type_ref(&mut self) -> Result<ComponentTypeRef> {
        Ok(match self.read_component_external_kind()? {
            ComponentExternalKind::Module => ComponentTypeRef::Module(self.read_var_u32()?),
            ComponentExternalKind::Func => ComponentTypeRef::Func(self.read_var_u32()?),
            ComponentExternalKind::Value => {
                ComponentTypeRef::Value(self.read_component_val_type()?)
            }
            ComponentExternalKind::Type => {
                ComponentTypeRef::Type(self.read_type_bounds()?, self.read_var_u32()?)
            }
            ComponentExternalKind::Instance => ComponentTypeRef::Instance(self.read_var_u32()?),
            ComponentExternalKind::Component => ComponentTypeRef::Component(self.read_var_u32()?),
        })
    }

    pub(crate) fn read_type_bounds(&mut self) -> Result<TypeBounds> {
        Ok(match self.read_u8()? {
            0x00 => TypeBounds::Eq,
            x => return self.invalid_leading_byte(x, "type bound"),
        })
    }

    pub(crate) fn read_canonical_func(&mut self) -> Result<CanonicalFunction> {
        Ok(match self.read_u8()? {
            0x00 => match self.read_u8()? {
                0x00 => CanonicalFunction::Lift {
                    core_func_index: self.read_var_u32()?,
                    options: (0..self
                        .read_size(MAX_WASM_CANONICAL_OPTIONS, "canonical options")?)
                        .map(|_| self.read_canonical_option())
                        .collect::<Result<_>>()?,
                    type_index: self.read_var_u32()?,
                },
                x => return self.invalid_leading_byte(x, "canonical function lift"),
            },
            0x01 => match self.read_u8()? {
                0x00 => CanonicalFunction::Lower {
                    func_index: self.read_var_u32()?,
                    options: (0..self
                        .read_size(MAX_WASM_CANONICAL_OPTIONS, "canonical options")?)
                        .map(|_| self.read_canonical_option())
                        .collect::<Result<_>>()?,
                },
                x => return self.invalid_leading_byte(x, "canonical function lower"),
            },
            x => return self.invalid_leading_byte(x, "canonical function"),
        })
    }

    pub(crate) fn read_canonical_option(&mut self) -> Result<CanonicalOption> {
        Ok(match self.read_u8()? {
            0x00 => CanonicalOption::UTF8,
            0x01 => CanonicalOption::UTF16,
            0x02 => CanonicalOption::CompactUTF16,
            0x03 => CanonicalOption::Memory(self.read_var_u32()?),
            0x04 => CanonicalOption::Realloc(self.read_var_u32()?),
            0x05 => CanonicalOption::PostReturn(self.read_var_u32()?),
            x => return self.invalid_leading_byte(x, "canonical option"),
        })
    }

    pub(crate) fn read_instance(&mut self) -> Result<Instance<'a>> {
        Ok(match self.read_u8()? {
            0x00 => Instance::Instantiate {
                module_index: self.read_var_u32()?,
                args: (0..self
                    .read_size(MAX_WASM_INSTANTIATION_ARGS, "core instantiation arguments")?)
                    .map(|_| self.read_instantiation_arg())
                    .collect::<Result<_>>()?,
            },
            0x01 => Instance::FromExports(
                (0..self
                    .read_size(MAX_WASM_INSTANTIATION_EXPORTS, "core instantiation exports")?)
                    .map(|_| self.read_export())
                    .collect::<Result<_>>()?,
            ),
            x => return self.invalid_leading_byte(x, "core instance"),
        })
    }

    pub(crate) fn read_component_instance(&mut self) -> Result<ComponentInstance<'a>> {
        Ok(match self.read_u8()? {
            0x00 => ComponentInstance::Instantiate {
                component_index: self.read_var_u32()?,
                args: (0..self
                    .read_size(MAX_WASM_INSTANTIATION_ARGS, "instantiation arguments")?)
                    .map(|_| self.read_component_instantiation_arg())
                    .collect::<Result<_>>()?,
            },
            0x01 => ComponentInstance::FromExports(
                (0..self.read_size(MAX_WASM_INSTANTIATION_EXPORTS, "instantiation exports")?)
                    .map(|_| self.read_component_export())
                    .collect::<Result<_>>()?,
            ),
            x => return self.invalid_leading_byte(x, "instance"),
        })
    }

    pub(crate) fn read_instantiation_arg_kind(&mut self) -> Result<InstantiationArgKind> {
        Ok(match self.read_u8()? {
            0x12 => InstantiationArgKind::Instance,
            x => return self.invalid_leading_byte(x, "instantiation arg kind"),
        })
    }

    pub(crate) fn read_instantiation_arg(&mut self) -> Result<InstantiationArg<'a>> {
        Ok(InstantiationArg {
            name: self.read_string()?,
            kind: self.read_instantiation_arg_kind()?,
            index: self.read_var_u32()?,
        })
    }

    pub(crate) fn read_component_instantiation_arg(
        &mut self,
    ) -> Result<ComponentInstantiationArg<'a>> {
        Ok(ComponentInstantiationArg {
            name: self.read_string()?,
            kind: self.read_component_external_kind()?,
            index: self.read_var_u32()?,
        })
    }

    pub(crate) fn read_alias(&mut self) -> Result<Alias<'a>> {
        let offset = self.original_position();
        let kind = self.read_u8()?;

        Ok(match self.read_u8()? {
            0x00 => Alias::InstanceExport {
                kind: Self::external_kind_from_byte(kind, offset)?,
                instance_index: self.read_var_u32()?,
                name: self.read_string()?,
            },
            0x01 => Alias::Outer {
                kind: match kind {
                    0x10 => OuterAliasKind::Type,
                    x => {
                        return Err(Self::invalid_leading_byte_error(
                            x,
                            "outer alias kind",
                            offset,
                        ))
                    }
                },
                count: self.read_var_u32()?,
                index: self.read_var_u32()?,
            },
            x => return self.invalid_leading_byte(x, "alias"),
        })
    }

    fn component_outer_alias_kind_from_bytes(
        byte1: u8,
        byte2: Option<u8>,
        offset: usize,
    ) -> Result<ComponentOuterAliasKind> {
        Ok(match byte1 {
            0x00 => match byte2.unwrap() {
                0x10 => ComponentOuterAliasKind::CoreType,
                0x11 => ComponentOuterAliasKind::CoreModule,
                x => {
                    return Err(Self::invalid_leading_byte_error(
                        x,
                        "component outer alias kind",
                        offset + 1,
                    ))
                }
            },
            0x03 => ComponentOuterAliasKind::Type,
            0x04 => ComponentOuterAliasKind::Component,
            x => {
                return Err(Self::invalid_leading_byte_error(
                    x,
                    "component outer alias kind",
                    offset,
                ))
            }
        })
    }

    pub(crate) fn read_component_alias(&mut self) -> Result<ComponentAlias<'a>> {
        // We don't know what type of alias it is yet, so just read the sort bytes
        let offset = self.original_position();
        let byte1 = self.read_u8()?;
        let byte2 = if byte1 == 0x00 {
            Some(self.read_u8()?)
        } else {
            None
        };

        Ok(match self.read_u8()? {
            0x00 => ComponentAlias::InstanceExport {
                kind: Self::component_external_kind_from_bytes(byte1, byte2, offset)?,
                instance_index: self.read_var_u32()?,
                name: self.read_string()?,
            },
            0x01 => ComponentAlias::Outer {
                kind: Self::component_outer_alias_kind_from_bytes(byte1, byte2, offset)?,
                count: self.read_var_u32()?,
                index: self.read_var_u32()?,
            },
            x => return self.invalid_leading_byte(x, "alias"),
        })
    }

    pub(crate) fn read_type_ref(&mut self) -> Result<TypeRef> {
        Ok(match self.read_external_kind()? {
            ExternalKind::Func => TypeRef::Func(self.read_var_u32()?),
            ExternalKind::Table => TypeRef::Table(self.read_table_type()?),
            ExternalKind::Memory => TypeRef::Memory(self.read_memory_type()?),
            ExternalKind::Global => TypeRef::Global(self.read_global_type()?),
            ExternalKind::Tag => TypeRef::Tag(self.read_tag_type()?),
        })
    }

    pub(crate) fn read_table_type(&mut self) -> Result<TableType> {
        let element_type = self.read_val_type()?;
        let has_max = match self.read_u8()? {
            0x00 => false,
            0x01 => true,
            _ => {
                return Err(BinaryReaderError::new(
                    "invalid table resizable limits flags",
                    self.original_position() - 1,
                ))
            }
        };
        let initial = self.read_var_u32()?;
        let maximum = if has_max {
            Some(self.read_var_u32()?)
        } else {
            None
        };
        Ok(TableType {
            element_type,
            initial,
            maximum,
        })
    }

    pub(crate) fn read_memory_type(&mut self) -> Result<MemoryType> {
        let pos = self.original_position();
        let flags = self.read_u8()?;
        if (flags & !0b111) != 0 {
            return Err(BinaryReaderError::new("invalid memory limits flags", pos));
        }

        let memory64 = flags & 0b100 != 0;
        let shared = flags & 0b010 != 0;
        let has_max = flags & 0b001 != 0;
        Ok(MemoryType {
            memory64,
            shared,
            // FIXME(WebAssembly/memory64#21) as currently specified if the
            // `shared` flag is set we should be reading a 32-bit limits field
            // here. That seems a bit odd to me at the time of this writing so
            // I've taken the liberty of reading a 64-bit limits field in those
            // situations. I suspect that this is a typo in the spec, but if not
            // we'll need to update this to read a 32-bit limits field when the
            // shared flag is set.
            initial: if memory64 {
                self.read_var_u64()?
            } else {
                self.read_var_u32()?.into()
            },
            maximum: if !has_max {
                None
            } else if memory64 {
                Some(self.read_var_u64()?)
            } else {
                Some(self.read_var_u32()?.into())
            },
        })
    }

    pub(crate) fn read_tag_type(&mut self) -> Result<TagType> {
        let attribute = self.read_u8()?;
        if attribute != 0 {
            return Err(BinaryReaderError::new(
                "invalid tag attributes",
                self.original_position() - 1,
            ));
        }
        Ok(TagType {
            kind: TagKind::Exception,
            func_type_idx: self.read_var_u32()?,
        })
    }

    pub(crate) fn read_global_type(&mut self) -> Result<GlobalType> {
        Ok(GlobalType {
            content_type: self.read_val_type()?,
            mutable: match self.read_u8()? {
                0x00 => false,
                0x01 => true,
                _ => {
                    return Err(BinaryReaderError::new(
                        "malformed mutability",
                        self.original_position() - 1,
                    ))
                }
            },
        })
    }

    // Reads a variable-length 32-bit size from the byte stream while checking
    // against a limit.
    fn read_size(&mut self, limit: usize, desc: &str) -> Result<usize> {
        let size = self.read_var_u32()? as usize;
        if size > limit {
            return Err(BinaryReaderError::new(
                format!("{} size is out of bounds", desc),
                self.original_position() - 4,
            ));
        }
        Ok(size)
    }

    fn read_first_byte_and_var_u32(&mut self) -> Result<(u8, u32)> {
        let pos = self.position;
        let val = self.read_var_u32()?;
        Ok((self.buffer[pos], val))
    }

    fn read_memarg(&mut self) -> Result<MemoryImmediate> {
        let flags_pos = self.original_position();
        let mut flags = self.read_var_u32()?;
        let memory = if flags & (1 << 6) != 0 {
            flags ^= 1 << 6;
            self.read_var_u32()?
        } else {
            0
        };
        let align = if flags >= (1 << 6) {
            return Err(BinaryReaderError::new("alignment too large", flags_pos));
        } else {
            flags as u8
        };
        let offset = if self.allow_memarg64 {
            self.read_var_u64()?
        } else {
            u64::from(self.read_var_u32()?)
        };
        Ok(MemoryImmediate {
            align,
            offset,
            memory,
        })
    }

    pub(crate) fn read_section_code(&mut self, id: u8, offset: usize) -> Result<SectionCode<'a>> {
        match id {
            0 => {
                let name = self.read_string()?;
                let kind = if is_name(name, "name") {
                    CustomSectionKind::Name
                } else if is_name(name, "producers") {
                    CustomSectionKind::Producers
                } else if is_name(name, "sourceMappingURL") {
                    CustomSectionKind::SourceMappingURL
                } else if is_name_prefix(name, "reloc.") {
                    CustomSectionKind::Reloc
                } else if is_name(name, "linking") {
                    CustomSectionKind::Linking
                } else {
                    CustomSectionKind::Unknown
                };
                Ok(SectionCode::Custom { name, kind })
            }
            1 => Ok(SectionCode::Type),
            2 => Ok(SectionCode::Import),
            3 => Ok(SectionCode::Function),
            4 => Ok(SectionCode::Table),
            5 => Ok(SectionCode::Memory),
            6 => Ok(SectionCode::Global),
            7 => Ok(SectionCode::Export),
            8 => Ok(SectionCode::Start),
            9 => Ok(SectionCode::Element),
            10 => Ok(SectionCode::Code),
            11 => Ok(SectionCode::Data),
            12 => Ok(SectionCode::DataCount),
            13 => Ok(SectionCode::Tag),
            _ => Err(BinaryReaderError::new("invalid section code", offset)),
        }
    }

    fn read_br_table(&mut self) -> Result<BrTable<'a>> {
        let cnt = self.read_size(MAX_WASM_BR_TABLE_SIZE, "br_table")?;
        let start = self.position;
        for _ in 0..cnt {
            self.read_var_u32()?;
        }
        let end = self.position;
        let default = self.read_var_u32()?;
        Ok(BrTable {
            reader: BinaryReader::new_with_offset(&self.buffer[start..end], start),
            cnt: cnt as u32,
            default,
        })
    }

    /// Returns whether the `BinaryReader` has reached the end of the file.
    pub fn eof(&self) -> bool {
        self.position >= self.buffer.len()
    }

    /// Returns the `BinaryReader`'s current position.
    pub fn current_position(&self) -> usize {
        self.position
    }

    /// Returns the number of bytes remaining in the `BinaryReader`.
    pub fn bytes_remaining(&self) -> usize {
        self.buffer.len() - self.position
    }

    /// Advances the `BinaryReader` `size` bytes, and returns a slice from the
    /// current position of `size` length.
    ///
    /// # Errors
    /// If `size` exceeds the remaining length in `BinaryReader`.
    pub fn read_bytes(&mut self, size: usize) -> Result<&'a [u8]> {
        self.ensure_has_bytes(size)?;
        let start = self.position;
        self.position += size;
        Ok(&self.buffer[start..self.position])
    }

    /// Advances the `BinaryReader` four bytes and returns a `u32`.
    /// # Errors
    /// If `BinaryReader` has less than four bytes remaining.
    pub fn read_u32(&mut self) -> Result<u32> {
        self.ensure_has_bytes(4)?;
        let word = u32::from_le_bytes(
            self.buffer[self.position..self.position + 4]
                .try_into()
                .unwrap(),
        );
        self.position += 4;
        Ok(word)
    }

    /// Advances the `BinaryReader` eight bytes and returns a `u64`.
    /// # Errors
    /// If `BinaryReader` has less than eight bytes remaining.
    pub fn read_u64(&mut self) -> Result<u64> {
        self.ensure_has_bytes(8)?;
        let word = u64::from_le_bytes(
            self.buffer[self.position..self.position + 8]
                .try_into()
                .unwrap(),
        );
        self.position += 8;
        Ok(word)
    }

    /// Advances the `BinaryReader` a single byte.
    ///
    /// # Errors
    ///
    /// If `BinaryReader` has no bytes remaining.
    pub fn read_u8(&mut self) -> Result<u8> {
        let b = match self.buffer.get(self.position) {
            Some(b) => *b,
            None => return Err(self.eof_err()),
        };
        self.position += 1;
        Ok(b)
    }

    #[cold]
    fn eof_err(&self) -> BinaryReaderError {
        BinaryReaderError::eof(self.original_position(), 1)
    }

    /// Advances the `BinaryReader` up to four bytes to parse a variable
    /// length integer as a `u32`.
    ///
    /// # Errors
    ///
    /// If `BinaryReader` has less than one or up to four bytes remaining, or
    /// the integer is larger than 32 bits.
    pub fn read_var_u32(&mut self) -> Result<u32> {
        // Optimization for single byte i32.
        let byte = self.read_u8()?;
        if (byte & 0x80) == 0 {
            return Ok(byte as u32);
        }

        let mut result = (byte & 0x7F) as u32;
        let mut shift = 7;
        loop {
            let byte = self.read_u8()?;
            result |= ((byte & 0x7F) as u32) << shift;
            if shift >= 25 && (byte >> (32 - shift)) != 0 {
                let msg = if byte & 0x80 != 0 {
                    "invalid var_u32: integer representation too long"
                } else {
                    "invalid var_u32: integer too large"
                };
                // The continuation bit or unused bits are set.
                return Err(BinaryReaderError::new(msg, self.original_position() - 1));
            }
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
        }
        Ok(result)
    }

    /// Advances the `BinaryReader` up to four bytes to parse a variable
    /// length integer as a `u64`.
    ///
    /// # Errors
    ///
    /// If `BinaryReader` has less than one or up to eight bytes remaining, or
    /// the integer is larger than 64 bits.
    pub fn read_var_u64(&mut self) -> Result<u64> {
        // Optimization for single byte u64.
        let byte = u64::from(self.read_u8()?);
        if (byte & 0x80) == 0 {
            return Ok(byte);
        }

        let mut result = byte & 0x7F;
        let mut shift = 7;
        loop {
            let byte = u64::from(self.read_u8()?);
            result |= (byte & 0x7F) << shift;
            if shift >= 57 && (byte >> (64 - shift)) != 0 {
                let msg = if byte & 0x80 != 0 {
                    "invalid var_u64: integer representation too long"
                } else {
                    "invalid var_u64: integer too large"
                };
                // The continuation bit or unused bits are set.
                return Err(BinaryReaderError::new(msg, self.original_position() - 1));
            }
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
        }
        Ok(result)
    }

    /// Advances the `BinaryReader` `len` bytes, skipping the result.
    /// # Errors
    /// If `BinaryReader` has less than `len` bytes remaining.
    pub fn skip_bytes(&mut self, len: usize) -> Result<()> {
        self.ensure_has_bytes(len)?;
        self.position += len;
        Ok(())
    }

    /// Advances the `BinaryReader` past a WebAssembly string. This method does
    /// not perform any utf-8 validation.
    /// # Errors
    /// If `BinaryReader` has less than four bytes, the string's length exceeds
    /// the remaining bytes, or the string length
    /// exceeds `limits::MAX_WASM_STRING_SIZE`.
    pub fn skip_string(&mut self) -> Result<()> {
        let len = self.read_var_u32()? as usize;
        if len > MAX_WASM_STRING_SIZE {
            return Err(BinaryReaderError::new(
                "string size out of bounds",
                self.original_position() - 1,
            ));
        }
        self.skip_bytes(len)
    }

    pub(crate) fn skip_to(&mut self, position: usize) {
        assert!(
            self.position <= position && position <= self.buffer.len(),
            "skip_to allowed only into region past current position"
        );
        self.position = position;
    }

    /// Advances the `BinaryReader` up to four bytes to parse a variable
    /// length integer as a `i32`.
    /// # Errors
    /// If `BinaryReader` has less than one or up to four bytes remaining, or
    /// the integer is larger than 32 bits.
    pub fn read_var_i32(&mut self) -> Result<i32> {
        // Optimization for single byte i32.
        let byte = self.read_u8()?;
        if (byte & 0x80) == 0 {
            return Ok(((byte as i32) << 25) >> 25);
        }

        let mut result = (byte & 0x7F) as i32;
        let mut shift = 7;
        loop {
            let byte = self.read_u8()?;
            result |= ((byte & 0x7F) as i32) << shift;
            if shift >= 25 {
                let continuation_bit = (byte & 0x80) != 0;
                let sign_and_unused_bit = (byte << 1) as i8 >> (32 - shift);
                if continuation_bit || (sign_and_unused_bit != 0 && sign_and_unused_bit != -1) {
                    let msg = if continuation_bit {
                        "invalid var_i32: integer representation too long"
                    } else {
                        "invalid var_i32: integer too large"
                    };
                    return Err(BinaryReaderError::new(msg, self.original_position() - 1));
                }
                return Ok(result);
            }
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
        }
        let ashift = 32 - shift;
        Ok((result << ashift) >> ashift)
    }

    /// Advances the `BinaryReader` up to four bytes to parse a variable
    /// length integer as a signed 33 bit integer, returned as a `i64`.
    /// # Errors
    /// If `BinaryReader` has less than one or up to five bytes remaining, or
    /// the integer is larger than 33 bits.
    pub fn read_var_s33(&mut self) -> Result<i64> {
        // Optimization for single byte.
        let byte = self.read_u8()?;
        if (byte & 0x80) == 0 {
            return Ok(((byte as i8) << 1) as i64 >> 1);
        }

        let mut result = (byte & 0x7F) as i64;
        let mut shift = 7;
        loop {
            let byte = self.read_u8()?;
            result |= ((byte & 0x7F) as i64) << shift;
            if shift >= 25 {
                let continuation_bit = (byte & 0x80) != 0;
                let sign_and_unused_bit = (byte << 1) as i8 >> (33 - shift);
                if continuation_bit || (sign_and_unused_bit != 0 && sign_and_unused_bit != -1) {
                    return Err(BinaryReaderError::new(
                        "invalid var_s33: integer representation too long",
                        self.original_position() - 1,
                    ));
                }
                return Ok(result);
            }
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
        }
        let ashift = 64 - shift;
        Ok((result << ashift) >> ashift)
    }

    /// Advances the `BinaryReader` up to eight bytes to parse a variable
    /// length integer as a 64 bit integer, returned as a `i64`.
    /// # Errors
    /// If `BinaryReader` has less than one or up to eight bytes remaining, or
    /// the integer is larger than 64 bits.
    pub fn read_var_i64(&mut self) -> Result<i64> {
        let mut result: i64 = 0;
        let mut shift = 0;
        loop {
            let byte = self.read_u8()?;
            result |= i64::from(byte & 0x7F) << shift;
            if shift >= 57 {
                let continuation_bit = (byte & 0x80) != 0;
                let sign_and_unused_bit = ((byte << 1) as i8) >> (64 - shift);
                if continuation_bit || (sign_and_unused_bit != 0 && sign_and_unused_bit != -1) {
                    let msg = if continuation_bit {
                        "invalid var_i64: integer representation too long"
                    } else {
                        "invalid var_i64: integer too large"
                    };
                    return Err(BinaryReaderError::new(msg, self.original_position() - 1));
                }
                return Ok(result);
            }
            shift += 7;
            if (byte & 0x80) == 0 {
                break;
            }
        }
        let ashift = 64 - shift;
        Ok((result << ashift) >> ashift)
    }

    /// Advances the `BinaryReader` up to four bytes to parse a variable
    /// length integer as a 32 bit floating point integer, returned as `Ieee32`.
    /// # Errors
    /// If `BinaryReader` has less than one or up to four bytes remaining, or
    /// the integer is larger than 32 bits.
    pub fn read_f32(&mut self) -> Result<Ieee32> {
        let value = self.read_u32()?;
        Ok(Ieee32(value))
    }

    /// Advances the `BinaryReader` up to four bytes to parse a variable
    /// length integer as a 32 bit floating point integer, returned as `Ieee32`.
    /// # Errors
    /// If `BinaryReader` has less than one or up to four bytes remaining, or
    /// the integer is larger than 32 bits.
    pub fn read_f64(&mut self) -> Result<Ieee64> {
        let value = self.read_u64()?;
        Ok(Ieee64(value))
    }

    /// Reads a WebAssembly string from the module.
    /// # Errors
    /// If `BinaryReader` has less than up to four bytes remaining, the string's
    /// length exceeds the remaining bytes, the string's length exceeds
    /// `limits::MAX_WASM_STRING_SIZE`, or the string contains invalid utf-8.
    pub fn read_string(&mut self) -> Result<&'a str> {
        let len = self.read_var_u32()? as usize;
        if len > MAX_WASM_STRING_SIZE {
            return Err(BinaryReaderError::new(
                "string size out of bounds",
                self.original_position() - 1,
            ));
        }
        let bytes = self.read_bytes(len)?;
        str::from_utf8(bytes).map_err(|_| {
            BinaryReaderError::new("invalid UTF-8 encoding", self.original_position() - 1)
        })
    }

    fn read_optional_val_type(&mut self) -> Result<Option<ComponentValType>> {
        match self.read_u8()? {
            0x0 => Ok(None),
            0x1 => Ok(Some(self.read_component_val_type()?)),
            x => self.invalid_leading_byte(x, "optional component value type"),
        }
    }

    fn read_memarg_of_align(&mut self, max_align: u8) -> Result<MemoryImmediate> {
        let align_pos = self.original_position();
        let imm = self.read_memarg()?;
        if imm.align > max_align {
            return Err(BinaryReaderError::new(
                "alignment must not be larger than natural",
                align_pos,
            ));
        }
        Ok(imm)
    }

    #[cold]
    fn invalid_leading_byte<T>(&self, byte: u8, desc: &str) -> Result<T> {
        Err(Self::invalid_leading_byte_error(
            byte,
            desc,
            self.original_position() - 1,
        ))
    }

    #[cold]
    fn invalid_leading_byte_error(byte: u8, desc: &str, offset: usize) -> BinaryReaderError {
        BinaryReaderError::new(
            format!("invalid leading byte (0x{:x}) for {}", byte, desc),
            offset,
        )
    }

    fn peek(&self) -> Result<u8> {
        self.ensure_has_byte()?;
        Ok(self.buffer[self.position])
    }

    fn val_type_from_byte(byte: u8) -> Option<ValType> {
        match byte {
            0x7F => Some(ValType::I32),
            0x7E => Some(ValType::I64),
            0x7D => Some(ValType::F32),
            0x7C => Some(ValType::F64),
            0x7B => Some(ValType::V128),
            0x70 => Some(ValType::FuncRef),
            0x6F => Some(ValType::ExternRef),
            _ => None,
        }
    }

    fn read_block_type(&mut self) -> Result<BlockType> {
        let b = self.peek()?;

        // Check for empty block
        if b == 0x40 {
            self.position += 1;
            return Ok(BlockType::Empty);
        }

        // Check for a block type of form [] -> [t].
        if let Some(ty) = Self::val_type_from_byte(b) {
            self.position += 1;
            return Ok(BlockType::Type(ty));
        }

        // Not empty or a singular type, so read the function type index
        let idx = self.read_var_s33()?;
        if idx < 0 || idx > (std::u32::MAX as i64) {
            return Err(BinaryReaderError::new(
                "invalid function type",
                self.original_position(),
            ));
        }

        Ok(BlockType::FuncType(idx as u32))
    }

    /// Reads the next available `Operator` and calls the respective visit method.
    ///
    /// # Errors
    ///
    /// If `BinaryReader` has less bytes remaining than required to parse
    /// the `Operator`.
    #[rustfmt::skip]
    pub fn visit_operator<T>(&mut self, visitor: &mut T) -> Result<<T as VisitOperator<'a>>::Output>
    where
        T: VisitOperator<'a>,
    {
        let pos = self.original_position();
        let code = self.read_u8()? as u8;
        Ok(match code {
            0x00 => visitor.visit_unreachable(pos),
            0x01 => visitor.visit_nop(pos),
            0x02 => visitor.visit_block(pos, self.read_block_type()?),
            0x03 => visitor.visit_loop(pos, self.read_block_type()?),
            0x04 => visitor.visit_if(pos, self.read_block_type()?),
            0x05 => visitor.visit_else(pos),
            0x06 => visitor.visit_try(pos, self.read_block_type()?),
            0x07 => visitor.visit_catch(pos, self.read_var_u32()?),
            0x08 => visitor.visit_throw(pos, self.read_var_u32()?),
            0x09 => visitor.visit_rethrow(pos, self.read_var_u32()?),
            0x0b => visitor.visit_end(pos),
            0x0c => visitor.visit_br(pos, self.read_var_u32()?),
            0x0d => visitor.visit_br_if(pos, self.read_var_u32()?),
            0x0e => visitor.visit_br_table(pos, &self.read_br_table()?),
            0x0f => visitor.visit_return(pos),
            0x10 => visitor.visit_call(pos, self.read_var_u32()?),
            0x11 => {
                let index = self.read_var_u32()?;
                let (table_byte, table_index) = self.read_first_byte_and_var_u32()?;
                visitor.visit_call_indirect(pos, index, table_index, table_byte)
            }
            0x12 => visitor.visit_return_call(pos, self.read_var_u32()?),
            0x13 => visitor.visit_return_call_indirect(pos, self.read_var_u32()?, self.read_var_u32()?),
            0x18 => visitor.visit_delegate(pos, self.read_var_u32()?),
            0x19 => visitor.visit_catch_all(pos),
            0x1a => visitor.visit_drop(pos),
            0x1b => visitor.visit_select(pos),
            0x1c => {
                let results = self.read_var_u32()?;
                if results != 1 {
                    return Err(BinaryReaderError::new(
                        "invalid result arity",
                        self.position,
                    ));
                }
                visitor.visit_typed_select(pos, self.read_val_type()?)
            }

            0x20 => visitor.visit_local_get(pos, self.read_var_u32()?),
            0x21 => visitor.visit_local_set(pos, self.read_var_u32()?),
            0x22 => visitor.visit_local_tee(pos, self.read_var_u32()?),
            0x23 => visitor.visit_global_get(pos, self.read_var_u32()?),
            0x24 => visitor.visit_global_set(pos, self.read_var_u32()?),
            0x25 => visitor.visit_table_get(pos, self.read_var_u32()?),
            0x26 => visitor.visit_table_set(pos, self.read_var_u32()?),

            0x28 => visitor.visit_i32_load(pos, self.read_memarg()?),
            0x29 => visitor.visit_i64_load(pos, self.read_memarg()?),
            0x2a => visitor.visit_f32_load(pos, self.read_memarg()?),
            0x2b => visitor.visit_f64_load(pos, self.read_memarg()?),
            0x2c => visitor.visit_i32_load8_s(pos, self.read_memarg()?),
            0x2d => visitor.visit_i32_load8_u(pos, self.read_memarg()?),
            0x2e => visitor.visit_i32_load16_s(pos, self.read_memarg()?),
            0x2f => visitor.visit_i32_load16_u(pos, self.read_memarg()?),
            0x30 => visitor.visit_i64_load8_s(pos, self.read_memarg()?),
            0x31 => visitor.visit_i64_load8_u(pos, self.read_memarg()?),
            0x32 => visitor.visit_i64_load16_s(pos, self.read_memarg()?),
            0x33 => visitor.visit_i64_load16_u(pos, self.read_memarg()?),
            0x34 => visitor.visit_i64_load32_s(pos, self.read_memarg()?),
            0x35 => visitor.visit_i64_load32_u(pos, self.read_memarg()?),
            0x36 => visitor.visit_i32_store(pos, self.read_memarg()?),
            0x37 => visitor.visit_i64_store(pos, self.read_memarg()?),
            0x38 => visitor.visit_f32_store(pos, self.read_memarg()?),
            0x39 => visitor.visit_f64_store(pos, self.read_memarg()?),
            0x3a => visitor.visit_i32_store8(pos, self.read_memarg()?),
            0x3b => visitor.visit_i32_store16(pos, self.read_memarg()?),
            0x3c => visitor.visit_i64_store8(pos, self.read_memarg()?),
            0x3d => visitor.visit_i64_store16(pos, self.read_memarg()?),
            0x3e => visitor.visit_i64_store32(pos, self.read_memarg()?),
            0x3f => {
                let (mem_byte, mem) = self.read_first_byte_and_var_u32()?;
                visitor.visit_memory_size(pos, mem, mem_byte)
            }
            0x40 => {
                let (mem_byte, mem) = self.read_first_byte_and_var_u32()?;
                visitor.visit_memory_grow(pos, mem, mem_byte)
            }

            0x41 => visitor.visit_i32_const(pos, self.read_var_i32()?),
            0x42 => visitor.visit_i64_const(pos, self.read_var_i64()?),
            0x43 => visitor.visit_f32_const(pos, self.read_f32()?),
            0x44 => visitor.visit_f64_const(pos, self.read_f64()?),

            0x45 => visitor.visit_i32_eqz(pos),
            0x46 => visitor.visit_i32_eq(pos),
            0x47 => visitor.visit_i32_ne(pos),
            0x48 => visitor.visit_i32_lt_s(pos),
            0x49 => visitor.visit_i32_lt_u(pos),
            0x4a => visitor.visit_i32_gt_s(pos),
            0x4b => visitor.visit_i32_gt_u(pos),
            0x4c => visitor.visit_i32_le_s(pos),
            0x4d => visitor.visit_i32_le_u(pos),
            0x4e => visitor.visit_i32_ge_s(pos),
            0x4f => visitor.visit_i32_ge_u(pos),
            0x50 => visitor.visit_i64_eqz(pos),
            0x51 => visitor.visit_i64_eq(pos),
            0x52 => visitor.visit_i64_ne(pos),
            0x53 => visitor.visit_i64_lt_s(pos),
            0x54 => visitor.visit_i64_lt_u(pos),
            0x55 => visitor.visit_i64_gt_s(pos),
            0x56 => visitor.visit_i64_gt_u(pos),
            0x57 => visitor.visit_i64_le_s(pos),
            0x58 => visitor.visit_i64_le_u(pos),
            0x59 => visitor.visit_i64_ge_s(pos),
            0x5a => visitor.visit_i64_ge_u(pos),
            0x5b => visitor.visit_f32_eq(pos),
            0x5c => visitor.visit_f32_ne(pos),
            0x5d => visitor.visit_f32_lt(pos),
            0x5e => visitor.visit_f32_gt(pos),
            0x5f => visitor.visit_f32_le(pos),
            0x60 => visitor.visit_f32_ge(pos),
            0x61 => visitor.visit_f64_eq(pos),
            0x62 => visitor.visit_f64_ne(pos),
            0x63 => visitor.visit_f64_lt(pos),
            0x64 => visitor.visit_f64_gt(pos),
            0x65 => visitor.visit_f64_le(pos),
            0x66 => visitor.visit_f64_ge(pos),
            0x67 => visitor.visit_i32_clz(pos),
            0x68 => visitor.visit_i32_ctz(pos),
            0x69 => visitor.visit_i32_popcnt(pos),
            0x6a => visitor.visit_i32_add(pos),
            0x6b => visitor.visit_i32_sub(pos),
            0x6c => visitor.visit_i32_mul(pos),
            0x6d => visitor.visit_i32_div_s(pos),
            0x6e => visitor.visit_i32_div_u(pos),
            0x6f => visitor.visit_i32_rem_s(pos),
            0x70 => visitor.visit_i32_rem_u(pos),
            0x71 => visitor.visit_i32_and(pos),
            0x72 => visitor.visit_i32_or(pos),
            0x73 => visitor.visit_i32_xor(pos),
            0x74 => visitor.visit_i32_shl(pos),
            0x75 => visitor.visit_i32_shr_s(pos),
            0x76 => visitor.visit_i32_shr_u(pos),
            0x77 => visitor.visit_i32_rotl(pos),
            0x78 => visitor.visit_i32_rotr(pos),
            0x79 => visitor.visit_i64_clz(pos),
            0x7a => visitor.visit_i64_ctz(pos),
            0x7b => visitor.visit_i64_popcnt(pos),
            0x7c => visitor.visit_i64_add(pos),
            0x7d => visitor.visit_i64_sub(pos),
            0x7e => visitor.visit_i64_mul(pos),
            0x7f => visitor.visit_i64_div_s(pos),
            0x80 => visitor.visit_i64_div_u(pos),
            0x81 => visitor.visit_i64_rem_s(pos),
            0x82 => visitor.visit_i64_rem_u(pos),
            0x83 => visitor.visit_i64_and(pos),
            0x84 => visitor.visit_i64_or(pos),
            0x85 => visitor.visit_i64_xor(pos),
            0x86 => visitor.visit_i64_shl(pos),
            0x87 => visitor.visit_i64_shr_s(pos),
            0x88 => visitor.visit_i64_shr_u(pos),
            0x89 => visitor.visit_i64_rotl(pos),
            0x8a => visitor.visit_i64_rotr(pos),
            0x8b => visitor.visit_f32_abs(pos),
            0x8c => visitor.visit_f32_neg(pos),
            0x8d => visitor.visit_f32_ceil(pos),
            0x8e => visitor.visit_f32_floor(pos),
            0x8f => visitor.visit_f32_trunc(pos),
            0x90 => visitor.visit_f32_nearest(pos),
            0x91 => visitor.visit_f32_sqrt(pos),
            0x92 => visitor.visit_f32_add(pos),
            0x93 => visitor.visit_f32_sub(pos),
            0x94 => visitor.visit_f32_mul(pos),
            0x95 => visitor.visit_f32_div(pos),
            0x96 => visitor.visit_f32_min(pos),
            0x97 => visitor.visit_f32_max(pos),
            0x98 => visitor.visit_f32_copysign(pos),
            0x99 => visitor.visit_f64_abs(pos),
            0x9a => visitor.visit_f64_neg(pos),
            0x9b => visitor.visit_f64_ceil(pos),
            0x9c => visitor.visit_f64_floor(pos),
            0x9d => visitor.visit_f64_trunc(pos),
            0x9e => visitor.visit_f64_nearest(pos),
            0x9f => visitor.visit_f64_sqrt(pos),
            0xa0 => visitor.visit_f64_add(pos),
            0xa1 => visitor.visit_f64_sub(pos),
            0xa2 => visitor.visit_f64_mul(pos),
            0xa3 => visitor.visit_f64_div(pos),
            0xa4 => visitor.visit_f64_min(pos),
            0xa5 => visitor.visit_f64_max(pos),
            0xa6 => visitor.visit_f64_copysign(pos),
            0xa7 => visitor.visit_i32_wrap_i64(pos),
            0xa8 => visitor.visit_i32_trunc_f32s(pos),
            0xa9 => visitor.visit_i32_trunc_f32u(pos),
            0xaa => visitor.visit_i32_trunc_f64s(pos),
            0xab => visitor.visit_i32_trunc_f64u(pos),
            0xac => visitor.visit_i64_extend_i32s(pos),
            0xad => visitor.visit_i64_extend_i32u(pos),
            0xae => visitor.visit_i64_trunc_f32s(pos),
            0xaf => visitor.visit_i64_trunc_f32u(pos),
            0xb0 => visitor.visit_i64_trunc_f64s(pos),
            0xb1 => visitor.visit_i64_trunc_f64u(pos),
            0xb2 => visitor.visit_f32_convert_i32s(pos),
            0xb3 => visitor.visit_f32_convert_i32u(pos),
            0xb4 => visitor.visit_f32_convert_i64s(pos),
            0xb5 => visitor.visit_f32_convert_i64u(pos),
            0xb6 => visitor.visit_f32_demote_f64(pos),
            0xb7 => visitor.visit_f64_convert_i32s(pos),
            0xb8 => visitor.visit_f64_convert_i32u(pos),
            0xb9 => visitor.visit_f64_convert_i64s(pos),
            0xba => visitor.visit_f64_convert_i64u(pos),
            0xbb => visitor.visit_f64_promote_f32(pos),
            0xbc => visitor.visit_i32_reinterpret_f32(pos),
            0xbd => visitor.visit_i64_reinterpret_f64(pos),
            0xbe => visitor.visit_f32_reinterpret_i32(pos),
            0xbf => visitor.visit_f64_reinterpret_i64(pos),

            0xc0 => visitor.visit_i32_extend8_s(pos),
            0xc1 => visitor.visit_i32_extend16_s(pos),
            0xc2 => visitor.visit_i64_extend8_s(pos),
            0xc3 => visitor.visit_i64_extend16_s(pos),
            0xc4 => visitor.visit_i64_extend32_s(pos),

            0xd0 => visitor.visit_ref_null(pos, self.read_val_type()?),
            0xd1 => visitor.visit_ref_is_null(pos),
            0xd2 => visitor.visit_ref_func(pos, self.read_var_u32()?),

            0xfc => self.visit_0xfc_operator(visitor)?,
            0xfd => self.visit_0xfd_operator(visitor)?,
            0xfe => self.visit_0xfe_operator(visitor)?,

            _ => {
                return Err(BinaryReaderError::new(
                    format!("illegal opcode: 0x{:x}", code),
                    self.original_position() - 1,
                ));
            }
        })
    }

    #[rustfmt::skip]
    fn visit_0xfc_operator<T>(&mut self, visitor: &mut T) -> Result<<T as VisitOperator<'a>>::Output>
    where
        T: VisitOperator<'a>,
    {
        let pos = self.original_position();
        let code = self.read_var_u32()?;
        Ok(match code {
            0x00 => visitor.visit_i32_trunc_sat_f32s(pos),
            0x01 => visitor.visit_i32_trunc_sat_f32u(pos),
            0x02 => visitor.visit_i32_trunc_sat_f64s(pos),
            0x03 => visitor.visit_i32_trunc_sat_f64u(pos),
            0x04 => visitor.visit_i64_trunc_sat_f32s(pos),
            0x05 => visitor.visit_i64_trunc_sat_f32u(pos),
            0x06 => visitor.visit_i64_trunc_sat_f64s(pos),
            0x07 => visitor.visit_i64_trunc_sat_f64u(pos),

            0x08 => {
                let segment = self.read_var_u32()?;
                let mem = self.read_var_u32()?;
                visitor.visit_memory_init(pos, segment,mem)
            }
            0x09 => {
                let segment = self.read_var_u32()?;
                visitor.visit_data_drop(pos, segment)
            }
            0x0a => {
                let dst = self.read_var_u32()?;
                let src = self.read_var_u32()?;
                visitor.visit_memory_copy(pos, dst,src)
            }
            0x0b => {
                let mem = self.read_var_u32()?;
                visitor.visit_memory_fill(pos, mem)
            }
            0x0c => {
                let segment = self.read_var_u32()?;
                let table = self.read_var_u32()?;
                visitor.visit_table_init(pos, segment, table)
            }
            0x0d => {
                let segment = self.read_var_u32()?;
                visitor.visit_elem_drop(pos, segment)
            }
            0x0e => {
                let dst_table = self.read_var_u32()?;
                let src_table = self.read_var_u32()?;
                visitor.visit_table_copy(pos, dst_table, src_table)
            }

            0x0f => {
                let table = self.read_var_u32()?;
                visitor.visit_table_grow(pos, table)
            }
            0x10 => {
                let table = self.read_var_u32()?;
                visitor.visit_table_size(pos, table)
            }

            0x11 => {
                let table = self.read_var_u32()?;
                visitor.visit_table_fill(pos, table)
            }

            _ => {
                return Err(BinaryReaderError::new(
                    format!("unknown 0xfc subopcode: 0x{:x}", code),
                    self.original_position() - 1,
                ));
            }
        })
    }

    #[rustfmt::skip]
    fn visit_0xfd_operator<T>(&mut self, visitor: &mut T) -> Result<<T as VisitOperator<'a>>::Output>
    where
        T: VisitOperator<'a>,
    {
        let pos = self.original_position();
        let code = self.read_var_u32()?;
        Ok(match code {
            0x00 => visitor.visit_v128_load(pos, self.read_memarg()?),
            0x01 => visitor.visit_v128_load8x8_s(pos, self.read_memarg_of_align(3)?),
            0x02 => visitor.visit_v128_load8x8_u(pos, self.read_memarg_of_align(3)?),
            0x03 => visitor.visit_v128_load16x4_s(pos, self.read_memarg_of_align(3)?),
            0x04 => visitor.visit_v128_load16x4_u(pos, self.read_memarg_of_align(3)?),
            0x05 => visitor.visit_v128_load32x2_s(pos, self.read_memarg_of_align(3)?),
            0x06 => visitor.visit_v128_load32x2_u(pos, self.read_memarg_of_align(3)?),
            0x07 => visitor.visit_v128_load8_splat(pos, self.read_memarg_of_align(0)?),
            0x08 => visitor.visit_v128_load16_splat(pos, self.read_memarg_of_align(1)?),
            0x09 => visitor.visit_v128_load32_splat(pos, self.read_memarg_of_align(2)?),
            0x0a => visitor.visit_v128_load64_splat(pos, self.read_memarg_of_align(3)?),

            0x0b => visitor.visit_v128_store(pos, self.read_memarg()?),
            0x0c => visitor.visit_v128_const(pos, self.read_v128()?),
            0x0d => {
                let mut lanes: [SIMDLaneIndex; 16] = [0; 16];
                for lane in &mut lanes {
                    *lane = self.read_lane_index(32)?
                }
                visitor.visit_i8x16_shuffle(pos, lanes)
            }

            0x0e => visitor.visit_i8x16_swizzle(pos),
            0x0f => visitor.visit_i8x16_splat(pos),
            0x10 => visitor.visit_i16x8_splat(pos),
            0x11 => visitor.visit_i32x4_splat(pos),
            0x12 => visitor.visit_i64x2_splat(pos),
            0x13 => visitor.visit_f32x4_splat(pos),
            0x14 => visitor.visit_f64x2_splat(pos),

            0x15 => visitor.visit_i8x16_extract_lane_s(pos, self.read_lane_index(16)?),
            0x16 => visitor.visit_i8x16_extract_lane_u(pos, self.read_lane_index(16)?),
            0x17 => visitor.visit_i8x16_replace_lane(pos, self.read_lane_index(16)?),
            0x18 => visitor.visit_i16x8_extract_lane_s(pos, self.read_lane_index(8)?),
            0x19 => visitor.visit_i16x8_extract_lane_u(pos, self.read_lane_index(8)?),
            0x1a => visitor.visit_i16x8_replace_lane(pos, self.read_lane_index(8)?),
            0x1b => visitor.visit_i32x4_extract_lane(pos, self.read_lane_index(4)?),

            0x1c => visitor.visit_i32x4_replace_lane(pos, self.read_lane_index(4)?),
            0x1d => visitor.visit_i64x2_extract_lane(pos, self.read_lane_index(2)?),
            0x1e => visitor.visit_i64x2_replace_lane(pos, self.read_lane_index(2)?),
            0x1f => visitor.visit_f32x4_extract_lane(pos, self.read_lane_index(4)?),
            0x20 => visitor.visit_f32x4_replace_lane(pos, self.read_lane_index(4)?),
            0x21 => visitor.visit_f64x2_extract_lane(pos, self.read_lane_index(2)?),
            0x22 => visitor.visit_f64x2_replace_lane(pos, self.read_lane_index(2)?),

            0x23 => visitor.visit_i8x16_eq(pos),
            0x24 => visitor.visit_i8x16_ne(pos),
            0x25 => visitor.visit_i8x16_lt_s(pos),
            0x26 => visitor.visit_i8x16_lt_u(pos),
            0x27 => visitor.visit_i8x16_gt_s(pos),
            0x28 => visitor.visit_i8x16_gt_u(pos),
            0x29 => visitor.visit_i8x16_le_s(pos),
            0x2a => visitor.visit_i8x16_le_u(pos),
            0x2b => visitor.visit_i8x16_ge_s(pos),
            0x2c => visitor.visit_i8x16_ge_u(pos),
            0x2d => visitor.visit_i16x8_eq(pos),
            0x2e => visitor.visit_i16x8_ne(pos),
            0x2f => visitor.visit_i16x8_lt_s(pos),
            0x30 => visitor.visit_i16x8_lt_u(pos),
            0x31 => visitor.visit_i16x8_gt_s(pos),
            0x32 => visitor.visit_i16x8_gt_u(pos),
            0x33 => visitor.visit_i16x8_le_s(pos),
            0x34 => visitor.visit_i16x8_le_u(pos),
            0x35 => visitor.visit_i16x8_ge_s(pos),
            0x36 => visitor.visit_i16x8_ge_u(pos),
            0x37 => visitor.visit_i32x4_eq(pos),
            0x38 => visitor.visit_i32x4_ne(pos),
            0x39 => visitor.visit_i32x4_lt_s(pos),
            0x3a => visitor.visit_i32x4_lt_u(pos),
            0x3b => visitor.visit_i32x4_gt_s(pos),
            0x3c => visitor.visit_i32x4_gt_u(pos),
            0x3d => visitor.visit_i32x4_le_s(pos),
            0x3e => visitor.visit_i32x4_le_u(pos),
            0x3f => visitor.visit_i32x4_ge_s(pos),
            0x40 => visitor.visit_i32x4_ge_u(pos),
            0x41 => visitor.visit_f32x4_eq(pos),
            0x42 => visitor.visit_f32x4_ne(pos),
            0x43 => visitor.visit_f32x4_lt(pos),
            0x44 => visitor.visit_f32x4_gt(pos),
            0x45 => visitor.visit_f32x4_le(pos),
            0x46 => visitor.visit_f32x4_ge(pos),
            0x47 => visitor.visit_f64x2_eq(pos),
            0x48 => visitor.visit_f64x2_ne(pos),
            0x49 => visitor.visit_f64x2_lt(pos),
            0x4a => visitor.visit_f64x2_gt(pos),
            0x4b => visitor.visit_f64x2_le(pos),
            0x4c => visitor.visit_f64x2_ge(pos),
            0x4d => visitor.visit_v128_not(pos),
            0x4e => visitor.visit_v128_and(pos),
            0x4f => visitor.visit_v128_and_not(pos),
            0x50 => visitor.visit_v128_or(pos),
            0x51 => visitor.visit_v128_xor(pos),
            0x52 => visitor.visit_v128_bitselect(pos),
            0x53 => visitor.visit_v128_any_true(pos),

            0x54 => {
                let memarg = self.read_memarg()?;
                let lane = self.read_lane_index(16)?;
                visitor.visit_v128_load8_lane(pos, memarg, lane)
            },
            0x55 => {
                let memarg = self.read_memarg()?;
                let lane = self.read_lane_index(8)?;
                visitor.visit_v128_load16_lane(pos, memarg, lane)
            },
            0x56 => {
                let memarg = self.read_memarg()?;
                let lane = self.read_lane_index(4)?;
                visitor.visit_v128_load32_lane(pos, memarg, lane)
            },
            0x57 => {
                let memarg = self.read_memarg()?;
                let lane = self.read_lane_index(2)?;
                visitor.visit_v128_load64_lane(pos, memarg, lane)
            },
            0x58 => {
                let memarg = self.read_memarg()?;
                let lane = self.read_lane_index(16)?;
                visitor.visit_v128_store8_lane(pos, memarg, lane)
            },
            0x59 => {
                let memarg = self.read_memarg()?;
                let lane = self.read_lane_index(8)?;
                visitor.visit_v128_store16_lane(pos, memarg, lane)
            },
            0x5a => {
                let memarg = self.read_memarg()?;
                let lane = self.read_lane_index(4)?;
                visitor.visit_v128_store32_lane(pos, memarg, lane)
            },
            0x5b => {
                let memarg = self.read_memarg()?;
                let lane = self.read_lane_index(2)?;
                visitor.visit_v128_store64_lane(pos, memarg, lane)
            },

            0x5c => visitor.visit_v128_load32_zero(pos, self.read_memarg_of_align(2)?),
            0x5d => visitor.visit_v128_load64_zero(pos, self.read_memarg_of_align(3)?),
            0x5e => visitor.visit_f32x4_demote_f64x2_zero(pos),
            0x5f => visitor.visit_f64x2_promote_low_f32x4(pos),
            0x60 => visitor.visit_i8x16_abs(pos),
            0x61 => visitor.visit_i8x16_neg(pos),
            0x62 => visitor.visit_i8x16_popcnt(pos),
            0x63 => visitor.visit_i8x16_all_true(pos),
            0x64 => visitor.visit_i8x16_bitmask(pos),
            0x65 => visitor.visit_i8x16_narrow_i16x8_s(pos),
            0x66 => visitor.visit_i8x16_narrow_i16x8_u(pos),
            0x67 => visitor.visit_f32x4_ceil(pos),
            0x68 => visitor.visit_f32x4_floor(pos),
            0x69 => visitor.visit_f32x4_trunc(pos),
            0x6a => visitor.visit_f32x4_nearest(pos),
            0x6b => visitor.visit_i8x16_shl(pos),
            0x6c => visitor.visit_i8x16_shr_s(pos),
            0x6d => visitor.visit_i8x16_shr_u(pos),
            0x6e => visitor.visit_i8x16_add(pos),
            0x6f => visitor.visit_i8x16_add_sat_s(pos),
            0x70 => visitor.visit_i8x16_add_sat_u(pos),
            0x71 => visitor.visit_i8x16_sub(pos),
            0x72 => visitor.visit_i8x16_sub_sat_s(pos),
            0x73 => visitor.visit_i8x16_sub_sat_u(pos),
            0x74 => visitor.visit_f64x2_ceil(pos),
            0x75 => visitor.visit_f64x2_floor(pos),
            0x76 => visitor.visit_i8x16_min_s(pos),
            0x77 => visitor.visit_i8x16_min_u(pos),
            0x78 => visitor.visit_i8x16_max_s(pos),
            0x79 => visitor.visit_i8x16_max_u(pos),
            0x7a => visitor.visit_f64x2_trunc(pos),
            0x7b => visitor.visit_i8x16_rounding_average_u(pos),
            0x7c => visitor.visit_i16x8_ext_add_pairwise_i8x16_s(pos),
            0x7d => visitor.visit_i16x8_ext_add_pairwise_i8x16_u(pos),
            0x7e => visitor.visit_i32x4_ext_add_pairwise_i16x8_s(pos),
            0x7f => visitor.visit_i32x4_ext_add_pairwise_i16x8_u(pos),
            0x80 => visitor.visit_i16x8_abs(pos),
            0x81 => visitor.visit_i16x8_neg(pos),
            0x82 => visitor.visit_i16x8_q15_mulr_sat_s(pos),
            0x83 => visitor.visit_i16x8_all_true(pos),
            0x84 => visitor.visit_i16x8_bitmask(pos),
            0x85 => visitor.visit_i16x8_narrow_i32x4_s(pos),
            0x86 => visitor.visit_i16x8_narrow_i32x4_u(pos),
            0x87 => visitor.visit_i16x8_extend_low_i8x16_s(pos),
            0x88 => visitor.visit_i16x8_extend_high_i8x16_s(pos),
            0x89 => visitor.visit_i16x8_extend_low_i8x16_u(pos),
            0x8a => visitor.visit_i16x8_extend_high_i8x16_u(pos),
            0x8b => visitor.visit_i16x8_shl(pos),
            0x8c => visitor.visit_i16x8_shr_s(pos),
            0x8d => visitor.visit_i16x8_shr_u(pos),
            0x8e => visitor.visit_i16x8_add(pos),
            0x8f => visitor.visit_i16x8_add_sat_s(pos),
            0x90 => visitor.visit_i16x8_add_sat_u(pos),
            0x91 => visitor.visit_i16x8_sub(pos),
            0x92 => visitor.visit_i16x8_sub_sat_s(pos),
            0x93 => visitor.visit_i16x8_sub_sat_u(pos),
            0x94 => visitor.visit_f64x2_nearest(pos),
            0x95 => visitor.visit_i16x8_mul(pos),
            0x96 => visitor.visit_i16x8_min_s(pos),
            0x97 => visitor.visit_i16x8_min_u(pos),
            0x98 => visitor.visit_i16x8_max_s(pos),
            0x99 => visitor.visit_i16x8_max_u(pos),
            0x9b => visitor.visit_i16x8_rounding_average_u(pos),
            0x9c => visitor.visit_i16x8_ext_mul_low_i8x16_s(pos),
            0x9d => visitor.visit_i16x8_ext_mul_high_i8x16_s(pos),
            0x9e => visitor.visit_i16x8_ext_mul_low_i8x16_u(pos),
            0x9f => visitor.visit_i16x8_ext_mul_high_i8x16_u(pos),
            0xa0 => visitor.visit_i32x4_abs(pos),
            0xa2 => visitor.visit_i8x16_relaxed_swizzle(pos),
            0xa1 => visitor.visit_i32x4_neg(pos),
            0xa3 => visitor.visit_i32x4_all_true(pos),
            0xa4 => visitor.visit_i32x4_bitmask(pos),
            0xa5 => visitor.visit_i32x4_relaxed_trunc_sat_f32x4_s(pos),
            0xa6 => visitor.visit_i32x4_relaxed_trunc_sat_f32x4_u(pos),
            0xa7 => visitor.visit_i32x4_extend_low_i16x8_s(pos),
            0xa8 => visitor.visit_i32x4_extend_high_i16x8_s(pos),
            0xa9 => visitor.visit_i32x4_extend_low_i16x8_u(pos),
            0xaa => visitor.visit_i32x4_extend_high_i16x8_u(pos),
            0xab => visitor.visit_i32x4_shl(pos),
            0xac => visitor.visit_i32x4_shr_s(pos),
            0xad => visitor.visit_i32x4_shr_u(pos),
            0xae => visitor.visit_i32x4_add(pos),
            0xaf => visitor.visit_f32x4_fma(pos),
            0xb0 => visitor.visit_f32x4_fms(pos),
            0xb1 => visitor.visit_i32x4_sub(pos),
            0xb2 => visitor.visit_i8x16_lane_select(pos),
            0xb3 => visitor.visit_i16x8_lane_select(pos),
            0xb4 => visitor.visit_f32x4_relaxed_min(pos),
            0xb5 => visitor.visit_i32x4_mul(pos),
            0xb6 => visitor.visit_i32x4_min_s(pos),
            0xb7 => visitor.visit_i32x4_min_u(pos),
            0xb8 => visitor.visit_i32x4_max_s(pos),
            0xb9 => visitor.visit_i32x4_max_u(pos),
            0xba => visitor.visit_i32x4_dot_i16x8_s(pos),
            0xbc => visitor.visit_i32x4_ext_mul_low_i16x8_s(pos),
            0xbd => visitor.visit_i32x4_ext_mul_high_i16x8_s(pos),
            0xbe => visitor.visit_i32x4_ext_mul_low_i16x8_u(pos),
            0xbf => visitor.visit_i32x4_ext_mul_high_i16x8_u(pos),
            0xc0 => visitor.visit_i64x2_abs(pos),
            0xc1 => visitor.visit_i64x2_neg(pos),
            0xc3 => visitor.visit_i64x2_all_true(pos),
            0xc4 => visitor.visit_i64x2_bitmask(pos),
            0xc5 => visitor.visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(pos),
            0xc6 => visitor.visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(pos),
            0xc7 => visitor.visit_i64x2_extend_low_i32x4_s(pos),
            0xc8 => visitor.visit_i64x2_extend_high_i32x4_s(pos),
            0xc9 => visitor.visit_i64x2_extend_low_i32x4_u(pos),
            0xca => visitor.visit_i64x2_extend_high_i32x4_u(pos),
            0xcb => visitor.visit_i64x2_shl(pos),
            0xcc => visitor.visit_i64x2_shr_s(pos),
            0xcd => visitor.visit_i64x2_shr_u(pos),
            0xce => visitor.visit_i64x2_add(pos),
            0xcf => visitor.visit_f64x2_fma(pos),
            0xd0 => visitor.visit_f64x2_fms(pos),
            0xd1 => visitor.visit_i64x2_sub(pos),
            0xd2 => visitor.visit_i32x4_lane_select(pos),
            0xd3 => visitor.visit_i64x2_lane_select(pos),
            0xd4 => visitor.visit_f64x2_relaxed_min(pos),
            0xd5 => visitor.visit_i64x2_mul(pos),
            0xd6 => visitor.visit_i64x2_eq(pos),
            0xd7 => visitor.visit_i64x2_ne(pos),
            0xd8 => visitor.visit_i64x2_lt_s(pos),
            0xd9 => visitor.visit_i64x2_gt_s(pos),
            0xda => visitor.visit_i64x2_le_s(pos),
            0xdb => visitor.visit_i64x2_ge_s(pos),
            0xdc => visitor.visit_i64x2_ext_mul_low_i32x4_s(pos),
            0xdd => visitor.visit_i64x2_ext_mul_high_i32x4_s(pos),
            0xde => visitor.visit_i64x2_ext_mul_low_i32x4_u(pos),
            0xdf => visitor.visit_i64x2_ext_mul_high_i32x4_u(pos),
            0xe0 => visitor.visit_f32x4_abs(pos),
            0xe1 => visitor.visit_f32x4_neg(pos),
            0xe2 => visitor.visit_f32x4_relaxed_max(pos),
            0xe3 => visitor.visit_f32x4_sqrt(pos),
            0xe4 => visitor.visit_f32x4_add(pos),
            0xe5 => visitor.visit_f32x4_sub(pos),
            0xe6 => visitor.visit_f32x4_mul(pos),
            0xe7 => visitor.visit_f32x4_div(pos),
            0xe8 => visitor.visit_f32x4_min(pos),
            0xe9 => visitor.visit_f32x4_max(pos),
            0xea => visitor.visit_f32x4_p_min(pos),
            0xeb => visitor.visit_f32x4_p_max(pos),
            0xec => visitor.visit_f64x2_abs(pos),
            0xed => visitor.visit_f64x2_neg(pos),
            0xee => visitor.visit_f64x2_relaxed_max(pos),
            0xef => visitor.visit_f64x2_sqrt(pos),
            0xf0 => visitor.visit_f64x2_add(pos),
            0xf1 => visitor.visit_f64x2_sub(pos),
            0xf2 => visitor.visit_f64x2_mul(pos),
            0xf3 => visitor.visit_f64x2_div(pos),
            0xf4 => visitor.visit_f64x2_min(pos),
            0xf5 => visitor.visit_f64x2_max(pos),
            0xf6 => visitor.visit_f64x2_p_min(pos),
            0xf7 => visitor.visit_f64x2_p_max(pos),
            0xf8 => visitor.visit_i32x4_trunc_sat_f32x4_s(pos),
            0xf9 => visitor.visit_i32x4_trunc_sat_f32x4_u(pos),
            0xfa => visitor.visit_f32x4_convert_i32x4_s(pos),
            0xfb => visitor.visit_f32x4_convert_i32x4_u(pos),
            0xfc => visitor.visit_i32x4_trunc_sat_f64x2_s_zero(pos),
            0xfd => visitor.visit_i32x4_trunc_sat_f64x2_u_zero(pos),
            0xfe => visitor.visit_f64x2_convert_low_i32x4_s(pos),
            0xff => visitor.visit_f64x2_convert_low_i32x4_u(pos),

            _ => {
                return Err(BinaryReaderError::new(
                    format!("unknown 0xfd subopcode: 0x{:x}", code),
                    self.original_position() - 1,
                ));
            }
        })
    }

    fn visit_0xfe_operator<T>(
        &mut self,
        visitor: &mut T,
    ) -> Result<<T as VisitOperator<'a>>::Output>
    where
        T: VisitOperator<'a>,
    {
        let pos = self.original_position();
        let code = self.read_var_u32()?;
        Ok(match code {
            0x00 => visitor.visit_memory_atomic_notify(pos, self.read_memarg_of_align(2)?),
            0x01 => visitor.visit_memory_atomic_wait32(pos, self.read_memarg_of_align(2)?),
            0x02 => visitor.visit_memory_atomic_wait64(pos, self.read_memarg_of_align(3)?),
            0x03 => visitor.visit_atomic_fence(pos, self.read_u8()? as u8),
            0x10 => visitor.visit_i32_atomic_load(pos, self.read_memarg_of_align(2)?),
            0x11 => visitor.visit_i64_atomic_load(pos, self.read_memarg_of_align(3)?),
            0x12 => visitor.visit_i32_atomic_load8_u(pos, self.read_memarg_of_align(0)?),
            0x13 => visitor.visit_i32_atomic_load16_u(pos, self.read_memarg_of_align(1)?),
            0x14 => visitor.visit_i64_atomic_load8_u(pos, self.read_memarg_of_align(0)?),
            0x15 => visitor.visit_i64_atomic_load16_u(pos, self.read_memarg_of_align(1)?),
            0x16 => visitor.visit_i64_atomic_load32_u(pos, self.read_memarg_of_align(2)?),
            0x17 => visitor.visit_i32_atomic_store(pos, self.read_memarg_of_align(2)?),
            0x18 => visitor.visit_i64_atomic_store(pos, self.read_memarg_of_align(3)?),
            0x19 => visitor.visit_i32_atomic_store8(pos, self.read_memarg_of_align(0)?),
            0x1a => visitor.visit_i32_atomic_store16(pos, self.read_memarg_of_align(1)?),
            0x1b => visitor.visit_i64_atomic_store8(pos, self.read_memarg_of_align(0)?),
            0x1c => visitor.visit_i64_atomic_store16(pos, self.read_memarg_of_align(1)?),
            0x1d => visitor.visit_i64_atomic_store32(pos, self.read_memarg_of_align(2)?),
            0x1e => visitor.visit_i32_atomic_rmw_add(pos, self.read_memarg_of_align(2)?),
            0x1f => visitor.visit_i64_atomic_rmw_add(pos, self.read_memarg_of_align(3)?),
            0x20 => visitor.visit_i32_atomic_rmw8_add_u(pos, self.read_memarg_of_align(0)?),
            0x21 => visitor.visit_i32_atomic_rmw16_add_u(pos, self.read_memarg_of_align(1)?),
            0x22 => visitor.visit_i64_atomic_rmw8_add_u(pos, self.read_memarg_of_align(0)?),
            0x23 => visitor.visit_i64_atomic_rmw16_add_u(pos, self.read_memarg_of_align(1)?),
            0x24 => visitor.visit_i64_atomic_rmw32_add_u(pos, self.read_memarg_of_align(2)?),
            0x25 => visitor.visit_i32_atomic_rmw_sub(pos, self.read_memarg_of_align(2)?),
            0x26 => visitor.visit_i64_atomic_rmw_sub(pos, self.read_memarg_of_align(3)?),
            0x27 => visitor.visit_i32_atomic_rmw8_sub_u(pos, self.read_memarg_of_align(0)?),
            0x28 => visitor.visit_i32_atomic_rmw16_sub_u(pos, self.read_memarg_of_align(1)?),
            0x29 => visitor.visit_i64_atomic_rmw8_sub_u(pos, self.read_memarg_of_align(0)?),
            0x2a => visitor.visit_i64_atomic_rmw16_sub_u(pos, self.read_memarg_of_align(1)?),
            0x2b => visitor.visit_i64_atomic_rmw32_sub_u(pos, self.read_memarg_of_align(2)?),
            0x2c => visitor.visit_i32_atomic_rmw_and(pos, self.read_memarg_of_align(2)?),
            0x2d => visitor.visit_i64_atomic_rmw_and(pos, self.read_memarg_of_align(3)?),
            0x2e => visitor.visit_i32_atomic_rmw8_and_u(pos, self.read_memarg_of_align(0)?),
            0x2f => visitor.visit_i32_atomic_rmw16_and_u(pos, self.read_memarg_of_align(1)?),
            0x30 => visitor.visit_i64_atomic_rmw8_and_u(pos, self.read_memarg_of_align(0)?),
            0x31 => visitor.visit_i64_atomic_rmw16_and_u(pos, self.read_memarg_of_align(1)?),
            0x32 => visitor.visit_i64_atomic_rmw32_and_u(pos, self.read_memarg_of_align(2)?),
            0x33 => visitor.visit_i32_atomic_rmw_or(pos, self.read_memarg_of_align(2)?),
            0x34 => visitor.visit_i64_atomic_rmw_or(pos, self.read_memarg_of_align(3)?),
            0x35 => visitor.visit_i32_atomic_rmw8_or_u(pos, self.read_memarg_of_align(0)?),
            0x36 => visitor.visit_i32_atomic_rmw16_or_u(pos, self.read_memarg_of_align(1)?),
            0x37 => visitor.visit_i64_atomic_rmw8_or_u(pos, self.read_memarg_of_align(0)?),
            0x38 => visitor.visit_i64_atomic_rmw16_or_u(pos, self.read_memarg_of_align(1)?),
            0x39 => visitor.visit_i64_atomic_rmw32_or_u(pos, self.read_memarg_of_align(2)?),
            0x3a => visitor.visit_i32_atomic_rmw_xor(pos, self.read_memarg_of_align(2)?),
            0x3b => visitor.visit_i64_atomic_rmw_xor(pos, self.read_memarg_of_align(3)?),
            0x3c => visitor.visit_i32_atomic_rmw8_xor_u(pos, self.read_memarg_of_align(0)?),
            0x3d => visitor.visit_i32_atomic_rmw16_xor_u(pos, self.read_memarg_of_align(1)?),
            0x3e => visitor.visit_i64_atomic_rmw8_xor_u(pos, self.read_memarg_of_align(0)?),
            0x3f => visitor.visit_i64_atomic_rmw16_xor_u(pos, self.read_memarg_of_align(1)?),
            0x40 => visitor.visit_i64_atomic_rmw32_xor_u(pos, self.read_memarg_of_align(2)?),
            0x41 => visitor.visit_i32_atomic_rmw_xchg(pos, self.read_memarg_of_align(2)?),
            0x42 => visitor.visit_i64_atomic_rmw_xchg(pos, self.read_memarg_of_align(3)?),
            0x43 => visitor.visit_i32_atomic_rmw8_xchg_u(pos, self.read_memarg_of_align(0)?),
            0x44 => visitor.visit_i32_atomic_rmw16_xchg_u(pos, self.read_memarg_of_align(1)?),
            0x45 => visitor.visit_i64_atomic_rmw8_xchg_u(pos, self.read_memarg_of_align(0)?),
            0x46 => visitor.visit_i64_atomic_rmw16_xchg_u(pos, self.read_memarg_of_align(1)?),
            0x47 => visitor.visit_i64_atomic_rmw32_xchg_u(pos, self.read_memarg_of_align(2)?),
            0x48 => visitor.visit_i32_atomic_rmw_cmpxchg(pos, self.read_memarg_of_align(2)?),
            0x49 => visitor.visit_i64_atomic_rmw_cmpxchg(pos, self.read_memarg_of_align(3)?),
            0x4a => visitor.visit_i32_atomic_rmw8_cmpxchg_u(pos, self.read_memarg_of_align(0)?),
            0x4b => visitor.visit_i32_atomic_rmw16_cmpxchg_u(pos, self.read_memarg_of_align(1)?),
            0x4c => visitor.visit_i64_atomic_rmw8_cmpxchg_u(pos, self.read_memarg_of_align(0)?),
            0x4d => visitor.visit_i64_atomic_rmw16_cmpxchg_u(pos, self.read_memarg_of_align(1)?),
            0x4e => visitor.visit_i64_atomic_rmw32_cmpxchg_u(pos, self.read_memarg_of_align(2)?),

            _ => {
                return Err(BinaryReaderError::new(
                    format!("unknown 0xfe subopcode: 0x{:x}", code),
                    self.original_position() - 1,
                ));
            }
        })
    }

    /// Reads the next available `Operator`.
    ///
    /// # Errors
    ///
    /// If `BinaryReader` has less bytes remaining than required to parse
    /// the `Operator`.
    pub fn read_operator(&mut self) -> Result<Operator<'a>> {
        self.visit_operator(&mut OperatorFactory::new())
    }

    fn read_lane_index(&mut self, max: u8) -> Result<SIMDLaneIndex> {
        let index = self.read_u8()?;
        if index >= max {
            return Err(BinaryReaderError::new(
                "invalid lane index",
                self.original_position() - 1,
            ));
        }
        Ok(index as SIMDLaneIndex)
    }

    fn read_v128(&mut self) -> Result<V128> {
        let mut bytes = [0; 16];
        bytes.clone_from_slice(self.read_bytes(16)?);
        Ok(V128(bytes))
    }

    pub(crate) fn read_header_version(&mut self) -> Result<u32> {
        let magic_number = self.read_bytes(4)?;
        if magic_number != WASM_MAGIC_NUMBER {
            return Err(BinaryReaderError::new(
                "magic header not detected: bad magic number",
                self.original_position() - 4,
            ));
        }
        self.read_u32()
    }

    pub(crate) fn read_name_type(&mut self) -> Result<NameType> {
        let code = self.read_u7()?;
        match code {
            0 => Ok(NameType::Module),
            1 => Ok(NameType::Function),
            2 => Ok(NameType::Local),
            3 => Ok(NameType::Label),
            4 => Ok(NameType::Type),
            5 => Ok(NameType::Table),
            6 => Ok(NameType::Memory),
            7 => Ok(NameType::Global),
            8 => Ok(NameType::Element),
            9 => Ok(NameType::Data),
            _ => Ok(NameType::Unknown(code)),
        }
    }

    pub(crate) fn read_linking_type(&mut self) -> Result<LinkingType> {
        let ty = self.read_var_u32()?;
        Ok(match ty {
            1 => LinkingType::StackPointer(self.read_var_u32()?),
            _ => {
                return Err(BinaryReaderError::new(
                    "invalid linking type",
                    self.original_position() - 1,
                ));
            }
        })
    }

    pub(crate) fn read_reloc_type(&mut self) -> Result<RelocType> {
        let code = self.read_u7()?;
        match code {
            0 => Ok(RelocType::FunctionIndexLEB),
            1 => Ok(RelocType::TableIndexSLEB),
            2 => Ok(RelocType::TableIndexI32),
            3 => Ok(RelocType::GlobalAddrLEB),
            4 => Ok(RelocType::GlobalAddrSLEB),
            5 => Ok(RelocType::GlobalAddrI32),
            6 => Ok(RelocType::TypeIndexLEB),
            7 => Ok(RelocType::GlobalIndexLEB),
            _ => Err(BinaryReaderError::new(
                "invalid reloc type",
                self.original_position() - 1,
            )),
        }
    }

    pub(crate) fn read_const_expr(&mut self) -> Result<ConstExpr<'a>> {
        let expr_offset = self.position;
        self.skip_const_expr()?;
        let data = &self.buffer[expr_offset..self.position];
        Ok(ConstExpr::new(data, self.original_offset + expr_offset))
    }

    pub(crate) fn skip_const_expr(&mut self) -> Result<()> {
        // TODO add skip_operator() method and/or validate ConstExpr operators.
        loop {
            if let Operator::End = self.read_operator()? {
                return Ok(());
            }
        }
    }
}

impl<'a> BrTable<'a> {
    /// Returns the number of `br_table` entries, not including the default
    /// label
    pub fn len(&self) -> u32 {
        self.cnt
    }

    /// Returns whether `BrTable` doesn't have any labels apart from the default one.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the default target of this `br_table` instruction.
    pub fn default(&self) -> u32 {
        self.default
    }

    /// Returns the list of targets that this `br_table` instruction will be
    /// jumping to.
    ///
    /// This method will return an iterator which parses each target of this
    /// `br_table` except the default target. The returned iterator will
    /// yield `self.len()` elements.
    ///
    /// # Examples
    ///
    /// ```rust
    /// let buf = [0x0e, 0x02, 0x01, 0x02, 0x00];
    /// let mut reader = wasmparser::BinaryReader::new(&buf);
    /// let op = reader.read_operator().unwrap();
    /// if let wasmparser::Operator::BrTable { table } = op {
    ///     let targets = table.targets().collect::<Result<Vec<_>, _>>().unwrap();
    ///     assert_eq!(targets, [1, 2]);
    /// }
    /// ```
    pub fn targets(&self) -> BrTableTargets {
        BrTableTargets {
            reader: self.reader.clone(),
            remaining: self.cnt,
        }
    }
}

/// An iterator over the targets of a [`BrTable`].
///
/// # Note
///
/// This iterator parses each target of the underlying `br_table`
/// except for the default target.
/// The iterator will yield exactly as many targets as the `br_table` has.
pub struct BrTableTargets<'a> {
    reader: crate::BinaryReader<'a>,
    remaining: u32,
}

impl<'a> Iterator for BrTableTargets<'a> {
    type Item = Result<u32>;

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = usize::try_from(self.remaining).unwrap_or_else(|error| {
            panic!("could not convert remaining `u32` into `usize`: {}", error)
        });
        (remaining, Some(remaining))
    }

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            if !self.reader.eof() {
                return Some(Err(BinaryReaderError::new(
                    "trailing data in br_table",
                    self.reader.original_position(),
                )));
            }
            return None;
        }
        self.remaining -= 1;
        Some(self.reader.read_var_u32())
    }
}

impl fmt::Debug for BrTable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("BrTable");
        f.field("count", &self.cnt);
        f.field("default", &self.default);
        match self.targets().collect::<Result<Vec<_>>>() {
            Ok(targets) => {
                f.field("targets", &targets);
            }
            Err(_) => {
                f.field("reader", &self.reader);
            }
        }
        f.finish()
    }
}

/// A factory to construct [`Operator`] instances via the [`VisitOperator`] trait.
struct OperatorFactory<'a> {
    marker: core::marker::PhantomData<fn() -> &'a ()>,
}

impl<'a> OperatorFactory<'a> {
    /// Creates a new [`OperatorFactory`].
    fn new() -> Self {
        Self {
            marker: core::marker::PhantomData,
        }
    }
}

#[rustfmt::skip]
impl<'a> VisitOperator<'a> for OperatorFactory<'a> {
    type Output = Operator<'a>;

    fn visit_nop(&mut self, _offset: usize) -> Self::Output { Operator::Nop }
    fn visit_unreachable(&mut self, _offset: usize) -> Self::Output { Operator::Unreachable }
    fn visit_block(&mut self, _offset: usize, ty: BlockType) -> Self::Output { Operator::Block { ty } }
    fn visit_loop(&mut self, _offset: usize, ty: BlockType) -> Self::Output { Operator::Loop { ty } }
    fn visit_if(&mut self, _offset: usize, ty: BlockType) -> Self::Output { Operator::If { ty } }
    fn visit_else(&mut self, _offset: usize) -> Self::Output { Operator::Else }
    fn visit_try(&mut self, _offset: usize, ty: BlockType) -> Self::Output { Operator::Try { ty } }
    fn visit_catch(&mut self, _offset: usize, index: u32) -> Self::Output { Operator::Catch { index } }
    fn visit_throw(&mut self, _offset: usize, index: u32) -> Self::Output { Operator::Throw { index } }
    fn visit_rethrow(&mut self, _offset: usize, relative_depth: u32) -> Self::Output { Operator::Rethrow { relative_depth } }
    fn visit_delegate(&mut self, _offset: usize, relative_depth: u32) -> Self::Output { Operator::Delegate { relative_depth } }
    fn visit_catch_all(&mut self, _offset: usize) -> Self::Output { Operator::CatchAll }
    fn visit_end(&mut self, _offset: usize) -> Self::Output { Operator::End }
    fn visit_br(&mut self, _offset: usize, relative_depth: u32) -> Self::Output { Operator::Br { relative_depth } }
    fn visit_br_if(&mut self, _offset: usize, relative_depth: u32) -> Self::Output { Operator::BrIf { relative_depth } }
    fn visit_br_table(&mut self, _offset: usize, table: &BrTable<'a>) -> Self::Output { Operator::BrTable { table: table.clone() } }
    fn visit_return(&mut self, _offset: usize) -> Self::Output { Operator::Return }
    fn visit_call(&mut self, _offset: usize, function_index: u32) -> Self::Output { Operator::Call { function_index } }
    fn visit_return_call(&mut self, _offset: usize, function_index: u32) -> Self::Output { Operator::ReturnCall { function_index } }
    fn visit_call_indirect(&mut self, _offset: usize, index: u32, table_index: u32, table_byte: u8) -> Self::Output { Operator::CallIndirect { index, table_index, table_byte } }
    fn visit_return_call_indirect(&mut self, _offset: usize, index: u32, table_index: u32) -> Self::Output { Operator::ReturnCallIndirect { index, table_index } }
    fn visit_drop(&mut self, _offset: usize) -> Self::Output { Operator::Drop }
    fn visit_select(&mut self, _offset: usize) -> Self::Output { Operator::Select }
    fn visit_typed_select(&mut self, _offset: usize, ty: ValType) -> Self::Output { Operator::TypedSelect { ty } }
    fn visit_local_get(&mut self, _offset: usize, local_index: u32) -> Self::Output { Operator::LocalGet { local_index } }
    fn visit_local_set(&mut self, _offset: usize, local_index: u32) -> Self::Output { Operator::LocalSet { local_index } }
    fn visit_local_tee(&mut self, _offset: usize, local_index: u32) -> Self::Output { Operator::LocalTee { local_index } }
    fn visit_global_get(&mut self, _offset: usize, global_index: u32) -> Self::Output { Operator::GlobalGet { global_index } }
    fn visit_global_set(&mut self, _offset: usize, global_index: u32) -> Self::Output { Operator::GlobalSet { global_index } }
    fn visit_i32_load(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32Load { memarg } }
    fn visit_i64_load(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64Load { memarg } }
    fn visit_f32_load(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::F32Load { memarg } }
    fn visit_f64_load(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::F64Load { memarg } }
    fn visit_i32_load8_s(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32Load8S { memarg } }
    fn visit_i32_load8_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32Load8U { memarg } }
    fn visit_i32_load16_s(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32Load16S { memarg } }
    fn visit_i32_load16_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32Load16U { memarg } }
    fn visit_i64_load8_s(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64Load8S { memarg } }
    fn visit_i64_load8_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64Load8U { memarg } }
    fn visit_i64_load16_s(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64Load16S { memarg } }
    fn visit_i64_load16_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64Load16U { memarg } }
    fn visit_i64_load32_s(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64Load32S { memarg } }
    fn visit_i64_load32_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64Load32U { memarg } }
    fn visit_i32_store(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32Store { memarg } }
    fn visit_i64_store(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64Store { memarg } }
    fn visit_f32_store(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::F32Store { memarg } }
    fn visit_f64_store(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::F64Store { memarg } }
    fn visit_i32_store8(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32Store8 { memarg } }
    fn visit_i32_store16(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32Store16 { memarg } }
    fn visit_i64_store8(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64Store8 { memarg } }
    fn visit_i64_store16(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64Store16 { memarg } }
    fn visit_i64_store32(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64Store32 { memarg } }
    fn visit_memory_size(&mut self, _offset: usize, mem: u32, mem_byte: u8) -> Self::Output { Operator::MemorySize { mem, mem_byte } }
    fn visit_memory_grow(&mut self, _offset: usize, mem: u32, mem_byte: u8) -> Self::Output { Operator::MemoryGrow { mem, mem_byte } }
    fn visit_i32_const(&mut self, _offset: usize, value: i32) -> Self::Output { Operator::I32Const { value } }
    fn visit_i64_const(&mut self, _offset: usize, value: i64) -> Self::Output { Operator::I64Const { value } }
    fn visit_f32_const(&mut self, _offset: usize, value: Ieee32) -> Self::Output { Operator::F32Const { value } }
    fn visit_f64_const(&mut self, _offset: usize, value: Ieee64) -> Self::Output { Operator::F64Const { value } }
    fn visit_i32_eqz(&mut self, _offset: usize) -> Self::Output { Operator::I32Eqz }
    fn visit_i32_eq(&mut self, _offset: usize) -> Self::Output { Operator::I32Eq }
    fn visit_i32_ne(&mut self, _offset: usize) -> Self::Output { Operator::I32Ne }
    fn visit_i32_lt_s(&mut self, _offset: usize) -> Self::Output { Operator::I32LtS }
    fn visit_i32_lt_u(&mut self, _offset: usize) -> Self::Output { Operator::I32LtU }
    fn visit_i32_gt_s(&mut self, _offset: usize) -> Self::Output { Operator::I32GtS }
    fn visit_i32_gt_u(&mut self, _offset: usize) -> Self::Output { Operator::I32GtU }
    fn visit_i32_le_s(&mut self, _offset: usize) -> Self::Output { Operator::I32LeS }
    fn visit_i32_le_u(&mut self, _offset: usize) -> Self::Output { Operator::I32LeU }
    fn visit_i32_ge_s(&mut self, _offset: usize) -> Self::Output { Operator::I32GeS }
    fn visit_i32_ge_u(&mut self, _offset: usize) -> Self::Output { Operator::I32GeU }
    fn visit_i64_eqz(&mut self, _offset: usize) -> Self::Output { Operator::I64Eqz }
    fn visit_i64_eq(&mut self, _offset: usize) -> Self::Output { Operator::I64Eq }
    fn visit_i64_ne(&mut self, _offset: usize) -> Self::Output { Operator::I64Ne }
    fn visit_i64_lt_s(&mut self, _offset: usize) -> Self::Output { Operator::I64LtS }
    fn visit_i64_lt_u(&mut self, _offset: usize) -> Self::Output { Operator::I64LtU }
    fn visit_i64_gt_s(&mut self, _offset: usize) -> Self::Output { Operator::I64GtS }
    fn visit_i64_gt_u(&mut self, _offset: usize) -> Self::Output { Operator::I64GtU }
    fn visit_i64_le_s(&mut self, _offset: usize) -> Self::Output { Operator::I64LeS }
    fn visit_i64_le_u(&mut self, _offset: usize) -> Self::Output { Operator::I64LeU }
    fn visit_i64_ge_s(&mut self, _offset: usize) -> Self::Output { Operator::I64GeS }
    fn visit_i64_ge_u(&mut self, _offset: usize) -> Self::Output { Operator::I64GeU }
    fn visit_f32_eq(&mut self, _offset: usize) -> Self::Output { Operator::F32Eq }
    fn visit_f32_ne(&mut self, _offset: usize) -> Self::Output { Operator::F32Ne }
    fn visit_f32_lt(&mut self, _offset: usize) -> Self::Output { Operator::F32Lt }
    fn visit_f32_gt(&mut self, _offset: usize) -> Self::Output { Operator::F32Gt }
    fn visit_f32_le(&mut self, _offset: usize) -> Self::Output { Operator::F32Le }
    fn visit_f32_ge(&mut self, _offset: usize) -> Self::Output { Operator::F32Ge }
    fn visit_f64_eq(&mut self, _offset: usize) -> Self::Output { Operator::F64Eq }
    fn visit_f64_ne(&mut self, _offset: usize) -> Self::Output { Operator::F64Ne }
    fn visit_f64_lt(&mut self, _offset: usize) -> Self::Output { Operator::F64Lt }
    fn visit_f64_gt(&mut self, _offset: usize) -> Self::Output { Operator::F64Gt }
    fn visit_f64_le(&mut self, _offset: usize) -> Self::Output { Operator::F64Le }
    fn visit_f64_ge(&mut self, _offset: usize) -> Self::Output { Operator::F64Ge }
    fn visit_i32_clz(&mut self, _offset: usize) -> Self::Output { Operator::I32Clz }
    fn visit_i32_ctz(&mut self, _offset: usize) -> Self::Output { Operator::I32Ctz }
    fn visit_i32_popcnt(&mut self, _offset: usize) -> Self::Output { Operator::I32Popcnt }
    fn visit_i32_add(&mut self, _offset: usize) -> Self::Output { Operator::I32Add }
    fn visit_i32_sub(&mut self, _offset: usize) -> Self::Output { Operator::I32Sub }
    fn visit_i32_mul(&mut self, _offset: usize) -> Self::Output { Operator::I32Mul }
    fn visit_i32_div_s(&mut self, _offset: usize) -> Self::Output { Operator::I32DivS }
    fn visit_i32_div_u(&mut self, _offset: usize) -> Self::Output { Operator::I32DivU }
    fn visit_i32_rem_s(&mut self, _offset: usize) -> Self::Output { Operator::I32RemS }
    fn visit_i32_rem_u(&mut self, _offset: usize) -> Self::Output { Operator::I32RemU }
    fn visit_i32_and(&mut self, _offset: usize) -> Self::Output { Operator::I32And }
    fn visit_i32_or(&mut self, _offset: usize) -> Self::Output { Operator::I32Or }
    fn visit_i32_xor(&mut self, _offset: usize) -> Self::Output { Operator::I32Xor }
    fn visit_i32_shl(&mut self, _offset: usize) -> Self::Output { Operator::I32Shl }
    fn visit_i32_shr_s(&mut self, _offset: usize) -> Self::Output { Operator::I32ShrS }
    fn visit_i32_shr_u(&mut self, _offset: usize) -> Self::Output { Operator::I32ShrU }
    fn visit_i32_rotl(&mut self, _offset: usize) -> Self::Output { Operator::I32Rotl }
    fn visit_i32_rotr(&mut self, _offset: usize) -> Self::Output { Operator::I32Rotr }
    fn visit_i64_clz(&mut self, _offset: usize) -> Self::Output { Operator::I64Clz }
    fn visit_i64_ctz(&mut self, _offset: usize) -> Self::Output { Operator::I64Ctz }
    fn visit_i64_popcnt(&mut self, _offset: usize) -> Self::Output { Operator::I64Popcnt }
    fn visit_i64_add(&mut self, _offset: usize) -> Self::Output { Operator::I64Add }
    fn visit_i64_sub(&mut self, _offset: usize) -> Self::Output { Operator::I64Sub }
    fn visit_i64_mul(&mut self, _offset: usize) -> Self::Output { Operator::I64Mul }
    fn visit_i64_div_s(&mut self, _offset: usize) -> Self::Output { Operator::I64DivS }
    fn visit_i64_div_u(&mut self, _offset: usize) -> Self::Output { Operator::I64DivU }
    fn visit_i64_rem_s(&mut self, _offset: usize) -> Self::Output { Operator::I64RemS }
    fn visit_i64_rem_u(&mut self, _offset: usize) -> Self::Output { Operator::I64RemU }
    fn visit_i64_and(&mut self, _offset: usize) -> Self::Output { Operator::I64And }
    fn visit_i64_or(&mut self, _offset: usize) -> Self::Output { Operator::I64Or }
    fn visit_i64_xor(&mut self, _offset: usize) -> Self::Output { Operator::I64Xor }
    fn visit_i64_shl(&mut self, _offset: usize) -> Self::Output { Operator::I64Shl }
    fn visit_i64_shr_s(&mut self, _offset: usize) -> Self::Output { Operator::I64ShrS }
    fn visit_i64_shr_u(&mut self, _offset: usize) -> Self::Output { Operator::I64ShrU }
    fn visit_i64_rotl(&mut self, _offset: usize) -> Self::Output { Operator::I64Rotl }
    fn visit_i64_rotr(&mut self, _offset: usize) -> Self::Output { Operator::I64Rotr }
    fn visit_f32_abs(&mut self, _offset: usize) -> Self::Output { Operator::F32Abs }
    fn visit_f32_neg(&mut self, _offset: usize) -> Self::Output { Operator::F32Neg }
    fn visit_f32_ceil(&mut self, _offset: usize) -> Self::Output { Operator::F32Ceil }
    fn visit_f32_floor(&mut self, _offset: usize) -> Self::Output { Operator::F32Floor }
    fn visit_f32_trunc(&mut self, _offset: usize) -> Self::Output { Operator::F32Trunc }
    fn visit_f32_nearest(&mut self, _offset: usize) -> Self::Output { Operator::F32Nearest }
    fn visit_f32_sqrt(&mut self, _offset: usize) -> Self::Output { Operator::F32Sqrt }
    fn visit_f32_add(&mut self, _offset: usize) -> Self::Output { Operator::F32Add }
    fn visit_f32_sub(&mut self, _offset: usize) -> Self::Output { Operator::F32Sub }
    fn visit_f32_mul(&mut self, _offset: usize) -> Self::Output { Operator::F32Mul }
    fn visit_f32_div(&mut self, _offset: usize) -> Self::Output { Operator::F32Div }
    fn visit_f32_min(&mut self, _offset: usize) -> Self::Output { Operator::F32Min }
    fn visit_f32_max(&mut self, _offset: usize) -> Self::Output { Operator::F32Max }
    fn visit_f32_copysign(&mut self, _offset: usize) -> Self::Output { Operator::F32Copysign }
    fn visit_f64_abs(&mut self, _offset: usize) -> Self::Output { Operator::F64Abs }
    fn visit_f64_neg(&mut self, _offset: usize) -> Self::Output { Operator::F64Neg }
    fn visit_f64_ceil(&mut self, _offset: usize) -> Self::Output { Operator::F64Ceil }
    fn visit_f64_floor(&mut self, _offset: usize) -> Self::Output { Operator::F64Floor }
    fn visit_f64_trunc(&mut self, _offset: usize) -> Self::Output { Operator::F64Trunc }
    fn visit_f64_nearest(&mut self, _offset: usize) -> Self::Output { Operator::F64Nearest }
    fn visit_f64_sqrt(&mut self, _offset: usize) -> Self::Output { Operator::F64Sqrt }
    fn visit_f64_add(&mut self, _offset: usize) -> Self::Output { Operator::F64Add }
    fn visit_f64_sub(&mut self, _offset: usize) -> Self::Output { Operator::F64Sub }
    fn visit_f64_mul(&mut self, _offset: usize) -> Self::Output { Operator::F64Mul }
    fn visit_f64_div(&mut self, _offset: usize) -> Self::Output { Operator::F64Div }
    fn visit_f64_min(&mut self, _offset: usize) -> Self::Output { Operator::F64Min }
    fn visit_f64_max(&mut self, _offset: usize) -> Self::Output { Operator::F64Max }
    fn visit_f64_copysign(&mut self, _offset: usize) -> Self::Output { Operator::F64Copysign }
    fn visit_i32_wrap_i64(&mut self, _offset: usize) -> Self::Output { Operator::I32WrapI64 }
    fn visit_i32_trunc_f32s(&mut self, _offset: usize) -> Self::Output { Operator::I32TruncF32S }
    fn visit_i32_trunc_f32u(&mut self, _offset: usize) -> Self::Output { Operator::I32TruncF32U }
    fn visit_i32_trunc_f64s(&mut self, _offset: usize) -> Self::Output { Operator::I32TruncF64S }
    fn visit_i32_trunc_f64u(&mut self, _offset: usize) -> Self::Output { Operator::I32TruncF64U }
    fn visit_i64_extend_i32s(&mut self, _offset: usize) -> Self::Output { Operator::I64ExtendI32S }
    fn visit_i64_extend_i32u(&mut self, _offset: usize) -> Self::Output { Operator::I64ExtendI32U }
    fn visit_i64_trunc_f32s(&mut self, _offset: usize) -> Self::Output { Operator::I64TruncF32S }
    fn visit_i64_trunc_f32u(&mut self, _offset: usize) -> Self::Output { Operator::I64TruncF32U }
    fn visit_i64_trunc_f64s(&mut self, _offset: usize) -> Self::Output { Operator::I64TruncF64S }
    fn visit_i64_trunc_f64u(&mut self, _offset: usize) -> Self::Output { Operator::I64TruncF64U }
    fn visit_f32_convert_i32s(&mut self, _offset: usize) -> Self::Output { Operator::F32ConvertI32S }
    fn visit_f32_convert_i32u(&mut self, _offset: usize) -> Self::Output { Operator::F32ConvertI32U }
    fn visit_f32_convert_i64s(&mut self, _offset: usize) -> Self::Output { Operator::F32ConvertI64S }
    fn visit_f32_convert_i64u(&mut self, _offset: usize) -> Self::Output { Operator::F32ConvertI64U }
    fn visit_f32_demote_f64(&mut self, _offset: usize) -> Self::Output { Operator::F32DemoteF64 }
    fn visit_f64_convert_i32s(&mut self, _offset: usize) -> Self::Output { Operator::F64ConvertI32S }
    fn visit_f64_convert_i32u(&mut self, _offset: usize) -> Self::Output { Operator::F64ConvertI32U }
    fn visit_f64_convert_i64s(&mut self, _offset: usize) -> Self::Output { Operator::F64ConvertI64S }
    fn visit_f64_convert_i64u(&mut self, _offset: usize) -> Self::Output { Operator::F64ConvertI64U }
    fn visit_f64_promote_f32(&mut self, _offset: usize) -> Self::Output { Operator::F64PromoteF32 }
    fn visit_i32_reinterpret_f32(&mut self, _offset: usize) -> Self::Output { Operator::I32ReinterpretF32 }
    fn visit_i64_reinterpret_f64(&mut self, _offset: usize) -> Self::Output { Operator::I64ReinterpretF64 }
    fn visit_f32_reinterpret_i32(&mut self, _offset: usize) -> Self::Output { Operator::F32ReinterpretI32 }
    fn visit_f64_reinterpret_i64(&mut self, _offset: usize) -> Self::Output { Operator::F64ReinterpretI64 }
    fn visit_i32_trunc_sat_f32s(&mut self, _offset: usize) -> Self::Output { Operator::I32TruncSatF32S }
    fn visit_i32_trunc_sat_f32u(&mut self, _offset: usize) -> Self::Output { Operator::I32TruncSatF32U }
    fn visit_i32_trunc_sat_f64s(&mut self, _offset: usize) -> Self::Output { Operator::I32TruncSatF64S }
    fn visit_i32_trunc_sat_f64u(&mut self, _offset: usize) -> Self::Output { Operator::I32TruncSatF64U }
    fn visit_i64_trunc_sat_f32s(&mut self, _offset: usize) -> Self::Output { Operator::I64TruncSatF32S }
    fn visit_i64_trunc_sat_f32u(&mut self, _offset: usize) -> Self::Output { Operator::I64TruncSatF32U }
    fn visit_i64_trunc_sat_f64s(&mut self, _offset: usize) -> Self::Output { Operator::I64TruncSatF64S }
    fn visit_i64_trunc_sat_f64u(&mut self, _offset: usize) -> Self::Output { Operator::I64TruncSatF64U }
    fn visit_i32_extend8_s(&mut self, _offset: usize) -> Self::Output { Operator::I32Extend8S }
    fn visit_i32_extend16_s(&mut self, _offset: usize) -> Self::Output { Operator::I32Extend16S }
    fn visit_i64_extend8_s(&mut self, _offset: usize) -> Self::Output { Operator::I64Extend8S }
    fn visit_i64_extend16_s(&mut self, _offset: usize) -> Self::Output { Operator::I64Extend16S }
    fn visit_i64_extend32_s(&mut self, _offset: usize) -> Self::Output { Operator::I64Extend32S }
    fn visit_i32_atomic_load(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicLoad { memarg } }
    fn visit_i32_atomic_load16_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicLoad16U { memarg } }
    fn visit_i32_atomic_load8_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicLoad8U { memarg } }
    fn visit_i64_atomic_load(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicLoad { memarg } }
    fn visit_i64_atomic_load32_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicLoad32U { memarg } }
    fn visit_i64_atomic_load16_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicLoad16U { memarg } }
    fn visit_i64_atomic_load8_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicLoad8U { memarg } }
    fn visit_i32_atomic_store(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicStore { memarg } }
    fn visit_i32_atomic_store16(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicStore16 { memarg } }
    fn visit_i32_atomic_store8(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicStore8 { memarg } }
    fn visit_i64_atomic_store(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicStore { memarg } }
    fn visit_i64_atomic_store32(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicStore32 { memarg } }
    fn visit_i64_atomic_store16(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicStore16 { memarg } }
    fn visit_i64_atomic_store8(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicStore8 { memarg } }
    fn visit_i32_atomic_rmw_add(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmwAdd { memarg } }
    fn visit_i32_atomic_rmw_sub(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmwSub { memarg } }
    fn visit_i32_atomic_rmw_and(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmwAnd { memarg } }
    fn visit_i32_atomic_rmw_or(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmwOr { memarg } }
    fn visit_i32_atomic_rmw_xor(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmwXor { memarg } }
    fn visit_i32_atomic_rmw16_add_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw16AddU { memarg } }
    fn visit_i32_atomic_rmw16_sub_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw16SubU { memarg } }
    fn visit_i32_atomic_rmw16_and_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw16AndU { memarg } }
    fn visit_i32_atomic_rmw16_or_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw16OrU { memarg } }
    fn visit_i32_atomic_rmw16_xor_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw16XorU { memarg } }
    fn visit_i32_atomic_rmw8_add_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw8AddU { memarg } }
    fn visit_i32_atomic_rmw8_sub_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw8SubU { memarg } }
    fn visit_i32_atomic_rmw8_and_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw8AndU { memarg } }
    fn visit_i32_atomic_rmw8_or_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw8OrU { memarg } }
    fn visit_i32_atomic_rmw8_xor_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw8XorU { memarg } }
    fn visit_i64_atomic_rmw_add(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmwAdd { memarg } }
    fn visit_i64_atomic_rmw_sub(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmwSub { memarg } }
    fn visit_i64_atomic_rmw_and(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmwAnd { memarg } }
    fn visit_i64_atomic_rmw_or(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmwOr { memarg } }
    fn visit_i64_atomic_rmw_xor(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmwXor { memarg } }
    fn visit_i64_atomic_rmw32_add_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw32AddU { memarg } }
    fn visit_i64_atomic_rmw32_sub_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw32SubU { memarg } }
    fn visit_i64_atomic_rmw32_and_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw32AndU { memarg } }
    fn visit_i64_atomic_rmw32_or_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw32OrU { memarg } }
    fn visit_i64_atomic_rmw32_xor_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw32XorU { memarg } }
    fn visit_i64_atomic_rmw16_add_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw16AddU { memarg } }
    fn visit_i64_atomic_rmw16_sub_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw16SubU { memarg } }
    fn visit_i64_atomic_rmw16_and_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw16AndU { memarg } }
    fn visit_i64_atomic_rmw16_or_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw16OrU { memarg } }
    fn visit_i64_atomic_rmw16_xor_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw16XorU { memarg } }
    fn visit_i64_atomic_rmw8_add_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw8AddU { memarg } }
    fn visit_i64_atomic_rmw8_sub_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw8SubU { memarg } }
    fn visit_i64_atomic_rmw8_and_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw8AndU { memarg } }
    fn visit_i64_atomic_rmw8_or_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw8OrU { memarg } }
    fn visit_i64_atomic_rmw8_xor_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw8XorU { memarg } }
    fn visit_i32_atomic_rmw_xchg(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmwXchg { memarg } }
    fn visit_i32_atomic_rmw16_xchg_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw16XchgU { memarg } }
    fn visit_i32_atomic_rmw8_xchg_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw8XchgU { memarg } }
    fn visit_i32_atomic_rmw_cmpxchg(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmwCmpxchg { memarg } }
    fn visit_i32_atomic_rmw16_cmpxchg_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw16CmpxchgU { memarg } }
    fn visit_i32_atomic_rmw8_cmpxchg_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I32AtomicRmw8CmpxchgU { memarg } }
    fn visit_i64_atomic_rmw_xchg(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmwXchg { memarg } }
    fn visit_i64_atomic_rmw32_xchg_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw32XchgU { memarg } }
    fn visit_i64_atomic_rmw16_xchg_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw16XchgU { memarg } }
    fn visit_i64_atomic_rmw8_xchg_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw8XchgU { memarg } }
    fn visit_i64_atomic_rmw_cmpxchg(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmwCmpxchg { memarg } }
    fn visit_i64_atomic_rmw32_cmpxchg_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw32CmpxchgU { memarg } }
    fn visit_i64_atomic_rmw16_cmpxchg_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw16CmpxchgU { memarg } }
    fn visit_i64_atomic_rmw8_cmpxchg_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::I64AtomicRmw8CmpxchgU { memarg } }
    fn visit_memory_atomic_notify(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::MemoryAtomicNotify { memarg } }
    fn visit_memory_atomic_wait32(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::MemoryAtomicWait32 { memarg } }
    fn visit_memory_atomic_wait64(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::MemoryAtomicWait64 { memarg } }
    fn visit_atomic_fence(&mut self, _offset: usize,  flags: u8) -> Self::Output { Operator::AtomicFence { flags } }
    fn visit_ref_null(&mut self, _offset: usize, ty: ValType) -> Self::Output { Operator::RefNull { ty } }
    fn visit_ref_is_null(&mut self, _offset: usize) -> Self::Output { Operator::RefIsNull }
    fn visit_ref_func(&mut self, _offset: usize,  function_index: u32) -> Self::Output { Operator::RefFunc { function_index } }
    fn visit_v128_load(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load { memarg } }
    fn visit_v128_store(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Store { memarg } }
    fn visit_v128_const(&mut self, _offset: usize, value: V128) -> Self::Output { Operator::V128Const { value } }
    fn visit_i8x16_splat(&mut self, _offset: usize) -> Self::Output { Operator::I8x16Splat }
    fn visit_i16x8_splat(&mut self, _offset: usize) -> Self::Output { Operator::I16x8Splat }
    fn visit_i32x4_splat(&mut self, _offset: usize) -> Self::Output { Operator::I32x4Splat }
    fn visit_i64x2_splat(&mut self, _offset: usize) -> Self::Output { Operator::I64x2Splat }
    fn visit_f32x4_splat(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Splat }
    fn visit_f64x2_splat(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Splat }
    fn visit_i8x16_extract_lane_s(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::I8x16ExtractLaneS { lane } }
    fn visit_i8x16_extract_lane_u(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::I8x16ExtractLaneU { lane } }
    fn visit_i16x8_extract_lane_s(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::I16x8ExtractLaneS { lane } }
    fn visit_i16x8_extract_lane_u(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::I16x8ExtractLaneU { lane } }
    fn visit_i32x4_extract_lane(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::I32x4ExtractLane { lane } }
    fn visit_i8x16_replace_lane(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::I8x16ReplaceLane { lane } }
    fn visit_i16x8_replace_lane(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::I16x8ReplaceLane { lane } }
    fn visit_i32x4_replace_lane(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::I32x4ReplaceLane { lane } }
    fn visit_i64x2_extract_lane(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::I64x2ExtractLane { lane } }
    fn visit_i64x2_replace_lane(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::I64x2ReplaceLane { lane } }
    fn visit_f32x4_extract_lane(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::F32x4ExtractLane { lane } }
    fn visit_f32x4_replace_lane(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::F32x4ReplaceLane { lane } }
    fn visit_f64x2_extract_lane(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::F64x2ExtractLane { lane } }
    fn visit_f64x2_replace_lane(&mut self, _offset: usize, lane: SIMDLaneIndex) -> Self::Output { Operator::F64x2ReplaceLane { lane } }
    fn visit_f32x4_eq(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Eq }
    fn visit_f32x4_ne(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Ne }
    fn visit_f32x4_lt(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Lt }
    fn visit_f32x4_gt(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Gt }
    fn visit_f32x4_le(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Le }
    fn visit_f32x4_ge(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Ge }
    fn visit_f64x2_eq(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Eq }
    fn visit_f64x2_ne(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Ne }
    fn visit_f64x2_lt(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Lt }
    fn visit_f64x2_gt(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Gt }
    fn visit_f64x2_le(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Le }
    fn visit_f64x2_ge(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Ge }
    fn visit_f32x4_add(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Add }
    fn visit_f32x4_sub(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Sub }
    fn visit_f32x4_mul(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Mul }
    fn visit_f32x4_div(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Div }
    fn visit_f32x4_min(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Min }
    fn visit_f32x4_max(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Max }
    fn visit_f32x4_p_min(&mut self, _offset: usize) -> Self::Output { Operator::F32x4PMin }
    fn visit_f32x4_p_max(&mut self, _offset: usize) -> Self::Output { Operator::F32x4PMax }
    fn visit_f64x2_add(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Add }
    fn visit_f64x2_sub(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Sub }
    fn visit_f64x2_mul(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Mul }
    fn visit_f64x2_div(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Div }
    fn visit_f64x2_min(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Min }
    fn visit_f64x2_max(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Max }
    fn visit_f64x2_p_min(&mut self, _offset: usize) -> Self::Output { Operator::F64x2PMin }
    fn visit_f64x2_p_max(&mut self, _offset: usize) -> Self::Output { Operator::F64x2PMax }
    fn visit_f32x4_relaxed_min(&mut self, _offset: usize) -> Self::Output { Operator::F32x4RelaxedMin }
    fn visit_f32x4_relaxed_max(&mut self, _offset: usize) -> Self::Output { Operator::F32x4RelaxedMax }
    fn visit_f64x2_relaxed_min(&mut self, _offset: usize) -> Self::Output { Operator::F64x2RelaxedMin }
    fn visit_f64x2_relaxed_max(&mut self, _offset: usize) -> Self::Output { Operator::F64x2RelaxedMax }
    fn visit_i8x16_eq(&mut self, _offset: usize) -> Self::Output { Operator::I8x16Eq }
    fn visit_i8x16_ne(&mut self, _offset: usize) -> Self::Output { Operator::I8x16Ne }
    fn visit_i8x16_lt_s(&mut self, _offset: usize) -> Self::Output { Operator::I8x16LtS }
    fn visit_i8x16_lt_u(&mut self, _offset: usize) -> Self::Output { Operator::I8x16LtU }
    fn visit_i8x16_gt_s(&mut self, _offset: usize) -> Self::Output { Operator::I8x16GtS }
    fn visit_i8x16_gt_u(&mut self, _offset: usize) -> Self::Output { Operator::I8x16GtU }
    fn visit_i8x16_le_s(&mut self, _offset: usize) -> Self::Output { Operator::I8x16LeS }
    fn visit_i8x16_le_u(&mut self, _offset: usize) -> Self::Output { Operator::I8x16LeU }
    fn visit_i8x16_ge_s(&mut self, _offset: usize) -> Self::Output { Operator::I8x16GeS }
    fn visit_i8x16_ge_u(&mut self, _offset: usize) -> Self::Output { Operator::I8x16GeU }
    fn visit_i16x8_eq(&mut self, _offset: usize) -> Self::Output { Operator::I16x8Eq }
    fn visit_i16x8_ne(&mut self, _offset: usize) -> Self::Output { Operator::I16x8Ne }
    fn visit_i16x8_lt_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8LtS }
    fn visit_i16x8_lt_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8LtU }
    fn visit_i16x8_gt_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8GtS }
    fn visit_i16x8_gt_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8GtU }
    fn visit_i16x8_le_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8LeS }
    fn visit_i16x8_le_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8LeU }
    fn visit_i16x8_ge_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8GeS }
    fn visit_i16x8_ge_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8GeU }
    fn visit_i32x4_eq(&mut self, _offset: usize) -> Self::Output { Operator::I32x4Eq }
    fn visit_i32x4_ne(&mut self, _offset: usize) -> Self::Output { Operator::I32x4Ne }
    fn visit_i32x4_lt_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4LtS }
    fn visit_i32x4_lt_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4LtU }
    fn visit_i32x4_gt_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4GtS }
    fn visit_i32x4_gt_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4GtU }
    fn visit_i32x4_le_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4LeS }
    fn visit_i32x4_le_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4LeU }
    fn visit_i32x4_ge_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4GeS }
    fn visit_i32x4_ge_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4GeU }
    fn visit_i64x2_eq(&mut self, _offset: usize) -> Self::Output { Operator::I64x2Eq }
    fn visit_i64x2_ne(&mut self, _offset: usize) -> Self::Output { Operator::I64x2Ne }
    fn visit_i64x2_lt_s(&mut self, _offset: usize) -> Self::Output { Operator::I64x2LtS }
    fn visit_i64x2_gt_s(&mut self, _offset: usize) -> Self::Output { Operator::I64x2GtS }
    fn visit_i64x2_le_s(&mut self, _offset: usize) -> Self::Output { Operator::I64x2LeS }
    fn visit_i64x2_ge_s(&mut self, _offset: usize) -> Self::Output { Operator::I64x2GeS }
    fn visit_v128_and(&mut self, _offset: usize) -> Self::Output { Operator::V128And }
    fn visit_v128_and_not(&mut self, _offset: usize) -> Self::Output { Operator::V128AndNot }
    fn visit_v128_or(&mut self, _offset: usize) -> Self::Output { Operator::V128Or }
    fn visit_v128_xor(&mut self, _offset: usize) -> Self::Output { Operator::V128Xor }
    fn visit_i8x16_add(&mut self, _offset: usize) -> Self::Output { Operator::I8x16Add }
    fn visit_i8x16_add_sat_s(&mut self, _offset: usize) -> Self::Output { Operator::I8x16AddSatS }
    fn visit_i8x16_add_sat_u(&mut self, _offset: usize) -> Self::Output { Operator::I8x16AddSatU }
    fn visit_i8x16_sub(&mut self, _offset: usize) -> Self::Output { Operator::I8x16Sub }
    fn visit_i8x16_sub_sat_s(&mut self, _offset: usize) -> Self::Output { Operator::I8x16SubSatS }
    fn visit_i8x16_sub_sat_u(&mut self, _offset: usize) -> Self::Output { Operator::I8x16SubSatU }
    fn visit_i8x16_min_s(&mut self, _offset: usize) -> Self::Output { Operator::I8x16MinS }
    fn visit_i8x16_min_u(&mut self, _offset: usize) -> Self::Output { Operator::I8x16MinU }
    fn visit_i8x16_max_s(&mut self, _offset: usize) -> Self::Output { Operator::I8x16MaxS }
    fn visit_i8x16_max_u(&mut self, _offset: usize) -> Self::Output { Operator::I8x16MaxU }
    fn visit_i16x8_add(&mut self, _offset: usize) -> Self::Output { Operator::I16x8Add }
    fn visit_i16x8_add_sat_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8AddSatS }
    fn visit_i16x8_add_sat_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8AddSatU }
    fn visit_i16x8_sub(&mut self, _offset: usize) -> Self::Output { Operator::I16x8Sub }
    fn visit_i16x8_sub_sat_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8SubSatS }
    fn visit_i16x8_sub_sat_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8SubSatU }
    fn visit_i16x8_mul(&mut self, _offset: usize) -> Self::Output { Operator::I16x8Mul }
    fn visit_i16x8_min_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8MinS }
    fn visit_i16x8_min_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8MinU }
    fn visit_i16x8_max_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8MaxS }
    fn visit_i16x8_max_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8MaxU }
    fn visit_i32x4_add(&mut self, _offset: usize) -> Self::Output { Operator::I32x4Add }
    fn visit_i32x4_sub(&mut self, _offset: usize) -> Self::Output { Operator::I32x4Sub }
    fn visit_i32x4_mul(&mut self, _offset: usize) -> Self::Output { Operator::I32x4Mul }
    fn visit_i32x4_min_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4MinS }
    fn visit_i32x4_min_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4MinU }
    fn visit_i32x4_max_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4MaxS }
    fn visit_i32x4_max_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4MaxU }
    fn visit_i32x4_dot_i16x8_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4DotI16x8S }
    fn visit_i64x2_add(&mut self, _offset: usize) -> Self::Output { Operator::I64x2Add }
    fn visit_i64x2_sub(&mut self, _offset: usize) -> Self::Output { Operator::I64x2Sub }
    fn visit_i64x2_mul(&mut self, _offset: usize) -> Self::Output { Operator::I64x2Mul }
    fn visit_i8x16_rounding_average_u(&mut self, _offset: usize) -> Self::Output { Operator::I8x16RoundingAverageU }
    fn visit_i16x8_rounding_average_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8RoundingAverageU }
    fn visit_i8x16_narrow_i16x8_s(&mut self, _offset: usize) -> Self::Output { Operator::I8x16NarrowI16x8S }
    fn visit_i8x16_narrow_i16x8_u(&mut self, _offset: usize) -> Self::Output { Operator::I8x16NarrowI16x8U }
    fn visit_i16x8_narrow_i32x4_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8NarrowI32x4S }
    fn visit_i16x8_narrow_i32x4_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8NarrowI32x4U }
    fn visit_i16x8_ext_mul_low_i8x16_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ExtMulLowI8x16S }
    fn visit_i16x8_ext_mul_high_i8x16_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ExtMulHighI8x16S }
    fn visit_i16x8_ext_mul_low_i8x16_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ExtMulLowI8x16U }
    fn visit_i16x8_ext_mul_high_i8x16_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ExtMulHighI8x16U }
    fn visit_i32x4_ext_mul_low_i16x8_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ExtMulLowI16x8S }
    fn visit_i32x4_ext_mul_high_i16x8_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ExtMulHighI16x8S }
    fn visit_i32x4_ext_mul_low_i16x8_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ExtMulLowI16x8U }
    fn visit_i32x4_ext_mul_high_i16x8_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ExtMulHighI16x8U }
    fn visit_i64x2_ext_mul_low_i32x4_s(&mut self, _offset: usize) -> Self::Output { Operator::I64x2ExtMulLowI32x4S }
    fn visit_i64x2_ext_mul_high_i32x4_s(&mut self, _offset: usize) -> Self::Output { Operator::I64x2ExtMulHighI32x4S }
    fn visit_i64x2_ext_mul_low_i32x4_u(&mut self, _offset: usize) -> Self::Output { Operator::I64x2ExtMulLowI32x4U }
    fn visit_i64x2_ext_mul_high_i32x4_u(&mut self, _offset: usize) -> Self::Output { Operator::I64x2ExtMulHighI32x4U }
    fn visit_i16x8_q15_mulr_sat_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8Q15MulrSatS }
    fn visit_f32x4_ceil(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Ceil }
    fn visit_f32x4_floor(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Floor }
    fn visit_f32x4_trunc(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Trunc }
    fn visit_f32x4_nearest(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Nearest }
    fn visit_f64x2_ceil(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Ceil }
    fn visit_f64x2_floor(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Floor }
    fn visit_f64x2_trunc(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Trunc }
    fn visit_f64x2_nearest(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Nearest }
    fn visit_f32x4_abs(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Abs }
    fn visit_f32x4_neg(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Neg }
    fn visit_f32x4_sqrt(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Sqrt }
    fn visit_f64x2_abs(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Abs }
    fn visit_f64x2_neg(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Neg }
    fn visit_f64x2_sqrt(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Sqrt }
    fn visit_f32x4_demote_f64x2_zero(&mut self, _offset: usize) -> Self::Output { Operator::F32x4DemoteF64x2Zero }
    fn visit_f64x2_promote_low_f32x4(&mut self, _offset: usize) -> Self::Output { Operator::F64x2PromoteLowF32x4 }
    fn visit_f64x2_convert_low_i32x4_s(&mut self, _offset: usize) -> Self::Output { Operator::F64x2ConvertLowI32x4S }
    fn visit_f64x2_convert_low_i32x4_u(&mut self, _offset: usize) -> Self::Output { Operator::F64x2ConvertLowI32x4U }
    fn visit_i32x4_trunc_sat_f32x4_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4TruncSatF32x4S }
    fn visit_i32x4_trunc_sat_f32x4_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4TruncSatF32x4U }
    fn visit_i32x4_trunc_sat_f64x2_s_zero(&mut self, _offset: usize) -> Self::Output { Operator::I32x4TruncSatF64x2SZero }
    fn visit_i32x4_trunc_sat_f64x2_u_zero(&mut self, _offset: usize) -> Self::Output { Operator::I32x4TruncSatF64x2UZero }
    fn visit_f32x4_convert_i32x4_s(&mut self, _offset: usize) -> Self::Output { Operator::F32x4ConvertI32x4S }
    fn visit_f32x4_convert_i32x4_u(&mut self, _offset: usize) -> Self::Output { Operator::F32x4ConvertI32x4U }
    fn visit_v128_not(&mut self, _offset: usize) -> Self::Output { Operator::V128Not }
    fn visit_i8x16_abs(&mut self, _offset: usize) -> Self::Output { Operator::I8x16Abs }
    fn visit_i8x16_neg(&mut self, _offset: usize) -> Self::Output { Operator::I8x16Neg }
    fn visit_i8x16_popcnt(&mut self, _offset: usize) -> Self::Output { Operator::I8x16Popcnt }
    fn visit_i16x8_abs(&mut self, _offset: usize) -> Self::Output { Operator::I16x8Abs }
    fn visit_i16x8_neg(&mut self, _offset: usize) -> Self::Output { Operator::I16x8Neg }
    fn visit_i32x4_abs(&mut self, _offset: usize) -> Self::Output { Operator::I32x4Abs }
    fn visit_i32x4_neg(&mut self, _offset: usize) -> Self::Output { Operator::I32x4Neg }
    fn visit_i64x2_abs(&mut self, _offset: usize) -> Self::Output { Operator::I64x2Abs }
    fn visit_i64x2_neg(&mut self, _offset: usize) -> Self::Output { Operator::I64x2Neg }
    fn visit_i16x8_extend_low_i8x16_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ExtendLowI8x16S }
    fn visit_i16x8_extend_high_i8x16_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ExtendHighI8x16S }
    fn visit_i16x8_extend_low_i8x16_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ExtendLowI8x16U }
    fn visit_i16x8_extend_high_i8x16_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ExtendHighI8x16U }
    fn visit_i32x4_extend_low_i16x8_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ExtendLowI16x8S }
    fn visit_i32x4_extend_high_i16x8_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ExtendHighI16x8S }
    fn visit_i32x4_extend_low_i16x8_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ExtendLowI16x8U }
    fn visit_i32x4_extend_high_i16x8_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ExtendHighI16x8U }
    fn visit_i64x2_extend_low_i32x4_s(&mut self, _offset: usize) -> Self::Output { Operator::I64x2ExtendLowI32x4S }
    fn visit_i64x2_extend_high_i32x4_s(&mut self, _offset: usize) -> Self::Output { Operator::I64x2ExtendHighI32x4S }
    fn visit_i64x2_extend_low_i32x4_u(&mut self, _offset: usize) -> Self::Output { Operator::I64x2ExtendLowI32x4U }
    fn visit_i64x2_extend_high_i32x4_u(&mut self, _offset: usize) -> Self::Output { Operator::I64x2ExtendHighI32x4U }
    fn visit_i16x8_ext_add_pairwise_i8x16_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ExtAddPairwiseI8x16S }
    fn visit_i16x8_ext_add_pairwise_i8x16_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ExtAddPairwiseI8x16U }
    fn visit_i32x4_ext_add_pairwise_i16x8_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ExtAddPairwiseI16x8S }
    fn visit_i32x4_ext_add_pairwise_i16x8_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ExtAddPairwiseI16x8U }
    fn visit_i32x4_relaxed_trunc_sat_f32x4_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4RelaxedTruncSatF32x4S }
    fn visit_i32x4_relaxed_trunc_sat_f32x4_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4RelaxedTruncSatF32x4U }
    fn visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(&mut self, _offset: usize) -> Self::Output { Operator::I32x4RelaxedTruncSatF64x2SZero }
    fn visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(&mut self, _offset: usize) -> Self::Output { Operator::I32x4RelaxedTruncSatF64x2UZero }
    fn visit_v128_bitselect(&mut self, _offset: usize) -> Self::Output { Operator::V128Bitselect }
    fn visit_f32x4_fma(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Fma }
    fn visit_f32x4_fms(&mut self, _offset: usize) -> Self::Output { Operator::F32x4Fms }
    fn visit_f64x2_fma(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Fma }
    fn visit_f64x2_fms(&mut self, _offset: usize) -> Self::Output { Operator::F64x2Fms }
    fn visit_i8x16_lane_select(&mut self, _offset: usize) -> Self::Output { Operator::I8x16LaneSelect }
    fn visit_i16x8_lane_select(&mut self, _offset: usize) -> Self::Output { Operator::I16x8LaneSelect }
    fn visit_i32x4_lane_select(&mut self, _offset: usize) -> Self::Output { Operator::I32x4LaneSelect }
    fn visit_i64x2_lane_select(&mut self, _offset: usize) -> Self::Output { Operator::I64x2LaneSelect }
    fn visit_v128_any_true(&mut self, _offset: usize) -> Self::Output { Operator::V128AnyTrue }
    fn visit_i8x16_all_true(&mut self, _offset: usize) -> Self::Output { Operator::I8x16AllTrue }
    fn visit_i8x16_bitmask(&mut self, _offset: usize) -> Self::Output { Operator::I8x16Bitmask }
    fn visit_i16x8_all_true(&mut self, _offset: usize) -> Self::Output { Operator::I16x8AllTrue }
    fn visit_i16x8_bitmask(&mut self, _offset: usize) -> Self::Output { Operator::I16x8Bitmask }
    fn visit_i32x4_all_true(&mut self, _offset: usize) -> Self::Output { Operator::I32x4AllTrue }
    fn visit_i32x4_bitmask(&mut self, _offset: usize) -> Self::Output { Operator::I32x4Bitmask }
    fn visit_i64x2_all_true(&mut self, _offset: usize) -> Self::Output { Operator::I64x2AllTrue }
    fn visit_i64x2_bitmask(&mut self, _offset: usize) -> Self::Output { Operator::I64x2Bitmask }
    fn visit_i8x16_shl(&mut self, _offset: usize) -> Self::Output { Operator::I8x16Shl }
    fn visit_i8x16_shr_s(&mut self, _offset: usize) -> Self::Output { Operator::I8x16ShrS }
    fn visit_i8x16_shr_u(&mut self, _offset: usize) -> Self::Output { Operator::I8x16ShrU }
    fn visit_i16x8_shl(&mut self, _offset: usize) -> Self::Output { Operator::I16x8Shl }
    fn visit_i16x8_shr_s(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ShrS }
    fn visit_i16x8_shr_u(&mut self, _offset: usize) -> Self::Output { Operator::I16x8ShrU }
    fn visit_i32x4_shl(&mut self, _offset: usize) -> Self::Output { Operator::I32x4Shl }
    fn visit_i32x4_shr_s(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ShrS }
    fn visit_i32x4_shr_u(&mut self, _offset: usize) -> Self::Output { Operator::I32x4ShrU }
    fn visit_i64x2_shl(&mut self, _offset: usize) -> Self::Output { Operator::I64x2Shl }
    fn visit_i64x2_shr_s(&mut self, _offset: usize) -> Self::Output { Operator::I64x2ShrS }
    fn visit_i64x2_shr_u(&mut self, _offset: usize) -> Self::Output { Operator::I64x2ShrU }
    fn visit_i8x16_swizzle(&mut self, _offset: usize) -> Self::Output { Operator::I8x16Swizzle }
    fn visit_i8x16_relaxed_swizzle(&mut self, _offset: usize) -> Self::Output { Operator::I8x16RelaxedSwizzle }
    fn visit_i8x16_shuffle(&mut self, _offset: usize, lanes: [SIMDLaneIndex; 16]) -> Self::Output { Operator::I8x16Shuffle { lanes } }
    fn visit_v128_load8_splat(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load8Splat { memarg } }
    fn visit_v128_load16_splat(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load16Splat { memarg } }
    fn visit_v128_load32_splat(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load32Splat { memarg } }
    fn visit_v128_load32_zero(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load32Zero { memarg } }
    fn visit_v128_load64_splat(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load64Splat { memarg } }
    fn visit_v128_load64_zero(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load64Zero { memarg } }
    fn visit_v128_load8x8_s(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load8x8S { memarg } }
    fn visit_v128_load8x8_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load8x8U { memarg } }
    fn visit_v128_load16x4_s(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load16x4S { memarg } }
    fn visit_v128_load16x4_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load16x4U { memarg } }
    fn visit_v128_load32x2_s(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load32x2S { memarg } }
    fn visit_v128_load32x2_u(&mut self, _offset: usize, memarg: MemoryImmediate) -> Self::Output { Operator::V128Load32x2U { memarg } }
    fn visit_v128_load8_lane(&mut self, _offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { Operator::V128Load8Lane { memarg, lane } }
    fn visit_v128_load16_lane(&mut self, _offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { Operator::V128Load16Lane { memarg, lane } }
    fn visit_v128_load32_lane(&mut self, _offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { Operator::V128Load32Lane { memarg, lane } }
    fn visit_v128_load64_lane(&mut self, _offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { Operator::V128Load64Lane { memarg, lane } }
    fn visit_v128_store8_lane(&mut self, _offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { Operator::V128Store8Lane { memarg, lane } }
    fn visit_v128_store16_lane(&mut self, _offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { Operator::V128Store16Lane { memarg, lane } }
    fn visit_v128_store32_lane(&mut self, _offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { Operator::V128Store32Lane { memarg, lane } }
    fn visit_v128_store64_lane(&mut self, _offset: usize, memarg: MemoryImmediate, lane: SIMDLaneIndex) -> Self::Output { Operator::V128Store64Lane { memarg, lane } }
    fn visit_memory_init(&mut self, _offset: usize, segment: u32, mem: u32) -> Self::Output { Operator::MemoryInit { mem, segment } }
    fn visit_data_drop(&mut self, _offset: usize, segment: u32) -> Self::Output { Operator::DataDrop { segment } }
    fn visit_memory_copy(&mut self, _offset: usize, dst: u32, src: u32) -> Self::Output { Operator::MemoryCopy { src, dst } }
    fn visit_memory_fill(&mut self, _offset: usize, mem: u32) -> Self::Output { Operator::MemoryFill { mem } }
    fn visit_table_init(&mut self, _offset: usize, segment: u32, table: u32) -> Self::Output { Operator::TableInit { segment, table } }
    fn visit_elem_drop(&mut self, _offset: usize, segment: u32) -> Self::Output { Operator::ElemDrop { segment } }
    fn visit_table_copy(&mut self, _offset: usize, dst_table: u32, src_table: u32) -> Self::Output { Operator::TableCopy { dst_table, src_table } }
    fn visit_table_get(&mut self, _offset: usize, table: u32) -> Self::Output { Operator::TableGet { table } }
    fn visit_table_set(&mut self, _offset: usize, table: u32) -> Self::Output { Operator::TableSet { table } }
    fn visit_table_grow(&mut self, _offset: usize, table: u32) -> Self::Output { Operator::TableGrow { table } }
    fn visit_table_size(&mut self, _offset: usize, table: u32) -> Self::Output { Operator::TableSize { table } }
    fn visit_table_fill(&mut self, _offset: usize, table: u32) -> Self::Output { Operator::TableFill { table } }
}
