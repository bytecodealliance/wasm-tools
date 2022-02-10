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

use crate::{
    limits::*, operators_validator::OperatorValidator, BinaryReaderError, CanonicalOption,
    DataKind, ElementItem, ElementKind, Encoding, ExternalKind, FuncType, FunctionBody, GlobalType,
    InitExpr, MemoryType, Operator, Parser, Payload, Range, Result, SectionReader,
    SectionWithLimitedItems, TableType, TagType, Type, TypeRef, WasmModuleResources,
    WASM_COMPONENT_VERSION, WASM_MODULE_VERSION,
};
use std::collections::{HashMap, HashSet};
use std::mem;
use std::rc::Rc;
use std::sync::Arc;

/// Test whether the given buffer contains a valid WebAssembly module or component,
/// analogous to [`WebAssembly.validate`][js] in the JS API.
///
/// This functions requires the bytes to validate are entirely resident in memory.
/// Additionally this validates the given bytes with the default set of WebAssembly
/// features implemented by `wasmparser`.
///
/// For more fine-tuned control over validation it's recommended to review the
/// documentation of [`Validator`].
///
/// [js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/validate
pub fn validate(bytes: &[u8]) -> Result<()> {
    Validator::new().validate_all(bytes)
}

#[test]
fn test_validate() -> Result<()> {
    validate(&[0x0, 0x61, 0x73, 0x6d, 0x1, 0x0, 0x0, 0x0])?;
    assert!(validate(&[0x0, 0x61, 0x73, 0x6d, 0x2, 0x0, 0x0, 0x0]).is_err());
    Ok(())
}

mod func;
pub use func::FuncValidator;

fn check_max(cur_len: usize, amt_added: u32, max: usize, desc: &str, offset: usize) -> Result<()> {
    if max
        .checked_sub(cur_len)
        .and_then(|amt| amt.checked_sub(amt_added as usize))
        .is_none()
    {
        if max == 1 {
            return Err(BinaryReaderError::new(format!("multiple {}", desc), offset));
        }

        return Err(BinaryReaderError::new(
            format!("{} count exceeds limit of {}", desc, max),
            offset,
        ));
    }

    Ok(())
}

fn check_value_type(ty: Type, features: &WasmFeatures, offset: usize) -> Result<()> {
    match features.check_value_type(ty) {
        Ok(()) => Ok(()),
        Err(e) => Err(BinaryReaderError::new(e, offset)),
    }
}

fn check_options(options: &[CanonicalOption], offset: usize) -> Result<()> {
    fn display(option: CanonicalOption) -> &'static str {
        match option {
            CanonicalOption::UTF8 => "utf8",
            CanonicalOption::UTF16 => "utf16",
            CanonicalOption::CompactUTF16 => "compact-utf16",
            CanonicalOption::Into(_) => "into",
        }
    }

    let mut encoding = None;
    let mut into = None;

    for option in options {
        match option {
            CanonicalOption::UTF8 | CanonicalOption::UTF16 | CanonicalOption::CompactUTF16 => {
                match encoding {
                    Some(existing) => {
                        return Err(BinaryReaderError::new(
                            format!(
                                "canonical option `{}` conflicts with option `{}`",
                                display(existing),
                                display(*option)
                            ),
                            offset,
                        ))
                    }
                    None => encoding = Some(*option),
                }
            }
            CanonicalOption::Into(i) => {
                into = match into {
                    None => Some(*i),
                    Some(_) => {
                        return Err(BinaryReaderError::new(
                            "canonical option `into` is specified more than once",
                            offset,
                        ))
                    }
                }
            }
        }
    }

    Ok(())
}

/// Validator for a WebAssembly binary module or component.
///
/// This structure encapsulates state necessary to validate a WebAssembly
/// binary. This implements validation as defined by the [core
/// specification][core]. A `Validator` is designed, like
/// [`Parser`], to accept incremental input over time.
/// Additionally a `Validator` is also designed for parallel validation of
/// functions as they are received.
///
/// It's expected that you'll be using a [`Parser`] in tandem with a
/// `Validator`. As each [`Payload`](crate::Payload) is received from a
/// [`Parser`] you'll pass it into a `Validator` to test the validity of the
/// payload. Note that all payloads received from a [`Parser`] are expected to
/// be passed to a [`Validator`]. For example if you receive
/// [`Payload::TypeSection`](crate::Payload) you'll call
/// [`Validator::type_section`] to validate this.
///
/// The design of [`Validator`] is intended that you'll interleave, in your own
/// application's processing, calls to validation. Each variant, after it's
/// received, will be validated and then your application would proceed as
/// usual. At all times, however, you'll have access to the [`Validator`] and
/// the validation context up to that point. This enables applications to check
/// the types of functions and learn how many globals there are, for example.
///
/// [core]: https://webassembly.github.io/spec/core/valid/index.html
#[derive(Default)]
pub struct Validator {
    /// The current state of the validator.
    state: State,

    /// With the component model enabled, this stores the pushed states
    /// of parent components.
    parents: Vec<ComponentState>,

    /// Enabled WebAssembly feature flags, dictating what's valid and what
    /// isn't.
    features: WasmFeatures,
}

enum State {
    /// A version has not yet been parsed.
    ///
    /// The value is the expected encoding for the header.
    Unparsed(Option<Encoding>),
    /// A module header has been parsed.
    Module(ModuleState),
    /// A component header has been parsed.
    Component(Box<ComponentState>),
}

impl Default for State {
    fn default() -> Self {
        Self::Unparsed(None)
    }
}

#[derive(Default)]
struct ModuleState {
    /// Internal state that is incrementally built-up for the module being
    /// validated. This houses type information for all wasm items, like
    /// functions. Note that this starts out as a solely owned `Arc<T>` so we can
    /// get mutable access, but after we get to the code section this is never
    /// mutated to we can clone it cheaply and hand it to sub-validators.
    module: arc::MaybeOwned<Module>,

    /// Where we are, order-wise, in the wasm binary.
    order: Order,

    /// The number of data segments we ended up finding in this module, or 0 if
    /// they either weren't present or none were found.
    data_found: u32,

    /// The number of functions we expect to be defined in the code section, or
    /// basically the length of the function section if it was found. The next
    /// index is where we are, in the code section index space, for the next
    /// entry in the code section (used to figure out what type is next for the
    /// function being validated).
    expected_code_bodies: Option<u32>,

    /// When parsing the code section, represents the current index in the section.
    code_section_index: Option<usize>,
}

impl ModuleState {
    fn update_order(&mut self, order: Order, offset: usize) -> Result<()> {
        if self.order >= order {
            return Err(BinaryReaderError::new("section out of order", offset));
        }

        self.order = order;

        Ok(())
    }

    fn init_expr(
        &mut self,
        expr: &InitExpr<'_>,
        expected_ty: Type,
        is_data_section: bool,
        features: &WasmFeatures,
        offset: usize,
    ) -> Result<()> {
        let mut ops = expr.get_operators_reader();
        let mut validator = OperatorValidator::new_init_expr(features, expected_ty);
        let mut uninserted_function_references = false;

        while !ops.eof() {
            let offset = ops.original_position();
            let op = ops.read()?;
            match &op {
                // These are always valid in const expressions.
                Operator::I32Const { .. }
                | Operator::I64Const { .. }
                | Operator::F32Const { .. }
                | Operator::F64Const { .. }
                | Operator::RefNull { .. }
                | Operator::V128Const { .. }
                | Operator::End => {}

                // These are valid const expressions when the extended-const proposal is enabled.
                Operator::I32Add
                | Operator::I32Sub
                | Operator::I32Mul
                | Operator::I64Add
                | Operator::I64Sub
                | Operator::I64Mul
                    if features.extended_const => {}

                // `global.get` is a valid const expression for imported, immutable globals.
                Operator::GlobalGet { global_index } => {
                    let global = self.module.get_global(*global_index, offset)?;
                    if *global_index >= self.module.num_imported_globals {
                        return Err(BinaryReaderError::new(
                            "constant expression required: global.get of locally defined global",
                            offset,
                        ));
                    }
                    if global.mutable {
                        return Err(BinaryReaderError::new(
                            "constant expression required: global.get of mutable global",
                            offset,
                        ));
                    }
                }

                // Functions in initialization expressions are only valid in
                // element segment initialization expressions and globals. In
                // these contexts we want to record all function references.
                //
                // Initialization expressions can also be found in the data
                // section, however. A `RefFunc` instruction in those situations
                // is always invalid and needs to produce a validation error. In
                // this situation, though, we can no longer modify
                // `self.cur.state` since it's been "snapshot" already for
                // parallel validation of functions.
                //
                // Consequently we test where we are in the module to determine
                // whether we can modify the set of function references. If we
                // cannot modify the function references then this function
                // *should* result in a validation error, but we defer that
                // validation error to happen later. The
                // `uninserted_function_references` boolean here is used to
                // track this and will cause a panic (aka a fuzz bug) if we
                // somehow forget to emit an error somewhere else.
                Operator::RefFunc { function_index } => {
                    if is_data_section {
                        uninserted_function_references = true;
                    } else {
                        self.module
                            .assert_mut()
                            .function_references
                            .insert(*function_index);
                    }
                }
                _ => {
                    return Err(BinaryReaderError::new(
                        "constant expression required: invalid init_expr operator",
                        offset,
                    ));
                }
            }

            validator
                .process_operator(&op, &*self.module)
                .map_err(|e| e.set_offset(offset))?;
        }

        validator.finish().map_err(|e| e.set_offset(offset))?;

        // See comment in `RefFunc` above for why this is an assert.
        assert!(!uninserted_function_references);

        Ok(())
    }
}

/// Represents the type space of a module.
#[derive(Default)]
struct ModuleTypeSpace(Vec<TypeDef>);

impl ModuleTypeSpace {
    fn type_def(
        &mut self,
        def: crate::TypeDef,
        features: &WasmFeatures,
        offset: usize,
    ) -> Result<()> {
        let def = match def {
            crate::TypeDef::Func(t) => {
                for ty in t.params.iter().chain(t.returns.iter()) {
                    check_value_type(*ty, features, offset)?;
                }
                if t.returns.len() > 1 && !features.multi_value {
                    return Err(BinaryReaderError::new(
                        "invalid result arity: func type returns multiple values",
                        offset,
                    ));
                }
                TypeDef::Func(Arc::new(t))
            }
        };

        self.0.push(def);
        Ok(())
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn get(&self, idx: u32, offset: usize) -> Result<&TypeDef> {
        match self.0.get(idx as usize) {
            Some(t) => Ok(t),
            None => Err(BinaryReaderError::new(
                format!("unknown type {}: type index out of bounds", idx),
                offset,
            )),
        }
    }

    fn func_type_at(&self, type_index: u32, offset: usize) -> Result<&Arc<FuncType>> {
        match self.get(type_index, offset)? {
            TypeDef::Func(item) => Ok(item),
        }
    }

    fn check_type_ref(
        &self,
        type_ref: &TypeRef,
        features: &WasmFeatures,
        offset: usize,
    ) -> Result<()> {
        match type_ref {
            TypeRef::Func(type_index) => {
                self.func_type_at(*type_index, offset)?;
            }
            TypeRef::Table(t) => {
                self.check_table_type(t, features, offset)?;
            }
            TypeRef::Memory(t) => {
                self.check_memory_type(t, features, offset)?;
            }
            TypeRef::Tag(t) => {
                self.check_tag_type(t, features, offset)?;
            }
            TypeRef::Global(t) => {
                self.check_global_type(t, features, offset)?;
            }
        }

        Ok(())
    }

    fn check_table_type(
        &self,
        ty: &TableType,
        features: &WasmFeatures,
        offset: usize,
    ) -> Result<()> {
        match ty.element_type {
            Type::FuncRef => {}
            Type::ExternRef => {
                if !features.reference_types {
                    return Err(BinaryReaderError::new("element is not anyfunc", offset));
                }
            }
            _ => {
                return Err(BinaryReaderError::new(
                    "element is not reference type",
                    offset,
                ))
            }
        }
        self.check_limits(ty.initial, ty.maximum, offset)?;
        if ty.initial > MAX_WASM_TABLE_ENTRIES as u32 {
            return Err(BinaryReaderError::new(
                "minimum table size is out of bounds",
                offset,
            ));
        }
        Ok(())
    }

    fn check_memory_type(
        &self,
        ty: &MemoryType,
        features: &WasmFeatures,
        offset: usize,
    ) -> Result<()> {
        self.check_limits(ty.initial, ty.maximum, offset)?;
        let (true_maximum, err) = if ty.memory64 {
            if !features.memory64 {
                return Err(BinaryReaderError::new(
                    "memory64 must be enabled for 64-bit memories",
                    offset,
                ));
            }
            (
                MAX_WASM_MEMORY64_PAGES,
                "memory size must be at most 2**48 pages",
            )
        } else {
            (
                MAX_WASM_MEMORY32_PAGES,
                "memory size must be at most 65536 pages (4GiB)",
            )
        };
        if ty.initial > true_maximum {
            return Err(BinaryReaderError::new(err, offset));
        }
        if let Some(maximum) = ty.maximum {
            if maximum > true_maximum {
                return Err(BinaryReaderError::new(err, offset));
            }
        }
        if ty.shared {
            if !features.threads {
                return Err(BinaryReaderError::new(
                    "threads must be enabled for shared memories",
                    offset,
                ));
            }
            if ty.maximum.is_none() {
                return Err(BinaryReaderError::new(
                    "shared memory must have maximum size",
                    offset,
                ));
            }
        }
        Ok(())
    }

    fn check_tag_type(&self, ty: &TagType, features: &WasmFeatures, offset: usize) -> Result<()> {
        if !features.exceptions {
            return Err(BinaryReaderError::new(
                "exceptions proposal not enabled",
                offset,
            ));
        }
        let ty = self.func_type_at(ty.func_type_idx, offset)?;
        if ty.returns.len() > 0 {
            return Err(BinaryReaderError::new(
                "invalid exception type: non-empty tag result type",
                offset,
            ));
        }
        Ok(())
    }

    fn check_global_type(
        &self,
        ty: &GlobalType,
        features: &WasmFeatures,
        offset: usize,
    ) -> Result<()> {
        check_value_type(ty.content_type, features, offset)
    }

    fn check_limits<T>(&self, initial: T, maximum: Option<T>, offset: usize) -> Result<()>
    where
        T: Into<u64>,
    {
        if let Some(max) = maximum {
            if initial.into() > max.into() {
                return Err(BinaryReaderError::new(
                    "size minimum must not be greater than maximum",
                    offset,
                ));
            }
        }
        Ok(())
    }
}

#[derive(Default)]
struct Module {
    types: ModuleTypeSpace,
    tables: Vec<TableType>,
    memories: Vec<MemoryType>,
    globals: Vec<GlobalType>,
    element_types: Vec<Type>,
    data_count: Option<u32>,
    functions: Vec<u32>, // has indexes into the module type space
    tags: Vec<TagType>,
    function_references: HashSet<u32>,
    imports: HashMap<(String, String), Vec<TypeRef>>,
    exports: HashMap<String, TypeRef>,
    num_imported_globals: u32,
    num_imported_functions: u32,
}

impl Module {
    fn import(
        &mut self,
        import: crate::Import,
        features: &WasmFeatures,
        has_parent: bool,
        offset: usize,
    ) -> Result<()> {
        self.types.check_type_ref(&import.ty, features, offset)?;

        let (len, max, desc) = match import.ty {
            TypeRef::Func(type_index) => {
                self.functions.push(type_index);
                self.num_imported_functions += 1;
                (self.functions.len(), MAX_WASM_FUNCTIONS, "functions")
            }
            TypeRef::Table(ty) => {
                self.tables.push(ty);
                (self.tables.len(), self.max_tables(features), "tables")
            }
            TypeRef::Memory(ty) => {
                self.memories.push(ty);
                (self.memories.len(), self.max_memories(features), "memories")
            }
            TypeRef::Tag(ty) => {
                self.tags.push(ty);
                (self.tags.len(), MAX_WASM_TAGS, "tags")
            }
            TypeRef::Global(ty) => {
                self.globals.push(ty);
                self.num_imported_globals += 1;
                (self.globals.len(), MAX_WASM_GLOBALS, "globals")
            }
        };

        check_max(len, 0, max, desc, offset)?;

        // If the module is defined as part of a component, imports must be unique
        if has_parent {
            if self
                .imports
                .insert(
                    (import.module.to_string(), import.name.to_string()),
                    vec![import.ty],
                )
                .is_some()
            {
                return Err(BinaryReaderError::new(
                    format!(
                        "duplicate import name `{}:{}` already defined",
                        import.module, import.name
                    ),
                    offset,
                ));
            }
        } else {
            self.imports
                .entry((import.module.to_string(), import.name.to_string()))
                .or_default()
                .push(import.ty);
        }

        Ok(())
    }

    fn get_type_ref(&mut self, kind: ExternalKind, index: u32, offset: usize) -> Result<TypeRef> {
        let check = |ty: &str, index: u32, total: usize| {
            if index as usize >= total {
                Err(BinaryReaderError::new(
                    format!(
                        "unknown {ty} {index}: exported {ty} index out of bounds",
                        index = index,
                        ty = ty,
                    ),
                    offset,
                ))
            } else {
                Ok(())
            }
        };

        Ok(match kind {
            ExternalKind::Func => {
                check("function", index, self.functions.len())?;
                self.function_references.insert(index);
                TypeRef::Func(index)
            }
            ExternalKind::Table => {
                check("table", index, self.tables.len())?;
                TypeRef::Table(self.tables[index as usize])
            }
            ExternalKind::Memory => {
                check("memory", index, self.memories.len())?;
                TypeRef::Memory(self.memories[index as usize])
            }
            ExternalKind::Global => {
                check("global", index, self.globals.len())?;
                TypeRef::Global(self.globals[index as usize])
            }
            ExternalKind::Tag => {
                check("tag", index, self.tags.len())?;
                TypeRef::Tag(self.tags[index as usize])
            }
        })
    }

    fn get_func_type(&self, func_idx: u32, offset: usize) -> Result<&Arc<FuncType>> {
        match self.functions.get(func_idx as usize) {
            Some(t) => self.types.func_type_at(*t, offset),
            None => Err(BinaryReaderError::new(
                format!("unknown function {}: func index out of bounds", func_idx),
                offset,
            )),
        }
    }

    fn get_global(&self, idx: u32, offset: usize) -> Result<&GlobalType> {
        match self.globals.get(idx as usize) {
            Some(t) => Ok(t),
            None => Err(BinaryReaderError::new(
                format!("unknown global {}: global index out of bounds", idx,),
                offset,
            )),
        }
    }

    fn get_table(&self, idx: u32, offset: usize) -> Result<&TableType> {
        match self.tables.get(idx as usize) {
            Some(t) => Ok(t),
            None => Err(BinaryReaderError::new(
                format!("unknown table {}: table index out of bounds", idx),
                offset,
            )),
        }
    }

    fn get_memory(&self, idx: u32, offset: usize) -> Result<&MemoryType> {
        match self.memories.get(idx as usize) {
            Some(t) => Ok(t),
            None => Err(BinaryReaderError::new(
                format!("unknown memory {}: memory index out of bounds", idx,),
                offset,
            )),
        }
    }

    fn max_tables(&self, features: &WasmFeatures) -> usize {
        if features.reference_types {
            MAX_WASM_TABLES
        } else {
            1
        }
    }

    fn max_memories(&self, features: &WasmFeatures) -> usize {
        if features.multi_memory {
            MAX_WASM_MEMORIES
        } else {
            1
        }
    }
}

impl WasmModuleResources for Module {
    type FuncType = crate::FuncType;

    fn table_at(&self, at: u32) -> Option<TableType> {
        self.tables.get(at as usize).cloned()
    }

    fn memory_at(&self, at: u32) -> Option<MemoryType> {
        self.memories.get(at as usize).cloned()
    }

    fn tag_at(&self, at: u32) -> Option<&Self::FuncType> {
        let ty = self.tags.get(at as usize)?;
        self.func_type_at(ty.func_type_idx)
    }

    fn global_at(&self, at: u32) -> Option<GlobalType> {
        self.globals.get(at as usize).cloned()
    }

    fn func_type_at(&self, at: u32) -> Option<&Self::FuncType> {
        match self.types.0.get(at as usize)? {
            TypeDef::Func(f) => Some(f),
        }
    }

    fn type_of_function(&self, at: u32) -> Option<&Self::FuncType> {
        self.func_type_at(*self.functions.get(at as usize)?)
    }

    fn element_type_at(&self, at: u32) -> Option<Type> {
        self.element_types.get(at as usize).cloned()
    }

    fn element_count(&self) -> u32 {
        self.element_types.len() as u32
    }

    fn data_count(&self) -> u32 {
        self.data_count.unwrap_or(0)
    }

    fn is_function_referenced(&self, idx: u32) -> bool {
        self.function_references.contains(&idx)
    }
}

// The function index space may have both core and component functions
// This is used to distinguish between the possible types of functions.
#[derive(Clone)]
enum FuncIndexType {
    Component(ComponentFuncType),
    Core(Arc<FuncType>),
}

// The instance index space may have both module, component, and "export" instances.
// This is used to distinguish between the possible types of instance.
#[derive(Clone)]
enum InstanceIndexType {
    Component(InstanceType),
    Module(ModuleInstanceType),
}

#[derive(Clone, PartialEq, Eq)]
enum InterfaceType {
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
    F32,
    F64,
    Char,
    String,
    Compound(Rc<CompoundType>),
}

impl InterfaceType {
    fn new(ty: crate::InterfaceType, types: &ComponentTypeSpace, offset: usize) -> Result<Self> {
        Ok(match ty {
            crate::InterfaceType::Unit => Self::Unit,
            crate::InterfaceType::Bool => Self::Bool,
            crate::InterfaceType::S8 => Self::S8,
            crate::InterfaceType::U8 => Self::U8,
            crate::InterfaceType::S16 => Self::S16,
            crate::InterfaceType::U16 => Self::U16,
            crate::InterfaceType::S32 => Self::S32,
            crate::InterfaceType::U32 => Self::U32,
            crate::InterfaceType::S64 => Self::S64,
            crate::InterfaceType::U64 => Self::U64,
            crate::InterfaceType::F32 => Self::F32,
            crate::InterfaceType::F64 => Self::F64,
            crate::InterfaceType::Char => Self::Char,
            crate::InterfaceType::String => Self::String,
            crate::InterfaceType::Compound(idx) => {
                Self::Compound(types.compound_type_at(idx, offset)?.clone())
            }
        })
    }

    fn push_wasm_types(&self, types: &mut Vec<Type>) -> Result<()> {
        match self {
            Self::Unit => {}
            Self::Bool
            | Self::S8
            | Self::U8
            | Self::S16
            | Self::U16
            | Self::S32
            | Self::U32
            | Self::Char => {
                types.push(Type::I32);
            }
            Self::S64 | Self::U64 => {
                types.push(Type::I64);
            }
            Self::F32 => types.push(Type::F32),
            Self::F64 => types.push(Type::F64),
            Self::String => types.extend([Type::I32, Type::I32]),
            Self::Compound(ct) => match ct.as_ref() {
                CompoundType::Record(fields) => {
                    for ty in fields.iter() {
                        ty.push_wasm_types(types)?;
                    }
                }
                CompoundType::Variant(cases) => {
                    Self::push_variant_types(cases.iter(), types)?;
                }
                CompoundType::List(_) => {
                    types.extend([Type::I32, Type::I32]);
                }
                CompoundType::Tuple(tys) => {
                    for ty in tys.iter() {
                        ty.push_wasm_types(types)?;
                    }
                }
                CompoundType::Flags(size) => {
                    if *size <= 32 {
                        types.push(Type::I32);
                    } else if *size <= 64 {
                        types.push(Type::I64);
                    } else {
                        for _ in 0..(*size + 31) / 32 {
                            types.push(Type::I32);
                        }
                    }
                }
                CompoundType::Enum(size) => {
                    if *size < u32::max_value() as usize {
                        types.push(Type::I32);
                    } else {
                        types.push(Type::I64);
                    }
                }
                CompoundType::Union(tys) => {
                    Self::push_variant_types(tys.iter(), types)?;
                }
                CompoundType::Optional(ty) => {
                    Self::push_variant_types([ty].into_iter(), types)?;
                }
                CompoundType::Expected(ok, error) => {
                    Self::push_variant_types([ok, error].into_iter(), types)?;
                }
            },
        }

        Ok(())
    }

    fn push_variant_types<'a>(
        cases: impl ExactSizeIterator<Item = &'a InterfaceType>,
        types: &mut Vec<Type>,
    ) -> Result<()> {
        if cases.len() < u32::max_value() as usize {
            types.push(Type::I32);
        } else {
            types.push(Type::I64);
        }

        let start = types.len();
        let mut temp = Vec::new();

        for ty in cases {
            ty.push_wasm_types(&mut temp)?;

            for (i, ty) in temp.drain(..).enumerate() {
                match types.get_mut(start + i) {
                    Some(prev) => *prev = Self::unify_wasm_type(*prev, ty),
                    None => types.push(ty),
                }
            }
        }

        Ok(())
    }

    fn unify_wasm_type(a: Type, b: Type) -> Type {
        use Type::*;

        match (a, b) {
            (I64, _) | (_, I64) | (I32, F64) | (F64, I32) => I64,
            (I32, I32) | (I32, F32) | (F32, I32) => I32,
            (F32, F32) => F32,
            (F64, F64) | (F32, F64) | (F64, F32) => F64,
            _ => panic!("unexpected wasm type for canonical ABI"),
        }
    }
}

#[derive(Clone)]
enum ComponentEntityType {
    Module(ModuleType),
    Component(ComponentType),
    Instance(InstanceType),
    Func(ComponentFuncType),
    Value(InterfaceType),
}

impl ComponentEntityType {
    fn from_type_def(def: ComponentTypeDef, desc: &str, offset: usize) -> Result<Self> {
        Ok(match def {
            ComponentTypeDef::Module(ty) => Self::Module(ty),
            ComponentTypeDef::Component(ty) => Self::Component(ty),
            ComponentTypeDef::Instance(ty) => Self::Instance(ty),
            ComponentTypeDef::Func(ty) => Self::Func(ty),
            ComponentTypeDef::Value(ty) => Self::Value(ty),
            ComponentTypeDef::Compound(_) => {
                return Err(BinaryReaderError::new(
                    format!("{} of compound types are not supported", desc),
                    offset,
                ))
            }
        })
    }

    fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Module(ty), Self::Module(other_ty)) => ty.is_subtype_of(other_ty),
            (Self::Component(ty), Self::Component(other_ty)) => ty.is_subtype_of(other_ty),
            (Self::Instance(ty), Self::Instance(other_ty)) => ty.is_subtype_of(other_ty),
            (Self::Func(ty), Self::Func(other_ty)) => ty == other_ty,
            (Self::Value(ty), Self::Value(other_ty)) => ty == other_ty,
            _ => false,
        }
    }

    fn desc(&self) -> &'static str {
        match self {
            Self::Module(_) => "module",
            Self::Component(_) => "component",
            Self::Instance(_) => "instance",
            Self::Func(_) => "function",
            Self::Value(_) => "value",
        }
    }
}

#[derive(Default)]
struct ComponentState {
    types: ComponentTypeSpace,
    modules: Vec<ModuleType>,
    components: Vec<ComponentType>,
    instances: Vec<InstanceIndexType>,
    functions: Vec<FuncIndexType>,
    values: Vec<(InterfaceType, bool)>,
    memories: Vec<MemoryType>,
    tables: Vec<TableType>,
    globals: Vec<GlobalType>,
    tags: Vec<Arc<FuncType>>,
    has_start: bool,
    imports: HashMap<String, ComponentEntityType>,
    exports: HashMap<String, ComponentEntityType>,
}

impl ComponentState {
    fn import(&mut self, import: crate::ComponentImport, offset: usize) -> Result<()> {
        let ty = self.types.get(import.ty, offset)?;
        let (len, max, desc) = match ty {
            ComponentTypeDef::Module(mt) => {
                self.modules.push(mt.clone());
                (self.modules.len(), MAX_WASM_MODULES, "modules")
            }
            ComponentTypeDef::Component(ct) => {
                self.components.push(ct.clone());
                (self.components.len(), MAX_WASM_COMPONENTS, "components")
            }
            ComponentTypeDef::Instance(it) => {
                self.instances
                    .push(InstanceIndexType::Component(it.clone()));
                (self.instances.len(), MAX_WASM_INSTANCES, "instances")
            }
            ComponentTypeDef::Func(ft) => {
                self.functions.push(FuncIndexType::Component(ft.clone()));
                (self.functions.len(), MAX_WASM_FUNCTIONS, "functions")
            }
            ComponentTypeDef::Value(it) => {
                self.values.push((it.clone(), false));
                (self.values.len(), MAX_WASM_VALUES, "values")
            }
            ComponentTypeDef::Compound(_) => {
                // TODO: check with spec authors to see if this behavior is correct
                return Err(BinaryReaderError::new(
                    "imports of compound types are not supported",
                    offset,
                ));
            }
        };

        check_max(len, 0, max, desc, offset)?;

        if self
            .imports
            .insert(
                import.name.to_string(),
                ComponentEntityType::from_type_def(ty.clone(), "import", offset)?,
            )
            .is_some()
        {
            return Err(BinaryReaderError::new(
                format!("duplicate import name `{}` already defined", import.name),
                offset,
            ));
        }

        Ok(())
    }

    fn instantiate_module(
        &self,
        module_index: u32,
        module_args: &[crate::ModuleArg],
        offset: usize,
    ) -> Result<ModuleInstanceType> {
        fn insert_arg<'a>(
            module: &'a str,
            name: &'a str,
            arg: EntityType,
            args: &mut HashMap<(&'a str, &'a str), EntityType>,
            offset: usize,
        ) -> Result<()> {
            if args.insert((module, name), arg).is_some() {
                return Err(BinaryReaderError::new(
                    format!(
                        "duplicate instantiation argument name `{}::{}` already defined",
                        module, name
                    ),
                    offset,
                ));
            }

            Ok(())
        }

        let module_type = self.module_at(module_index, offset)?;
        let mut args = HashMap::new();

        // Populate the arguments
        for module_arg in module_args {
            match &module_arg.kind {
                crate::ModuleArgKind::Instance(idx) => {
                    let instance_type = self.module_instance_at(*idx, offset)?;
                    for (name, ty) in instance_type.exports.iter() {
                        insert_arg(module_arg.name, name, ty.clone(), &mut args, offset)?;
                    }
                }
                crate::ModuleArgKind::Exports(exports) => {
                    for export in exports.iter() {
                        insert_arg(
                            module_arg.name,
                            export.name,
                            EntityType::from_export(self, export, offset)?,
                            &mut args,
                            offset,
                        )?;
                    }
                }
            }
        }

        // Validate the arguments
        for ((module, name), b) in module_type.imports.iter() {
            match args.get(&(module.as_str(), name.as_str())) {
                Some(a) => {
                    let desc = match (a, b) {
                        (EntityType::Func(_), EntityType::Func(_)) => "function",
                        (EntityType::Table(_), EntityType::Table(_)) => "table",
                        (EntityType::Memory(_), EntityType::Memory(_)) => "memory",
                        (EntityType::Global(_), EntityType::Global(_)) => "global",
                        (EntityType::Tag(_), EntityType::Tag(_)) => "tag",
                        _ => {
                            return Err(BinaryReaderError::new(
                                format!(
                                "expected module instantiation argument `{}::{}` to be of type `{}`",
                                module,
                                name,
                                b.desc()
                            ),
                                offset,
                            ))
                        }
                    };

                    if !a.is_subtype_of(b) {
                        return Err(BinaryReaderError::new(
                            format!(
                                "{} type mismatch for module instantiation argument `{}::{}`",
                                desc, module, name
                            ),
                            offset,
                        ));
                    }
                }
                None => {
                    return Err(BinaryReaderError::new(
                        format!(
                            "missing module instantiation argument named `{}::{}`",
                            module, name
                        ),
                        offset,
                    ));
                }
            }
        }

        Ok(ModuleInstanceType {
            exports: module_type.exports.clone(),
        })
    }

    fn instantiate_component(
        &mut self,
        component_index: u32,
        component_args: &[crate::ComponentArg],
        offset: usize,
    ) -> Result<InstanceType> {
        fn insert_arg<'a>(
            name: &'a str,
            arg: ComponentEntityType,
            args: &mut HashMap<&'a str, ComponentEntityType>,
            offset: usize,
        ) -> Result<()> {
            if args.insert(name, arg).is_some() {
                return Err(BinaryReaderError::new(
                    format!(
                        "duplicate instantiation argument name `{}` already defined",
                        name
                    ),
                    offset,
                ));
            }

            Ok(())
        }

        let ty = self.component_at(component_index, offset)?.clone();
        let mut args = HashMap::new();

        // Populate the arguments
        for component_arg in component_args {
            match &component_arg.kind {
                crate::ComponentArgKind::Module(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Module(self.module_at(*idx, offset)?.clone()),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Component(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Component(self.component_at(*idx, offset)?.clone()),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Instance(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Instance(
                            self.component_instance_at(*idx, offset)?.clone(),
                        ),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Function(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Func(
                            self.component_function_at(*idx, offset)?.clone(),
                        ),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Value(idx) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Value(self.value_at(*idx, offset)?.clone()),
                        &mut args,
                        offset,
                    )?;
                }
                crate::ComponentArgKind::Exports(exports) => {
                    insert_arg(
                        component_arg.name,
                        ComponentEntityType::Instance(
                            self.instantiate_exports(exports.as_ref(), offset)?,
                        ),
                        &mut args,
                        offset,
                    )?;
                }
            }
        }

        // Validate the arguments
        for (name, b) in ty.imports.iter() {
            match args.get(name.as_str()) {
                Some(a) => {
                    let desc = match (a, b) {
                        (ComponentEntityType::Module(_), ComponentEntityType::Module(_)) => {
                            "module"
                        }
                        (ComponentEntityType::Component(_), ComponentEntityType::Component(_)) => {
                            "component"
                        }
                        (ComponentEntityType::Instance(_), ComponentEntityType::Instance(_)) => {
                            "instance"
                        }
                        (ComponentEntityType::Func(_), ComponentEntityType::Func(_)) => "function",
                        (ComponentEntityType::Value(_), ComponentEntityType::Value(_)) => "value",
                        _ => {
                            return Err(BinaryReaderError::new(
                                format!(
                                "expected component instantiation argument `{}` to be of type `{}`",
                                name,
                                b.desc()
                            ),
                                offset,
                            ))
                        }
                    };

                    if !a.is_subtype_of(b) {
                        return Err(BinaryReaderError::new(
                            format!(
                                "{} type mismatch for component instantiation argument `{}`",
                                desc, name
                            ),
                            offset,
                        ));
                    }
                }
                None => {
                    return Err(BinaryReaderError::new(
                        format!("missing component instantiation argument named `{}`", name),
                        offset,
                    ))
                }
            }
        }

        Ok(InstanceType {
            exports: ty.exports,
        })
    }

    fn instantiate_exports(
        &mut self,
        exports: &[crate::ComponentExport],
        offset: usize,
    ) -> Result<InstanceType> {
        fn insert_export(
            name: &str,
            export: ComponentEntityType,
            exports: &mut HashMap<String, ComponentEntityType>,
            offset: usize,
        ) -> Result<()> {
            if exports.insert(name.to_string(), export).is_some() {
                return Err(BinaryReaderError::new(
                    format!(
                        "duplicate instantiation export name `{}` already defined",
                        name
                    ),
                    offset,
                ));
            }

            Ok(())
        }

        let mut inst_exports = HashMap::new();
        for export in exports {
            match &export.kind {
                crate::ComponentExportKind::Module(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Module(self.module_at(*idx, offset)?.clone()),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Component(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Component(self.component_at(*idx, offset)?.clone()),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Instance(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Instance(
                            self.component_instance_at(*idx, offset)?.clone(),
                        ),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Function(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Func(
                            self.component_function_at(*idx, offset)?.clone(),
                        ),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Value(idx) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Value(self.value_at(*idx, offset)?.clone()),
                        &mut inst_exports,
                        offset,
                    )?;
                }
                crate::ComponentExportKind::Exports(exports) => {
                    insert_export(
                        export.name,
                        ComponentEntityType::Instance(
                            self.instantiate_exports(exports.as_ref(), offset)?,
                        ),
                        &mut inst_exports,
                        offset,
                    )?;
                }
            }
        }

        Ok(InstanceType {
            exports: Rc::new(inst_exports),
        })
    }

    fn alias_instance_export(
        &mut self,
        kind: crate::AliasKind,
        idx: u32,
        name: &str,
        offset: usize,
    ) -> Result<()> {
        macro_rules! push_module_export {
            ($expected:path, $collection:ident, $ty:literal) => {{
                match self.module_instance_export(idx, name, offset)?.clone() {
                    $expected(ty) => {
                        self.$collection.push(ty);
                        Ok(())
                    }
                    _ => {
                        return Err(BinaryReaderError::new(
                            format!("export `{}` for instance {} is not a {}", name, idx, $ty),
                            offset,
                        ))
                    }
                }
            }};
        }

        macro_rules! push_component_export {
            ($expected:path, $collection:ident, $ty:literal) => {{
                match self.component_instance_export(idx, name, offset)?.clone() {
                    $expected(ty) => {
                        self.$collection.push(ty);
                        Ok(())
                    }
                    _ => {
                        return Err(BinaryReaderError::new(
                            format!("export `{}` for instance {} is not a {}", name, idx, $ty),
                            offset,
                        ))
                    }
                }
            }};
        }

        match kind {
            crate::AliasKind::Module => {
                push_component_export!(ComponentEntityType::Module, modules, "module")
            }
            crate::AliasKind::Component => {
                push_component_export!(ComponentEntityType::Component, components, "component")
            }
            crate::AliasKind::Instance => {
                match self.component_instance_export(idx, name, offset)?.clone() {
                    ComponentEntityType::Instance(ty) => {
                        self.instances.push(InstanceIndexType::Component(ty));
                        Ok(())
                    }
                    _ => {
                        return Err(BinaryReaderError::new(
                            format!("export `{}` for instance {} is not an instance", name, idx),
                            offset,
                        ))
                    }
                }
            }
            crate::AliasKind::Value => {
                match self.component_instance_export(idx, name, offset)?.clone() {
                    ComponentEntityType::Value(ty) => {
                        self.values.push((ty, false));
                        Ok(())
                    }
                    _ => {
                        return Err(BinaryReaderError::new(
                            format!("export `{}` for instance {} is not a value", name, idx),
                            offset,
                        ))
                    }
                }
            }
            crate::AliasKind::Function => {
                self.functions
                    .push(self.instance_exported_function(idx, name, offset)?);
                Ok(())
            }
            crate::AliasKind::Table => {
                push_module_export!(EntityType::Table, tables, "table")
            }
            crate::AliasKind::Memory => {
                push_module_export!(EntityType::Memory, memories, "memory")
            }
            crate::AliasKind::Global => {
                push_module_export!(EntityType::Global, globals, "global")
            }
            crate::AliasKind::Tag => {
                push_module_export!(EntityType::Tag, tags, "tag")
            }
        }
    }

    fn alias_module(
        &mut self,
        count: u32,
        index: u32,
        parents: &[Self],
        offset: usize,
    ) -> Result<()> {
        let count = count as usize;
        let ty = if count == 0 {
            self.module_at(index, offset)?.clone()
        } else {
            if count > parents.len() {
                return Err(BinaryReaderError::new(
                    format!("invalid outer alias count of {}", count),
                    offset,
                ));
            }

            parents[parents.len() - count]
                .module_at(index, offset)?
                .clone()
        };

        self.modules.push(ty);

        Ok(())
    }

    fn alias_component(
        &mut self,
        count: u32,
        index: u32,
        parents: &[Self],
        offset: usize,
    ) -> Result<()> {
        let count = count as usize;
        let ty = if count == 0 {
            self.component_at(index, offset)?.clone()
        } else {
            if count > parents.len() {
                return Err(BinaryReaderError::new(
                    format!("invalid outer alias count of {}", count),
                    offset,
                ));
            }

            parents[parents.len() - count]
                .component_at(index, offset)?
                .clone()
        };

        self.components.push(ty);

        Ok(())
    }

    fn function_at(&self, idx: u32, offset: usize) -> Result<&FuncIndexType> {
        match self.functions.get(idx as usize) {
            Some(f) => Ok(f),
            None => Err(BinaryReaderError::new(
                format!("unknown function {}: function index out of bounds", idx),
                offset,
            )),
        }
    }

    fn component_function_at(&self, idx: u32, offset: usize) -> Result<&ComponentFuncType> {
        match self.function_at(idx, offset)? {
            FuncIndexType::Component(c) => Ok(c),
            FuncIndexType::Core(_) => Err(BinaryReaderError::new(
                format!("function {} is not a component function", idx),
                offset,
            )),
        }
    }

    fn core_function_at(&self, idx: u32, offset: usize) -> Result<&Arc<FuncType>> {
        match self.function_at(idx, offset)? {
            FuncIndexType::Core(c) => Ok(c),
            FuncIndexType::Component(_) => Err(BinaryReaderError::new(
                format!("function {} is not a core WebAssembly function", idx),
                offset,
            )),
        }
    }

    fn module_at(&self, idx: u32, offset: usize) -> Result<&ModuleType> {
        match self.modules.get(idx as usize) {
            Some(m) => Ok(m),
            None => Err(BinaryReaderError::new(
                format!("unknown module {}: module index out of bounds", idx),
                offset,
            )),
        }
    }

    fn component_at(&self, idx: u32, offset: usize) -> Result<&ComponentType> {
        match self.components.get(idx as usize) {
            Some(c) => Ok(c),
            None => Err(BinaryReaderError::new(
                format!("unknown component {}: component index out of bounds", idx),
                offset,
            )),
        }
    }

    fn instance_at(&self, idx: u32, offset: usize) -> Result<&InstanceIndexType> {
        match self.instances.get(idx as usize) {
            Some(i) => Ok(i),
            None => Err(BinaryReaderError::new(
                format!("unknown instance {}: instance index out of bounds", idx),
                offset,
            )),
        }
    }

    fn module_instance_at(&self, idx: u32, offset: usize) -> Result<&ModuleInstanceType> {
        match self.instance_at(idx, offset)? {
            InstanceIndexType::Module(m) => Ok(m),
            _ => Err(BinaryReaderError::new(
                format!("instance {} is not a module instance", idx),
                offset,
            )),
        }
    }

    fn component_instance_at(&self, idx: u32, offset: usize) -> Result<&InstanceType> {
        match self.instance_at(idx, offset)? {
            InstanceIndexType::Component(c) => Ok(c),
            _ => Err(BinaryReaderError::new(
                format!("instance {} is not a component instance", idx),
                offset,
            )),
        }
    }

    fn instance_exported_function(
        &self,
        idx: u32,
        name: &str,
        offset: usize,
    ) -> Result<FuncIndexType> {
        match self.instance_at(idx, offset)? {
            InstanceIndexType::Component(ty) => {
                if let Some(ty) = ty.exports.get(name) {
                    match ty {
                        ComponentEntityType::Func(ty) => {
                            return Ok(FuncIndexType::Component(ty.clone()))
                        }
                        _ => {
                            return Err(BinaryReaderError::new(
                                format!("export `{}` for instance {} is not a function", name, idx),
                                offset,
                            ))
                        }
                    }
                }
            }
            InstanceIndexType::Module(ty) => {
                if let Some(ty) = ty.exports.get(name) {
                    match ty {
                        EntityType::Func(ty) => return Ok(FuncIndexType::Core(ty.clone())),
                        _ => {
                            return Err(BinaryReaderError::new(
                                format!("export `{}` for instance {} is not a function", name, idx),
                                offset,
                            ))
                        }
                    }
                }
            }
        }

        Err(BinaryReaderError::new(
            format!("instance {} has no export named `{}`", idx, name),
            offset,
        ))
    }

    fn module_instance_export(&self, idx: u32, name: &str, offset: usize) -> Result<&EntityType> {
        match self.module_instance_at(idx, offset)?.exports.get(name) {
            Some(export) => Ok(export),
            None => {
                return Err(BinaryReaderError::new(
                    format!("instance {} has no export named `{}`", idx, name),
                    offset,
                ))
            }
        }
    }

    fn component_instance_export(
        &self,
        idx: u32,
        name: &str,
        offset: usize,
    ) -> Result<&ComponentEntityType> {
        match self.component_instance_at(idx, offset)?.exports.get(name) {
            Some(export) => Ok(export),
            None => {
                return Err(BinaryReaderError::new(
                    format!("instance {} has no export named `{}`", idx, name),
                    offset,
                ))
            }
        }
    }

    fn value_at(&mut self, idx: u32, offset: usize) -> Result<&InterfaceType> {
        match self.values.get_mut(idx as usize) {
            Some((ty, used)) if !*used => {
                *used = true;
                Ok(ty)
            }
            Some(_) => Err(BinaryReaderError::new(
                format!("value {} cannot be used more than once", idx),
                offset,
            )),
            None => Err(BinaryReaderError::new(
                format!("unknown value {}: value index out of bounds", idx),
                offset,
            )),
        }
    }

    fn memory_at(&self, idx: u32, offset: usize) -> Result<&MemoryType> {
        match self.memories.get(idx as usize) {
            Some(m) => Ok(m),
            None => Err(BinaryReaderError::new(
                format!("unknown memory {}: memory index out of bounds", idx),
                offset,
            )),
        }
    }

    fn table_at(&self, idx: u32, offset: usize) -> Result<&TableType> {
        match self.tables.get(idx as usize) {
            Some(m) => Ok(m),
            None => Err(BinaryReaderError::new(
                format!("unknown table {}: table index out of bounds", idx),
                offset,
            )),
        }
    }

    fn global_at(&self, idx: u32, offset: usize) -> Result<&GlobalType> {
        match self.globals.get(idx as usize) {
            Some(g) => Ok(g),
            None => Err(BinaryReaderError::new(
                format!("unknown global {}: global index out of bounds", idx),
                offset,
            )),
        }
    }

    fn tag_at(&self, idx: u32, offset: usize) -> Result<&Arc<FuncType>> {
        match self.tags.get(idx as usize) {
            Some(t) => Ok(t),
            None => Err(BinaryReaderError::new(
                format!("unknown tag {}: tag index out of bounds", idx),
                offset,
            )),
        }
    }
}

#[derive(Default)]
struct ComponentTypeSpace(Vec<ComponentTypeDef>);

impl ComponentTypeSpace {
    fn type_def(
        &mut self,
        def: crate::ComponentTypeDef,
        features: &WasmFeatures,
        parents: &[ComponentState],
        offset: usize,
    ) -> Result<()> {
        let def = match def {
            crate::ComponentTypeDef::Module(defs) => {
                ComponentTypeDef::Module(Self::module_type(defs.as_ref(), features, offset)?)
            }
            crate::ComponentTypeDef::Component(defs) => ComponentTypeDef::Component(
                Self::component_type(defs.as_ref(), features, parents, offset)?,
            ),
            crate::ComponentTypeDef::Instance(defs) => ComponentTypeDef::Instance(
                Self::instance_type(defs.as_ref(), features, parents, offset)?,
            ),
            crate::ComponentTypeDef::Function(ty) => {
                ComponentTypeDef::Func(self.function_type(ty, offset)?)
            }
            crate::ComponentTypeDef::Value(ty) => {
                ComponentTypeDef::Value(InterfaceType::new(ty, self, offset)?)
            }
            crate::ComponentTypeDef::Compound(ct) => {
                ComponentTypeDef::Compound(Rc::new(self.compound_type(ct, offset)?))
            }
        };

        self.0.push(def);

        Ok(())
    }

    fn alias_type(
        &mut self,
        count: u32,
        index: u32,
        parents: &[ComponentState],
        offset: usize,
    ) -> Result<()> {
        let count = count as usize;
        let ty = if count == 0 {
            self.get(index, offset)?.clone()
        } else {
            if count > parents.len() {
                return Err(BinaryReaderError::new(
                    format!("invalid outer alias count of {}", count),
                    offset,
                ));
            }

            parents[parents.len() - count]
                .types
                .get(index, offset)?
                .clone()
        };

        self.0.push(ty);

        Ok(())
    }

    fn module_type(
        defs: &[crate::ModuleType],
        features: &WasmFeatures,
        offset: usize,
    ) -> Result<ModuleType> {
        let mut types = ModuleTypeSpace::default();
        let mut imports = HashMap::new();
        let mut exports = HashMap::new();

        for def in defs {
            match def {
                crate::ModuleType::Type(ty) => {
                    types.type_def(ty.clone(), features, offset)?;
                }
                crate::ModuleType::Export { name, ty } => {
                    types.check_type_ref(ty, features, offset)?;
                    if exports
                        .insert(
                            name.to_string(),
                            EntityType::from_type_ref(&types, *ty, offset)?,
                        )
                        .is_some()
                    {
                        return Err(BinaryReaderError::new(
                            format!("duplicate export name `{}` already defined", name),
                            offset,
                        ));
                    }
                }
                crate::ModuleType::Import(i) => {
                    types.check_type_ref(&i.ty, features, offset)?;

                    if imports
                        .insert(
                            (i.module.to_string(), i.name.to_string()),
                            EntityType::from_type_ref(&types, i.ty, offset)?,
                        )
                        .is_some()
                    {
                        return Err(BinaryReaderError::new(
                            format!(
                                "duplicate import name `{}::{}` already defined",
                                i.module, i.name
                            ),
                            offset,
                        ));
                    }
                }
            };
        }

        Ok(ModuleType {
            imports: Rc::new(imports),
            exports: Rc::new(exports),
        })
    }

    fn component_type(
        defs: &[crate::ComponentType],
        features: &WasmFeatures,
        parents: &[ComponentState],
        offset: usize,
    ) -> Result<ComponentType> {
        let mut types = ComponentTypeSpace::default();
        let mut imports = HashMap::new();
        let mut exports = HashMap::new();

        for def in defs {
            match def {
                crate::ComponentType::Type(ty) => {
                    types.type_def(ty.clone(), features, parents, offset)?;
                }
                crate::ComponentType::Export { name, ty } => {
                    if exports
                        .insert(
                            name.to_string(),
                            ComponentEntityType::from_type_def(
                                types.get(*ty, offset)?.clone(),
                                "export",
                                offset,
                            )?,
                        )
                        .is_some()
                    {
                        return Err(BinaryReaderError::new(
                            format!("duplicate export name `{}` already defined", name),
                            offset,
                        ));
                    }
                }
                crate::ComponentType::Import(i) => {
                    if imports
                        .insert(
                            i.name.to_string(),
                            ComponentEntityType::from_type_def(
                                types.get(i.ty, offset)?.clone(),
                                "import",
                                offset,
                            )?,
                        )
                        .is_some()
                    {
                        return Err(BinaryReaderError::new(
                            format!("duplicate import name `{}` already defined", i.name),
                            offset,
                        ));
                    }
                }
                crate::ComponentType::OuterType { count, index } => {
                    let ty = if *count == 0 {
                        types.get(*index, offset)?.clone()
                    } else {
                        let count = *count as usize;
                        if count > parents.len() {
                            return Err(BinaryReaderError::new(
                                format!("invalid outer alias count of {}", count),
                                offset,
                            ));
                        }

                        parents[parents.len() - count]
                            .types
                            .get(*index, offset)?
                            .clone()
                    };

                    types.0.push(ty);
                }
            };
        }

        Ok(ComponentType {
            imports: Rc::new(imports),
            exports: Rc::new(exports),
        })
    }

    fn instance_type(
        defs: &[crate::InstanceType],
        features: &WasmFeatures,
        parents: &[ComponentState],
        offset: usize,
    ) -> Result<InstanceType> {
        let mut types = ComponentTypeSpace::default();
        let mut exports = HashMap::new();

        for def in defs {
            match def {
                crate::InstanceType::Type(ty) => {
                    types.type_def(ty.clone(), features, parents, offset)?;
                }
                crate::InstanceType::Export { name, ty } => {
                    if exports
                        .insert(
                            name.to_string(),
                            ComponentEntityType::from_type_def(
                                types.get(*ty, offset)?.clone(),
                                "export",
                                offset,
                            )?,
                        )
                        .is_some()
                    {
                        return Err(BinaryReaderError::new(
                            format!("duplicate export name `{}` already defined", name),
                            offset,
                        ));
                    }
                }
                crate::InstanceType::OuterType { count, index } => {
                    let ty = if *count == 0 {
                        types.get(*index, offset)?.clone()
                    } else {
                        let count = *count as usize;
                        if count > parents.len() {
                            return Err(BinaryReaderError::new(
                                format!("invalid outer alias count of {}", count),
                                offset,
                            ));
                        }

                        parents[parents.len() - count]
                            .types
                            .get(*index, offset)?
                            .clone()
                    };

                    types.0.push(ty);
                }
            };
        }

        Ok(InstanceType {
            exports: Rc::new(exports),
        })
    }

    fn function_type(
        &self,
        ty: crate::ComponentFuncType,
        offset: usize,
    ) -> Result<ComponentFuncType> {
        let mut core_params = Vec::new();
        let mut core_returns = Vec::new();

        let params = ty
            .params
            .iter()
            .map(|(name, ty)| {
                Self::check_name(name, "function parameter", offset)?;
                let ty = InterfaceType::new(*ty, self, offset)?;
                ty.push_wasm_types(&mut core_params)?;
                Ok((name.to_string(), ty))
            })
            .collect::<Result<_>>()?;

        let result = InterfaceType::new(ty.result, self, offset)?;
        result.push_wasm_types(&mut core_returns)?;

        Ok(ComponentFuncType {
            params,
            result,
            core_type: Arc::new(FuncType {
                params: core_params.into_boxed_slice(),
                returns: core_returns.into_boxed_slice(),
            }),
        })
    }

    fn check_name(name: &str, desc: &str, offset: usize) -> Result<()> {
        if name.is_empty() {
            return Err(BinaryReaderError::new(
                format!("{} name cannot be empty", desc),
                offset,
            ));
        }

        Ok(())
    }

    fn compound_type(&self, ct: crate::CompoundType, offset: usize) -> Result<CompoundType> {
        Ok(match ct {
            crate::CompoundType::Record(fields) => CompoundType::Record(
                fields
                    .iter()
                    .map(|(name, ty)| {
                        Self::check_name(name, "record field", offset)?;
                        InterfaceType::new(*ty, self, offset)
                    })
                    .collect::<Result<_>>()?,
            ),
            crate::CompoundType::Variant { cases, default } => {
                if let Some(default) = default {
                    if default >= cases.len() as u32 {
                        return Err(BinaryReaderError::new(
                            format!("variant default index {} is out of bounds", default),
                            offset,
                        ));
                    }
                }
                CompoundType::Variant(
                    cases
                        .iter()
                        .map(|(name, ty)| {
                            Self::check_name(name, "variant case", offset)?;
                            InterfaceType::new(*ty, self, offset)
                        })
                        .collect::<Result<_>>()?,
                )
            }
            crate::CompoundType::List(ty) => {
                CompoundType::List(InterfaceType::new(ty, self, offset)?)
            }
            crate::CompoundType::Tuple(types) => CompoundType::Tuple(
                types
                    .iter()
                    .map(|ty| InterfaceType::new(*ty, self, offset))
                    .collect::<Result<_>>()?,
            ),
            crate::CompoundType::Flags(names) => {
                for name in names.iter() {
                    Self::check_name(name, "flag", offset)?
                }

                CompoundType::Flags(names.len())
            }
            crate::CompoundType::Enum(names) => {
                for name in names.iter() {
                    Self::check_name(name, "enum tag", offset)?
                }

                CompoundType::Enum(names.len())
            }
            crate::CompoundType::Union(types) => CompoundType::Union(
                types
                    .iter()
                    .map(|ty| InterfaceType::new(*ty, self, offset))
                    .collect::<Result<_>>()?,
            ),
            crate::CompoundType::Optional(ty) => {
                CompoundType::Optional(InterfaceType::new(ty, self, offset)?)
            }
            crate::CompoundType::Expected { ok, error } => CompoundType::Expected(
                InterfaceType::new(ok, self, offset)?,
                InterfaceType::new(error, self, offset)?,
            ),
        })
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn get(&self, idx: u32, offset: usize) -> Result<&ComponentTypeDef> {
        match self.0.get(idx as usize) {
            Some(t) => Ok(t),
            None => Err(BinaryReaderError::new(
                format!("unknown type {}: type index out of bounds", idx),
                offset,
            )),
        }
    }

    fn compound_type_at(&self, idx: u32, offset: usize) -> Result<&Rc<CompoundType>> {
        match self.get(idx, offset)? {
            ComponentTypeDef::Compound(ct) => Ok(ct),
            _ => Err(BinaryReaderError::new(
                format!("type index {} is not a compound type", idx),
                offset,
            )),
        }
    }

    fn function_type_at(&self, idx: u32, offset: usize) -> Result<&ComponentFuncType> {
        match self.get(idx, offset)? {
            ComponentTypeDef::Func(ft) => Ok(ft),
            _ => Err(BinaryReaderError::new(
                format!("type index {} is not a function type", idx),
                offset,
            )),
        }
    }
}

/// Flags for features that are enabled for validation.
#[derive(Hash, Debug, Copy, Clone)]
pub struct WasmFeatures {
    /// The WebAssembly reference types proposal (enabled by default)
    pub reference_types: bool,
    /// The WebAssembly multi-value proposal (enabled by default)
    pub multi_value: bool,
    /// The WebAssembly bulk memory operations proposal (enabled by default)
    pub bulk_memory: bool,
    /// The WebAssembly SIMD proposal
    pub simd: bool,
    /// The WebAssembly Relaxed SIMD proposal
    pub relaxed_simd: bool,
    /// The WebAssembly threads proposal
    pub threads: bool,
    /// The WebAssembly tail-call proposal
    pub tail_call: bool,
    /// Whether or not only deterministic instructions are allowed
    pub deterministic_only: bool,
    /// The WebAssembly multi memory proposal
    pub multi_memory: bool,
    /// The WebAssembly exception handling proposal
    pub exceptions: bool,
    /// The WebAssembly memory64 proposal
    pub memory64: bool,
    /// The WebAssembly extended_const proposal
    pub extended_const: bool,
    /// The WebAssembly component model proposal.
    pub component_model: bool,
}

impl Default for WasmFeatures {
    fn default() -> WasmFeatures {
        WasmFeatures {
            // off-by-default features
            relaxed_simd: false,
            threads: false,
            tail_call: false,
            multi_memory: false,
            exceptions: false,
            memory64: false,
            extended_const: false,
            deterministic_only: cfg!(feature = "deterministic"),

            // on-by-default features
            bulk_memory: true,
            multi_value: true,
            reference_types: true,
            simd: true,
            component_model: false,
        }
    }
}

// Section order for WebAssembly modules.
//
// Component sections are unordered and allow for duplicates,
// so this isn't used for components.
#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
enum Order {
    AfterHeader,
    Type,
    Import,
    Function,
    Table,
    Memory,
    Tag,
    Global,
    Export,
    Start,
    Element,
    DataCount,
    Code,
    Data,
}

impl Default for Order {
    fn default() -> Order {
        Order::AfterHeader
    }
}

#[derive(Clone)]
enum TypeDef {
    Func(Arc<FuncType>),
}

#[derive(Clone)]
enum EntityType {
    Func(Arc<FuncType>),
    Table(TableType),
    Memory(MemoryType),
    Global(GlobalType),
    Tag(Arc<FuncType>),
}

impl EntityType {
    fn from_type_ref(types: &ModuleTypeSpace, tr: TypeRef, offset: usize) -> Result<Self> {
        match tr {
            TypeRef::Func(idx) => Ok(EntityType::Func(types.func_type_at(idx, offset)?.clone())),
            TypeRef::Table(ty) => Ok(EntityType::Table(ty)),
            TypeRef::Memory(ty) => Ok(EntityType::Memory(ty)),
            TypeRef::Global(ty) => Ok(EntityType::Global(ty)),
            TypeRef::Tag(ty) => Ok(EntityType::Tag(
                types.func_type_at(ty.func_type_idx, offset)?.clone(),
            )),
        }
    }

    fn from_export(state: &ComponentState, export: &crate::Export, offset: usize) -> Result<Self> {
        Ok(match export.kind {
            ExternalKind::Func => Self::Func(state.core_function_at(export.index, offset)?.clone()),
            ExternalKind::Table => Self::Table(*state.table_at(export.index, offset)?),
            ExternalKind::Memory => Self::Memory(*state.memory_at(export.index, offset)?),
            ExternalKind::Global => Self::Global(*state.global_at(export.index, offset)?),
            ExternalKind::Tag => Self::Tag(state.tag_at(export.index, offset)?.clone()),
        })
    }

    fn is_subtype_of(&self, b: &Self) -> bool {
        macro_rules! limits_match {
            ($a:expr, $b:expr) => {{
                let a = $a;
                let b = $b;
                a.initial >= b.initial
                    && match b.maximum {
                        Some(b_max) => match a.maximum {
                            Some(a_max) => a_max <= b_max,
                            None => false,
                        },
                        None => true,
                    }
            }};
        }

        match (self, b) {
            (EntityType::Func(a), EntityType::Func(b)) => a == b,
            (EntityType::Table(a), EntityType::Table(b)) => {
                a.element_type == b.element_type && limits_match!(a, b)
            }
            (EntityType::Memory(a), EntityType::Memory(b)) => {
                a.shared == b.shared && a.memory64 == b.memory64 && limits_match!(a, b)
            }
            (EntityType::Global(a), EntityType::Global(b)) => a == b,
            (EntityType::Tag(a), EntityType::Tag(b)) => a == b,
            _ => false,
        }
    }

    fn desc(&self) -> &'static str {
        match self {
            Self::Func(_) => "function",
            Self::Table(_) => "table",
            Self::Memory(_) => "memory",
            Self::Global(_) => "global",
            Self::Tag(_) => "tag",
        }
    }
}

#[derive(Clone)]
struct ModuleType {
    imports: Rc<HashMap<(String, String), EntityType>>,
    exports: Rc<HashMap<String, EntityType>>,
}

impl ModuleType {
    fn is_subtype_of(&self, other: &Self) -> bool {
        self.imports
            .iter()
            .all(|(k, ty)| match other.imports.get(k) {
                Some(other) => ty.is_subtype_of(other),
                None => false,
            })
            && other
                .exports
                .iter()
                .all(|(k, other)| match self.exports.get(k) {
                    Some(ty) => ty.is_subtype_of(other),
                    None => false,
                })
    }
}

#[derive(Clone)]
struct ModuleInstanceType {
    exports: Rc<HashMap<String, EntityType>>,
}

#[derive(Clone)]
struct ComponentType {
    imports: Rc<HashMap<String, ComponentEntityType>>,
    exports: Rc<HashMap<String, ComponentEntityType>>,
}

impl ComponentType {
    fn is_subtype_of(&self, other: &Self) -> bool {
        self.imports
            .iter()
            .all(|(k, ty)| match other.imports.get(k) {
                Some(other) => ty.is_subtype_of(other),
                None => false,
            })
            && other
                .exports
                .iter()
                .all(|(k, other)| match self.exports.get(k) {
                    Some(ty) => ty.is_subtype_of(other),
                    None => false,
                })
    }
}

#[derive(Clone)]
struct InstanceType {
    exports: Rc<HashMap<String, ComponentEntityType>>,
}

impl InstanceType {
    fn is_subtype_of(&self, other: &Self) -> bool {
        other
            .exports
            .iter()
            .all(|(k, other)| match self.exports.get(k) {
                Some(ty) => ty.is_subtype_of(other),
                None => false,
            })
    }
}

#[derive(Clone, Eq)]
struct ComponentFuncType {
    params: Rc<[(String, InterfaceType)]>,
    result: InterfaceType,
    core_type: Arc<FuncType>,
}

impl PartialEq for ComponentFuncType {
    fn eq(&self, other: &Self) -> bool {
        self.params
            .iter()
            .map(|(_, ty)| ty)
            .eq(other.params.iter().map(|(_, ty)| ty))
            && self.result.eq(&other.result)
    }
}

#[derive(Clone, Eq)]
enum CompoundType {
    Record(Box<[InterfaceType]>),
    Variant(Box<[InterfaceType]>),
    List(InterfaceType),
    Tuple(Box<[InterfaceType]>),
    Flags(usize),
    Enum(usize),
    Union(Box<[InterfaceType]>),
    Optional(InterfaceType),
    Expected(InterfaceType, InterfaceType),
}

impl PartialEq for CompoundType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Record(l), Self::Record(r))
            | (Self::Variant(l), Self::Variant(r))
            | (Self::Tuple(l), Self::Tuple(r))
            | (Self::Union(l), Self::Union(r)) => l == r,

            (Self::List(l), Self::List(r)) => l == r,
            (Self::Flags(l), Self::Flags(r)) | (Self::Enum(l), Self::Enum(r)) => l == r,
            (Self::Optional(l), Self::Optional(r)) => l == r,
            (Self::Expected(lo, le), Self::Expected(ro, re)) => lo == ro && le == re,
            _ => false,
        }
    }
}

#[derive(Clone)]
enum ComponentTypeDef {
    Module(ModuleType),
    Component(ComponentType),
    Instance(InstanceType),
    Func(ComponentFuncType),
    Value(InterfaceType),
    Compound(Rc<CompoundType>),
}

/// Possible return values from [`Validator::payload`].
pub enum ValidPayload<'a> {
    /// The payload validated, no further action need be taken.
    Ok,
    /// The payload validated, but it started a nested module.
    ///
    /// This result indicates that the specified parser should be used instead
    /// of the currently-used parser until this returned one ends.
    Submodule(Parser),
    /// A function was found to be validate.
    Func(FuncValidator<ValidatorResources>, FunctionBody<'a>),
}

impl Validator {
    /// Creates a new [`Validator`] ready to validate a WebAssembly module
    /// or component.
    ///
    /// The new validator will receive payloads parsed from
    /// [`Parser`], and expects the first payload received to be
    /// the version header from the parser.
    pub fn new() -> Validator {
        Validator::default()
    }

    /// Configures the enabled WebAssembly features for this `Validator`.
    pub fn wasm_features(&mut self, features: WasmFeatures) -> &mut Validator {
        self.features = features;
        self
    }

    /// Validates an entire in-memory module or component  with this validator.
    ///
    /// This function will internally create a [`Parser`] to parse the `bytes`
    /// provided. The entire module or component specified by `bytes` will be
    /// parsed and validated. Parse and validation errors will be returned through
    /// `Err(_)`, and otherwise a successful validation means `Ok(())` is
    /// returned.
    pub fn validate_all(&mut self, bytes: &[u8]) -> Result<()> {
        let mut functions_to_validate = Vec::new();
        for payload in Parser::new(0).parse_all(bytes) {
            if let ValidPayload::Func(a, b) = self.payload(&payload?)? {
                functions_to_validate.push((a, b));
            }
        }

        for (mut validator, body) in functions_to_validate {
            validator.validate(&body)?;
        }
        Ok(())
    }

    /// Convenience function to validate a single [`Payload`].
    ///
    /// This function is intended to be used as a convenience. It will
    /// internally perform any validation necessary to validate the [`Payload`]
    /// provided. The convenience part is that you're likely already going to
    /// be matching on [`Payload`] in your application, at which point it's more
    /// appropriate to call the individual methods on [`Validator`] per-variant
    /// in [`Payload`], such as [`Validator::type_section`].
    ///
    /// This function returns a [`ValidPayload`] variant on success, indicating
    /// one of a few possible actions that need to be taken after a payload is
    /// validated. For example function contents are not validated here, they're
    /// returned through [`ValidPayload`] for validation by the caller.
    pub fn payload<'a>(&mut self, payload: &Payload<'a>) -> Result<ValidPayload<'a>> {
        use crate::Payload::*;
        match payload {
            Version {
                num,
                encoding,
                range,
            } => self.version(*num, *encoding, range)?,

            // Module sections
            TypeSection(s) => self.type_section(s)?,
            ImportSection(s) => self.import_section(s)?,
            FunctionSection(s) => self.function_section(s)?,
            TableSection(s) => self.table_section(s)?,
            MemorySection(s) => self.memory_section(s)?,
            TagSection(s) => self.tag_section(s)?,
            GlobalSection(s) => self.global_section(s)?,
            ExportSection(s) => self.export_section(s)?,
            StartSection { func, range } => self.start_section(*func, range)?,
            ElementSection(s) => self.element_section(s)?,
            DataCountSection { count, range } => self.data_count_section(*count, range)?,
            CodeSectionStart {
                count,
                range,
                size: _,
            } => self.code_section_start(*count, range)?,
            CodeSectionEntry(body) => {
                let func_validator = self.code_section_entry(body)?;
                return Ok(ValidPayload::Func(func_validator, *body));
            }
            DataSection(s) => self.data_section(s)?,

            // Component sections
            ComponentTypeSection(s) => self.component_type_section(s)?,
            ComponentImportSection(s) => self.component_import_section(s)?,
            ComponentFunctionSection(s) => self.component_function_section(s)?,
            ModuleSection { range, .. } => self.module_section(range)?,
            ComponentSection { range, .. } => self.component_section(range)?,
            InstanceSection(s) => self.instance_section(s)?,
            ComponentExportSection(s) => self.component_export_section(s)?,
            ComponentStartSection { func, args, range } => {
                self.component_start_section(*func, args, range)?
            }
            AliasSection(s) => self.alias_section(s)?,

            End(offset) => self.end(*offset)?,

            CustomSection { .. } => {} // no validation for custom sections
            UnknownSection { id, range, .. } => self.unknown_section(*id, range)?,
        }
        Ok(ValidPayload::Ok)
    }

    /// Validates [`Payload::Version`](crate::Payload).
    pub fn version(&mut self, num: u32, encoding: Encoding, range: &Range) -> Result<()> {
        match &self.state {
            State::Unparsed(expected) => {
                if let Some(expected) = expected {
                    if *expected != encoding {
                        return Err(BinaryReaderError::new(
                            format!(
                                "expected a version header for a {}",
                                match expected {
                                    Encoding::Module => "module",
                                    Encoding::Component => "component",
                                }
                            ),
                            range.start,
                        ));
                    }
                }
            }
            _ => {
                return Err(BinaryReaderError::new(
                    "wasm version header out of order",
                    range.start,
                ))
            }
        }

        self.state = match (encoding, num) {
            (Encoding::Module, WASM_MODULE_VERSION) => State::Module(ModuleState::default()),
            (Encoding::Component, WASM_COMPONENT_VERSION) => {
                if !self.features.component_model {
                    return Err(BinaryReaderError::new(
                        "WebAssembly component model feature not enabled",
                        range.start,
                    ));
                }

                State::Component(Box::new(ComponentState::default()))
            }
            _ => {
                return Err(BinaryReaderError::new(
                    "unsupported wasm header version",
                    range.start,
                ));
            }
        };

        Ok(())
    }

    /// Validates [`Payload::TypeSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn type_section(&mut self, section: &crate::TypeSectionReader<'_>) -> Result<()> {
        let features = self.features;
        self.ensure_module_section(Order::Type, section, "type", |state, item, offset| {
            check_max(
                state.module.types.len(),
                section.get_count(),
                MAX_WASM_TYPES,
                "types",
                offset,
            )?;

            state
                .module
                .assert_mut()
                .types
                .type_def(item, &features, offset)
        })
    }

    /// Validates [`Payload::ImportSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn import_section(&mut self, section: &crate::ImportSectionReader<'_>) -> Result<()> {
        let features = self.features;
        let has_parent = !self.parents.is_empty();
        self.ensure_module_section(Order::Import, section, "import", |state, item, offset| {
            check_max(
                state.module.imports.len(),
                section.get_count(),
                MAX_WASM_IMPORTS,
                "imports",
                offset,
            )?;

            state
                .module
                .assert_mut()
                .import(item, &features, has_parent, offset)
        })
    }

    /// Validates [`Payload::FunctionSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn function_section(&mut self, section: &crate::FunctionSectionReader<'_>) -> Result<()> {
        self.ensure_module_section(Order::Function, section, "function", |state, ty, offset| {
            state.expected_code_bodies = Some(section.get_count());
            check_max(
                state.module.functions.len(),
                section.get_count(),
                MAX_WASM_FUNCTIONS,
                "functions",
                offset,
            )?;

            // Assert that the type index is indeed a function type
            let module = state.module.assert_mut();
            module.types.func_type_at(ty, offset)?;
            module.functions.push(ty);

            Ok(())
        })
    }

    /// Validates [`Payload::TableSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn table_section(&mut self, section: &crate::TableSectionReader<'_>) -> Result<()> {
        let features = self.features;
        self.ensure_module_section(Order::Table, section, "table", |state, ty, offset| {
            check_max(
                state.module.tables.len(),
                section.get_count(),
                state.module.max_tables(&features),
                "tables",
                offset,
            )?;

            let module = state.module.assert_mut();
            module.types.check_table_type(&ty, &features, offset)?;
            module.tables.push(ty);
            Ok(())
        })
    }

    /// Validates [`Payload::MemorySection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn memory_section(&mut self, section: &crate::MemorySectionReader<'_>) -> Result<()> {
        let features = self.features;
        self.ensure_module_section(Order::Memory, section, "memory", |state, ty, offset| {
            check_max(
                state.module.memories.len(),
                section.get_count(),
                state.module.max_memories(&features),
                "memories",
                offset,
            )?;

            let module = state.module.assert_mut();
            module.types.check_memory_type(&ty, &features, offset)?;
            module.memories.push(ty);
            Ok(())
        })
    }

    /// Validates [`Payload::TagSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn tag_section(&mut self, section: &crate::TagSectionReader<'_>) -> Result<()> {
        let features = self.features;
        if !features.exceptions {
            return Err(BinaryReaderError::new(
                "exceptions proposal not enabled",
                section.range().start,
            ));
        }

        self.ensure_module_section(Order::Tag, section, "tag", |state, ty, offset| {
            check_max(
                state.module.tags.len(),
                section.get_count(),
                MAX_WASM_TAGS,
                "tags",
                offset,
            )?;

            let module = state.module.assert_mut();
            module.types.check_tag_type(&ty, &features, offset)?;
            module.tags.push(ty);
            Ok(())
        })
    }

    /// Validates [`Payload::GlobalSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn global_section(&mut self, section: &crate::GlobalSectionReader<'_>) -> Result<()> {
        let features = self.features;
        self.ensure_module_section(Order::Global, section, "global", |state, g, offset| {
            check_max(
                state.module.globals.len(),
                section.get_count(),
                MAX_WASM_GLOBALS,
                "globals",
                offset,
            )?;

            state.init_expr(&g.init_expr, g.ty.content_type, false, &features, offset)?;

            let module = state.module.assert_mut();
            module.types.check_global_type(&g.ty, &features, offset)?;
            module.globals.push(g.ty);

            Ok(())
        })
    }

    /// Validates [`Payload::ExportSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn export_section(&mut self, section: &crate::ExportSectionReader<'_>) -> Result<()> {
        self.ensure_module_section(Order::Export, section, "export", |state, e, offset| {
            check_max(
                state.module.exports.len(),
                section.get_count(),
                MAX_WASM_EXPORTS,
                "exports",
                offset,
            )?;

            let module = state.module.assert_mut();
            let tr = module.get_type_ref(e.kind, e.index, offset)?;
            match module.exports.insert(e.name.to_string(), tr) {
                Some(_) => Err(BinaryReaderError::new(
                    format!("duplicate export name `{}` already defined", e.name),
                    offset,
                )),
                None => Ok(()),
            }
        })
    }

    /// Validates [`Payload::StartSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn start_section(&mut self, func: u32, range: &Range) -> Result<()> {
        let offset = range.start;
        let state = self.module_state("start", offset)?;
        state.update_order(Order::Start, offset)?;

        let module = state.module.assert_mut();
        let ty = module.get_func_type(func, offset)?;
        if !ty.params.is_empty() || !ty.returns.is_empty() {
            return Err(BinaryReaderError::new(
                "invalid start function type",
                offset,
            ));
        }

        Ok(())
    }

    /// Validates [`Payload::ElementSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn element_section(&mut self, section: &crate::ElementSectionReader<'_>) -> Result<()> {
        let features = self.features;
        self.ensure_module_section(Order::Element, section, "element", |state, e, offset| {
            match e.ty {
                Type::FuncRef => {}
                Type::ExternRef if features.reference_types => {}
                Type::ExternRef => {
                    return Err(BinaryReaderError::new(
                        "reference types must be enabled for externref elem segment",
                        offset,
                    ))
                }
                _ => return Err(BinaryReaderError::new("invalid reference type", offset)),
            }
            match e.kind {
                ElementKind::Active {
                    table_index,
                    init_expr,
                } => {
                    let table = state.module.get_table(table_index, offset)?;
                    if e.ty != table.element_type {
                        return Err(BinaryReaderError::new(
                            "invalid element type for table type",
                            offset,
                        ));
                    }
                    state.init_expr(&init_expr, Type::I32, false, &features, offset)?;
                }
                ElementKind::Passive | ElementKind::Declared => {
                    if !features.bulk_memory {
                        return Err(BinaryReaderError::new(
                            "bulk memory must be enabled",
                            offset,
                        ));
                    }
                }
            }
            let mut items = e.items.get_items_reader()?;
            if items.get_count() > MAX_WASM_TABLE_ENTRIES as u32 {
                return Err(BinaryReaderError::new(
                    "number of elements is out of bounds",
                    offset,
                ));
            }
            for _ in 0..items.get_count() {
                let offset = items.original_position();
                match items.read()? {
                    ElementItem::Expr(expr) => {
                        state.init_expr(&expr, e.ty, false, &features, offset)?;
                    }
                    ElementItem::Func(f) => {
                        if e.ty != Type::FuncRef {
                            return Err(BinaryReaderError::new(
                                "type mismatch: segment does not have funcref type",
                                offset,
                            ));
                        }
                        let module = state.module.assert_mut();
                        module.get_func_type(f, offset)?;
                        module.function_references.insert(f);
                    }
                }
            }

            let module = state.module.assert_mut();
            module.element_types.push(e.ty);
            Ok(())
        })
    }

    /// Validates [`Payload::DataCountSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn data_count_section(&mut self, count: u32, range: &Range) -> Result<()> {
        let offset = range.start;
        let state = self.module_state("data count", offset)?;
        state.update_order(Order::DataCount, offset)?;

        if count > MAX_WASM_DATA_SEGMENTS as u32 {
            return Err(BinaryReaderError::new(
                "data count section specifies too many data segments",
                offset,
            ));
        }

        state.module.assert_mut().data_count = Some(count);
        Ok(())
    }

    /// Validates [`Payload::CodeSectionStart`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn code_section_start(&mut self, count: u32, range: &Range) -> Result<()> {
        let offset = range.start;
        let state = self.module_state("code", offset)?;
        state.update_order(Order::Code, offset)?;

        match state.expected_code_bodies.take() {
            Some(n) if n == count => {}
            Some(_) => {
                return Err(BinaryReaderError::new(
                    "function and code section have inconsistent lengths",
                    offset,
                ));
            }
            // empty code sections are allowed even if the function section is
            // missing
            None if count == 0 => {}
            None => {
                return Err(BinaryReaderError::new(
                    "code section without function section",
                    offset,
                ))
            }
        }

        Ok(())
    }

    /// Validates [`Payload::CodeSectionEntry`](crate::Payload).
    ///
    /// This function will prepare a [`FuncValidator`] which can be used to
    /// validate the function. The function body provided will be parsed only
    /// enough to create the function validation context. After this the
    /// [`OperatorsReader`](crate::readers::OperatorsReader) returned can be used to read the
    /// opcodes of the function as well as feed information into the validator.
    ///
    /// Note that the returned [`FuncValidator`] is "connected" to this
    /// [`Validator`] in that it uses the internal context of this validator for
    /// validating the function. The [`FuncValidator`] can be sent to
    /// another thread, for example, to offload actual processing of functions
    /// elsewhere.
    ///
    /// This method should only be called when parsing a module.
    pub fn code_section_entry(
        &mut self,
        body: &crate::FunctionBody,
    ) -> Result<FuncValidator<ValidatorResources>> {
        let offset = body.range().start;
        let state = self.module_state("code", offset)?;

        let index = state
            .code_section_index
            .get_or_insert(state.module.num_imported_functions as usize);

        if *index >= state.module.functions.len() {
            return Err(BinaryReaderError::new(
                "code section entry exceeds number of functions",
                offset,
            ));
        }

        let ty = state.module.functions[*index];
        *index += 1;

        let resources = ValidatorResources(state.module.arc().clone());
        Ok(FuncValidator::new(ty as u32, 0, resources, &self.features).unwrap())
    }

    /// Validates [`Payload::DataSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn data_section(&mut self, section: &crate::DataSectionReader<'_>) -> Result<()> {
        let mut section = section.clone();
        let count = section.get_count();
        section.forbid_bulk_memory(!self.features.bulk_memory);

        let features = self.features;
        self.ensure_module_section(Order::Data, &section, "data", |state, d, offset| {
            check_max(0, count, MAX_WASM_DATA_SEGMENTS, "data segments", offset)?;

            state.data_found = count;

            match d.kind {
                DataKind::Passive => Ok(()),
                DataKind::Active {
                    memory_index,
                    init_expr,
                } => {
                    let ty = state.module.get_memory(memory_index, offset)?.index_type();
                    state.init_expr(&init_expr, ty, true, &features, offset)
                }
            }
        })
    }

    /// Validates [`Payload::ComponentTypeSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn component_type_section(
        &mut self,
        section: &crate::ComponentTypeSectionReader,
    ) -> Result<()> {
        let features = self.features;
        self.ensure_component_section(section, "type", |state, parents, ty, offset| {
            check_max(
                state.types.len(),
                section.get_count(),
                MAX_WASM_TYPES,
                "types",
                offset,
            )?;

            state.types.type_def(ty, &features, parents, offset)
        })
    }

    /// Validates [`Payload::ComponentImportSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn component_import_section(
        &mut self,
        section: &crate::ComponentImportSectionReader,
    ) -> Result<()> {
        self.ensure_component_section(section, "import", |state, _, import, offset| {
            check_max(
                state.imports.len(),
                section.get_count(),
                MAX_WASM_IMPORTS,
                "imports",
                offset,
            )?;

            state.import(import, offset)
        })
    }

    /// Validates [`Payload::ComponentFunctionSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn component_function_section(
        &mut self,
        section: &crate::ComponentFunctionSectionReader,
    ) -> Result<()> {
        self.ensure_component_section(section, "function", |state, _, func, offset| {
            check_max(
                state.functions.len(),
                section.get_count(),
                MAX_WASM_FUNCTIONS,
                "functions",
                offset,
            )?;

            match func {
                crate::ComponentFunction::Lift {
                    type_index,
                    func_index,
                    options,
                } => {
                    let ty = state.types.function_type_at(type_index, offset)?;
                    let core_ty = state.core_function_at(func_index, offset)?;

                    if ty.core_type != *core_ty {
                        return Err(BinaryReaderError::new(
                            "lowered function type does not match core function type",
                            offset,
                        ));
                    }

                    check_options(&options, offset)?;
                    state.functions.push(FuncIndexType::Component(ty.clone()));
                }
                crate::ComponentFunction::Lower {
                    func_index,
                    options,
                } => {
                    let ty = state.types.function_type_at(func_index, offset)?;
                    check_options(&options, offset)?;

                    state
                        .functions
                        .push(FuncIndexType::Core(ty.core_type.clone()));
                }
            }

            Ok(())
        })
    }

    /// Validates [`Payload::ModuleSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn module_section(&mut self, range: &Range) -> Result<()> {
        {
            let (state, _) = self.component_state("module", range.start)?;
            check_max(
                state.modules.len(),
                1,
                MAX_WASM_MODULES,
                "modules",
                range.start,
            )?;
        }

        match mem::replace(&mut self.state, State::Unparsed(Some(Encoding::Module))) {
            State::Component(state) => self.parents.push(*state),
            _ => unreachable!(),
        }

        Ok(())
    }

    /// Validates [`Payload::ComponentSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn component_section(&mut self, range: &Range) -> Result<()> {
        {
            let (state, _) = self.component_state("component", range.start)?;
            check_max(
                state.components.len(),
                1,
                MAX_WASM_COMPONENTS,
                "components",
                range.start,
            )?;
        }

        match mem::replace(&mut self.state, State::Unparsed(Some(Encoding::Component))) {
            State::Component(state) => self.parents.push(*state),
            _ => unreachable!(),
        }

        Ok(())
    }

    /// Validates [`Payload::InstanceSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn instance_section(&mut self, section: &crate::InstanceSectionReader) -> Result<()> {
        self.ensure_component_section(section, "instance", |state, _, instance, offset| {
            check_max(
                state.instances.len(),
                section.get_count(),
                MAX_WASM_INSTANCES,
                "instances",
                offset,
            )?;

            let instance = match instance {
                crate::Instance::Module { index, args } => InstanceIndexType::Module(
                    state.instantiate_module(index, args.as_ref(), offset)?,
                ),
                crate::Instance::Component { index, args } => InstanceIndexType::Component(
                    state.instantiate_component(index, args.as_ref(), offset)?,
                ),
                crate::Instance::Exports(exports) => InstanceIndexType::Component(
                    state.instantiate_exports(exports.as_ref(), offset)?,
                ),
            };

            state.instances.push(instance);

            Ok(())
        })
    }

    /// Validates [`Payload::ComponentExportSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn component_export_section(
        &mut self,
        section: &crate::ComponentExportSectionReader,
    ) -> Result<()> {
        self.ensure_component_section(section, "export", |state, _, export, offset| {
            check_max(
                state.exports.len(),
                section.get_count(),
                MAX_WASM_EXPORTS,
                "exports",
                offset,
            )?;

            let ty = match export.kind {
                crate::ComponentExportKind::Module(idx) => {
                    ComponentEntityType::Module(state.module_at(idx, offset)?.clone())
                }
                crate::ComponentExportKind::Component(idx) => {
                    ComponentEntityType::Component(state.component_at(idx, offset)?.clone())
                }
                crate::ComponentExportKind::Instance(idx) => {
                    ComponentEntityType::Instance(state.component_instance_at(idx, offset)?.clone())
                }
                crate::ComponentExportKind::Function(idx) => {
                    ComponentEntityType::Func(state.component_function_at(idx, offset)?.clone())
                }
                crate::ComponentExportKind::Value(idx) => {
                    ComponentEntityType::Value(state.value_at(idx, offset)?.clone())
                }
                crate::ComponentExportKind::Exports(exports) => ComponentEntityType::Instance(
                    state.instantiate_exports(exports.as_ref(), offset)?,
                ),
            };

            if state.exports.insert(export.name.to_string(), ty).is_some() {
                return Err(BinaryReaderError::new(
                    format!("duplicate export name `{}` already defined", export.name),
                    offset,
                ));
            }

            Ok(())
        })
    }

    /// Validates [`Payload::ComponentStartSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn component_start_section(
        &mut self,
        func_index: u32,
        args: &[u32],
        range: &Range,
    ) -> Result<()> {
        let (state, _) = self.component_state("start", range.start)?;

        if state.has_start {
            return Err(BinaryReaderError::new(
                "component cannot have more than one start function",
                range.start,
            ));
        }

        let ft = state
            .component_function_at(func_index, range.start)?
            .clone();

        if ft.params.len() != args.len() {
            return Err(BinaryReaderError::new(
                format!(
                    "component start function requires {} arguments but was given {}",
                    ft.params.len(),
                    args.len()
                ),
                range.start,
            ));
        }

        for (i, ((_, ty), arg)) in ft.params.iter().zip(args).enumerate() {
            if ty != state.value_at(*arg, range.start)? {
                return Err(BinaryReaderError::new(
                    format!(
                        "value type mismatch for component start function argument {}",
                        i
                    ),
                    range.start,
                ));
            }
        }

        match ft.result {
            InterfaceType::Unit => {}
            ty => {
                state.values.push((ty, false));
            }
        }

        state.has_start = true;

        Ok(())
    }

    /// Validates [`Payload::AliasSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn alias_section(&mut self, section: &crate::AliasSectionReader) -> Result<()> {
        self.ensure_component_section(
            section,
            "alias",
            |state, parents, alias, offset| -> Result<(), BinaryReaderError> {
                match alias {
                    crate::Alias::InstanceExport {
                        kind,
                        instance,
                        name,
                    } => state.alias_instance_export(kind, instance, name, offset),
                    crate::Alias::OuterModule { count, index } => {
                        state.alias_module(count, index, parents, offset)
                    }
                    crate::Alias::OuterComponent { count, index } => {
                        state.alias_component(count, index, parents, offset)
                    }
                    crate::Alias::OuterType { count, index } => {
                        state.types.alias_type(count, index, parents, offset)
                    }
                }
            },
        )
    }

    /// Validates [`Payload::UnknownSection`](crate::Payload).
    ///
    /// Currently always returns an error.
    pub fn unknown_section(&mut self, id: u8, range: &Range) -> Result<()> {
        Err(BinaryReaderError::new(
            format!("invalid section code: {}", id),
            range.start,
        ))
    }

    /// Validates [`Payload::End`](crate::Payload).
    pub fn end(&mut self, offset: usize) -> Result<()> {
        match std::mem::replace(&mut self.state, State::Unparsed(None)) {
            State::Unparsed(_) => Err(BinaryReaderError::new(
                "cannot call `end` before a header has been parsed",
                offset,
            )),
            State::Module(state) => self.end_module(state, offset),
            State::Component(state) => self.end_component(state),
        }
    }

    fn end_module(&mut self, state: ModuleState, offset: usize) -> Result<()> {
        let module = &state.module;

        // Ensure that the data count section, if any, was correct.
        if let Some(data_count) = module.data_count {
            if data_count != state.data_found {
                return Err(BinaryReaderError::new(
                    "data count section and passive data mismatch",
                    offset,
                ));
            }
        }
        // Ensure that the function section, if nonzero, was paired with a code
        // section with the appropriate length.
        if let Some(n) = state.expected_code_bodies {
            if n > 0 {
                return Err(BinaryReaderError::new(
                    "function and code sections have inconsistent lengths",
                    offset,
                ));
            }
        }

        // If there's a parent component, we'll push a module to the parent state
        if let Some(mut parent) = self.parents.pop() {
            let imports = module
                .imports
                .iter()
                .map(|(k, v)| {
                    assert_eq!(v.len(), 1);
                    Ok((
                        k.clone(),
                        EntityType::from_type_ref(&module.types, v[0], offset)?,
                    ))
                })
                .collect::<Result<_>>()?;

            let exports = module
                .exports
                .iter()
                .map(|(k, v)| {
                    Ok((
                        k.clone(),
                        EntityType::from_type_ref(&module.types, *v, offset)?,
                    ))
                })
                .collect::<Result<_>>()?;

            parent.modules.push(ModuleType {
                imports: Rc::new(imports),
                exports: Rc::new(exports),
            });

            self.state = State::Component(Box::new(parent));
        }

        Ok(())
    }

    fn end_component(&mut self, state: Box<ComponentState>) -> Result<()> {
        // If there's a parent component, we'll push a component to the parent state
        if let Some(mut parent) = self.parents.pop() {
            parent.components.push(ComponentType {
                imports: Rc::new(state.imports),
                exports: Rc::new(state.exports),
            });

            self.state = State::Component(Box::new(parent));
        }

        Ok(())
    }

    fn module_state(&mut self, section: &str, offset: usize) -> Result<&mut ModuleState> {
        match &mut self.state {
            State::Unparsed(_) => Err(BinaryReaderError::new(
                "unexpected section before header was parsed",
                offset,
            )),
            State::Module(state) => Ok(state),
            State::Component(_) => Err(BinaryReaderError::new(
                format!(
                    "module {} sections are not supported when parsing WebAssembly components",
                    section
                ),
                offset,
            )),
        }
    }

    fn component_state(
        &mut self,
        section: &str,
        offset: usize,
    ) -> Result<(&mut ComponentState, &[ComponentState])> {
        match &mut self.state {
            State::Unparsed(_) => Err(BinaryReaderError::new(
                "unexpected section before header was parsed",
                offset,
            )),
            State::Module(_) => Err(BinaryReaderError::new(
                format!(
                    "component {} sections are not supported when parsing WebAssembly modules",
                    section
                ),
                offset,
            )),
            State::Component(state) => Ok((state, &self.parents)),
        }
    }

    fn ensure_module_section<T>(
        &mut self,
        order: Order,
        section: &T,
        name: &str,
        mut validate_item: impl FnMut(&mut ModuleState, T::Item, usize) -> Result<()>,
    ) -> Result<()>
    where
        T: SectionReader + Clone + SectionWithLimitedItems,
    {
        let offset = section.range().start;
        let state = self.module_state(name, offset)?;

        state.update_order(order, offset)?;

        let mut section = section.clone();
        for _ in 0..section.get_count() {
            let offset = section.original_position();
            let item = section.read()?;
            validate_item(state, item, offset)?;
        }

        section.ensure_end()?;

        Ok(())
    }

    fn ensure_component_section<T>(
        &mut self,
        section: &T,
        name: &str,
        mut validate_item: impl FnMut(
            &mut ComponentState,
            &[ComponentState],
            T::Item,
            usize,
        ) -> Result<()>,
    ) -> Result<()>
    where
        T: SectionReader + Clone + SectionWithLimitedItems,
    {
        let offset = section.range().start;

        if !self.features.component_model {
            return Err(BinaryReaderError::new(
                "component model feature is not enabled",
                offset,
            ));
        }

        let (state, parents) = self.component_state(name, offset)?;

        let mut section = section.clone();
        for _ in 0..section.get_count() {
            let offset = section.original_position();
            let item = section.read()?;
            validate_item(state, parents, item, offset)?;
        }

        section.ensure_end()?;

        Ok(())
    }
}

impl WasmFeatures {
    pub(crate) fn check_value_type(&self, ty: Type) -> Result<(), &'static str> {
        match ty {
            Type::I32 | Type::I64 | Type::F32 | Type::F64 => Ok(()),
            Type::FuncRef | Type::ExternRef => {
                if self.reference_types {
                    Ok(())
                } else {
                    Err("reference types support is not enabled")
                }
            }
            Type::V128 => {
                if self.simd {
                    Ok(())
                } else {
                    Err("SIMD support is not enabled")
                }
            }
        }
    }
}

/// The implementation of [`WasmModuleResources`] used by [`Validator`].
pub struct ValidatorResources(Arc<Module>);

impl WasmModuleResources for ValidatorResources {
    type FuncType = crate::FuncType;

    fn table_at(&self, at: u32) -> Option<TableType> {
        self.0.table_at(at)
    }

    fn memory_at(&self, at: u32) -> Option<MemoryType> {
        self.0.memory_at(at)
    }

    fn tag_at(&self, at: u32) -> Option<&Self::FuncType> {
        self.0.tag_at(at)
    }

    fn global_at(&self, at: u32) -> Option<GlobalType> {
        self.0.global_at(at)
    }

    fn func_type_at(&self, at: u32) -> Option<&Self::FuncType> {
        self.0.func_type_at(at)
    }

    fn type_of_function(&self, at: u32) -> Option<&Self::FuncType> {
        self.0.type_of_function(at)
    }

    fn element_type_at(&self, at: u32) -> Option<Type> {
        self.0.element_type_at(at)
    }

    fn element_count(&self) -> u32 {
        self.0.element_count()
    }

    fn data_count(&self) -> u32 {
        self.0.data_count()
    }

    fn is_function_referenced(&self, idx: u32) -> bool {
        self.0.is_function_referenced(idx)
    }
}

const _: () = {
    fn assert_send<T: Send>() {}

    // Assert that `ValidatorResources` is Send so function validation
    // can be parallelizable
    fn assert() {
        assert_send::<ValidatorResources>();
    }
};

mod arc {
    use std::ops::Deref;
    use std::sync::Arc;

    pub struct MaybeOwned<T> {
        owned: bool,
        arc: Arc<T>,
    }

    impl<T> MaybeOwned<T> {
        fn as_mut(&mut self) -> Option<&mut T> {
            if !self.owned {
                return None;
            }
            debug_assert!(Arc::get_mut(&mut self.arc).is_some());
            Some(unsafe { &mut *(&*self.arc as *const T as *mut T) })
        }

        pub fn assert_mut(&mut self) -> &mut T {
            self.as_mut().unwrap()
        }

        pub fn arc(&mut self) -> &Arc<T> {
            self.owned = false;
            &self.arc
        }
    }

    impl<T: Default> Default for MaybeOwned<T> {
        fn default() -> MaybeOwned<T> {
            MaybeOwned {
                owned: true,
                arc: Arc::default(),
            }
        }
    }

    impl<T> Deref for MaybeOwned<T> {
        type Target = T;

        fn deref(&self) -> &T {
            &self.arc
        }
    }
}
