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

use crate::ExternalKind;
use crate::{
    limits::*, operators_validator::OperatorValidator, BinaryReaderError, DataKind, ElementItem,
    ElementKind, Encoding, FuncType, FunctionBody, GlobalType, Import, InitExpr, MemoryType,
    Operator, Parser, Payload, Range, Result, SectionReader, SectionWithLimitedItems, TableType,
    TagType, Type, TypeRef, WasmModuleResources, WASM_COMPONENT_VERSION, WASM_MODULE_VERSION,
};
use std::collections::{HashMap, HashSet};
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
        Err(e) => return Err(BinaryReaderError::new(e, offset)),
    }
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
    /// of parent validators.
    parents: Vec<State>,

    /// Enabled WebAssembly feature flags, dictating what's valid and what
    /// isn't.
    features: WasmFeatures,
}

enum State {
    /// A version has not yet been parsed.
    Unparsed,
    /// A module header has been parsed.
    Module(ModuleState),
    /// A component header has been parsed.
    Component(ComponentState),
}

impl Default for State {
    fn default() -> Self {
        Self::Unparsed
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
                TypeDef::Func(t)
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

    fn func_type_at(&self, type_index: u32, offset: usize) -> Result<&FuncType> {
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
    exports: HashMap<String, EntityType>,
    num_imported_globals: u32,
    num_imported_functions: u32,
}

impl Module {
    fn import(&mut self, entry: Import<'_>, features: &WasmFeatures, offset: usize) -> Result<()> {
        self.types.check_type_ref(&entry.ty, features, offset)?;
        let (len, max, desc) = match entry.ty {
            TypeRef::Func(type_index) => {
                self.functions.push(type_index);
                self.num_imported_functions += 1;
                (self.functions.len(), MAX_WASM_FUNCTIONS, "funcs")
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
        Ok(())
    }

    fn entity_type(&mut self, kind: ExternalKind, index: u32, offset: usize) -> Result<EntityType> {
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
                EntityType::Func(index as usize)
            }
            ExternalKind::Table => {
                check("table", index, self.tables.len())?;
                EntityType::Table(self.tables[index as usize])
            }
            ExternalKind::Memory => {
                check("memory", index, self.memories.len())?;
                EntityType::Memory(self.memories[index as usize])
            }
            ExternalKind::Global => {
                check("global", index, self.globals.len())?;
                EntityType::Global(self.globals[index as usize])
            }
            ExternalKind::Tag => {
                check("tag", index, self.tags.len())?;
                EntityType::Tag(self.tags[index as usize])
            }
        })
    }

    fn get_func_type(&self, func_idx: u32, offset: usize) -> Result<&FuncType> {
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
        match &self.types.0[ty.func_type_idx as usize] {
            TypeDef::Func(f) => Some(f),
        }
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
        let i = *self.functions.get(at as usize)?;
        match &self.types.0[i as usize] {
            TypeDef::Func(f) => Some(f),
        }
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

#[derive(Default)]
struct ComponentState {}

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

enum TypeDef {
    Func(FuncType),
}

#[derive(Clone)]
enum EntityType {
    Global(GlobalType),
    Memory(MemoryType),
    Table(TableType),
    Func(usize), // pointer into `validator.types`
    Tag(TagType),
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
            ComponentTypeSection(_) => todo!("component-model"),
            ComponentImportSection(_) => todo!("component-model"),
            ComponentFunctionSection(_) => todo!("component-model"),
            ModuleSection { .. } => todo!("component-model"),
            ComponentSection { .. } => todo!("component-model"),
            InstanceSection(_) => todo!("component-model"),
            ComponentExportSection(_) => todo!("component-model"),
            ComponentStartSection { .. } => todo!("component-model"),
            AliasSection(_) => todo!("component-model"),

            End(offset) => self.end(*offset)?,

            CustomSection { .. } => {} // no validation for custom sections
            UnknownSection { id, range, .. } => self.unknown_section(*id, range)?,
        }
        Ok(ValidPayload::Ok)
    }

    /// Validates [`Payload::Version`](crate::Payload).
    pub fn version(&mut self, num: u32, encoding: Encoding, range: &Range) -> Result<()> {
        match &self.state {
            State::Unparsed => {}
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

                State::Component(ComponentState::default())
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
        self.module_section(Order::Type, section, "type", |state, item, offset| {
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
        self.module_section(Order::Import, section, "import", |state, item, offset| {
            state.module.assert_mut().import(item, &features, offset)
        })
    }

    /// Validates [`Payload::FunctionSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn function_section(&mut self, section: &crate::FunctionSectionReader<'_>) -> Result<()> {
        self.module_section(Order::Function, section, "function", |state, ty, offset| {
            state.expected_code_bodies = Some(section.get_count());
            check_max(
                state.module.functions.len(),
                section.get_count(),
                MAX_WASM_FUNCTIONS,
                "funcs",
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
        self.module_section(Order::Table, section, "table", |state, ty, offset| {
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
        self.module_section(Order::Memory, section, "memory", |state, ty, offset| {
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

        self.module_section(Order::Tag, section, "tag", |state, ty, offset| {
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
        self.module_section(Order::Global, section, "global", |state, g, offset| {
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
        self.module_section(Order::Export, section, "export", |state, e, offset| {
            let module = state.module.assert_mut();
            let ty = module.entity_type(e.kind, e.index, offset)?;
            match module.exports.insert(e.name.to_string(), ty) {
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
        self.module_section(Order::Element, section, "element", |state, e, offset| {
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
    pub fn data_section(&mut self, section: &crate::DataSectionReader<'_>) -> Result<()> {
        let mut section = section.clone();
        let count = section.get_count();
        section.forbid_bulk_memory(!self.features.bulk_memory);

        let features = self.features;
        self.module_section(Order::Data, &section, "data", |state, d, offset| {
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
        match &mut self.state {
            State::Unparsed => Err(BinaryReaderError::new(
                "cannot call end before a header has been parsed",
                offset,
            )),
            State::Module(state) => Self::end_module(state, offset),
            State::Component(_) => todo!(),
        }
    }

    fn end_module(state: &ModuleState, offset: usize) -> Result<()> {
        // Ensure that the data count section, if any, was correct.
        if let Some(data_count) = state.module.data_count {
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

        Ok(())
    }

    fn module_state(&mut self, section: &str, offset: usize) -> Result<&mut ModuleState> {
        match &mut self.state {
            State::Unparsed => Err(BinaryReaderError::new(
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

    fn module_section<T>(
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
