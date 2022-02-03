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
use std::mem;
use std::sync::Arc;

/// Test whether the given buffer contains a valid WebAssembly module,
/// analogous to [`WebAssembly.validate`][js] in the JS API.
///
/// This functions requires the wasm module is entirely resident in memory and
/// is specified by `bytes`. Additionally this validates the given bytes with
/// the default set of WebAssembly features implemented by `wasmparser`.
///
/// For more fine-tuned control over validation it's recommended to review the
/// documentation of [`Validator`].
///
/// [js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/validate
pub fn validate(bytes: &[u8]) -> Result<()> {
    Validator::new().validate_all(bytes)
}

#[test]
fn test_validate() {
    assert!(validate(&[0x0, 0x61, 0x73, 0x6d, 0x1, 0x0, 0x0, 0x0]).is_ok());
    assert!(validate(&[0x0, 0x61, 0x73, 0x6d, 0x2, 0x0, 0x0, 0x0]).is_err());
}

mod func;
pub use func::FuncValidator;

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
    /// The current module that we're validating.
    cur: Module,

    /// This is the global list of all types shared by this validator which all
    /// modules will reference.
    types: SnapshotList<TypeDef>,

    /// Enabled WebAssembly feature flags, dictating what's valid and what
    /// isn't.
    features: WasmFeatures,

    /// The current byte-level offset in the wasm binary. This is updated to
    /// produce error messages in `create_error`.
    offset: usize,
}

struct Module {
    /// The module's encoding based upon the parsed file header.
    encoding: Encoding,

    /// Internal state that is incrementally built-up for the module being
    /// validated. This houses type information for all wasm items, like
    /// functions. Note that this starts out as a solely owned `Arc<T>` so we can
    /// get mutable access, but after we get to the code section this is never
    /// mutated to we can clone it cheaply and hand it to sub-validators.
    state: arc::MaybeOwned<ModuleState>,

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
    code_section_index: usize,
}

impl Default for Module {
    fn default() -> Self {
        Self {
            encoding: Encoding::Module,
            state: Default::default(),
            order: Default::default(),
            data_found: Default::default(),
            expected_code_bodies: Default::default(),
            code_section_index: Default::default(),
        }
    }
}

#[derive(Default)]
struct ModuleState {
    /// This is a snapshot of `validator.types` when it is created. This is not
    /// initially filled in but once everything in a module except the code
    /// section has been parsed then this will be filled in.
    ///
    /// Note that this `ModuleState` will be a separately-owned structure living
    /// in each function's validator. This is done to allow parallel validation
    /// of functions while the main module is possibly still being parsed.
    all_types: Option<Arc<SnapshotList<TypeDef>>>,

    types: Vec<usize>, // pointer into `validator.types`
    tables: Vec<TableType>,
    memories: Vec<MemoryType>,
    globals: Vec<GlobalType>,
    num_imported_globals: u32,
    element_types: Vec<Type>,
    data_count: Option<u32>,
    code_type_indexes: Vec<u32>, // pointer into `types` above
    func_types: Vec<usize>,      // pointer into `validator.types`
    tags: Vec<usize>,            // pointer into `validator.types`
    function_references: HashSet<u32>,
    exports: HashMap<String, EntityType>,
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

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
enum Order {
    Initial,
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
        Order::Initial
    }
}

enum TypeDef {
    Func(FuncType),
}

impl TypeDef {
    fn unwrap_func(&self) -> &FuncType {
        match self {
            TypeDef::Func(f) => f,
        }
    }
}

#[derive(Clone)]
enum EntityType {
    Global(GlobalType),
    Memory(MemoryType),
    Table(TableType),
    Func(usize), // pointer into `validator.types`
    Tag(usize),  // pointer into `validator.types`
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
                let func_validator = self.code_section_entry()?;
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

            End => self.end()?,

            CustomSection { .. } => {} // no validation for custom sections
            UnknownSection { id, range, .. } => self.unknown_section(*id, range)?,
        }
        Ok(ValidPayload::Ok)
    }

    fn create_error<T>(&self, msg: impl Into<String>) -> Result<T> {
        Err(BinaryReaderError::new(msg.into(), self.offset))
    }

    /// Validates [`Payload::Version`](crate::Payload).
    pub fn version(&mut self, num: u32, encoding: Encoding, range: &Range) -> Result<()> {
        if self.cur.order != Order::Initial {
            return self.create_error("wasm version header out of order");
        }

        match (encoding, num) {
            (Encoding::Module, WASM_MODULE_VERSION) => {}
            (Encoding::Component, WASM_COMPONENT_VERSION) => {
                if !self.features.component_model {
                    return self.create_error("WebAssembly component model feature not enabled");
                }
            }
            _ => {
                return self.create_error("unsupported wasm header version");
            }
        }

        self.offset = range.start;
        self.cur.encoding = encoding;
        self.cur.order = Order::AfterHeader;

        Ok(())
    }

    /// Validates [`Payload::TypeSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn type_section(&mut self, section: &crate::TypeSectionReader<'_>) -> Result<()> {
        self.check_max(
            self.cur.state.types.len(),
            section.get_count(),
            MAX_WASM_TYPES,
            "types",
        )?;
        self.section(self.header_order(Order::Type), section, |me, item| {
            me.type_def(item)
        })
    }

    /// Validates [`Payload::ImportSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn import_section(&mut self, section: &crate::ImportSectionReader<'_>) -> Result<()> {
        self.section(self.header_order(Order::Import), section, |me, item| {
            me.import(item)
        })?;
        Ok(())
    }

    /// Validates [`Payload::FunctionSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn function_section(&mut self, section: &crate::FunctionSectionReader<'_>) -> Result<()> {
        self.cur.expected_code_bodies = Some(section.get_count());
        self.check_max(
            self.cur.state.func_types.len(),
            section.get_count(),
            MAX_WASM_FUNCTIONS,
            "funcs",
        )?;
        // Assert that each type index is indeed a function type, and otherwise
        // just push it for handling later.
        self.section(self.header_order(Order::Function), section, |me, func| {
            me.func_type_at(func)?;
            let state = me.cur.state.assert_mut();
            state.func_types.push(state.types[func as usize]);
            state.code_type_indexes.push(func);

            // TODO: further validation of canonical functions
            Ok(())
        })
    }

    /// Validates [`Payload::TableSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn table_section(&mut self, section: &crate::TableSectionReader<'_>) -> Result<()> {
        self.check_module_section("table")?;
        self.check_max(
            self.cur.state.tables.len(),
            section.get_count(),
            self.max_tables(),
            "tables",
        )?;
        self.section(self.header_order(Order::Table), section, |me, ty| {
            me.table_type(&ty)?;
            me.cur.state.assert_mut().tables.push(ty);
            Ok(())
        })
    }

    /// Validates [`Payload::MemorySection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn memory_section(&mut self, section: &crate::MemorySectionReader<'_>) -> Result<()> {
        self.check_module_section("memory")?;
        self.check_max(
            self.cur.state.memories.len(),
            section.get_count(),
            self.max_memories(),
            "memories",
        )?;
        self.section(self.header_order(Order::Memory), section, |me, ty| {
            me.memory_type(&ty)?;
            me.cur.state.assert_mut().memories.push(ty);
            Ok(())
        })
    }

    /// Validates [`Payload::TagSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn tag_section(&mut self, section: &crate::TagSectionReader<'_>) -> Result<()> {
        self.check_module_section("tag")?;
        if !self.features.exceptions {
            return self.create_error("exceptions proposal not enabled");
        }
        self.check_max(
            self.cur.state.tags.len(),
            section.get_count(),
            MAX_WASM_TAGS,
            "tags",
        )?;
        self.section(self.header_order(Order::Tag), section, |me, ty| {
            me.tag_type(&ty)?;
            let state = me.cur.state.assert_mut();
            state.tags.push(state.types[ty.func_type_idx as usize]);
            Ok(())
        })
    }

    /// Validates [`Payload::GlobalSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn global_section(&mut self, section: &crate::GlobalSectionReader<'_>) -> Result<()> {
        self.check_module_section("global")?;
        self.check_max(
            self.cur.state.globals.len(),
            section.get_count(),
            MAX_WASM_GLOBALS,
            "globals",
        )?;
        self.section(self.header_order(Order::Global), section, |me, g| {
            me.global_type(&g.ty)?;
            me.init_expr(&g.init_expr, g.ty.content_type)?;
            me.cur.state.assert_mut().globals.push(g.ty);
            Ok(())
        })
    }

    /// Validates [`Payload::ExportSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn export_section(&mut self, section: &crate::ExportSectionReader<'_>) -> Result<()> {
        self.section(self.header_order(Order::Export), section, |me, e| {
            let ty = me.check_export_kind("exported", e.kind, e.index)?;
            let state = me.cur.state.assert_mut();
            match state.exports.insert(e.name.to_string(), ty) {
                Some(_) => Err(BinaryReaderError::new(
                    format!("duplicate export name `{}` already defined", e.name),
                    me.offset,
                )),
                None => Ok(()),
            }
        })
    }

    /// Validates [`Payload::StartSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn start_section(&mut self, func: u32, range: &Range) -> Result<()> {
        self.check_module_section("start")?;
        self.offset = range.start;
        self.update_order(Some(Order::Start))?;
        let ty = self.get_func_type(func)?;
        if !ty.params.is_empty() || !ty.returns.is_empty() {
            return self.create_error("invalid start function type");
        }
        Ok(())
    }

    /// Validates [`Payload::ElementSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn element_section(&mut self, section: &crate::ElementSectionReader<'_>) -> Result<()> {
        self.check_module_section("element")?;
        self.section(self.header_order(Order::Element), section, |me, e| {
            match e.ty {
                Type::FuncRef => {}
                Type::ExternRef if me.features.reference_types => {}
                Type::ExternRef => {
                    return me.create_error(
                        "reference types must be enabled for externref elem segment",
                    );
                }
                _ => return me.create_error("invalid reference type"),
            }
            match e.kind {
                ElementKind::Active {
                    table_index,
                    init_expr,
                } => {
                    let table = me.get_table(table_index)?;
                    if e.ty != table.element_type {
                        return me.create_error("element_type != table type");
                    }
                    me.init_expr(&init_expr, Type::I32)?;
                }
                ElementKind::Passive | ElementKind::Declared => {
                    if !me.features.bulk_memory {
                        return me.create_error("bulk memory must be enabled");
                    }
                }
            }
            let mut items = e.items.get_items_reader()?;
            if items.get_count() > MAX_WASM_TABLE_ENTRIES as u32 {
                return me.create_error("num_elements is out of bounds");
            }
            for _ in 0..items.get_count() {
                me.offset = items.original_position();
                match items.read()? {
                    ElementItem::Expr(expr) => {
                        me.init_expr(&expr, e.ty)?;
                    }
                    ElementItem::Func(f) => {
                        if e.ty != Type::FuncRef {
                            return me
                                .create_error("type mismatch: segment does not have funcref type");
                        }
                        me.get_func_type(f)?;
                        me.cur.state.assert_mut().function_references.insert(f);
                    }
                }
            }

            me.cur.state.assert_mut().element_types.push(e.ty);
            Ok(())
        })
    }

    /// Validates [`Payload::DataCountSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn data_count_section(&mut self, count: u32, range: &Range) -> Result<()> {
        self.check_module_section("data count")?;
        self.offset = range.start;
        self.update_order(Some(Order::DataCount))?;
        self.cur.state.assert_mut().data_count = Some(count);
        if count > MAX_WASM_DATA_SEGMENTS as u32 {
            return self.create_error("data count section specifies too many data segments");
        }
        Ok(())
    }

    /// Validates [`Payload::CodeSectionStart`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn code_section_start(&mut self, count: u32, range: &Range) -> Result<()> {
        self.check_module_section("code")?;
        self.offset = range.start;
        self.update_order(Some(Order::Code))?;
        match self.cur.expected_code_bodies.take() {
            Some(n) if n == count => {}
            Some(_) => {
                return self.create_error("function and code section have inconsistent lengths");
            }
            // empty code sections are allowed even if the function section is
            // missing
            None if count == 0 => {}
            None => return self.create_error("code section without function section"),
        }

        // Prepare our module's view into the global `types` array. This enables
        // parallel function validation to accesss our built-so-far list off
        // types. Note that all the `WasmModuleResources` methods rely on
        // `all_types` being filled in, and this is the point at which they're
        // filled in.
        let types = self.types.commit();
        self.cur.state.assert_mut().all_types = Some(Arc::new(types));

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
    pub fn code_section_entry(&mut self) -> Result<FuncValidator<ValidatorResources>> {
        self.check_module_section("code")?;
        let ty = self.cur.state.code_type_indexes[self.cur.code_section_index];
        self.cur.code_section_index += 1;
        let resources = ValidatorResources(self.cur.state.arc().clone());
        Ok(FuncValidator::new(ty, 0, resources, &self.features).unwrap())
    }

    /// Validates [`Payload::DataSection`](crate::Payload).
    pub fn data_section(&mut self, section: &crate::DataSectionReader<'_>) -> Result<()> {
        self.check_module_section("data")?;
        self.cur.data_found = section.get_count();
        self.check_max(0, section.get_count(), MAX_WASM_DATA_SEGMENTS, "segments")?;
        let mut section = section.clone();
        section.forbid_bulk_memory(!self.features.bulk_memory);
        self.section(self.header_order(Order::Data), &section, |me, d| {
            match d.kind {
                DataKind::Passive => {}
                DataKind::Active {
                    memory_index,
                    init_expr,
                } => {
                    let ty = me.get_memory(memory_index)?.index_type();
                    me.init_expr(&init_expr, ty)?;
                }
            }
            Ok(())
        })
    }

    /// Validates [`Payload::UnknownSection`](crate::Payload).
    ///
    /// Currently always returns an error.
    pub fn unknown_section(&mut self, id: u8, range: &Range) -> Result<()> {
        self.offset = range.start;
        self.create_error(format!("invalid section code: {}", id))
    }

    /// Validates [`Payload::End`](crate::Payload).
    pub fn end(&mut self) -> Result<()> {
        // Ensure that the data count section, if any, was correct.
        if let Some(data_count) = self.cur.state.data_count {
            if data_count != self.cur.data_found {
                return self.create_error("data count section and passive data mismatch");
            }
        }
        // Ensure that the function section, if nonzero, was paired with a code
        // section with the appropriate length.
        if let Some(n) = self.cur.expected_code_bodies.take() {
            if n > 0 {
                return self.create_error("function and code sections have inconsistent lengths");
            }
        }

        Ok(())
    }

    fn header_order(&self, order: Order) -> Option<Order> {
        if self.cur.encoding == Encoding::Component {
            None
        } else {
            Some(order)
        }
    }

    fn update_order(&mut self, order: Option<Order>) -> Result<()> {
        match order {
            Some(order) => {
                assert_eq!(self.cur.encoding, Encoding::Module);

                let prev = mem::replace(&mut self.cur.order, order);

                // For modules, validate that the sections are in order
                if prev >= order {
                    return self.create_error("section out of order");
                }
            }
            None => {
                assert_eq!(self.cur.encoding, Encoding::Component);
            }
        }

        Ok(())
    }

    fn get_type(&self, idx: u32) -> Result<&TypeDef> {
        match self.cur.state.types.get(idx as usize) {
            Some(t) => Ok(&self.types[*t]),
            None => self.create_error(format!("unknown type {}: type index out of bounds", idx)),
        }
    }

    fn get_table(&self, idx: u32) -> Result<&TableType> {
        match self.cur.state.tables.get(idx as usize) {
            Some(t) => Ok(t),
            None => self.create_error(format!("unknown table {}: table index out of bounds", idx)),
        }
    }

    fn get_memory(&self, idx: u32) -> Result<&MemoryType> {
        match self.cur.state.memories.get(idx as usize) {
            Some(t) => Ok(t),
            None => self.create_error(format!(
                "unknown memory {}: memory index out of bounds",
                idx,
            )),
        }
    }

    fn get_global(&self, idx: u32) -> Result<&GlobalType> {
        match self.cur.state.globals.get(idx as usize) {
            Some(t) => Ok(t),
            None => self.create_error(format!(
                "unknown global {}: global index out of bounds",
                idx,
            )),
        }
    }

    fn get_func_type(&self, func_idx: u32) -> Result<&FuncType> {
        match self.cur.state.func_types.get(func_idx as usize) {
            Some(t) => Ok(self.types[*t].unwrap_func()),
            None => self.create_error(format!(
                "unknown function {}: func index out of bounds",
                func_idx,
            )),
        }
    }

    fn func_type_at(&self, type_index: u32) -> Result<&FuncType> {
        let def = self.get_type(type_index)?;
        match def {
            TypeDef::Func(item) => Ok(item),
        }
    }

    fn check_module_section(&self, kind: &str) -> Result<()> {
        if self.cur.encoding != Encoding::Module {
            return self.create_error(format!(
                "{} sections are not supported when parsing WebAssembly modules",
                kind
            ));
        }

        Ok(())
    }

    fn check_component_section(&self, kind: &str) -> Result<()> {
        if self.cur.encoding != Encoding::Component {
            return self.create_error(format!(
                "{} sections are not supported when parsing WebAssembly components ",
                kind
            ));
        }

        Ok(())
    }

    fn check_max(&self, cur_len: usize, amt_added: u32, max: usize, desc: &str) -> Result<()> {
        if max
            .checked_sub(cur_len)
            .and_then(|amt| amt.checked_sub(amt_added as usize))
            .is_none()
        {
            if max == 1 {
                return self.create_error(format!("multiple {}", desc));
            }

            return self.create_error(format!("{} count exceeds limit of {}", desc, max));
        }

        Ok(())
    }

    fn section<T>(
        &mut self,
        order: Option<Order>,
        section: &T,
        mut validate_item: impl FnMut(&mut Self, T::Item) -> Result<()>,
    ) -> Result<()>
    where
        T: SectionReader + Clone + SectionWithLimitedItems,
    {
        self.offset = section.range().start;
        self.update_order(order)?;
        let mut section = section.clone();
        for _ in 0..section.get_count() {
            self.offset = section.original_position();
            let item = section.read()?;
            validate_item(self, item)?;
        }
        self.offset = section.range().end;
        section.ensure_end()?;
        Ok(())
    }

    fn type_def(&mut self, def: crate::TypeDef) -> Result<()> {
        let def = match def {
            crate::TypeDef::Func(t) => {
                for ty in t.params.iter().chain(t.returns.iter()) {
                    self.value_type(*ty)?;
                }
                if t.returns.len() > 1 && !self.features.multi_value {
                    return self
                        .create_error("invalid result arity: func type returns multiple values");
                }
                TypeDef::Func(t)
            }
        };
        self.cur.state.assert_mut().types.push(self.types.len());
        self.types.push(def);
        Ok(())
    }

    fn value_type(&self, ty: Type) -> Result<()> {
        match self.features.check_value_type(ty) {
            Ok(()) => Ok(()),
            Err(e) => self.create_error(e),
        }
    }

    fn import_entry_type(&self, import_type: &TypeRef) -> Result<EntityType> {
        match import_type {
            TypeRef::Func(type_index) => {
                self.func_type_at(*type_index)?;
                Ok(EntityType::Func(self.cur.state.types[*type_index as usize]))
            }
            TypeRef::Table(t) => {
                self.table_type(t)?;
                Ok(EntityType::Table(*t))
            }
            TypeRef::Memory(t) => {
                self.memory_type(t)?;
                Ok(EntityType::Memory(*t))
            }
            TypeRef::Tag(t) => {
                self.tag_type(t)?;
                Ok(EntityType::Tag(
                    self.cur.state.types[t.func_type_idx as usize],
                ))
            }
            TypeRef::Global(t) => {
                self.global_type(t)?;
                Ok(EntityType::Global(*t))
            }
        }
    }

    fn table_type(&self, ty: &TableType) -> Result<()> {
        match ty.element_type {
            Type::FuncRef => {}
            Type::ExternRef => {
                if !self.features.reference_types {
                    return self.create_error("element is not anyfunc");
                }
            }
            _ => return self.create_error("element is not reference type"),
        }
        self.limits(ty.initial, ty.maximum)?;
        if ty.initial > MAX_WASM_TABLE_ENTRIES as u32 {
            return self.create_error("minimum table size is out of bounds");
        }
        Ok(())
    }

    fn memory_type(&self, ty: &MemoryType) -> Result<()> {
        self.limits(ty.initial, ty.maximum)?;
        let (true_maximum, err) = if ty.memory64 {
            if !self.features.memory64 {
                return self.create_error("memory64 must be enabled for 64-bit memories");
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
            return self.create_error(err);
        }
        if let Some(maximum) = ty.maximum {
            if maximum > true_maximum {
                return self.create_error(err);
            }
        }
        if ty.shared {
            if !self.features.threads {
                return self.create_error("threads must be enabled for shared memories");
            }
            if ty.maximum.is_none() {
                return self.create_error("shared memory must have maximum size");
            }
        }
        Ok(())
    }

    fn tag_type(&self, ty: &TagType) -> Result<()> {
        if !self.features.exceptions {
            return self.create_error("exceptions proposal not enabled");
        }
        let ty = self.func_type_at(ty.func_type_idx)?;
        if ty.returns.len() > 0 {
            return self.create_error("invalid exception type: non-empty tag result type");
        }
        Ok(())
    }

    fn global_type(&self, ty: &GlobalType) -> Result<()> {
        self.value_type(ty.content_type)
    }

    fn limits<T>(&self, initial: T, maximum: Option<T>) -> Result<()>
    where
        T: Into<u64>,
    {
        if let Some(max) = maximum {
            if initial.into() > max.into() {
                return self.create_error("size minimum must not be greater than maximum");
            }
        }
        Ok(())
    }

    fn import(&mut self, entry: Import<'_>) -> Result<()> {
        // if self.cur.encoding == Encoding::Module && entry.module.is_none() {
        //     return self.create_error("modules must have two-level imports");
        // }
        // if self.cur.encoding == Encoding::Component && entry.module.is_some() {
        //     return self.create_error("components may not have two-level imports");
        // }

        self.import_entry_type(&entry.ty)?;
        let state = self.cur.state.assert_mut();
        let (len, max, desc) = match entry.ty {
            TypeRef::Func(type_index) => {
                let ty = state.types[type_index as usize];
                state.func_types.push(ty);
                (state.func_types.len(), MAX_WASM_FUNCTIONS, "funcs")
            }
            TypeRef::Table(ty) => {
                state.tables.push(ty);
                (state.tables.len(), self.max_tables(), "tables")
            }
            TypeRef::Memory(ty) => {
                state.memories.push(ty);
                (state.memories.len(), self.max_memories(), "memories")
            }
            TypeRef::Tag(ty) => {
                let ty = state.types[ty.func_type_idx as usize];
                state.tags.push(ty);
                (state.tags.len(), MAX_WASM_TAGS, "tags")
            }
            TypeRef::Global(ty) => {
                state.globals.push(ty);
                state.num_imported_globals += 1;
                (state.globals.len(), MAX_WASM_GLOBALS, "globals")
            }
        };
        self.check_max(len, 0, max, desc)?;
        Ok(())
    }

    fn max_tables(&self) -> usize {
        if self.features.reference_types {
            MAX_WASM_TABLES
        } else {
            1
        }
    }

    fn max_memories(&self) -> usize {
        if self.features.multi_memory {
            MAX_WASM_MEMORIES
        } else {
            1
        }
    }

    fn init_expr(&mut self, expr: &InitExpr<'_>, expected_ty: Type) -> Result<()> {
        let mut ops = expr.get_operators_reader();
        let mut validator = OperatorValidator::new_init_expr(&self.features, expected_ty);
        let mut uninserted_function_references = false;

        while !ops.eof() {
            let offset = ops.original_position();
            self.offset = offset;
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
                    if self.features.extended_const => {}

                // `global.get` is a valid const expression for imported, immutable globals.
                Operator::GlobalGet { global_index } => {
                    let global = self.get_global(*global_index)?;
                    if *global_index >= self.cur.state.num_imported_globals {
                        return self.create_error(
                            "constant expression required: global.get of locally defined global",
                        );
                    }
                    if global.mutable {
                        return self.create_error(
                            "constant expression required: global.get of mutable global",
                        );
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
                    if self.cur.order <= Order::Code {
                        self.cur
                            .state
                            .assert_mut()
                            .function_references
                            .insert(*function_index);
                    } else {
                        uninserted_function_references = true;
                    }
                }
                _ => {
                    return self
                        .create_error("constant expression required: invalid init_expr operator")
                }
            }
            validator
                .process_operator(&op, self)
                .map_err(|e| e.set_offset(offset))?;
        }
        validator.finish().map_err(|e| e.set_offset(self.offset))?;

        // See comment in `RefFunc` above for why this is an assert.
        assert!(!uninserted_function_references);

        Ok(())
    }

    fn check_export_kind(
        &mut self,
        desc: &str,
        kind: ExternalKind,
        index: u32,
    ) -> Result<EntityType> {
        let check = |ty: &str, index: u32, total: usize| {
            if index as usize >= total {
                self.create_error(&format!(
                    "unknown {ty} {index}: {desc} {ty} index out of bounds",
                    desc = desc,
                    index = index,
                    ty = ty,
                ))
            } else {
                Ok(())
            }
        };
        Ok(match kind {
            ExternalKind::Func => {
                check("function", index, self.cur.state.func_types.len())?;
                self.cur
                    .state
                    .assert_mut()
                    .function_references
                    .insert(index);
                EntityType::Func(self.cur.state.func_types[index as usize])
            }
            ExternalKind::Table => {
                check("table", index, self.cur.state.tables.len())?;
                EntityType::Table(self.cur.state.tables[index as usize])
            }
            ExternalKind::Memory => {
                check("memory", index, self.cur.state.memories.len())?;
                EntityType::Memory(self.cur.state.memories[index as usize])
            }
            ExternalKind::Global => {
                check("global", index, self.cur.state.globals.len())?;
                EntityType::Global(self.cur.state.globals[index as usize])
            }
            ExternalKind::Tag => {
                check("tag", index, self.cur.state.tags.len())?;
                EntityType::Tag(self.cur.state.tags[index as usize])
            }
        })
    }
}

impl WasmModuleResources for Validator {
    type FuncType = crate::FuncType;

    fn table_at(&self, at: u32) -> Option<TableType> {
        self.cur.state.table_at(at)
    }

    fn memory_at(&self, at: u32) -> Option<MemoryType> {
        self.cur.state.memory_at(at)
    }

    fn tag_at(&self, at: u32) -> Option<&Self::FuncType> {
        self.cur.state.tag_at(at)
    }

    fn global_at(&self, at: u32) -> Option<GlobalType> {
        self.cur.state.global_at(at)
    }

    fn func_type_at(&self, type_idx: u32) -> Option<&Self::FuncType> {
        self.cur.state.func_type_at(type_idx)
    }

    fn type_of_function(&self, func_idx: u32) -> Option<&Self::FuncType> {
        self.get_func_type(func_idx).ok()
    }

    fn element_type_at(&self, at: u32) -> Option<Type> {
        self.cur.state.element_type_at(at)
    }

    fn element_count(&self) -> u32 {
        self.cur.state.element_count()
    }

    fn data_count(&self) -> u32 {
        self.cur.state.data_count()
    }

    fn is_function_referenced(&self, idx: u32) -> bool {
        self.cur.state.is_function_referenced(idx)
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

impl WasmModuleResources for ModuleState {
    type FuncType = crate::FuncType;

    fn table_at(&self, at: u32) -> Option<TableType> {
        self.tables.get(at as usize).cloned()
    }

    fn memory_at(&self, at: u32) -> Option<MemoryType> {
        self.memories.get(at as usize).cloned()
    }

    fn tag_at(&self, at: u32) -> Option<&Self::FuncType> {
        let types = self.all_types.as_ref().unwrap();
        let i = *self.tags.get(at as usize)?;
        match &types[i] {
            TypeDef::Func(f) => Some(f),
        }
    }

    fn global_at(&self, at: u32) -> Option<GlobalType> {
        self.globals.get(at as usize).cloned()
    }

    fn func_type_at(&self, at: u32) -> Option<&Self::FuncType> {
        let types = self.all_types.as_ref().unwrap();
        let i = *self.types.get(at as usize)?;
        match &types[i] {
            TypeDef::Func(f) => Some(f),
        }
    }

    fn type_of_function(&self, at: u32) -> Option<&Self::FuncType> {
        let types = self.all_types.as_ref().unwrap();
        let i = *self.func_types.get(at as usize)?;
        match &types[i] {
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

/// The implementation of [`WasmModuleResources`] used by [`Validator`].
pub struct ValidatorResources(Arc<ModuleState>);

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

/// This is a type which mirrors a subset of the `Vec<T>` API, but is intended
/// to be able to be cheaply snapshotted and cloned.
///
/// When each module's code sections start we "commit" the current list of types
/// in the global list of types. This means that the temporary `cur` vec here is
/// pushed onto `snapshots` and wrapped up in an `Arc`. At that point we clone
/// this entire list (which is then O(modules), not O(types in all modules)) and
/// pass out as a context to each function validator.
///
/// Otherwise, though, this type behaves as if it were a large `Vec<T>`, but
/// it's represented by lists of contiguous chunks.
struct SnapshotList<T> {
    // All previous snapshots, the "head" of the list that this type represents.
    // The first entry in this pair is the starting index for all elements
    // contained in the list, and the second element is the list itself. Note
    // the `Arc` wrapper around sub-lists, which makes cloning time for this
    // `SnapshotList` O(snapshots) rather than O(snapshots_total), which for
    // us in this context means the number of modules, not types.
    //
    // Note that this list is sorted least-to-greatest in order of the index for
    // binary searching.
    snapshots: Vec<(usize, Arc<Vec<T>>)>,

    // This is the total length of all lists in the `snapshots` array.
    snapshots_total: usize,

    // The current list of types for the current snapshot that are being built.
    cur: Vec<T>,
}

impl<T> SnapshotList<T> {
    /// Same as `<&[T]>::get`
    fn get(&self, index: usize) -> Option<&T> {
        // Check to see if this index falls on our local list
        if index >= self.snapshots_total {
            return self.cur.get(index - self.snapshots_total);
        }
        // ... and failing that we do a binary search to figure out which bucket
        // it's in. Note the `i-1` in the `Err` case because if we don't find an
        // exact match the type is located in the previous bucket.
        let i = match self.snapshots.binary_search_by_key(&index, |(i, _)| *i) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let (len, list) = &self.snapshots[i];
        Some(&list[index - len])
    }

    /// Same as `<&mut [T]>::get_mut`, except only works for indexes into the
    /// current snapshot being built.
    ///
    /// # Panics
    ///
    /// Panics if an index is passed in which falls within the
    /// previously-snapshotted list of types. This should never happen in our
    /// context and the panic is intended to weed out possible bugs in
    /// wasmparser.
    fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index >= self.snapshots_total {
            return self.cur.get_mut(index - self.snapshots_total);
        }
        panic!("cannot get a mutable reference in snapshotted part of list")
    }

    /// Same as `Vec::push`
    fn push(&mut self, val: T) {
        self.cur.push(val);
    }

    /// Same as `<[T]>::len`
    fn len(&self) -> usize {
        self.cur.len() + self.snapshots_total
    }

    /// Commits previously pushed types into this snapshot vector, and returns a
    /// clone of this list.
    ///
    /// The returned `SnapshotList` can be used to access all the same types as
    /// this list itself. This list also is not changed (from an external
    /// perspective) and can continue to access all the same types.
    fn commit(&mut self) -> SnapshotList<T> {
        // If the current chunk has new elements, commit them in to an
        // `Arc`-wrapped vector in the snapshots list. Note the `shrink_to_fit`
        // ahead of time to hopefully keep memory usage lower than it would
        // otherwise be.
        let len = self.cur.len();
        if len > 0 {
            self.cur.shrink_to_fit();
            self.snapshots
                .push((self.snapshots_total, Arc::new(mem::take(&mut self.cur))));
            self.snapshots_total += len;
        }
        SnapshotList {
            snapshots: self.snapshots.clone(),
            snapshots_total: self.snapshots_total,
            cur: Vec::new(),
        }
    }
}

impl<T> std::ops::Index<usize> for SnapshotList<T> {
    type Output = T;

    fn index(&self, index: usize) -> &T {
        self.get(index).unwrap()
    }
}

impl<T> std::ops::IndexMut<usize> for SnapshotList<T> {
    fn index_mut(&mut self, index: usize) -> &mut T {
        self.get_mut(index).unwrap()
    }
}

impl<T> Default for SnapshotList<T> {
    fn default() -> SnapshotList<T> {
        SnapshotList {
            snapshots: Vec::new(),
            snapshots_total: 0,
            cur: Vec::new(),
        }
    }
}
