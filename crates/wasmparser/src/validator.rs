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
    limits::*, BinaryReaderError, Encoding, FunctionBody, Parser, Payload, Range, Result,
    SectionReader, SectionWithLimitedItems, Type, WasmModuleResources, WASM_COMPONENT_VERSION,
    WASM_MODULE_VERSION,
};
use std::mem;

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
fn test_validate() {
    assert!(validate(&[0x0, 0x61, 0x73, 0x6d, 0x1, 0x0, 0x0, 0x0]).is_ok());
    assert!(validate(&[0x0, 0x61, 0x73, 0x6d, 0x2, 0x0, 0x0, 0x0]).is_err());
}

mod component;
mod core;
mod func;

use self::component::*;
use self::core::*;
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
            component_model: false,
            deterministic_only: cfg!(feature = "deterministic"),

            // on-by-default features
            bulk_memory: true,
            multi_value: true,
            reference_types: true,
            simd: true,
        }
    }
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
            ComponentStartSection(s) => self.component_start_section(s)?,
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
        self.ensure_module_section(
            Order::Type,
            section,
            "type",
            |state, count, offset| {
                check_max(
                    state.module.type_count(),
                    count,
                    MAX_WASM_TYPES,
                    "types",
                    offset,
                )
            },
            |state, def, offset| state.module.assert_mut().add_type(def, &features, offset),
        )
    }

    /// Validates [`Payload::ImportSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn import_section(&mut self, section: &crate::ImportSectionReader<'_>) -> Result<()> {
        let features = self.features;
        let has_parent = !self.parents.is_empty();
        self.ensure_module_section(
            Order::Import,
            section,
            "import",
            |_, _, _| Ok(()), // add_import will check limits
            |state, import, offset| {
                state
                    .module
                    .assert_mut()
                    .add_import(import, &features, has_parent, offset)
            },
        )
    }

    /// Validates [`Payload::FunctionSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn function_section(&mut self, section: &crate::FunctionSectionReader<'_>) -> Result<()> {
        self.ensure_module_section(
            Order::Function,
            section,
            "function",
            |state, count, offset| {
                state.set_expected_code_bodies(section.get_count());
                check_max(
                    state.module.function_count(),
                    count,
                    MAX_WASM_FUNCTIONS,
                    "functions",
                    offset,
                )
            },
            |state, ty, offset| state.module.assert_mut().add_function(ty, offset),
        )
    }

    /// Validates [`Payload::TableSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn table_section(&mut self, section: &crate::TableSectionReader<'_>) -> Result<()> {
        let features = self.features;
        self.ensure_module_section(
            Order::Table,
            section,
            "table",
            |state, count, offset| {
                check_max(
                    state.module.table_count(),
                    count,
                    state.module.max_tables(&features),
                    "tables",
                    offset,
                )
            },
            |state, ty, offset| state.module.assert_mut().add_table(ty, &features, offset),
        )
    }

    /// Validates [`Payload::MemorySection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn memory_section(&mut self, section: &crate::MemorySectionReader<'_>) -> Result<()> {
        let features = self.features;
        self.ensure_module_section(
            Order::Memory,
            section,
            "memory",
            |state, count, offset| {
                check_max(
                    state.module.memory_count(),
                    count,
                    state.module.max_memories(&features),
                    "memories",
                    offset,
                )
            },
            |state, ty, offset| state.module.assert_mut().add_memory(ty, &features, offset),
        )
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

        self.ensure_module_section(
            Order::Tag,
            section,
            "tag",
            |state, count, offset| {
                check_max(
                    state.module.tag_count(),
                    count,
                    MAX_WASM_TAGS,
                    "tags",
                    offset,
                )
            },
            |state, ty, offset| state.module.assert_mut().add_tag(ty, &features, offset),
        )
    }

    /// Validates [`Payload::GlobalSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn global_section(&mut self, section: &crate::GlobalSectionReader<'_>) -> Result<()> {
        let features = self.features;
        self.ensure_module_section(
            Order::Global,
            section,
            "global",
            |state, count, offset| {
                check_max(
                    state.module.global_count(),
                    count,
                    MAX_WASM_GLOBALS,
                    "globals",
                    offset,
                )
            },
            |state, global, offset| {
                state
                    .module
                    .assert_mut()
                    .add_global(global, &features, offset)
            },
        )
    }

    /// Validates [`Payload::ExportSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn export_section(&mut self, section: &crate::ExportSectionReader<'_>) -> Result<()> {
        self.ensure_module_section(
            Order::Export,
            section,
            "export",
            |state, count, offset| {
                check_max(
                    state.module.export_count(),
                    count,
                    MAX_WASM_EXPORTS,
                    "exports",
                    offset,
                )
            },
            |state, e, offset| state.module.assert_mut().add_export(&e, offset),
        )
    }

    /// Validates [`Payload::StartSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn start_section(&mut self, func: u32, range: &Range) -> Result<()> {
        let offset = range.start;
        let state = self.module_state("start", offset)?;
        state.update_order(Order::Start, offset)?;

        let ty = state.module.get_func_type(func, offset)?;
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
        self.ensure_module_section(
            Order::Element,
            section,
            "element",
            |state, count, offset| {
                check_max(
                    state.module.element_count() as usize,
                    count,
                    MAX_WASM_ELEMENT_SEGMENTS,
                    "element segments",
                    offset,
                )
            },
            |state, e, offset| {
                state
                    .module
                    .assert_mut()
                    .add_element_segment(e, &features, offset)
            },
        )
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

        state.module.assert_mut().set_data_count(count);
        Ok(())
    }

    /// Validates [`Payload::CodeSectionStart`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn code_section_start(&mut self, count: u32, range: &Range) -> Result<()> {
        let offset = range.start;
        let state = self.module_state("code", offset)?;
        state.update_order(Order::Code, offset)?;

        match state.take_expected_code_bodies() {
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

        Ok(FuncValidator::new(
            state.next_code_entry_type(offset)? as u32,
            0,
            ValidatorResources(state.module.arc().clone()),
            &self.features,
        )
        .unwrap())
    }

    /// Validates [`Payload::DataSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a module.
    pub fn data_section(&mut self, section: &crate::DataSectionReader<'_>) -> Result<()> {
        let mut section = section.clone();
        let count = section.get_count();
        section.forbid_bulk_memory(!self.features.bulk_memory);

        let features = self.features;
        self.ensure_module_section(
            Order::Data,
            &section,
            "data",
            |state, count, offset| {
                check_max(
                    state.module.data_count() as usize,
                    count,
                    MAX_WASM_DATA_SEGMENTS,
                    "data segments",
                    offset,
                )
            },
            |state, d, offset| {
                state.set_data_segment_count(count);
                state.module.check_data_segment(d, &features, offset)
            },
        )
    }

    /// Validates [`Payload::ComponentTypeSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn component_type_section(
        &mut self,
        section: &crate::ComponentTypeSectionReader,
    ) -> Result<()> {
        let features = self.features;
        self.ensure_component_section(
            section,
            "type",
            |state, count, offset| {
                check_max(state.type_count(), count, MAX_WASM_TYPES, "types", offset)
            },
            |state, parents, ty, offset| state.add_type(ty, &features, parents, offset),
        )
    }

    /// Validates [`Payload::ComponentImportSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn component_import_section(
        &mut self,
        section: &crate::ComponentImportSectionReader,
    ) -> Result<()> {
        self.ensure_component_section(
            section,
            "import",
            |_, _, _| Ok(()), // add_import will check limits
            |state, _, import, offset| state.add_import(import, offset),
        )
    }

    /// Validates [`Payload::ComponentFunctionSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn component_function_section(
        &mut self,
        section: &crate::ComponentFunctionSectionReader,
    ) -> Result<()> {
        self.ensure_component_section(
            section,
            "function",
            |state, count, offset| {
                check_max(
                    state.function_count(),
                    count,
                    MAX_WASM_FUNCTIONS,
                    "functions",
                    offset,
                )
            },
            |state, _, func, offset| match func {
                crate::ComponentFunction::Lift {
                    type_index,
                    func_index,
                    options,
                } => state.lift_function(type_index, func_index, options.as_ref(), offset),
                crate::ComponentFunction::Lower {
                    func_index,
                    options,
                } => state.lower_function(func_index, options.as_ref(), offset),
            },
        )
    }

    /// Validates [`Payload::ModuleSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn module_section(&mut self, range: &Range) -> Result<()> {
        {
            let (state, _) = self.component_state("module", range.start)?;
            check_max(
                state.module_count(),
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
                state.component_count(),
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
        self.ensure_component_section(
            section,
            "instance",
            |state, count, offset| {
                check_max(
                    state.instance_count(),
                    count,
                    MAX_WASM_INSTANCES,
                    "instances",
                    offset,
                )
            },
            |state, _, instance, offset| state.add_instance(instance, offset),
        )
    }

    /// Validates [`Payload::ComponentExportSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn component_export_section(
        &mut self,
        section: &crate::ComponentExportSectionReader,
    ) -> Result<()> {
        self.ensure_component_section(
            section,
            "export",
            |state, count, offset| {
                check_max(
                    state.export_count(),
                    count,
                    MAX_WASM_EXPORTS,
                    "exports",
                    offset,
                )
            },
            |state, _, export, offset| state.add_export(export, offset),
        )
    }

    /// Validates [`Payload::ComponentStartSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn component_start_section(
        &mut self,
        section: &crate::ComponentStartSectionReader,
    ) -> Result<()> {
        let range = section.range();
        let (state, _) = self.component_state("start", range.start)?;
        let f = section.clone().read()?;
        state.add_start(f.func_index, &f.arguments, range.start)
    }

    /// Validates [`Payload::AliasSection`](crate::Payload).
    ///
    /// This method should only be called when parsing a component.
    pub fn alias_section(&mut self, section: &crate::AliasSectionReader) -> Result<()> {
        self.ensure_component_section(
            section,
            "alias",
            |_, _, _| Ok(()), // maximums checked via `add_alias`
            |state, parents, alias, offset| -> Result<(), BinaryReaderError> {
                state.add_alias(alias, parents, offset)
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
            State::Unparsed(_) => {
                return Err(BinaryReaderError::new(
                    "cannot call `end` before a header has been parsed",
                    offset,
                ))
            }
            State::Module(state) => {
                state.validate_end(offset)?;

                // If there's a parent component, we'll add a module to the parent state
                if let Some(mut parent) = self.parents.pop() {
                    parent.add_module(&state.module)?;
                    self.state = State::Component(Box::new(parent));
                }
            }
            State::Component(state) => {
                // If there's a parent component, we'll add a component to the parent state
                if let Some(mut parent) = self.parents.pop() {
                    parent.add_component(*state);
                    self.state = State::Component(Box::new(parent));
                }
            }
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
        validate_section: impl FnOnce(&mut ModuleState, u32, usize) -> Result<()>,
        mut validate_item: impl FnMut(&mut ModuleState, T::Item, usize) -> Result<()>,
    ) -> Result<()>
    where
        T: SectionReader + Clone + SectionWithLimitedItems,
    {
        let offset = section.range().start;
        let state = self.module_state(name, offset)?;

        state.update_order(order, offset)?;

        validate_section(state, section.get_count(), offset)?;

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
        validate_section: impl FnOnce(&mut ComponentState, u32, usize) -> Result<()>,
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
        validate_section(state, section.get_count(), offset)?;

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
