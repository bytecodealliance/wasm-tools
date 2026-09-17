use crate::types::EntityType;
use crate::validator::TypesRef;
use crate::{
    BranchHintSectionReader, CompositeInnerType, FunctionBody, IndirectNameMap, KnownCustom, Name,
    NameMap, NameSectionReader, Operator, Payload, Result, Validator,
};
use alloc::vec;
use alloc::vec::Vec;

/// A validator for the contents of custom sections.
///
/// Custom sections in WebAssembly do not affect whether a module is valid, but
/// it can still be useful for debugging and specification purposes to know and
/// test whether a custom section is valid.
///
/// For known custom sections specified in the WebAssembly specification, such
/// as the `name` custom section or the branch-hint custom section, this
/// validator will ensure that those contents are valid according to the
/// specification.
///
/// This structure is intended to be used in parallel with a [`Validator`].
/// After a [`Payload`] is fed to a [`Validator`] it's then fed into this with
/// [`CustomSectionValidator::payload`]. Function validation then later invokes
/// [`CustomSectionValidator::code_section_entry`] to ensure the validity of
/// custom sections referring to contents of function bodies.
#[derive(Default)]
pub struct CustomSectionValidator {
    states: Vec<State>,
}

#[derive(Default)]
struct State {
    seen_name: bool,
    num_data: u32,
    name_local_max_per_function: Vec<NameMax>,
    params_per_function: Vec<u32>,
    name_label_max_per_function: Vec<NameMax>,
    per_function_hints: Vec<Vec<(u32, u64)>>,
}

#[derive(Copy, Clone)]
enum NameMax {
    None,
    EmptyList {
        offset_specifying_index: u64,
    },
    Index {
        index: u32,
        offset_specifying_index: u64,
    },
}

impl CustomSectionValidator {
    /// Creates a new validator ready for validating a component or a module.
    pub fn new() -> Self {
        Self::default()
    }

    /// Validates the `payload` provided which should already have been
    /// validated with the `validator` provided.
    pub fn payload(&mut self, payload: &Payload<'_>, validator: &Validator) -> Result<()> {
        match payload {
            Payload::Version { .. } => {
                self.states.push(State::default());
            }
            Payload::End(_) => {}
            other => {
                if let Some(state) = self.states.last_mut() {
                    state.payload(other, validator)?;
                }
            }
        }
        Ok(())
    }

    /// Gets an id for the current module to pass to
    /// [`CustomSectionValidator::code_section_entry`] when the function
    /// contents are ready to be validated.
    pub fn current_module_id(&self) -> usize {
        self.states.len() - 1
    }

    /// Validates the contents of `body` w.r.t. custom sections.
    ///
    /// The `module_id` should have been previously acquired from
    /// [`CustomSectionValidator::current_module_id`]. The `func_index` and
    /// `body` are specified relative to that module.
    ///
    /// This function should only be invoked after the entire module has been
    /// processed with [`CustomSectionValidator::payload`] to ensure that all
    /// data is known about all custom sections present.
    pub fn code_section_entry(
        &self,
        module_id: usize,
        func_index: u32,
        body: &FunctionBody<'_>,
    ) -> Result<()> {
        self.states[module_id].code_section_entry(func_index, body)
    }
}

impl State {
    fn payload(&mut self, payload: &Payload<'_>, validator: &Validator) -> Result<()> {
        match payload {
            Payload::CustomSection(s) => self.known_custom_section(s.as_known(), validator)?,
            _ => {
                if let Some((_id, range)) = payload.as_section() {
                    if self.seen_name {
                        bail!(range.start, "name section must come last");
                    }
                }
            }
        }
        Ok(())
    }

    fn known_custom_section(
        &mut self,
        section: KnownCustom<'_>,
        validator: &Validator,
    ) -> Result<()> {
        match section {
            KnownCustom::Name(name) => self.name_section(name, validator),
            KnownCustom::BranchHints(hints) => self.branch_hint_section(hints),
            _ => Ok(()),
        }
    }

    fn name_section(&mut self, names: NameSectionReader<'_>, validator: &Validator) -> Result<()> {
        if self.seen_name {
            bail!(names.sections.range().start, "duplicate name section");
        }
        self.seen_name = true;
        let types = validator.types(0).unwrap();
        for name in names {
            match name? {
                Name::Module { .. } => {}
                Name::Function(s) => {
                    name_map(s, types.function_count(), "func")?;
                }
                Name::Type(s) => {
                    name_map(s, types.core_type_count_in_module(), "type")?;
                }
                Name::Table(s) => {
                    name_map(s, types.table_count(), "table")?;
                }
                Name::Memory(s) => {
                    name_map(s, types.memory_count(), "memory")?;
                }
                Name::Global(s) => {
                    name_map(s, types.global_count(), "global")?;
                }
                Name::Local(s) => {
                    self.name_local_max_per_function =
                        indirect_name_map(s, types.function_count(), "func", "local")?;

                    // Record the number of parameters for all functions to
                    // avoid needing `validator` over in function validation.
                    for i in 0..self.name_local_max_per_function.len() {
                        let i = i as u32;
                        let ty = types.core_function_at(i);
                        let params = match &types[ty].composite_type.inner {
                            CompositeInnerType::Func(ty) => ty.params().len() as u32,
                            _ => 0,
                        };
                        self.params_per_function.push(params);
                    }

                    // Imported functions only have local for their type's parameters,
                    // and defined functions must get validated later.
                    let imported_funcs_to_validate =
                        num_imported_functions(types).min(self.name_local_max_per_function.len());
                    validate_indirect_name_map(
                        &self.name_local_max_per_function[..imported_funcs_to_validate],
                        "local name",
                        |i, _offset| Ok(self.params_per_function[i as usize]),
                    )?;
                }
                Name::Label(s) => {
                    self.name_label_max_per_function =
                        indirect_name_map(s, types.function_count(), "func", "label")?;

                    // Imported functions have no labels, and defined functions
                    // get validated later.
                    let imported_funcs_to_validate =
                        num_imported_functions(types).min(self.name_label_max_per_function.len());
                    validate_indirect_name_map(
                        &self.name_label_max_per_function[..imported_funcs_to_validate],
                        "label name",
                        |_i, _offset| Ok(0),
                    )?;
                }
                Name::Element(s) => {
                    name_map(s, types.element_count(), "element")?;
                }
                Name::Data(s) => {
                    name_map(s, self.num_data, "data")?;
                }
                Name::Field(s) => {
                    let max =
                        indirect_name_map(s, types.core_type_count_in_module(), "type", "field")?;
                    validate_indirect_name_map(&max, "field name", |i, offset| {
                        let ty = types.core_type_at_in_module(i);
                        match &types[ty].composite_type.inner {
                            CompositeInnerType::Struct(ty) => Ok(ty.fields.len() as u32),
                            _ => {
                                bail!(offset, "field name specified when type {i} is not a struct")
                            }
                        }
                    })?;
                }
                Name::Tag(s) => {
                    name_map(s, types.tag_count(), "tag")?;
                }
                Name::Parameter(s) => {
                    let max =
                        indirect_name_map(s, types.core_type_count_in_module(), "type", "param")?;
                    validate_indirect_name_map(&max, "parameter name", |i, offset| {
                        let ty = types.core_type_at_in_module(i);
                        match &types[ty].composite_type.inner {
                            CompositeInnerType::Func(ty) => Ok(ty.params().len() as u32),
                            _ => {
                                bail!(
                                    offset,
                                    "parameter name specified when type {i} is not a func"
                                )
                            }
                        }
                    })?;
                }
                Name::TagParameter(s) => {
                    let max = indirect_name_map(s, types.tag_count(), "tag", "param")?;
                    validate_indirect_name_map(&max, "tag parameter name", |i, offset| {
                        let ty = types.tag_at(i);
                        match &types[ty].composite_type.inner {
                            CompositeInnerType::Func(ty) => Ok(ty.params().len() as u32),
                            _ => bail!(offset, "tag {i} not typed as a func"),
                        }
                    })?;
                }
                Name::Unknown { range, ty, .. } => {
                    bail!(range.start, "unknown name subsection: {ty}");
                }
            }
        }
        Ok(())
    }

    fn branch_hint_section(&mut self, hints: BranchHintSectionReader<'_>) -> Result<()> {
        let mut prev = None;
        for func in hints.into_iter_with_offsets() {
            let (offset, func) = func?;
            if let Some(prev) = prev {
                if prev >= func.func {
                    bail!(offset, "branch hints must be sorted by function index");
                }
            }
            prev = Some(func.func);

            while self.per_function_hints.len() <= func.func as usize {
                self.per_function_hints.push(Vec::new());
            }

            let hints = self.per_function_hints.last_mut().unwrap();
            let mut prev = None;
            for hint in func.hints.into_iter_with_offsets() {
                let (offset, hint) = hint?;
                if let Some(prev) = prev {
                    if prev >= hint.func_offset {
                        bail!(offset, "branch hints must be sorted by function offset");
                    }
                }
                prev = Some(hint.func_offset);
                hints.push((hint.func_offset, offset));
            }
        }
        Ok(())
    }

    fn code_section_entry(&self, func_index: u32, body: &FunctionBody<'_>) -> Result<()> {
        if let Some(max) = self.name_local_max_per_function.get(func_index as usize) {
            let mut locals = self.params_per_function[func_index as usize];
            for cnt in body.get_locals_reader()? {
                locals += cnt?.0;
            }
            if let NameMax::Index {
                index,
                offset_specifying_index,
            } = max
            {
                if *index >= locals {
                    bail!(*offset_specifying_index, "invalid local index {index}");
                }
            }
        }

        let mut hints = self
            .per_function_hints
            .get(func_index as usize)
            .map(|v| v.as_slice())
            .unwrap_or(&[]);

        let mut labels = 0;
        let mut ops = body.get_operators_reader()?;
        while !ops.eof() {
            let cur_func_offset =
                u32::try_from(ops.original_position() - body.range().start).unwrap();
            let branch_hint_offset = match hints.first() {
                Some(&(func_offset, hint_offset)) => {
                    if func_offset < cur_func_offset {
                        None
                    } else if func_offset == cur_func_offset {
                        hints = &hints[1..];
                        Some(hint_offset)
                    } else {
                        bail!(hint_offset, "branch hint for bytes between instructions");
                    }
                }
                None => None,
            };

            let operator = ops.read()?;

            // Keep track of the number of labels in this function to validate
            // the name subsection for labels.
            match &operator {
                Operator::If { .. }
                | Operator::Block { .. }
                | Operator::Loop { .. }
                | Operator::Try { .. }
                | Operator::TryTable { .. } => {
                    labels += 1;
                }
                _ => {}
            }

            // Validate that if this instruction has a branch hint it's allowed
            // to have one.
            match &operator {
                Operator::If { .. } | Operator::BrIf { .. } => {}
                _ => {
                    if let Some(offset) = branch_hint_offset {
                        bail!(offset, "branch hint for instruction that isn't if or br_if")
                    }
                }
            }
        }

        if let Some((_, hint_offset)) = hints.first() {
            bail!(*hint_offset, "branch hint for nonexistent instruction");
        }

        if let Some(max) = self.name_label_max_per_function.get(func_index as usize) {
            if let NameMax::Index {
                index,
                offset_specifying_index,
            } = max
            {
                if *index >= labels {
                    bail!(*offset_specifying_index, "invalid label index {index}");
                }
            }
        }
        Ok(())
    }
}

fn validate_indirect_name_map(
    max: &[NameMax],
    desc: &str,
    mut get_max: impl FnMut(u32, u64) -> Result<u32>,
) -> Result<()> {
    for (i, max) in max.iter().enumerate() {
        let offset = match max {
            NameMax::Index {
                offset_specifying_index,
                ..
            }
            | NameMax::EmptyList {
                offset_specifying_index,
            } => *offset_specifying_index,
            NameMax::None => continue,
        };
        let i = i as u32;
        let max_index = get_max(i, offset)?;
        if let NameMax::Index { index, .. } = max {
            if *index >= max_index {
                bail!(offset, "invalid {desc} index {index}");
            }
        }
    }
    Ok(())
}

fn indirect_name_map(
    mut names: IndirectNameMap<'_>,
    items: u32,
    desc: &str,
    item_desc: &str,
) -> Result<Vec<NameMax>> {
    let mut named = vec![false; items as usize];
    let mut max = vec![NameMax::None; items as usize];
    loop {
        let offset = names.names.original_position();
        let naming = match names.next() {
            Some(naming) => naming?,
            None => break,
        };
        if naming.index >= items {
            bail!(offset, "invalid {desc} naming index {}", naming.index);
        }
        if named[naming.index as usize] {
            bail!(
                offset,
                "invalid {desc} naming index {} named twice",
                naming.index
            );
        }
        named[naming.index as usize] = true;

        max[naming.index as usize] = match name_map(naming.names, u32::MAX, item_desc)? {
            Some((i, offset)) => NameMax::Index {
                index: i,
                offset_specifying_index: offset,
            },
            None => NameMax::EmptyList {
                offset_specifying_index: offset,
            },
        };
    }
    Ok(max)
}

fn name_map(mut names: NameMap<'_>, items: u32, desc: &str) -> Result<Option<(u32, u64)>> {
    let mut named = vec![false; items as usize];
    let mut max = None;
    loop {
        let offset = names.names.original_position();
        let naming = match names.next() {
            Some(naming) => naming?,
            None => break,
        };
        if naming.index >= items {
            bail!(offset, "invalid {desc} naming index {}", naming.index);
        }
        if named[naming.index as usize] {
            bail!(
                offset,
                "invalid {desc} naming index {} named twice",
                naming.index
            );
        }
        named[naming.index as usize] = true;
        max = Some((naming.index, offset));
    }
    Ok(max)
}

fn num_imported_functions(types: TypesRef<'_>) -> usize {
    types
        .core_imports()
        .map(|i| {
            i.filter(|(_, _, i)| matches!(i, EntityType::Func(_)))
                .count()
        })
        .unwrap_or(0)
}
