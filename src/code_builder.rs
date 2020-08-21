use super::{BlockType, FuncType, Import, Instruction, MemArg, Module, ValType};
use arbitrary::{Result, Unstructured};

// The static set of choices of instruction to generate that could be valid at
// some given time. One entry per Wasm instruction.
static CHOICES: &[(
    // Predicate for whether this is a valid choice, if any. None means that the
    // choice is always applicable.
    Option<fn(&Module, &mut CodeBuilder) -> bool>,
    // The function to generate the instruction, given that we've made this choice.
    fn(&mut Unstructured, &Module, &mut CodeBuilder) -> Result<Instruction>,
)] = &[
    // Control instructions.
    (None, unreachable),
    (None, nop),
    (None, block),
    (None, r#loop),
    (Some(if_valid), r#if),
    (Some(else_valid), r#else),
    (Some(end_valid), end),
    (Some(br_valid), br),
    (Some(br_if_valid), br_if),
    (Some(br_table_valid), br_table),
    (Some(return_valid), r#return),
    (Some(call_valid), call),
    (Some(call_indirect_valid), call_indirect),
    // Parametric instructions.
    (Some(drop_valid), drop),
    (Some(select_valid), select),
    // Variable instructions.
    (Some(local_get_valid), local_get),
    (Some(local_set_valid), local_set),
    (Some(local_set_valid), local_tee),
    (Some(global_get_valid), global_get),
    (Some(global_set_valid), global_set),
    // Memory instructions.
    (Some(have_memory_and_offset), i32_load),
    (Some(have_memory_and_offset), i64_load),
    (Some(have_memory_and_offset), f32_load),
    (Some(have_memory_and_offset), f64_load),
    (Some(have_memory_and_offset), i32_load_8_s),
    (Some(have_memory_and_offset), i32_load_8_u),
    (Some(have_memory_and_offset), i32_load_16_s),
    (Some(have_memory_and_offset), i32_load_16_u),
    (Some(have_memory_and_offset), i64_load_8_s),
    (Some(have_memory_and_offset), i64_load_16_s),
    (Some(have_memory_and_offset), i64_load_32_s),
    (Some(have_memory_and_offset), i64_load_8_u),
    (Some(have_memory_and_offset), i64_load_16_u),
    (Some(have_memory_and_offset), i64_load_32_u),
    (Some(i32_store_valid), i32_store),
    (Some(i64_store_valid), i64_store),
    (Some(f32_store_valid), f32_store),
    (Some(f64_store_valid), f64_store),
    (Some(i32_store_valid), i32_store_8),
    (Some(i32_store_valid), i32_store_16),
    (Some(i64_store_valid), i64_store_8),
    (Some(i64_store_valid), i64_store_16),
    (Some(i64_store_valid), i64_store_32),
    (Some(have_memory), memory_size),
    (Some(memory_grow_valid), memory_grow),
    // Numeric instructions.
    (None, i32_const),
    (None, i64_const),
    (None, f32_const),
    (None, f64_const),
    (Some(i32_on_stack), i32_eqz),
    (Some(i32_i32_on_stack), i32_eq),
    (Some(i32_i32_on_stack), i32_neq),
    (Some(i32_i32_on_stack), i32_lt_s),
    (Some(i32_i32_on_stack), i32_lt_u),
    (Some(i32_i32_on_stack), i32_gt_s),
    (Some(i32_i32_on_stack), i32_gt_u),
    (Some(i32_i32_on_stack), i32_le_s),
    (Some(i32_i32_on_stack), i32_le_u),
    (Some(i32_i32_on_stack), i32_ge_s),
    (Some(i32_i32_on_stack), i32_ge_u),
    (Some(i64_on_stack), i64_eqz),
    (Some(i64_i64_on_stack), i64_eq),
    (Some(i64_i64_on_stack), i64_neq),
    (Some(i64_i64_on_stack), i64_lt_s),
    (Some(i64_i64_on_stack), i64_lt_u),
    (Some(i64_i64_on_stack), i64_gt_s),
    (Some(i64_i64_on_stack), i64_gt_u),
    (Some(i64_i64_on_stack), i64_le_s),
    (Some(i64_i64_on_stack), i64_le_u),
    (Some(i64_i64_on_stack), i64_ge_s),
    (Some(i64_i64_on_stack), i64_ge_u),
    (Some(f32_f32_on_stack), f32_eq),
    (Some(f32_f32_on_stack), f32_neq),
    (Some(f32_f32_on_stack), f32_lt),
    (Some(f32_f32_on_stack), f32_gt),
    (Some(f32_f32_on_stack), f32_le),
    (Some(f32_f32_on_stack), f32_ge),
    (Some(f64_f64_on_stack), f64_eq),
    (Some(f64_f64_on_stack), f64_neq),
    (Some(f64_f64_on_stack), f64_lt),
    (Some(f64_f64_on_stack), f64_gt),
    (Some(f64_f64_on_stack), f64_le),
    (Some(f64_f64_on_stack), f64_ge),
    (Some(i32_on_stack), i32_clz),
    (Some(i32_on_stack), i32_ctz),
    (Some(i32_on_stack), i32_popcnt),
    (Some(i32_i32_on_stack), i32_add),
    (Some(i32_i32_on_stack), i32_sub),
    (Some(i32_i32_on_stack), i32_mul),
    (Some(i32_i32_on_stack), i32_div_s),
    (Some(i32_i32_on_stack), i32_div_u),
    (Some(i32_i32_on_stack), i32_rem_s),
    (Some(i32_i32_on_stack), i32_rem_u),
    (Some(i32_i32_on_stack), i32_and),
    (Some(i32_i32_on_stack), i32_or),
    (Some(i32_i32_on_stack), i32_xor),
    (Some(i32_i32_on_stack), i32_shl),
    (Some(i32_i32_on_stack), i32_shr_s),
    (Some(i32_i32_on_stack), i32_shr_u),
    (Some(i32_i32_on_stack), i32_rotl),
    (Some(i32_i32_on_stack), i32_rotr),
    (Some(i64_on_stack), i64_clz),
    (Some(i64_on_stack), i64_ctz),
    (Some(i64_on_stack), i64_popcnt),
    (Some(i64_i64_on_stack), i64_add),
    (Some(i64_i64_on_stack), i64_sub),
    (Some(i64_i64_on_stack), i64_mul),
    (Some(i64_i64_on_stack), i64_div_s),
    (Some(i64_i64_on_stack), i64_div_u),
    (Some(i64_i64_on_stack), i64_rem_s),
    (Some(i64_i64_on_stack), i64_rem_u),
    (Some(i64_i64_on_stack), i64_and),
    (Some(i64_i64_on_stack), i64_or),
    (Some(i64_i64_on_stack), i64_xor),
    (Some(i64_i64_on_stack), i64_shl),
    (Some(i64_i64_on_stack), i64_shr_s),
    (Some(i64_i64_on_stack), i64_shr_u),
    (Some(i64_i64_on_stack), i64_rotl),
    (Some(i64_i64_on_stack), i64_rotr),
    (Some(f32_on_stack), f32_abs),
    (Some(f32_on_stack), f32_neg),
    (Some(f32_on_stack), f32_ceil),
    (Some(f32_on_stack), f32_floor),
    (Some(f32_on_stack), f32_trunc),
    (Some(f32_on_stack), f32_nearest),
    (Some(f32_on_stack), f32_sqrt),
    (Some(f32_f32_on_stack), f32_add),
    (Some(f32_f32_on_stack), f32_sub),
    (Some(f32_f32_on_stack), f32_mul),
    (Some(f32_f32_on_stack), f32_div),
    (Some(f32_f32_on_stack), f32_min),
    (Some(f32_f32_on_stack), f32_max),
    (Some(f32_f32_on_stack), f32_copysign),
    (Some(f64_on_stack), f64_abs),
    (Some(f64_on_stack), f64_neg),
    (Some(f64_on_stack), f64_ceil),
    (Some(f64_on_stack), f64_floor),
    (Some(f64_on_stack), f64_trunc),
    (Some(f64_on_stack), f64_nearest),
    (Some(f64_on_stack), f64_sqrt),
    (Some(f64_f64_on_stack), f64_add),
    (Some(f64_f64_on_stack), f64_sub),
    (Some(f64_f64_on_stack), f64_mul),
    (Some(f64_f64_on_stack), f64_div),
    (Some(f64_f64_on_stack), f64_min),
    (Some(f64_f64_on_stack), f64_max),
    (Some(f64_f64_on_stack), f64_copysign),
    (Some(i64_on_stack), i32_wrap_i64),
    (Some(f32_on_stack), i32_trunc_f32_s),
    (Some(f32_on_stack), i32_trunc_f32_u),
    (Some(f64_on_stack), i32_trunc_f64_s),
    (Some(f64_on_stack), i32_trunc_f64_u),
    (Some(i32_on_stack), i64_extend_i32_s),
    (Some(i32_on_stack), i64_extend_i32_u),
    (Some(f32_on_stack), i64_trunc_f32_s),
    (Some(f32_on_stack), i64_trunc_f32_u),
    (Some(f64_on_stack), i64_trunc_f64_s),
    (Some(f64_on_stack), i64_trunc_f64_u),
    (Some(i32_on_stack), f32_convert_i32_s),
    (Some(i32_on_stack), f32_convert_i32_u),
    (Some(i64_on_stack), f32_convert_i64_s),
    (Some(i64_on_stack), f32_convert_i64_u),
    (Some(f64_on_stack), f32_demote_f64),
    (Some(i32_on_stack), f64_convert_i32_s),
    (Some(i32_on_stack), f64_convert_i32_u),
    (Some(i64_on_stack), f64_convert_i64_s),
    (Some(i64_on_stack), f64_convert_i64_u),
    (Some(f32_on_stack), f64_promote_f32),
    (Some(f32_on_stack), i32_reinterpret_f32),
    (Some(f64_on_stack), i64_reinterpret_f64),
    (Some(i32_on_stack), f32_reinterpret_i32),
    (Some(i64_on_stack), f64_reinterpret_i64),
    (Some(i32_on_stack), i32_extend_8_s),
    (Some(i32_on_stack), i32_extend_16_s),
    (Some(i64_on_stack), i64_extend_8_s),
    (Some(i64_on_stack), i64_extend_16_s),
    (Some(i64_on_stack), i64_extend_32_s),
    (Some(f32_on_stack), i32_trunc_sat_f32_s),
    (Some(f32_on_stack), i32_trunc_sat_f32_u),
    (Some(f64_on_stack), i32_trunc_sat_f64_s),
    (Some(f64_on_stack), i32_trunc_sat_f64_u),
    (Some(f32_on_stack), i64_trunc_sat_f32_s),
    (Some(f32_on_stack), i64_trunc_sat_f32_u),
    (Some(f64_on_stack), i64_trunc_sat_f64_s),
    (Some(f64_on_stack), i64_trunc_sat_f64_u),
];

pub(crate) struct CodeBuilderAllocations {
    // The control labels in scope right now.
    controls: Vec<Control>,

    // The types on the operand stack right now.
    operands: Vec<Option<ValType>>,

    // Dynamic set of choices of instruction we can generate that are known to
    // be valid right now.
    choices: Vec<fn(&mut Unstructured, &Module, &mut CodeBuilder) -> Result<Instruction>>,
}

pub(crate) struct CodeBuilder<'a> {
    func_ty: &'a FuncType,
    locals: &'a Vec<ValType>,
    allocs: &'a mut CodeBuilderAllocations,
}

#[derive(Debug)]
struct Control {
    kind: ControlKind,
    params: Vec<ValType>,
    results: Vec<ValType>,
    height: usize,
}

impl Control {
    fn label_types(&self) -> &[ValType] {
        if self.kind == ControlKind::Loop {
            &self.params
        } else {
            &self.results
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ControlKind {
    Block,
    If,
    Loop,
}

impl Default for CodeBuilderAllocations {
    fn default() -> Self {
        CodeBuilderAllocations {
            controls: Vec::with_capacity(4),
            operands: Vec::with_capacity(16),
            choices: Vec::with_capacity(CHOICES.len()),
        }
    }
}

impl CodeBuilderAllocations {
    pub(crate) fn builder<'a>(
        &'a mut self,
        func_ty: &'a FuncType,
        locals: &'a Vec<ValType>,
    ) -> CodeBuilder<'a> {
        self.controls.clear();
        self.controls.push(Control {
            kind: ControlKind::Block,
            params: vec![],
            results: func_ty.results.clone(),
            height: 0,
        });

        self.operands.clear();
        self.choices.clear();

        CodeBuilder {
            func_ty,
            locals,
            allocs: self,
        }
    }
}

impl<'a> CodeBuilder<'a> {
    /// Get the operands that are in-scope within the current control frame.
    fn operands(&self) -> &[Option<ValType>] {
        let height = self.allocs.controls.last().map_or(0, |c| c.height);
        &self.allocs.operands[height..]
    }

    fn pop_operands(&mut self, to_pop: &[ValType]) {
        debug_assert!(self.types_on_stack(to_pop));
        self.allocs
            .operands
            .truncate(self.allocs.operands.len() - to_pop.len());
    }

    fn push_operands(&mut self, to_push: &[ValType]) {
        self.allocs
            .operands
            .extend(to_push.iter().copied().map(Some));
    }

    fn label_types_on_stack(&self, to_check: &Control) -> bool {
        self.types_on_stack(to_check.label_types())
    }

    fn type_on_stack(&self, ty: ValType) -> bool {
        match self.operands().last() {
            None => false,
            Some(None) => true,
            Some(Some(x)) => *x == ty,
        }
    }

    fn types_on_stack(&self, types: &[ValType]) -> bool {
        self.operands().len() >= types.len()
            && self
                .operands()
                .iter()
                .rev()
                .zip(types.iter().rev())
                .all(|(a, b)| match (a, b) {
                    (None, _) => true,
                    (Some(x), y) => x == y,
                })
    }

    #[inline(never)]
    fn arbitrary_block_type(&self, u: &mut Unstructured, module: &Module) -> Result<BlockType> {
        let mut choices: Vec<Box<dyn Fn(&mut Unstructured) -> Result<BlockType>>> = vec![
            Box::new(|_| Ok(BlockType::Empty)),
            Box::new(|u| Ok(BlockType::Result(u.arbitrary()?))),
        ];

        for (i, ty) in module.types.iter().enumerate() {
            if self.types_on_stack(&ty.params) {
                choices.push(Box::new(move |_| Ok(BlockType::FuncType(i as u32))));
            }
        }

        let f = u.choose(&choices)?;
        f(u)
    }

    pub(crate) fn arbitrary(
        mut self,
        u: &mut Unstructured,
        module: &Module,
    ) -> Result<Vec<Instruction>> {
        const MAX_INSTRUCTIONS: usize = 100;
        let mut instructions = vec![];

        while !self.allocs.controls.is_empty() {
            let keep_going =
                instructions.len() < MAX_INSTRUCTIONS && u.arbitrary().unwrap_or(false);
            if !keep_going {
                while !self.allocs.controls.is_empty() {
                    let num_operands = self.operands().len();
                    let label = self.allocs.controls.pop().unwrap();

                    // If we don't have the right operands on the stack for this
                    // control frame, add an `unreachable`.
                    if label.results.len() != num_operands || !self.types_on_stack(&label.results) {
                        self.allocs.operands.push(None);
                        instructions.push(Instruction::Unreachable);
                    }

                    // If this is an `if` that is not stack neutral, then it
                    // must have an `else`.
                    if label.kind == ControlKind::If && label.params != label.results {
                        instructions.push(Instruction::Else);
                        instructions.push(Instruction::Unreachable);
                    }

                    // The last control frame for the function return does not
                    // need an `end` instruction.
                    if !self.allocs.controls.is_empty() {
                        instructions.push(Instruction::End);
                    }

                    self.allocs.operands.truncate(label.height);
                    self.allocs
                        .operands
                        .extend(label.results.into_iter().map(Some));
                }
                break;
            }

            self.allocs.choices.clear();
            for (is_valid, choice) in CHOICES {
                if is_valid.map_or(true, |f| f(module, &mut self)) {
                    self.allocs.choices.push(*choice);
                }
            }

            let f = u.choose(&self.allocs.choices)?;
            let inst = f(u, module, &mut self)?;
            instructions.push(inst);
        }

        Ok(instructions)
    }
}

fn unreachable(_: &mut Unstructured, _: &Module, _: &mut CodeBuilder) -> Result<Instruction> {
    Ok(Instruction::Unreachable)
}

fn nop(_: &mut Unstructured, _: &Module, _: &mut CodeBuilder) -> Result<Instruction> {
    Ok(Instruction::Nop)
}

fn block(u: &mut Unstructured, module: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let block_ty = builder.arbitrary_block_type(u, module)?;
    let (params, results) = match block_ty {
        BlockType::Empty => (vec![], vec![]),
        BlockType::Result(t) => (vec![], vec![t]),
        BlockType::FuncType(ty) => {
            let ty = &module.types[ty as usize];
            (ty.params.clone(), ty.results.clone())
        }
    };
    let height = builder.allocs.operands.len() - params.len();
    builder.allocs.controls.push(Control {
        kind: ControlKind::Block,
        params,
        results,
        height,
    });
    Ok(Instruction::Block(block_ty))
}

fn r#loop(u: &mut Unstructured, module: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let block_ty = builder.arbitrary_block_type(u, module)?;
    let (params, results) = match block_ty {
        BlockType::Empty => (vec![], vec![]),
        BlockType::Result(t) => (vec![], vec![t]),
        BlockType::FuncType(ty) => {
            let ty = &module.types[ty as usize];
            (ty.params.clone(), ty.results.clone())
        }
    };
    let height = builder.allocs.operands.len() - params.len();
    builder.allocs.controls.push(Control {
        kind: ControlKind::Loop,
        params,
        results,
        height,
    });
    Ok(Instruction::Loop(block_ty))
}

fn if_valid(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder.type_on_stack(ValType::I32)
}

fn r#if(u: &mut Unstructured, module: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);

    let block_ty = builder.arbitrary_block_type(u, module)?;
    let (params, results) = match block_ty {
        BlockType::Empty => (vec![], vec![]),
        BlockType::Result(t) => (vec![], vec![t]),
        BlockType::FuncType(ty) => {
            let ty = &module.types[ty as usize];
            (ty.params.clone(), ty.results.clone())
        }
    };
    let height = builder.allocs.operands.len() - params.len();
    builder.allocs.controls.push(Control {
        kind: ControlKind::If,
        params,
        results,
        height,
    });
    Ok(Instruction::If(block_ty))
}

fn else_valid(_: &Module, builder: &mut CodeBuilder) -> bool {
    let last_control = builder.allocs.controls.last().unwrap();
    last_control.kind == ControlKind::If
        && builder.operands().len() == last_control.results.len()
        && builder.types_on_stack(&last_control.results)
}

fn r#else(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let control = builder.allocs.controls.pop().unwrap();
    builder.pop_operands(&control.results);
    builder.push_operands(&control.params);
    builder.allocs.controls.push(Control {
        kind: ControlKind::Block,
        ..control
    });
    Ok(Instruction::Else)
}

fn end_valid(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder.allocs.controls.len() > 1
        && {
            let last_control = builder.allocs.controls.last().unwrap();
            builder.operands().len() == last_control.results.len()
            && builder.types_on_stack(&last_control.results)
            // `if`s that don't leave the stack as they found it must have an
            // `else`.
            && !(last_control.kind == ControlKind::If && last_control.params != last_control.results)
        }
}

fn end(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.allocs.controls.pop();
    Ok(Instruction::End)
}

fn br_valid(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder
        .allocs
        .controls
        .iter()
        .any(|l| builder.label_types_on_stack(l))
}

fn br(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let n = builder
        .allocs
        .controls
        .iter()
        .filter(|l| builder.label_types_on_stack(l))
        .count();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    let (target, _) = builder
        .allocs
        .controls
        .iter()
        .rev()
        .enumerate()
        .filter(|(_, l)| builder.label_types_on_stack(l))
        .nth(i)
        .unwrap();
    let control = &builder.allocs.controls[builder.allocs.controls.len() - 1 - target];
    let tys = control.label_types().to_vec();
    builder.pop_operands(&tys);
    Ok(Instruction::Br(target as u32))
}

fn br_if_valid(_: &Module, builder: &mut CodeBuilder) -> bool {
    if !builder.type_on_stack(ValType::I32) {
        return false;
    }
    let ty = builder.allocs.operands.pop().unwrap();
    let is_valid = builder
        .allocs
        .controls
        .iter()
        .any(|l| builder.label_types_on_stack(l));
    builder.allocs.operands.push(ty);
    is_valid
}

fn br_if(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);

    let n = builder
        .allocs
        .controls
        .iter()
        .filter(|l| builder.label_types_on_stack(l))
        .count();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    let (target, _) = builder
        .allocs
        .controls
        .iter()
        .rev()
        .enumerate()
        .filter(|(_, l)| builder.label_types_on_stack(l))
        .nth(i)
        .unwrap();
    Ok(Instruction::BrIf(target as u32))
}

fn br_table_valid(module: &Module, builder: &mut CodeBuilder) -> bool {
    if !builder.type_on_stack(ValType::I32) {
        return false;
    }
    let ty = builder.allocs.operands.pop().unwrap();
    let is_valid = br_valid(module, builder);
    builder.allocs.operands.push(ty);
    is_valid
}

fn br_table(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);

    let n = builder
        .allocs
        .controls
        .iter()
        .filter(|l| builder.label_types_on_stack(l))
        .count();
    debug_assert!(n > 0);

    let i = u.int_in_range(0..=n - 1)?;
    let (default_target, _) = builder
        .allocs
        .controls
        .iter()
        .rev()
        .enumerate()
        .filter(|(_, l)| builder.label_types_on_stack(l))
        .nth(i)
        .unwrap();
    let control = &builder.allocs.controls[builder.allocs.controls.len() - 1 - default_target];

    let targets = builder
        .allocs
        .controls
        .iter()
        .rev()
        .enumerate()
        .filter(|(_, l)| l.label_types() == control.label_types())
        .map(|(t, _)| t as u32)
        .collect();

    let tys = control.label_types().to_vec();
    builder.pop_operands(&tys);

    Ok(Instruction::BrTable(targets, default_target as u32))
}

fn return_valid(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder.label_types_on_stack(&builder.allocs.controls[0])
}

fn r#return(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let results = builder.allocs.controls[0].results.clone();
    builder.pop_operands(&results);
    Ok(Instruction::Return)
}

fn call_valid(module: &Module, builder: &mut CodeBuilder) -> bool {
    module
        .imports
        .iter()
        .filter_map(|(_, _, imp)| match imp {
            Import::Func(ty) => Some(ty),
            _ => None,
        })
        .chain(&module.funcs)
        .any(|ty| builder.types_on_stack(&module.types[*ty as usize].params))
}

fn call(u: &mut Unstructured, module: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let n = module
        .imports
        .iter()
        .filter_map(|(_, _, imp)| match imp {
            Import::Func(ty) => Some(ty),
            _ => None,
        })
        .chain(&module.funcs)
        .filter(|ty| builder.types_on_stack(&module.types[**ty as usize].params))
        .count();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    let (func_idx, ty) = module
        .imports
        .iter()
        .filter_map(|(_, _, imp)| match imp {
            Import::Func(ty) => Some(ty),
            _ => None,
        })
        .chain(&module.funcs)
        .enumerate()
        .map(|(idx, ty)| {
            let ty = &module.types[*ty as usize];
            (idx, ty)
        })
        .filter(|(_, ty)| builder.types_on_stack(&ty.params))
        .nth(i)
        .unwrap();
    builder.pop_operands(&ty.params);
    builder.push_operands(&ty.results);
    Ok(Instruction::Call(func_idx as u32))
}

fn call_indirect_valid(module: &Module, builder: &mut CodeBuilder) -> bool {
    if !(module.table.is_some() || module.table_imports() == 1)
        || !builder.type_on_stack(ValType::I32)
    {
        return false;
    }
    let ty = builder.allocs.operands.pop().unwrap();
    let is_valid = module
        .types
        .iter()
        .any(|ty| builder.types_on_stack(&ty.params));
    builder.allocs.operands.push(ty);
    is_valid
}

fn call_indirect(
    u: &mut Unstructured,
    module: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);

    let n = module
        .types
        .iter()
        .filter(|ty| builder.types_on_stack(&ty.params))
        .count();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    let (type_idx, ty) = module
        .types
        .iter()
        .enumerate()
        .filter(|(_, ty)| builder.types_on_stack(&ty.params))
        .nth(i)
        .unwrap();
    builder.pop_operands(&ty.params);
    builder.push_operands(&ty.results);
    Ok(Instruction::CallIndirect(type_idx as u32))
}

fn drop_valid(_: &Module, builder: &mut CodeBuilder) -> bool {
    !builder.operands().is_empty()
}

fn drop(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.allocs.operands.pop();
    Ok(Instruction::Drop)
}

fn select_valid(_: &Module, builder: &mut CodeBuilder) -> bool {
    if !(builder.operands().len() >= 3 && builder.type_on_stack(ValType::I32)) {
        return false;
    }
    let t = builder.operands()[builder.operands().len() - 2];
    let u = builder.operands()[builder.operands().len() - 3];
    t.is_none() || u.is_none() || t == u
}

fn select(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.allocs.operands.pop();
    let t = builder.allocs.operands.pop().unwrap();
    let u = builder.allocs.operands.pop().unwrap();
    builder.allocs.operands.push(t.or(u));
    Ok(Instruction::Select)
}

fn local_get_valid(_: &Module, builder: &mut CodeBuilder) -> bool {
    !builder.func_ty.params.is_empty() || !builder.locals.is_empty()
}

fn local_get(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let num_params = builder.func_ty.params.len();
    let n = num_params + builder.locals.len();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    builder.allocs.operands.push(Some(if i < num_params {
        builder.func_ty.params[i]
    } else {
        builder.locals[i - num_params]
    }));
    Ok(Instruction::LocalGet(i as u32))
}

fn local_set_valid(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder
        .func_ty
        .params
        .iter()
        .chain(builder.locals)
        .any(|ty| builder.type_on_stack(*ty))
}

fn local_set(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let n = builder
        .func_ty
        .params
        .iter()
        .chain(builder.locals)
        .filter(|ty| builder.type_on_stack(**ty))
        .count();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    let (j, _) = builder
        .func_ty
        .params
        .iter()
        .chain(builder.locals)
        .enumerate()
        .filter(|(_, ty)| builder.type_on_stack(**ty))
        .nth(i)
        .unwrap();
    builder.allocs.operands.pop();
    Ok(Instruction::LocalSet(j as u32))
}

fn local_tee(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let n = builder
        .func_ty
        .params
        .iter()
        .chain(builder.locals)
        .filter(|ty| builder.type_on_stack(**ty))
        .count();
    debug_assert!(n > 0);
    let i = u.int_in_range(0..=n - 1)?;
    let (j, _) = builder
        .func_ty
        .params
        .iter()
        .chain(builder.locals)
        .enumerate()
        .filter(|(_, ty)| builder.type_on_stack(**ty))
        .nth(i)
        .unwrap();
    Ok(Instruction::LocalTee(j as u32))
}

fn global_get_valid(module: &Module, _: &mut CodeBuilder) -> bool {
    !module.globals.is_empty() || module.global_imports() > 0
}

fn global_get(
    u: &mut Unstructured,
    module: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let n = module.globals.len() + module.global_imports() as usize;
    debug_assert!(n > 0);
    let mut i = u.int_in_range(0..=n - 1)?;
    let mut global_idx = 0;
    for (_, _, imp) in &module.imports {
        match imp {
            Import::Global(g) => {
                if i == 0 {
                    builder.allocs.operands.push(Some(g.val_type));
                    return Ok(Instruction::GlobalGet(global_idx));
                }
                i -= 1;
                global_idx += 1;
            }
            _ => continue,
        }
    }
    for g in &module.globals {
        if i == 0 {
            builder.allocs.operands.push(Some(g.ty.val_type));
            return Ok(Instruction::GlobalGet(global_idx));
        }
        i -= 1;
        global_idx += 1;
    }
    unreachable!()
}

fn global_set_valid(module: &Module, builder: &mut CodeBuilder) -> bool {
    module
        .globals
        .iter()
        .map(|g| &g.ty)
        .chain(module.imports.iter().filter_map(|(_, _, imp)| match imp {
            Import::Global(g) => Some(g),
            _ => None,
        }))
        .any(|g| g.mutable && builder.type_on_stack(g.val_type))
}

fn global_set(
    u: &mut Unstructured,
    module: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let n = module
        .imports
        .iter()
        .filter_map(|(_, _, imp)| match imp {
            Import::Global(g) => Some(g),
            _ => None,
        })
        .chain(module.globals.iter().map(|g| &g.ty))
        .filter(|g| g.mutable && builder.type_on_stack(g.val_type))
        .count();
    debug_assert!(n > 0);
    let mut i = u.int_in_range(0..=n - 1)?;
    let mut global_idx = 0;
    for (_, _, imp) in &module.imports {
        match imp {
            Import::Global(g) => {
                if g.mutable && builder.type_on_stack(g.val_type) {
                    if i == 0 {
                        builder.allocs.operands.pop();
                        return Ok(Instruction::GlobalSet(global_idx));
                    }
                    i -= 1;
                }
                global_idx += 1;
            }
            _ => continue,
        }
    }
    for g in &module.globals {
        if g.ty.mutable && builder.type_on_stack(g.ty.val_type) {
            if i == 0 {
                builder.allocs.operands.pop();
                return Ok(Instruction::GlobalSet(global_idx));
            }
            i -= 1;
        }
        global_idx += 1;
    }
    unreachable!()
}

fn have_memory(module: &Module, _: &mut CodeBuilder) -> bool {
    module.memory.is_some() || module.memory_imports() > 0
}

fn have_memory_and_offset(module: &Module, builder: &mut CodeBuilder) -> bool {
    have_memory(module, builder) && builder.type_on_stack(ValType::I32)
}

fn i32_load(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1])?;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load(MemArg { offset, align }))
}

fn i64_load(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1, 2])?;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load(MemArg { offset, align }))
}

fn f32_load(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1])?;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::F32));
    Ok(Instruction::F32Load(MemArg { offset, align }))
}

fn f64_load(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1, 2])?;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::F64));
    Ok(Instruction::F64Load(MemArg { offset, align }))
}

fn i32_load_8_s(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = 0;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load8_S(MemArg { offset, align }))
}

fn i32_load_8_u(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = 0;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load8_U(MemArg { offset, align }))
}

fn i32_load_16_s(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1])?;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load16_S(MemArg { offset, align }))
}

fn i32_load_16_u(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1])?;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load16_U(MemArg { offset, align }))
}

fn i64_load_8_s(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = 0;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load8_S(MemArg { offset, align }))
}

fn i64_load_16_s(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1])?;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load16_S(MemArg { offset, align }))
}

fn i64_load_32_s(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1, 2])?;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load32_S(MemArg { offset, align }))
}

fn i64_load_8_u(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = 0;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load8_U(MemArg { offset, align }))
}

fn i64_load_16_u(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1])?;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load16_U(MemArg { offset, align }))
}

fn i64_load_32_u(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1, 2])?;
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load32_U(MemArg { offset, align }))
}

fn store_valid(module: &Module, builder: &mut CodeBuilder, f: impl FnOnce() -> ValType) -> bool {
    have_memory(module, builder) && builder.types_on_stack(&[ValType::I32, f()])
}

fn i32_store_valid(module: &Module, builder: &mut CodeBuilder) -> bool {
    store_valid(module, builder, || ValType::I32)
}

fn i32_store(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1])?;
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    Ok(Instruction::I32Store(MemArg { offset, align }))
}

fn i64_store_valid(module: &Module, builder: &mut CodeBuilder) -> bool {
    store_valid(module, builder, || ValType::I64)
}

fn i64_store(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1, 2])?;
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store(MemArg { offset, align }))
}

fn f32_store_valid(module: &Module, builder: &mut CodeBuilder) -> bool {
    store_valid(module, builder, || ValType::F32)
}

fn f32_store(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1])?;
    builder.pop_operands(&[ValType::I32, ValType::F32]);
    Ok(Instruction::F32Store(MemArg { offset, align }))
}

fn f64_store_valid(module: &Module, builder: &mut CodeBuilder) -> bool {
    store_valid(module, builder, || ValType::F64)
}

fn f64_store(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1, 2])?;
    builder.pop_operands(&[ValType::I32, ValType::F64]);
    Ok(Instruction::F64Store(MemArg { offset, align }))
}

fn i32_store_8(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = 0;
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    Ok(Instruction::I32Store8(MemArg { offset, align }))
}

fn i32_store_16(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1])?;
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    Ok(Instruction::I32Store16(MemArg { offset, align }))
}

fn i64_store_8(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = 0;
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store8(MemArg { offset, align }))
}

fn i64_store_16(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1])?;
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store16(MemArg { offset, align }))
}

fn i64_store_32(
    u: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    let offset = u.arbitrary()?;
    let align = *u.choose(&[0, 1, 2])?;
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store32(MemArg { offset, align }))
}

fn memory_size(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::MemorySize)
}

fn memory_grow_valid(module: &Module, builder: &mut CodeBuilder) -> bool {
    have_memory(module, builder) && builder.type_on_stack(ValType::I32)
}

fn memory_grow(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::MemoryGrow)
}

fn i32_const(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Const(x))
}

fn i64_const(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Const(x))
}

fn f32_const(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Const(x))
}

fn f64_const(u: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Const(x))
}

fn i32_on_stack(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder.type_on_stack(ValType::I32)
}

fn i32_eqz(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Eqz)
}

fn i32_i32_on_stack(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder.types_on_stack(&[ValType::I32, ValType::I32])
}

fn i32_eq(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Eq)
}

fn i32_neq(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Neq)
}

fn i32_lt_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LtS)
}

fn i32_lt_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LtU)
}

fn i32_gt_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GtS)
}

fn i32_gt_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GtU)
}

fn i32_le_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LeS)
}

fn i32_le_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LeU)
}

fn i32_ge_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GeS)
}

fn i32_ge_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GeU)
}

fn i64_on_stack(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder.types_on_stack(&[ValType::I64])
}

fn i64_eqz(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64Eqz)
}

fn i64_i64_on_stack(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder.types_on_stack(&[ValType::I64, ValType::I64])
}

fn i64_eq(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64Eq)
}

fn i64_neq(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64Neq)
}

fn i64_lt_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LtS)
}

fn i64_lt_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LtU)
}

fn i64_gt_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GtS)
}

fn i64_gt_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GtU)
}

fn i64_le_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LeS)
}

fn i64_le_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LeU)
}

fn i64_ge_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GeS)
}

fn i64_ge_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GeU)
}

fn f32_f32_on_stack(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder.types_on_stack(&[ValType::F32, ValType::F32])
}

fn f32_eq(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Eq)
}

fn f32_neq(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Neq)
}

fn f32_lt(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Lt)
}

fn f32_gt(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Gt)
}

fn f32_le(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Le)
}

fn f32_ge(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Ge)
}

fn f64_f64_on_stack(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder.types_on_stack(&[ValType::F64, ValType::F64])
}

fn f64_eq(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Eq)
}

fn f64_neq(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Neq)
}

fn f64_lt(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Lt)
}

fn f64_gt(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Gt)
}

fn f64_le(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Le)
}

fn f64_ge(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Ge)
}

fn i32_clz(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Clz)
}

fn i32_ctz(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Ctz)
}

fn i32_popcnt(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Popcnt)
}

fn i32_add(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Add)
}

fn i32_sub(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Sub)
}

fn i32_mul(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Mul)
}

fn i32_div_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32DivS)
}

fn i32_div_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32DivU)
}

fn i32_rem_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32RemS)
}

fn i32_rem_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32RemU)
}

fn i32_and(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32And)
}

fn i32_or(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Or)
}

fn i32_xor(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Xor)
}

fn i32_shl(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Shl)
}

fn i32_shr_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32ShrS)
}

fn i32_shr_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32ShrU)
}

fn i32_rotl(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Rotl)
}

fn i32_rotr(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Rotr)
}

fn i64_clz(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Clz)
}

fn i64_ctz(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Ctz)
}

fn i64_popcnt(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Popcnt)
}

fn i64_add(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Add)
}

fn i64_sub(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Sub)
}

fn i64_mul(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Mul)
}

fn i64_div_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64DivS)
}

fn i64_div_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64DivU)
}

fn i64_rem_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64RemS)
}

fn i64_rem_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64RemU)
}

fn i64_and(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64And)
}

fn i64_or(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Or)
}

fn i64_xor(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Xor)
}

fn i64_shl(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Shl)
}

fn i64_shr_s(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ShrS)
}

fn i64_shr_u(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ShrU)
}

fn i64_rotl(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Rotl)
}

fn i64_rotr(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Rotr)
}

fn f32_on_stack(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder.types_on_stack(&[ValType::F32])
}

fn f32_abs(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Abs)
}

fn f32_neg(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Neg)
}

fn f32_ceil(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Ceil)
}

fn f32_floor(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Floor)
}

fn f32_trunc(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Trunc)
}

fn f32_nearest(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Nearest)
}

fn f32_sqrt(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Sqrt)
}

fn f32_add(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Add)
}

fn f32_sub(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Sub)
}

fn f32_mul(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Mul)
}

fn f32_div(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Div)
}

fn f32_min(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Min)
}

fn f32_max(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Max)
}

fn f32_copysign(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Copysign)
}

fn f64_on_stack(_: &Module, builder: &mut CodeBuilder) -> bool {
    builder.types_on_stack(&[ValType::F64])
}

fn f64_abs(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Abs)
}

fn f64_neg(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Neg)
}

fn f64_ceil(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Ceil)
}

fn f64_floor(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Floor)
}

fn f64_trunc(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Trunc)
}

fn f64_nearest(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Nearest)
}

fn f64_sqrt(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Sqrt)
}

fn f64_add(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Add)
}

fn f64_sub(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Sub)
}

fn f64_mul(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Mul)
}

fn f64_div(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Div)
}

fn f64_min(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Min)
}

fn f64_max(_: &mut Unstructured, _: &Module, builder: &mut CodeBuilder) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Max)
}

fn f64_copysign(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Copysign)
}

fn i32_wrap_i64(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32WrapI64)
}

fn i32_trunc_f32_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF32S)
}

fn i32_trunc_f32_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF32U)
}

fn i32_trunc_f64_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF64S)
}

fn i32_trunc_f64_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF64U)
}

fn i64_extend_i32_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ExtendI32S)
}

fn i64_extend_i32_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ExtendI32U)
}

fn i64_trunc_f32_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF32S)
}

fn i64_trunc_f32_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF32U)
}

fn i64_trunc_f64_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF64S)
}

fn i64_trunc_f64_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF64U)
}

fn f32_convert_i32_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI32S)
}

fn f32_convert_i32_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI32U)
}

fn f32_convert_i64_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI64S)
}

fn f32_convert_i64_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI64U)
}

fn f32_demote_f64(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32DemoteF64)
}

fn f64_convert_i32_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI32S)
}

fn f64_convert_i32_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI32U)
}

fn f64_convert_i64_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI64S)
}

fn f64_convert_i64_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI64U)
}

fn f64_promote_f32(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64PromoteF32)
}

fn i32_reinterpret_f32(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32ReinterpretF32)
}

fn i64_reinterpret_f64(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ReinterpretF64)
}

fn f32_reinterpret_i32(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ReinterpretI32)
}

fn f64_reinterpret_i64(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ReinterpretI64)
}

fn i32_extend_8_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Extend8S)
}

fn i32_extend_16_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Extend16S)
}

fn i64_extend_8_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Extend8S)
}

fn i64_extend_16_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Extend16S)
}

fn i64_extend_32_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Extend32S)
}

fn i32_trunc_sat_f32_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF32S)
}

fn i32_trunc_sat_f32_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF32U)
}

fn i32_trunc_sat_f64_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF64S)
}

fn i32_trunc_sat_f64_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF64U)
}

fn i64_trunc_sat_f32_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF32S)
}

fn i64_trunc_sat_f32_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF32U)
}

fn i64_trunc_sat_f64_s(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF64S)
}

fn i64_trunc_sat_f64_u(
    _: &mut Unstructured,
    _: &Module,
    builder: &mut CodeBuilder,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF64U)
}
