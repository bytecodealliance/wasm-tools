use super::{BlockType, Config, ConfiguredModule, FuncType, Import, Instruction, MemArg, ValType};
use arbitrary::{Result, Unstructured};
use std::collections::BTreeMap;

macro_rules! instructions {
	(
        $(
            ($predicate:expr, $generator_fn:ident),
        )*
    ) => {
        static NUM_OPTIONS: usize = instructions!(
            @count;
            $( $generator_fn )*
        );

        fn gather_options<C>(
            module: &ConfiguredModule<C>,
            builder: &mut CodeBuilder<C>,
        )
        where
            C: Config
        {
            builder.allocs.options.clear();

            // Unroll the loop that checks whether each instruction is valid in
            // the current context and, if it is valid, pushes it onto our
            // options. Unrolling this loops lets us avoid dynamic calls through
            // function pointers and, furthermore, each call site can be branch
            // predicted and even inlined. This saved us about 30% of time in
            // the `corpus` benchmark.
            $(
                let predicate: Option<fn(&ConfiguredModule<C>, &mut CodeBuilder<C>) -> bool> = $predicate;
                if predicate.map_or(true, |f| f(module, builder)) {
                    builder.allocs.options.push($generator_fn);
                }
            )*
        }
	};

    ( @count; ) => {
        0
    };
    ( @count; $x:ident $( $xs:ident )* ) => {
        1 + instructions!( @count; $( $xs )* )
    };
}

// The static set of options of instruction to generate that could be valid at
// some given time. One entry per Wasm instruction.
//
// Each entry is made up of two parts:
//
// 1. A predicate for whether this is a valid choice, if any. `None` means that
//    the choice is always applicable.
//
// 2. The function to generate the instruction, given that we've made this
//    choice.
instructions! {
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
}

pub(crate) struct CodeBuilderAllocations<C>
where
    C: Config,
{
    // The control labels in scope right now.
    controls: Vec<Control>,

    // The types on the operand stack right now.
    operands: Vec<Option<ValType>>,

    // Dynamic set of options of instruction we can generate that are known to
    // be valid right now.
    options: Vec<
        fn(&mut Unstructured, &ConfiguredModule<C>, &mut CodeBuilder<C>) -> Result<Instruction>,
    >,

    // Cached information about the module that we're generating functions for,
    // used to speed up validity checks. The mutable globals map is a map of the
    // type of global to the global indices which have that type (and they're
    // all mutable).
    mutable_globals: BTreeMap<ValType, Vec<u32>>,

    // Like mutable globals above this is a map from function types to the list
    // of functions that have that function type.
    functions: BTreeMap<Vec<ValType>, Vec<u32>>,

    // Number of linear memories in this module
    num_memories: u32,

    // Whether or not this module has a function table
    has_funcref_table: bool,
}

pub(crate) struct CodeBuilder<'a, C>
where
    C: Config,
{
    func_ty: &'a FuncType,
    locals: &'a Vec<ValType>,
    allocs: &'a mut CodeBuilderAllocations<C>,
}

/// A control frame.
#[derive(Debug)]
struct Control {
    kind: ControlKind,
    /// Value types that must be on the stack when entering this control frame.
    params: Vec<ValType>,
    /// Value types that are left on the stack when exiting this control frame.
    results: Vec<ValType>,
    /// How far down the operand stack instructions inside this control frame
    /// can reach.
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

impl<C> CodeBuilderAllocations<C>
where
    C: Config,
{
    pub(crate) fn new(module: &ConfiguredModule<C>) -> Self {
        let mut mutable_globals = BTreeMap::new();
        for (i, global) in module
            .imports
            .iter()
            .filter_map(|(_, _, imp)| match imp {
                Import::Global(g) => Some(g),
                _ => None,
            })
            .chain(module.globals.iter().map(|g| &g.ty))
            .enumerate()
        {
            if global.mutable {
                mutable_globals
                    .entry(global.val_type)
                    .or_insert(Vec::new())
                    .push(i as u32);
            }
        }

        let mut functions = BTreeMap::new();
        for (idx, func) in module.funcs() {
            functions
                .entry(func.params.clone())
                .or_insert(Vec::new())
                .push(idx);
        }
        CodeBuilderAllocations {
            controls: Vec::with_capacity(4),
            operands: Vec::with_capacity(16),
            options: Vec::with_capacity(NUM_OPTIONS),
            functions,
            mutable_globals,
            num_memories: module.memories.len() as u32 + module.memory_imports(),
            has_funcref_table: module.table.is_some() || module.table_imports() == 1,
        }
    }

    pub(crate) fn builder<'a>(
        &'a mut self,
        func_ty: &'a FuncType,
        locals: &'a Vec<ValType>,
    ) -> CodeBuilder<'a, C> {
        self.controls.clear();
        self.controls.push(Control {
            kind: ControlKind::Block,
            params: vec![],
            results: func_ty.results.clone(),
            height: 0,
        });

        self.operands.clear();
        self.options.clear();

        CodeBuilder {
            func_ty,
            locals,
            allocs: self,
        }
    }
}

impl<'a, C> CodeBuilder<'a, C>
where
    C: Config,
{
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
    fn arbitrary_block_type(
        &self,
        u: &mut Unstructured,
        module: &ConfiguredModule<C>,
    ) -> Result<BlockType> {
        let mut options: Vec<Box<dyn Fn(&mut Unstructured) -> Result<BlockType>>> = vec![
            Box::new(|_| Ok(BlockType::Empty)),
            Box::new(|u| Ok(BlockType::Result(u.arbitrary()?))),
        ];

        for (i, ty) in module.types.iter().enumerate() {
            if self.types_on_stack(&ty.params) {
                options.push(Box::new(move |_| Ok(BlockType::FuncType(i as u32))));
            }
        }

        let f = u.choose(&options)?;
        f(u)
    }

    pub(crate) fn arbitrary(
        mut self,
        u: &mut Unstructured,
        module: &ConfiguredModule<C>,
    ) -> Result<Vec<Instruction>> {
        let max_instructions = module.config.max_instructions();
        let mut instructions = vec![];

        while !self.allocs.controls.is_empty() {
            let keep_going =
                instructions.len() < max_instructions && u.arbitrary().unwrap_or(false);
            if !keep_going {
                self.end_active_control_frames(&mut instructions);
                break;
            }

            gather_options(module, &mut self);

            let f = u.choose(&self.allocs.options)?;
            let inst = f(u, module, &mut self)?;
            instructions.push(inst);
        }

        Ok(instructions)
    }

    fn end_active_control_frames(&mut self, instructions: &mut Vec<Instruction>) {
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
    }
}

fn unreachable<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    _: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    Ok(Instruction::Unreachable)
}

fn nop<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    _: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    Ok(Instruction::Nop)
}

fn block<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let block_ty = builder.arbitrary_block_type(u, module)?;
    let (params, results) = block_ty.params_results(module);
    let height = builder.allocs.operands.len() - params.len();
    builder.allocs.controls.push(Control {
        kind: ControlKind::Block,
        params,
        results,
        height,
    });
    Ok(Instruction::Block(block_ty))
}

fn r#loop<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let block_ty = builder.arbitrary_block_type(u, module)?;
    let (params, results) = block_ty.params_results(module);
    let height = builder.allocs.operands.len() - params.len();
    builder.allocs.controls.push(Control {
        kind: ControlKind::Loop,
        params,
        results,
        height,
    });
    Ok(Instruction::Loop(block_ty))
}

#[inline]
fn if_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.type_on_stack(ValType::I32)
}

fn r#if<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);

    let block_ty = builder.arbitrary_block_type(u, module)?;
    let (params, results) = block_ty.params_results(module);
    let height = builder.allocs.operands.len() - params.len();
    builder.allocs.controls.push(Control {
        kind: ControlKind::If,
        params,
        results,
        height,
    });
    Ok(Instruction::If(block_ty))
}

#[inline]
fn else_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    let last_control = builder.allocs.controls.last().unwrap();
    last_control.kind == ControlKind::If
        && builder.operands().len() == last_control.results.len()
        && builder.types_on_stack(&last_control.results)
}

fn r#else<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let control = builder.allocs.controls.pop().unwrap();
    builder.pop_operands(&control.results);
    builder.push_operands(&control.params);
    builder.allocs.controls.push(Control {
        kind: ControlKind::Block,
        ..control
    });
    Ok(Instruction::Else)
}

#[inline]
fn end_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    // Note: first control frame is the function return's control frame, which
    // does not have an associated `end`.
    if builder.allocs.controls.len() <= 1 {
        return false;
    }
    let control = builder.allocs.controls.last().unwrap();
    builder.operands().len() == control.results.len()
        && builder.types_on_stack(&control.results)
        // `if`s that don't leave the stack as they found it must have an
        // `else`.
        && !(control.kind == ControlKind::If && control.params != control.results)
}

fn end<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.allocs.controls.pop();
    Ok(Instruction::End)
}

#[inline]
fn br_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder
        .allocs
        .controls
        .iter()
        .any(|l| builder.label_types_on_stack(l))
}

fn br<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
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

#[inline]
fn br_if_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
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

fn br_if<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
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

#[inline]
fn br_table_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    if !builder.type_on_stack(ValType::I32) {
        return false;
    }
    let ty = builder.allocs.operands.pop().unwrap();
    let is_valid = br_valid(module, builder);
    builder.allocs.operands.push(ty);
    is_valid
}

fn br_table<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
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

#[inline]
fn return_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.label_types_on_stack(&builder.allocs.controls[0])
}

fn r#return<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let results = builder.allocs.controls[0].results.clone();
    builder.pop_operands(&results);
    Ok(Instruction::Return)
}

#[inline]
fn call_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder
        .allocs
        .functions
        .keys()
        .any(|k| builder.types_on_stack(k))
}

fn call<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let candidates = builder
        .allocs
        .functions
        .iter()
        .filter(|(k, _)| builder.types_on_stack(k))
        .flat_map(|(_, v)| v.iter().copied())
        .collect::<Vec<_>>();
    assert!(candidates.len() > 0);
    let i = u.int_in_range(0..=candidates.len() - 1)?;
    let (func_idx, ty) = module.funcs().nth(candidates[i] as usize).unwrap();
    builder.pop_operands(&ty.params);
    builder.push_operands(&ty.results);
    Ok(Instruction::Call(func_idx as u32))
}

#[inline]
fn call_indirect_valid<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    if !builder.allocs.has_funcref_table || !builder.type_on_stack(ValType::I32) {
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

fn call_indirect<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
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

#[inline]
fn drop_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    !builder.operands().is_empty()
}

fn drop<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.allocs.operands.pop();
    Ok(Instruction::Drop)
}

#[inline]
fn select_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    if !(builder.operands().len() >= 3 && builder.type_on_stack(ValType::I32)) {
        return false;
    }
    let t = builder.operands()[builder.operands().len() - 2];
    let u = builder.operands()[builder.operands().len() - 3];
    t.is_none() || u.is_none() || t == u
}

fn select<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.allocs.operands.pop();
    let t = builder.allocs.operands.pop().unwrap();
    let u = builder.allocs.operands.pop().unwrap();
    builder.allocs.operands.push(t.or(u));
    Ok(Instruction::Select)
}

#[inline]
fn local_get_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    !builder.func_ty.params.is_empty() || !builder.locals.is_empty()
}

fn local_get<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
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

#[inline]
fn local_set_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder
        .func_ty
        .params
        .iter()
        .chain(builder.locals)
        .any(|ty| builder.type_on_stack(*ty))
}

fn local_set<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
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

fn local_tee<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
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

#[inline]
fn global_get_valid<C: Config>(module: &ConfiguredModule<C>, _: &mut CodeBuilder<C>) -> bool {
    !module.globals.is_empty() || module.global_imports() > 0
}

fn global_get<C: Config>(
    u: &mut Unstructured,
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
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

#[inline]
fn global_set_valid<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder
        .allocs
        .mutable_globals
        .iter()
        .any(|(ty, _)| builder.type_on_stack(*ty))
}

fn global_set<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let candidates = builder
        .allocs
        .mutable_globals
        .iter()
        .find(|(ty, _)| builder.type_on_stack(**ty))
        .unwrap()
        .1;
    let i = u.int_in_range(0..=candidates.len() - 1)?;
    builder.allocs.operands.pop();
    Ok(Instruction::GlobalSet(candidates[i]))
}

#[inline]
fn have_memory<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.allocs.num_memories > 0
}

#[inline]
fn have_memory_and_offset<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    have_memory(module, builder) && builder.type_on_stack(ValType::I32)
}

fn i32_load<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load(mem_arg(u, builder, &[0, 1])?))
}

fn i64_load<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load(mem_arg(u, builder, &[0, 1, 2])?))
}

fn f32_load<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::F32));
    Ok(Instruction::F32Load(mem_arg(u, builder, &[0, 1])?))
}

fn f64_load<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::F64));
    Ok(Instruction::F64Load(mem_arg(u, builder, &[0, 1, 2])?))
}

fn i32_load_8_s<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load8_S(mem_arg(u, builder, &[0])?))
}

fn i32_load_8_u<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load8_U(mem_arg(u, builder, &[0])?))
}

fn i32_load_16_s<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load16_S(mem_arg(u, builder, &[0, 1])?))
}

fn i32_load_16_u<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I32));
    Ok(Instruction::I32Load16_U(mem_arg(u, builder, &[0, 1])?))
}

fn i64_load_8_s<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load8_S(mem_arg(u, builder, &[0])?))
}

fn i64_load_16_s<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load16_S(mem_arg(u, builder, &[0, 1])?))
}

fn i64_load_32_s<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load32_S(mem_arg(u, builder, &[0, 1, 2])?))
}

fn i64_load_8_u<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load8_U(mem_arg(u, builder, &[0])?))
}

fn i64_load_16_u<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load16_U(mem_arg(u, builder, &[0, 1])?))
}

fn i64_load_32_u<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.allocs.operands.push(Some(ValType::I64));
    Ok(Instruction::I64Load32_U(mem_arg(u, builder, &[0, 1, 2])?))
}

#[inline]
fn store_valid<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
    f: impl FnOnce() -> ValType,
) -> bool {
    have_memory(module, builder) && builder.types_on_stack(&[ValType::I32, f()])
}

#[inline]
fn i32_store_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    store_valid(module, builder, || ValType::I32)
}

fn i32_store<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    Ok(Instruction::I32Store(mem_arg(u, builder, &[0, 1])?))
}

#[inline]
fn i64_store_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    store_valid(module, builder, || ValType::I64)
}

fn i64_store<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store(mem_arg(u, builder, &[0, 1, 2])?))
}

#[inline]
fn f32_store_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    store_valid(module, builder, || ValType::F32)
}

fn f32_store<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::F32]);
    Ok(Instruction::F32Store(mem_arg(u, builder, &[0, 1])?))
}

#[inline]
fn f64_store_valid<C: Config>(module: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    store_valid(module, builder, || ValType::F64)
}

fn f64_store<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::F64]);
    Ok(Instruction::F64Store(mem_arg(u, builder, &[0, 1, 2])?))
}

fn i32_store_8<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    Ok(Instruction::I32Store8(mem_arg(u, builder, &[0])?))
}

fn i32_store_16<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    Ok(Instruction::I32Store16(mem_arg(u, builder, &[0, 1])?))
}

fn i64_store_8<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store8(mem_arg(u, builder, &[0])?))
}

fn i64_store_16<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store16(mem_arg(u, builder, &[0, 1])?))
}

fn i64_store_32<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I64]);
    Ok(Instruction::I64Store32(mem_arg(u, builder, &[0, 1, 2])?))
}

fn memory_size<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::MemorySize(
        u.int_in_range(0..=builder.allocs.num_memories - 1)?,
    ))
}

#[inline]
fn memory_grow_valid<C: Config>(
    module: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> bool {
    have_memory(module, builder) && builder.type_on_stack(ValType::I32)
}

fn memory_grow<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::MemoryGrow(
        u.int_in_range(0..=builder.allocs.num_memories - 1)?,
    ))
}

fn i32_const<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Const(x))
}

fn i64_const<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Const(x))
}

fn f32_const<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Const(x))
}

fn f64_const<C: Config>(
    u: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    let x = u.arbitrary()?;
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Const(x))
}

#[inline]
fn i32_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.type_on_stack(ValType::I32)
}

fn i32_eqz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Eqz)
}

#[inline]
fn i32_i32_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::I32, ValType::I32])
}

fn i32_eq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Eq)
}

fn i32_neq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Neq)
}

fn i32_lt_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LtS)
}

fn i32_lt_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LtU)
}

fn i32_gt_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GtS)
}

fn i32_gt_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GtU)
}

fn i32_le_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LeS)
}

fn i32_le_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32LeU)
}

fn i32_ge_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GeS)
}

fn i32_ge_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32GeU)
}

#[inline]
fn i64_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::I64])
}

fn i64_eqz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64Eqz)
}

#[inline]
fn i64_i64_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::I64, ValType::I64])
}

fn i64_eq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64Eq)
}

fn i64_neq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64Neq)
}

fn i64_lt_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LtS)
}

fn i64_lt_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LtU)
}

fn i64_gt_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GtS)
}

fn i64_gt_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GtU)
}

fn i64_le_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LeS)
}

fn i64_le_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64LeU)
}

fn i64_ge_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GeS)
}

fn i64_ge_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I64GeU)
}

fn f32_f32_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::F32, ValType::F32])
}

fn f32_eq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Eq)
}

fn f32_neq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Neq)
}

fn f32_lt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Lt)
}

fn f32_gt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Gt)
}

fn f32_le<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Le)
}

fn f32_ge<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F32Ge)
}

fn f64_f64_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::F64, ValType::F64])
}

fn f64_eq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Eq)
}

fn f64_neq<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Neq)
}

fn f64_lt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Lt)
}

fn f64_gt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Gt)
}

fn f64_le<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Le)
}

fn f64_ge<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::F64Ge)
}

fn i32_clz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Clz)
}

fn i32_ctz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Ctz)
}

fn i32_popcnt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Popcnt)
}

fn i32_add<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Add)
}

fn i32_sub<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Sub)
}

fn i32_mul<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Mul)
}

fn i32_div_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32DivS)
}

fn i32_div_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32DivU)
}

fn i32_rem_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32RemS)
}

fn i32_rem_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32RemU)
}

fn i32_and<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32And)
}

fn i32_or<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Or)
}

fn i32_xor<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Xor)
}

fn i32_shl<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Shl)
}

fn i32_shr_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32ShrS)
}

fn i32_shr_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32ShrU)
}

fn i32_rotl<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Rotl)
}

fn i32_rotr<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32, ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Rotr)
}

fn i64_clz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Clz)
}

fn i64_ctz<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Ctz)
}

fn i64_popcnt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Popcnt)
}

fn i64_add<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Add)
}

fn i64_sub<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Sub)
}

fn i64_mul<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Mul)
}

fn i64_div_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64DivS)
}

fn i64_div_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64DivU)
}

fn i64_rem_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64RemS)
}

fn i64_rem_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64RemU)
}

fn i64_and<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64And)
}

fn i64_or<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Or)
}

fn i64_xor<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Xor)
}

fn i64_shl<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Shl)
}

fn i64_shr_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ShrS)
}

fn i64_shr_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ShrU)
}

fn i64_rotl<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Rotl)
}

fn i64_rotr<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64, ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Rotr)
}

#[inline]
fn f32_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::F32])
}

fn f32_abs<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Abs)
}

fn f32_neg<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Neg)
}

fn f32_ceil<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Ceil)
}

fn f32_floor<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Floor)
}

fn f32_trunc<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Trunc)
}

fn f32_nearest<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Nearest)
}

fn f32_sqrt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Sqrt)
}

fn f32_add<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Add)
}

fn f32_sub<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Sub)
}

fn f32_mul<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Mul)
}

fn f32_div<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Div)
}

fn f32_min<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Min)
}

fn f32_max<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Max)
}

fn f32_copysign<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32, ValType::F32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32Copysign)
}

#[inline]
fn f64_on_stack<C: Config>(_: &ConfiguredModule<C>, builder: &mut CodeBuilder<C>) -> bool {
    builder.types_on_stack(&[ValType::F64])
}

fn f64_abs<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Abs)
}

fn f64_neg<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Neg)
}

fn f64_ceil<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Ceil)
}

fn f64_floor<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Floor)
}

fn f64_trunc<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Trunc)
}

fn f64_nearest<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Nearest)
}

fn f64_sqrt<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Sqrt)
}

fn f64_add<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Add)
}

fn f64_sub<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Sub)
}

fn f64_mul<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Mul)
}

fn f64_div<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Div)
}

fn f64_min<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Min)
}

fn f64_max<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Max)
}

fn f64_copysign<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64, ValType::F64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64Copysign)
}

fn i32_wrap_i64<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32WrapI64)
}

fn i32_trunc_f32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF32S)
}

fn i32_trunc_f32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF32U)
}

fn i32_trunc_f64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF64S)
}

fn i32_trunc_f64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncF64U)
}

fn i64_extend_i32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ExtendI32S)
}

fn i64_extend_i32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ExtendI32U)
}

fn i64_trunc_f32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF32S)
}

fn i64_trunc_f32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF32U)
}

fn i64_trunc_f64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF64S)
}

fn i64_trunc_f64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncF64U)
}

fn f32_convert_i32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI32S)
}

fn f32_convert_i32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI32U)
}

fn f32_convert_i64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI64S)
}

fn f32_convert_i64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ConvertI64U)
}

fn f32_demote_f64<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32DemoteF64)
}

fn f64_convert_i32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI32S)
}

fn f64_convert_i32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI32U)
}

fn f64_convert_i64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI64S)
}

fn f64_convert_i64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ConvertI64U)
}

fn f64_promote_f32<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64PromoteF32)
}

fn i32_reinterpret_f32<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32ReinterpretF32)
}

fn i64_reinterpret_f64<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64ReinterpretF64)
}

fn f32_reinterpret_i32<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::F32]);
    Ok(Instruction::F32ReinterpretI32)
}

fn f64_reinterpret_i64<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::F64]);
    Ok(Instruction::F64ReinterpretI64)
}

fn i32_extend_8_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Extend8S)
}

fn i32_extend_16_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32Extend16S)
}

fn i64_extend_8_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Extend8S)
}

fn i64_extend_16_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Extend16S)
}

fn i64_extend_32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::I64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64Extend32S)
}

fn i32_trunc_sat_f32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF32S)
}

fn i32_trunc_sat_f32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF32U)
}

fn i32_trunc_sat_f64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF64S)
}

fn i32_trunc_sat_f64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I32]);
    Ok(Instruction::I32TruncSatF64U)
}

fn i64_trunc_sat_f32_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF32S)
}

fn i64_trunc_sat_f32_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F32]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF32U)
}

fn i64_trunc_sat_f64_s<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF64S)
}

fn i64_trunc_sat_f64_u<C: Config>(
    _: &mut Unstructured,
    _: &ConfiguredModule<C>,
    builder: &mut CodeBuilder<C>,
) -> Result<Instruction> {
    builder.pop_operands(&[ValType::F64]);
    builder.push_operands(&[ValType::I64]);
    Ok(Instruction::I64TruncSatF64U)
}

fn mem_arg<C: Config>(
    u: &mut Unstructured,
    builder: &mut CodeBuilder<C>,
    alignments: &[u32],
) -> Result<MemArg> {
    let offset = u.arbitrary()?;
    let align = *u.choose(alignments)?;
    let memory_index = u.int_in_range(0..=builder.allocs.num_memories - 1)?;
    Ok(MemArg {
        offset,
        align,
        memory_index,
    })
}
