use super::*;
use std::convert::TryFrom;

impl Module {
    /// Encode this Wasm module into bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        self.inner.to_bytes()
    }
}

impl<C> ConfiguredModule<C>
where
    C: Config,
{
    /// Encode this Wasm module into bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        self.encoded().finish()
    }

    fn encoded(&self) -> wasm_encoder::Module {
        let mut module = wasm_encoder::Module::new();

        self.encode_initializers(&mut module);
        self.encode_funcs(&mut module);
        self.encode_tables(&mut module);
        self.encode_memories(&mut module);
        self.encode_globals(&mut module);
        self.encode_exports(&mut module);
        self.encode_start(&mut module);
        self.encode_elems(&mut module);
        self.encode_data_count(&mut module);
        self.encode_code(&mut module);
        self.encode_data(&mut module);

        module
    }

    fn encode_initializers(&self, module: &mut wasm_encoder::Module) {
        for init in self.initial_sections.iter() {
            match init {
                InitialSection::Type(types) => self.encode_types(module, types),
                InitialSection::Import(imports) => self.encode_imports(module, imports),
                InitialSection::Alias(aliases) => self.encode_aliases(module, aliases),
                InitialSection::Instance(list) => self.encode_instances(module, list),
                InitialSection::Module(list) => self.encode_modules(module, list),
            }
        }
    }

    fn encode_types(&self, module: &mut wasm_encoder::Module, types: &[Type]) {
        let mut section = wasm_encoder::TypeSection::new();
        for ty in types {
            match ty {
                Type::Func(ty) => {
                    section.function(
                        ty.params.iter().map(|t| translate_val_type(*t)),
                        ty.results.iter().map(|t| translate_val_type(*t)),
                    );
                }
                Type::Module(ty) => {
                    section.module(
                        ty.imports.iter().map(|(module, name, ty)| {
                            (module.as_str(), name.as_deref(), translate_entity_type(ty))
                        }),
                        ty.exports
                            .exports
                            .iter()
                            .map(|(name, ty)| (name.as_str(), translate_entity_type(ty))),
                    );
                }
                Type::Instance(ty) => {
                    section.instance(
                        ty.exports
                            .iter()
                            .map(|(name, ty)| (name.as_str(), translate_entity_type(ty))),
                    );
                }
            }
        }
        module.section(&section);
    }

    fn encode_imports(
        &self,
        module: &mut wasm_encoder::Module,
        imports: &[(String, Option<String>, EntityType)],
    ) {
        let mut section = wasm_encoder::ImportSection::new();
        for (module, name, ty) in imports {
            section.import(module, name.as_deref(), translate_entity_type(ty));
        }
        module.section(&section);
    }

    fn encode_aliases(&self, module: &mut wasm_encoder::Module, imports: &[Alias]) {
        let mut section = wasm_encoder::AliasSection::new();
        for alias in imports {
            match alias {
                Alias::InstanceExport {
                    instance,
                    kind,
                    name,
                } => {
                    section.instance_export(*instance, translate_item_kind(kind), name);
                }
                Alias::OuterType { depth, index } => {
                    section.outer_type(*depth, *index);
                }
                Alias::OuterModule { depth, index } => {
                    section.outer_module(*depth, *index);
                }
            }
        }
        module.section(&section);
    }

    fn encode_instances(&self, module: &mut wasm_encoder::Module, list: &[Instance]) {
        let mut section = wasm_encoder::InstanceSection::new();
        for instance in list {
            section.instantiate(
                instance.module,
                instance
                    .args
                    .iter()
                    .map(|(name, export)| (name.as_str(), translate_export(export))),
            );
        }
        module.section(&section);
    }

    fn encode_modules(&self, module: &mut wasm_encoder::Module, list: &[Self]) {
        let mut section = wasm_encoder::ModuleSection::new();
        for module in list {
            let encoded = module.encoded();
            section.module(&encoded);
        }
        module.section(&section);
    }

    fn encode_funcs(&self, module: &mut wasm_encoder::Module) {
        if self.num_defined_funcs == 0 {
            return;
        }
        let mut funcs = wasm_encoder::FunctionSection::new();
        for (ty, _) in self.funcs[self.funcs.len() - self.num_defined_funcs..].iter() {
            funcs.function(ty.unwrap());
        }
        module.section(&funcs);
    }

    fn encode_tables(&self, module: &mut wasm_encoder::Module) {
        if self.num_defined_tables == 0 {
            return;
        }
        let mut tables = wasm_encoder::TableSection::new();
        for t in self.tables[self.tables.len() - self.num_defined_tables..].iter() {
            tables.table(translate_table_type(t));
        }
        module.section(&tables);
    }

    fn encode_memories(&self, module: &mut wasm_encoder::Module) {
        if self.num_defined_memories == 0 {
            return;
        }
        let mut mems = wasm_encoder::MemorySection::new();
        for m in self.memories[self.memories.len() - self.num_defined_memories..].iter() {
            mems.memory(translate_memory_type(m));
        }
        module.section(&mems);
    }

    fn encode_globals(&self, module: &mut wasm_encoder::Module) {
        if self.globals.is_empty() {
            return;
        }
        let mut globals = wasm_encoder::GlobalSection::new();
        for (idx, expr) in &self.defined_globals {
            let ty = &self.globals[*idx as usize];
            globals.global(translate_global_type(ty), translate_instruction(expr));
        }
        module.section(&globals);
    }

    fn encode_exports(&self, module: &mut wasm_encoder::Module) {
        if self.exports.is_empty() {
            return;
        }
        let mut exports = wasm_encoder::ExportSection::new();
        for (name, export) in &self.exports {
            exports.export(name, translate_export(export));
        }
        module.section(&exports);
    }

    fn encode_start(&self, module: &mut wasm_encoder::Module) {
        if let Some(f) = self.start {
            module.section(&wasm_encoder::StartSection { function_index: f });
        }
    }

    fn encode_elems(&self, module: &mut wasm_encoder::Module) {
        if self.elems.is_empty() {
            return;
        }
        let mut elems = wasm_encoder::ElementSection::new();
        let mut exps = vec![];
        for el in &self.elems {
            let elem_ty = translate_val_type(el.ty);
            let elements = match &el.items {
                Elements::Expressions(es) => {
                    exps.clear();
                    exps.extend(es.iter().map(|e| match e {
                        Some(i) => wasm_encoder::Element::Func(*i),
                        None => wasm_encoder::Element::Null,
                    }));
                    wasm_encoder::Elements::Expressions(&exps)
                }
                Elements::Functions(fs) => wasm_encoder::Elements::Functions(fs),
            };
            match &el.kind {
                ElementKind::Active { table, offset } => {
                    elems.active(*table, translate_instruction(offset), elem_ty, elements);
                }
                ElementKind::Passive => {
                    elems.passive(elem_ty, elements);
                }
                ElementKind::Declared => {
                    elems.declared(elem_ty, elements);
                }
            }
        }
        module.section(&elems);
    }

    fn encode_data_count(&self, module: &mut wasm_encoder::Module) {
        // Without bulk memory there's no need for a data count section,
        if !self.config.bulk_memory_enabled() {
            return;
        }
        // ... and also if there's no data no need for a data count section.
        if self.data.is_empty() {
            return;
        }
        module.section(&wasm_encoder::DataCountSection {
            count: u32::try_from(self.data.len()).unwrap(),
        });
    }

    fn encode_code(&self, module: &mut wasm_encoder::Module) {
        if self.code.is_empty() {
            return;
        }
        let mut code = wasm_encoder::CodeSection::new();
        for c in &self.code {
            // Skip the run-length encoding because it is a little
            // annoying to compute; use a length of one for every local.
            let mut func =
                wasm_encoder::Function::new(c.locals.iter().map(|l| (1, translate_val_type(*l))));
            match &c.instructions {
                Instructions::Generated(instrs) => {
                    for instr in instrs {
                        func.instruction(translate_instruction(instr));
                    }
                    func.instruction(wasm_encoder::Instruction::End);
                }
                Instructions::Arbitrary(body) => {
                    func.raw(body.iter().copied());
                }
            }
            code.function(&func);
        }
        module.section(&code);
    }

    fn encode_data(&self, module: &mut wasm_encoder::Module) {
        if self.data.is_empty() {
            return;
        }
        let mut data = wasm_encoder::DataSection::new();
        for seg in &self.data {
            match &seg.kind {
                DataSegmentKind::Active {
                    memory_index,
                    offset,
                } => {
                    data.active(
                        *memory_index,
                        translate_instruction(offset),
                        seg.init.iter().copied(),
                    );
                }
                DataSegmentKind::Passive => {
                    data.passive(seg.init.iter().copied());
                }
            }
        }
        module.section(&data);
    }
}

fn translate_val_type(ty: ValType) -> wasm_encoder::ValType {
    match ty {
        ValType::I32 => wasm_encoder::ValType::I32,
        ValType::I64 => wasm_encoder::ValType::I64,
        ValType::F32 => wasm_encoder::ValType::F32,
        ValType::F64 => wasm_encoder::ValType::F64,
        ValType::FuncRef => wasm_encoder::ValType::FuncRef,
        ValType::ExternRef => wasm_encoder::ValType::ExternRef,
    }
}

fn translate_entity_type(ty: &EntityType) -> wasm_encoder::EntityType {
    match ty {
        EntityType::Func(f, _) => wasm_encoder::EntityType::Function(*f),
        EntityType::Instance(i, _) => wasm_encoder::EntityType::Instance(*i),
        EntityType::Module(i, _) => wasm_encoder::EntityType::Module(*i),
        EntityType::Table(ty) => translate_table_type(ty).into(),
        EntityType::Memory(m) => translate_memory_type(m).into(),
        EntityType::Global(g) => translate_global_type(g).into(),
    }
}

fn translate_limits(limits: &Limits) -> wasm_encoder::Limits {
    wasm_encoder::Limits {
        min: limits.min,
        max: limits.max,
    }
}

fn translate_table_type(ty: &TableType) -> wasm_encoder::TableType {
    wasm_encoder::TableType {
        element_type: translate_val_type(ty.elem_ty),
        limits: translate_limits(&ty.limits),
    }
}

fn translate_memory_type(ty: &MemoryType) -> wasm_encoder::MemoryType {
    wasm_encoder::MemoryType {
        limits: translate_limits(&ty.limits),
    }
}

fn translate_global_type(ty: &GlobalType) -> wasm_encoder::GlobalType {
    wasm_encoder::GlobalType {
        val_type: translate_val_type(ty.val_type),
        mutable: ty.mutable,
    }
}

fn translate_block_type(ty: BlockType) -> wasm_encoder::BlockType {
    match ty {
        BlockType::Empty => wasm_encoder::BlockType::Empty,
        BlockType::Result(ty) => wasm_encoder::BlockType::Result(translate_val_type(ty)),
        BlockType::FuncType(f) => wasm_encoder::BlockType::FunctionType(f),
    }
}

fn translate_mem_arg(m: MemArg) -> wasm_encoder::MemArg {
    wasm_encoder::MemArg {
        offset: m.offset,
        align: m.align,
        memory_index: m.memory_index,
    }
}

fn translate_item_kind(kind: &ItemKind) -> wasm_encoder::ItemKind {
    match kind {
        ItemKind::Func => wasm_encoder::ItemKind::Function,
        ItemKind::Table => wasm_encoder::ItemKind::Table,
        ItemKind::Memory => wasm_encoder::ItemKind::Memory,
        ItemKind::Global => wasm_encoder::ItemKind::Global,
        ItemKind::Instance => wasm_encoder::ItemKind::Instance,
        ItemKind::Module => wasm_encoder::ItemKind::Module,
    }
}

fn translate_export(export: &Export) -> wasm_encoder::Export {
    match export {
        Export::Func(idx) => wasm_encoder::Export::Function(*idx),
        Export::Table(idx) => wasm_encoder::Export::Table(*idx),
        Export::Memory(idx) => wasm_encoder::Export::Memory(*idx),
        Export::Global(idx) => wasm_encoder::Export::Global(*idx),
        Export::Instance(idx) => wasm_encoder::Export::Instance(*idx),
        Export::Module(idx) => wasm_encoder::Export::Module(*idx),
    }
}

fn translate_instruction(inst: &Instruction) -> wasm_encoder::Instruction {
    use Instruction::*;
    match *inst {
        // Control instructions.
        Unreachable => wasm_encoder::Instruction::Unreachable,
        Nop => wasm_encoder::Instruction::Nop,
        Block(bt) => wasm_encoder::Instruction::Block(translate_block_type(bt)),
        Loop(bt) => wasm_encoder::Instruction::Loop(translate_block_type(bt)),
        If(bt) => wasm_encoder::Instruction::If(translate_block_type(bt)),
        Else => wasm_encoder::Instruction::Else,
        End => wasm_encoder::Instruction::End,
        Br(x) => wasm_encoder::Instruction::Br(x),
        BrIf(x) => wasm_encoder::Instruction::BrIf(x),
        BrTable(ref ls, l) => wasm_encoder::Instruction::BrTable(ls, l),
        Return => wasm_encoder::Instruction::Return,
        Call(x) => wasm_encoder::Instruction::Call(x),
        CallIndirect { ty, table } => wasm_encoder::Instruction::CallIndirect { ty, table },

        // Parametric instructions.
        Drop => wasm_encoder::Instruction::Drop,
        Select => wasm_encoder::Instruction::Select,

        // Variable instructions.
        LocalGet(x) => wasm_encoder::Instruction::LocalGet(x),
        LocalSet(x) => wasm_encoder::Instruction::LocalSet(x),
        LocalTee(x) => wasm_encoder::Instruction::LocalTee(x),
        GlobalGet(x) => wasm_encoder::Instruction::GlobalGet(x),
        GlobalSet(x) => wasm_encoder::Instruction::GlobalSet(x),

        // Memory instructions.
        I32Load(m) => wasm_encoder::Instruction::I32Load(translate_mem_arg(m)),
        I64Load(m) => wasm_encoder::Instruction::I64Load(translate_mem_arg(m)),
        F32Load(m) => wasm_encoder::Instruction::F32Load(translate_mem_arg(m)),
        F64Load(m) => wasm_encoder::Instruction::F64Load(translate_mem_arg(m)),
        I32Load8_S(m) => wasm_encoder::Instruction::I32Load8_S(translate_mem_arg(m)),
        I32Load8_U(m) => wasm_encoder::Instruction::I32Load8_U(translate_mem_arg(m)),
        I32Load16_S(m) => wasm_encoder::Instruction::I32Load16_S(translate_mem_arg(m)),
        I32Load16_U(m) => wasm_encoder::Instruction::I32Load16_U(translate_mem_arg(m)),
        I64Load8_S(m) => wasm_encoder::Instruction::I64Load8_S(translate_mem_arg(m)),
        I64Load8_U(m) => wasm_encoder::Instruction::I64Load8_U(translate_mem_arg(m)),
        I64Load16_S(m) => wasm_encoder::Instruction::I64Load16_S(translate_mem_arg(m)),
        I64Load16_U(m) => wasm_encoder::Instruction::I64Load16_U(translate_mem_arg(m)),
        I64Load32_S(m) => wasm_encoder::Instruction::I64Load32_S(translate_mem_arg(m)),
        I64Load32_U(m) => wasm_encoder::Instruction::I64Load32_U(translate_mem_arg(m)),
        I32Store(m) => wasm_encoder::Instruction::I32Store(translate_mem_arg(m)),
        I64Store(m) => wasm_encoder::Instruction::I64Store(translate_mem_arg(m)),
        F32Store(m) => wasm_encoder::Instruction::F32Store(translate_mem_arg(m)),
        F64Store(m) => wasm_encoder::Instruction::F64Store(translate_mem_arg(m)),
        I32Store8(m) => wasm_encoder::Instruction::I32Store8(translate_mem_arg(m)),
        I32Store16(m) => wasm_encoder::Instruction::I32Store16(translate_mem_arg(m)),
        I64Store8(m) => wasm_encoder::Instruction::I64Store8(translate_mem_arg(m)),
        I64Store16(m) => wasm_encoder::Instruction::I64Store16(translate_mem_arg(m)),
        I64Store32(m) => wasm_encoder::Instruction::I64Store32(translate_mem_arg(m)),
        MemorySize(x) => wasm_encoder::Instruction::MemorySize(x),
        MemoryGrow(x) => wasm_encoder::Instruction::MemoryGrow(x),
        MemoryInit { mem, data } => wasm_encoder::Instruction::MemoryInit { mem, data },
        DataDrop(x) => wasm_encoder::Instruction::DataDrop(x),
        MemoryCopy { src, dst } => wasm_encoder::Instruction::MemoryCopy { src, dst },
        MemoryFill(x) => wasm_encoder::Instruction::MemoryFill(x),

        // Numeric instructions.
        I32Const(x) => wasm_encoder::Instruction::I32Const(x),
        I64Const(x) => wasm_encoder::Instruction::I64Const(x),
        F32Const(x) => wasm_encoder::Instruction::F32Const(x),
        F64Const(x) => wasm_encoder::Instruction::F64Const(x),
        I32Eqz => wasm_encoder::Instruction::I32Eqz,
        I32Eq => wasm_encoder::Instruction::I32Eq,
        I32Neq => wasm_encoder::Instruction::I32Neq,
        I32LtS => wasm_encoder::Instruction::I32LtS,
        I32LtU => wasm_encoder::Instruction::I32LtU,
        I32GtS => wasm_encoder::Instruction::I32GtS,
        I32GtU => wasm_encoder::Instruction::I32GtU,
        I32LeS => wasm_encoder::Instruction::I32LeS,
        I32LeU => wasm_encoder::Instruction::I32LeU,
        I32GeS => wasm_encoder::Instruction::I32GeS,
        I32GeU => wasm_encoder::Instruction::I32GeU,
        I64Eqz => wasm_encoder::Instruction::I64Eqz,
        I64Eq => wasm_encoder::Instruction::I64Eq,
        I64Neq => wasm_encoder::Instruction::I64Neq,
        I64LtS => wasm_encoder::Instruction::I64LtS,
        I64LtU => wasm_encoder::Instruction::I64LtU,
        I64GtS => wasm_encoder::Instruction::I64GtS,
        I64GtU => wasm_encoder::Instruction::I64GtU,
        I64LeS => wasm_encoder::Instruction::I64LeS,
        I64LeU => wasm_encoder::Instruction::I64LeU,
        I64GeS => wasm_encoder::Instruction::I64GeS,
        I64GeU => wasm_encoder::Instruction::I64GeU,
        F32Eq => wasm_encoder::Instruction::F32Eq,
        F32Neq => wasm_encoder::Instruction::F32Neq,
        F32Lt => wasm_encoder::Instruction::F32Lt,
        F32Gt => wasm_encoder::Instruction::F32Gt,
        F32Le => wasm_encoder::Instruction::F32Le,
        F32Ge => wasm_encoder::Instruction::F32Ge,
        F64Eq => wasm_encoder::Instruction::F64Eq,
        F64Neq => wasm_encoder::Instruction::F64Neq,
        F64Lt => wasm_encoder::Instruction::F64Lt,
        F64Gt => wasm_encoder::Instruction::F64Gt,
        F64Le => wasm_encoder::Instruction::F64Le,
        F64Ge => wasm_encoder::Instruction::F64Ge,
        I32Clz => wasm_encoder::Instruction::I32Clz,
        I32Ctz => wasm_encoder::Instruction::I32Ctz,
        I32Popcnt => wasm_encoder::Instruction::I32Popcnt,
        I32Add => wasm_encoder::Instruction::I32Add,
        I32Sub => wasm_encoder::Instruction::I32Sub,
        I32Mul => wasm_encoder::Instruction::I32Mul,
        I32DivS => wasm_encoder::Instruction::I32DivS,
        I32DivU => wasm_encoder::Instruction::I32DivU,
        I32RemS => wasm_encoder::Instruction::I32RemS,
        I32RemU => wasm_encoder::Instruction::I32RemU,
        I32And => wasm_encoder::Instruction::I32And,
        I32Or => wasm_encoder::Instruction::I32Or,
        I32Xor => wasm_encoder::Instruction::I32Xor,
        I32Shl => wasm_encoder::Instruction::I32Shl,
        I32ShrS => wasm_encoder::Instruction::I32ShrS,
        I32ShrU => wasm_encoder::Instruction::I32ShrU,
        I32Rotl => wasm_encoder::Instruction::I32Rotl,
        I32Rotr => wasm_encoder::Instruction::I32Rotr,
        I64Clz => wasm_encoder::Instruction::I64Clz,
        I64Ctz => wasm_encoder::Instruction::I64Ctz,
        I64Popcnt => wasm_encoder::Instruction::I64Popcnt,
        I64Add => wasm_encoder::Instruction::I64Add,
        I64Sub => wasm_encoder::Instruction::I64Sub,
        I64Mul => wasm_encoder::Instruction::I64Mul,
        I64DivS => wasm_encoder::Instruction::I64DivS,
        I64DivU => wasm_encoder::Instruction::I64DivU,
        I64RemS => wasm_encoder::Instruction::I64RemS,
        I64RemU => wasm_encoder::Instruction::I64RemU,
        I64And => wasm_encoder::Instruction::I64And,
        I64Or => wasm_encoder::Instruction::I64Or,
        I64Xor => wasm_encoder::Instruction::I64Xor,
        I64Shl => wasm_encoder::Instruction::I64Shl,
        I64ShrS => wasm_encoder::Instruction::I64ShrS,
        I64ShrU => wasm_encoder::Instruction::I64ShrU,
        I64Rotl => wasm_encoder::Instruction::I64Rotl,
        I64Rotr => wasm_encoder::Instruction::I64Rotr,
        F32Abs => wasm_encoder::Instruction::F32Abs,
        F32Neg => wasm_encoder::Instruction::F32Neg,
        F32Ceil => wasm_encoder::Instruction::F32Ceil,
        F32Floor => wasm_encoder::Instruction::F32Floor,
        F32Trunc => wasm_encoder::Instruction::F32Trunc,
        F32Nearest => wasm_encoder::Instruction::F32Nearest,
        F32Sqrt => wasm_encoder::Instruction::F32Sqrt,
        F32Add => wasm_encoder::Instruction::F32Add,
        F32Sub => wasm_encoder::Instruction::F32Sub,
        F32Mul => wasm_encoder::Instruction::F32Mul,
        F32Div => wasm_encoder::Instruction::F32Div,
        F32Min => wasm_encoder::Instruction::F32Min,
        F32Max => wasm_encoder::Instruction::F32Max,
        F32Copysign => wasm_encoder::Instruction::F32Copysign,
        F64Abs => wasm_encoder::Instruction::F64Abs,
        F64Neg => wasm_encoder::Instruction::F64Neg,
        F64Ceil => wasm_encoder::Instruction::F64Ceil,
        F64Floor => wasm_encoder::Instruction::F64Floor,
        F64Trunc => wasm_encoder::Instruction::F64Trunc,
        F64Nearest => wasm_encoder::Instruction::F64Nearest,
        F64Sqrt => wasm_encoder::Instruction::F64Sqrt,
        F64Add => wasm_encoder::Instruction::F64Add,
        F64Sub => wasm_encoder::Instruction::F64Sub,
        F64Mul => wasm_encoder::Instruction::F64Mul,
        F64Div => wasm_encoder::Instruction::F64Div,
        F64Min => wasm_encoder::Instruction::F64Min,
        F64Max => wasm_encoder::Instruction::F64Max,
        F64Copysign => wasm_encoder::Instruction::F64Copysign,
        I32WrapI64 => wasm_encoder::Instruction::I32WrapI64,
        I32TruncF32S => wasm_encoder::Instruction::I32TruncF32S,
        I32TruncF32U => wasm_encoder::Instruction::I32TruncF32U,
        I32TruncF64S => wasm_encoder::Instruction::I32TruncF64S,
        I32TruncF64U => wasm_encoder::Instruction::I32TruncF64U,
        I64ExtendI32S => wasm_encoder::Instruction::I64ExtendI32S,
        I64ExtendI32U => wasm_encoder::Instruction::I64ExtendI32U,
        I64TruncF32S => wasm_encoder::Instruction::I64TruncF32S,
        I64TruncF32U => wasm_encoder::Instruction::I64TruncF32U,
        I64TruncF64S => wasm_encoder::Instruction::I64TruncF64S,
        I64TruncF64U => wasm_encoder::Instruction::I64TruncF64U,
        F32ConvertI32S => wasm_encoder::Instruction::F32ConvertI32S,
        F32ConvertI32U => wasm_encoder::Instruction::F32ConvertI32U,
        F32ConvertI64S => wasm_encoder::Instruction::F32ConvertI64S,
        F32ConvertI64U => wasm_encoder::Instruction::F32ConvertI64U,
        F32DemoteF64 => wasm_encoder::Instruction::F32DemoteF64,
        F64ConvertI32S => wasm_encoder::Instruction::F64ConvertI32S,
        F64ConvertI32U => wasm_encoder::Instruction::F64ConvertI32U,
        F64ConvertI64S => wasm_encoder::Instruction::F64ConvertI64S,
        F64ConvertI64U => wasm_encoder::Instruction::F64ConvertI64U,
        F64PromoteF32 => wasm_encoder::Instruction::F64PromoteF32,
        I32ReinterpretF32 => wasm_encoder::Instruction::I32ReinterpretF32,
        I64ReinterpretF64 => wasm_encoder::Instruction::I64ReinterpretF64,
        F32ReinterpretI32 => wasm_encoder::Instruction::F32ReinterpretI32,
        F64ReinterpretI64 => wasm_encoder::Instruction::F64ReinterpretI64,
        I32Extend8S => wasm_encoder::Instruction::I32Extend8S,
        I32Extend16S => wasm_encoder::Instruction::I32Extend16S,
        I64Extend8S => wasm_encoder::Instruction::I64Extend8S,
        I64Extend16S => wasm_encoder::Instruction::I64Extend16S,
        I64Extend32S => wasm_encoder::Instruction::I64Extend32S,
        I32TruncSatF32S => wasm_encoder::Instruction::I32TruncSatF32S,
        I32TruncSatF32U => wasm_encoder::Instruction::I32TruncSatF32U,
        I32TruncSatF64S => wasm_encoder::Instruction::I32TruncSatF64S,
        I32TruncSatF64U => wasm_encoder::Instruction::I32TruncSatF64U,
        I64TruncSatF32S => wasm_encoder::Instruction::I64TruncSatF32S,
        I64TruncSatF32U => wasm_encoder::Instruction::I64TruncSatF32U,
        I64TruncSatF64S => wasm_encoder::Instruction::I64TruncSatF64S,
        I64TruncSatF64U => wasm_encoder::Instruction::I64TruncSatF64U,
        TypedSelect(ty) => wasm_encoder::Instruction::TypedSelect(translate_val_type(ty)),
        RefNull(ty) => wasm_encoder::Instruction::RefNull(translate_val_type(ty)),
        RefIsNull => wasm_encoder::Instruction::RefIsNull,
        RefFunc(x) => wasm_encoder::Instruction::RefFunc(x),
        TableInit { segment, table } => wasm_encoder::Instruction::TableInit { segment, table },
        ElemDrop { segment } => wasm_encoder::Instruction::ElemDrop { segment },
        TableFill { table } => wasm_encoder::Instruction::TableFill { table },
        TableSet { table } => wasm_encoder::Instruction::TableSet { table },
        TableGet { table } => wasm_encoder::Instruction::TableGet { table },
        TableGrow { table } => wasm_encoder::Instruction::TableGrow { table },
        TableSize { table } => wasm_encoder::Instruction::TableSize { table },
        TableCopy { src, dst } => wasm_encoder::Instruction::TableCopy { src, dst },
    }
}
