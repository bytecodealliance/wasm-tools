use crate::ast::*;

pub fn encode(module: &Module<'_>) -> Vec<u8> {
    let fields = match &module.kind {
        ModuleKind::Text(fields) => fields,
        ModuleKind::Binary(bytes) => {
            return bytes.iter().flat_map(|b| b.iter().cloned()).collect();
        }
    };
    let mut wasm = Vec::new();
    wasm.extend(b"\0asm");
    wasm.extend(b"\x01\0\0\0");

    let mut types = Vec::new();
    let mut imports = Vec::new();
    let mut funcs = Vec::new();
    let mut tables = Vec::new();
    let mut memories = Vec::new();
    let mut globals = Vec::new();
    let mut exports = Vec::new();
    let mut start = Vec::new();
    let mut elem = Vec::new();
    let mut data = Vec::new();
    for field in fields {
        match field {
            ModuleField::Type(i) => types.push(i),
            ModuleField::Import(i) => imports.push(i),
            ModuleField::Func(i) => funcs.push(i),
            ModuleField::Table(i) => tables.push(i),
            ModuleField::Memory(i) => memories.push(i),
            ModuleField::Global(i) => globals.push(i),
            ModuleField::Export(i) => exports.push(i),
            ModuleField::Start(i) => start.push(i),
            ModuleField::Elem(i) => elem.push(i),
            ModuleField::Data(i) => data.push(i),
        }
    }

    let mut tmp = Vec::new();
    section_list(1, &types, &mut tmp, &mut wasm);
    section_list(2, &imports, &mut tmp, &mut wasm);
    let functys = funcs.iter().map(|f| &f.ty).collect::<Vec<_>>();
    section_list(3, &functys, &mut tmp, &mut wasm);
    section_list(4, &tables, &mut tmp, &mut wasm);
    section_list(5, &memories, &mut tmp, &mut wasm);
    section_list(6, &globals, &mut tmp, &mut wasm);
    section_list(7, &exports, &mut tmp, &mut wasm);
    if let Some(start) = start.get(0) {
        section(8, start, &mut tmp, &mut wasm);
    }
    section_list(9, &elem, &mut tmp, &mut wasm);
    if contains_bulk_memory(&funcs) {
        section(12, data.len(), &mut tmp, &mut wasm);
    }
    section_list(10, &funcs, &mut tmp, &mut wasm);
    section_list(11, &data, &mut tmp, &mut wasm);

    let names = find_names(module, fields);
    if !names.is_empty() {
        section(0, ("name", names), &mut tmp, &mut wasm);
    }

    return wasm;

    fn section_list<T: Encode>(id: u8, list: &[T], tmp: &mut Vec<u8>, dst: &mut Vec<u8>) {
        if !list.is_empty() {
            section(id, list, tmp, dst)
        }
    }

    fn section<T: Encode>(id: u8, list: T, tmp: &mut Vec<u8>, dst: &mut Vec<u8>) {
        tmp.truncate(0);
        list.encode(tmp);
        dst.push(id);
        tmp.encode(dst);
    }

    fn contains_bulk_memory(funcs: &[&Func<'_>]) -> bool {
        funcs
            .iter()
            .filter_map(|f| match &f.kind {
                FuncKind::Inline { expression, .. } => Some(expression),
                _ => None,
            })
            .flat_map(|e| e.instrs.iter())
            .any(|i| match i {
                Instruction::MemoryInit(_) | Instruction::DataDrop(_) => true,
                _ => false,
            })
    }
}

pub(crate) trait Encode {
    fn encode(&self, e: &mut Vec<u8>);
}

impl<T: Encode + ?Sized> Encode for &'_ T {
    fn encode(&self, e: &mut Vec<u8>) {
        T::encode(self, e)
    }
}

impl<T: Encode> Encode for [T] {
    fn encode(&self, e: &mut Vec<u8>) {
        self.len().encode(e);
        for item in self {
            item.encode(e);
        }
    }
}

impl<T: Encode> Encode for Vec<T> {
    fn encode(&self, e: &mut Vec<u8>) {
        <[T]>::encode(self, e)
    }
}

impl Encode for str {
    fn encode(&self, e: &mut Vec<u8>) {
        self.len().encode(e);
        e.extend_from_slice(self.as_bytes());
    }
}

impl Encode for usize {
    fn encode(&self, e: &mut Vec<u8>) {
        assert!(*self <= u32::max_value() as usize);
        (*self as u32).encode(e)
    }
}

impl Encode for u8 {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(*self);
    }
}

impl Encode for u32 {
    fn encode(&self, e: &mut Vec<u8>) {
        leb128::write::unsigned(e, (*self).into()).unwrap();
    }
}

impl Encode for i32 {
    fn encode(&self, e: &mut Vec<u8>) {
        leb128::write::signed(e, (*self).into()).unwrap();
    }
}

impl Encode for Type<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x60);
        self.func.params.encode(e);
        self.func.results.encode(e);
    }
}

impl Encode for Option<Id<'_>> {
    fn encode(&self, _e: &mut Vec<u8>) {
        // used for parameters in the tuple impl as well as instruction labels
    }
}

impl<T: Encode, U: Encode> Encode for (T, U) {
    fn encode(&self, e: &mut Vec<u8>) {
        self.0.encode(e);
        self.1.encode(e);
    }
}

impl Encode for ValType {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            ValType::I32 => e.push(0x7f),
            ValType::I64 => e.push(0x7e),
            ValType::F32 => e.push(0x7d),
            ValType::F64 => e.push(0x7c),
            ValType::V128 => e.push(0x7b),
            ValType::Funcref => e.push(0x70),
            ValType::Anyref => e.push(0x6f),
            ValType::Nullref => e.push(0x6e),
        }
    }
}

impl Encode for Import<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.module.encode(e);
        self.name.encode(e);
        match &self.kind {
            ImportKind::Func(f) => {
                e.push(0x00);
                f.encode(e);
            }
            ImportKind::Table(f) => {
                e.push(0x01);
                f.encode(e);
            }
            ImportKind::Memory(f) => {
                e.push(0x02);
                f.encode(e);
            }
            ImportKind::Global(f) => {
                e.push(0x03);
                f.encode(e);
            }
        }
    }
}

impl Encode for TypeUse<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.index
            .as_ref()
            .expect("TypeUse should be filled in by this point")
            .encode(e)
    }
}

impl Encode for Index<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            Index::Num(n) => n.encode(e),
            Index::Id(n) => panic!("unresolved index in emission: {}", n.name()),
        }
    }
}

impl Encode for TableType {
    fn encode(&self, e: &mut Vec<u8>) {
        self.elem.encode(e);
        self.limits.encode(e);
    }
}

impl Encode for TableElemType {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            TableElemType::Funcref => ValType::Funcref.encode(e),
            TableElemType::Anyref => ValType::Anyref.encode(e),
            TableElemType::Nullref => ValType::Nullref.encode(e),
        }
    }
}

impl Encode for Limits {
    fn encode(&self, e: &mut Vec<u8>) {
        match self.max {
            Some(max) => {
                e.push(0x01);
                self.min.encode(e);
                max.encode(e);
            }
            None => {
                e.push(0x00);
                self.min.encode(e);
            }
        }
    }
}

impl Encode for MemoryType {
    fn encode(&self, e: &mut Vec<u8>) {
        if self.shared {
            e.push(0x03);
            self.limits.min.encode(e);
            // Handle a textual error here by deferring the validation error
            // until later. This isn't great though and we should probably just
            // make `Encode` fallible if this comes up somewhere else
            self.limits.max.unwrap_or(0).encode(e);
        } else {
            self.limits.encode(e);
        }
    }
}

impl Encode for GlobalType {
    fn encode(&self, e: &mut Vec<u8>) {
        self.ty.encode(e);
        if self.mutable {
            e.push(0x01);
        } else {
            e.push(0x00);
        }
    }
}

impl Encode for Table<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        assert!(self.exports.names.is_empty());
        match &self.kind {
            TableKind::Normal(t) => t.encode(e),
            _ => panic!("TableKind should be normal during encoding"),
        }
    }
}

impl Encode for Memory<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        assert!(self.exports.names.is_empty());
        match &self.kind {
            MemoryKind::Normal(t) => t.encode(e),
            _ => panic!("MemoryKind should be normal during encoding"),
        }
    }
}

impl Encode for Global<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        assert!(self.exports.names.is_empty());
        self.ty.encode(e);
        match &self.kind {
            GlobalKind::Inline(expr) => expr.encode(e),
            _ => panic!("GlobalKind should be inline during encoding"),
        }
    }
}

impl Encode for Export<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.name.encode(e);
        match &self.kind {
            ExportKind::Func(f) => {
                e.push(0x00);
                f.encode(e);
            }
            ExportKind::Table(f) => {
                e.push(0x01);
                f.encode(e);
            }
            ExportKind::Memory(f) => {
                e.push(0x02);
                f.encode(e);
            }
            ExportKind::Global(f) => {
                e.push(0x03);
                f.encode(e);
            }
        }
    }
}

impl Encode for Elem<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        // Try to switch element expressions to indices if we can which uses a
        // more MVP-compatible encoding.
        let mut to_encode = self.payload.clone();
        if let ElemPayload::Exprs {
            ty: TableElemType::Funcref,
            exprs,
        } = &to_encode
        {
            if let Some(indices) = extract_indices(exprs) {
                to_encode = ElemPayload::Indices(indices);
            }
        }

        match (&self.kind, &to_encode) {
            (
                ElemKind::Active {
                    table: Index::Num(0),
                    offset,
                },
                ElemPayload::Indices(_),
            ) => {
                e.push(0x00);
                offset.encode(e);
            }
            (ElemKind::Passive, ElemPayload::Indices(_)) => {
                e.push(0x01); // flags
                e.push(0x00); // extern_kind
            }
            (ElemKind::Active { table, offset }, ElemPayload::Indices(_)) => {
                e.push(0x02); // flags
                table.encode(e);
                offset.encode(e);
                e.push(0x00); // extern_kind
            }
            (
                ElemKind::Active {
                    table: Index::Num(0),
                    offset,
                },
                ElemPayload::Exprs {
                    ty: TableElemType::Funcref,
                    ..
                },
            ) => {
                e.push(0x04);
                offset.encode(e);
            }
            (ElemKind::Passive, ElemPayload::Exprs { ty, .. }) => {
                e.push(0x05);
                ty.encode(e);
            }
            (ElemKind::Active { table, offset }, ElemPayload::Exprs { ty, .. }) => {
                e.push(0x06);
                table.encode(e);
                offset.encode(e);
                ty.encode(e);
            }
        }

        to_encode.encode(e);

        fn extract_indices<'a>(indices: &[Option<Index<'a>>]) -> Option<Vec<Index<'a>>> {
            indices.iter().cloned().collect()
        }
    }
}

impl Encode for ElemPayload<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            ElemPayload::Indices(v) => v.encode(e),
            ElemPayload::Exprs { exprs, .. } => {
                exprs.len().encode(e);
                for idx in exprs {
                    match idx {
                        Some(idx) => {
                            Instruction::RefFunc(*idx).encode(e);
                        }
                        None => {
                            Instruction::RefNull.encode(e);
                        }
                    }
                    Instruction::End(None).encode(e);
                }
            }
        }
    }
}

impl Encode for Data<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match &self.kind {
            DataKind::Passive => e.push(0x01),
            DataKind::Active { memory, offset } => {
                if *memory == Index::Num(0) {
                    e.push(0x00);
                } else {
                    e.push(0x02);
                    memory.encode(e);
                }
                offset.encode(e);
            }
        }
        self.data.iter().map(|l| l.len()).sum::<usize>().encode(e);
        for list in self.data.iter() {
            e.extend_from_slice(list);
        }
    }
}

impl Encode for Func<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        assert!(self.exports.names.is_empty());
        let mut tmp = Vec::new();
        let (expr, locals) = match &self.kind {
            FuncKind::Inline { expression, locals } => (expression, locals),
            _ => panic!("should only have inline functions in emission"),
        };

        let mut locals_compressed = Vec::<(u32, ValType)>::new();
        for (_, ty) in locals {
            if let Some((cnt, prev)) = locals_compressed.last_mut() {
                if prev == ty {
                    *cnt += 1;
                    continue;
                }
            }
            locals_compressed.push((1, *ty));
        }
        locals_compressed.encode(&mut tmp);
        expr.encode(&mut tmp);

        tmp.len().encode(e);
        e.extend_from_slice(&tmp);
    }
}

impl Encode for Expression<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        for instr in self.instrs.iter() {
            instr.encode(e);
        }
        e.push(0x0b);
    }
}

impl Encode for BlockType<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        if let Some(index) = &self.ty.index {
            return index.encode(e);
        }
        if self.ty.ty.params.is_empty() && self.ty.ty.results.is_empty() {
            return e.push(0x40);
        }
        if self.ty.ty.params.is_empty() && self.ty.ty.results.len() == 1 {
            return self.ty.ty.results[0].encode(e);
        }
        panic!("multi-value block types should have an index");
    }
}

impl Encode for MemArg {
    fn encode(&self, e: &mut Vec<u8>) {
        self.align.trailing_zeros().encode(e);
        self.offset.encode(e);
    }
}

impl Encode for CallIndirect<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.ty.encode(e);
        self.table.encode(e);
    }
}

impl Encode for TableInit<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        // TODO: this agrees with `wabt` but disagrees with the current online
        // spec. Online spec says `0x00` comes before elem segment, wabt says
        // otherwise. Let's match `wabt` for now.
        self.elem.encode(e);
        e.push(0x00);
    }
}

impl Encode for MemoryInit<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        // TODO: this agrees with `wabt` but disagrees with the current online
        // spec. Online spec says `0x00` comes before data segment, wabt says
        // otherwise. Let's match `wabt` for now.
        self.data.encode(e);
        e.push(0x00);
    }
}

impl Encode for BrTableIndices<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.labels.encode(e);
        self.default.encode(e);
    }
}

impl Encode for i64 {
    fn encode(&self, e: &mut Vec<u8>) {
        leb128::write::signed(e, *self).unwrap();
    }
}

impl Encode for Float32 {
    fn encode(&self, e: &mut Vec<u8>) {
        e.extend_from_slice(&self.bits.to_le_bytes());
    }
}

impl Encode for Float64 {
    fn encode(&self, e: &mut Vec<u8>) {
        e.extend_from_slice(&self.bits.to_le_bytes());
    }
}

struct Names<'a> {
    module: Option<Id<'a>>,
    funcs: Vec<(u32, Id<'a>)>,
    locals: Vec<(u32, Vec<(u32, Id<'a>)>)>,
}

fn find_names<'a>(module: &Module<'a>, fields: &[ModuleField<'a>]) -> Names<'a> {
    let mut funcs = Vec::new();
    let mut locals = Vec::new();
    let mut idx = 0;
    for field in fields {
        match field {
            ModuleField::Import(i) => {
                match i.kind {
                    ImportKind::Func(_) => {}
                    _ => continue,
                }

                if let Some(id) = i.id {
                    funcs.push((idx, id));
                }

                idx += 1;
            }
            ModuleField::Func(f) => {
                if let Some(id) = f.name {
                    funcs.push((idx, id));
                }
                let mut local_names = Vec::new();
                let mut local_idx = 0;
                for (name, _) in f.ty.ty.params.iter() {
                    if let Some(id) = name {
                        local_names.push((local_idx, *id));
                    }
                    local_idx += 1;
                }
                if let FuncKind::Inline { locals, .. } = &f.kind {
                    for (name, _) in locals {
                        if let Some(id) = name {
                            local_names.push((local_idx, *id));
                        }
                        local_idx += 1;
                    }
                }
                if local_names.len() > 0 {
                    locals.push((idx, local_names));
                }
                idx += 1;
            }
            _ => {}
        }
    }

    Names {
        module: module.name,
        funcs,
        locals,
    }
}

impl Names<'_> {
    fn is_empty(&self) -> bool {
        self.module.is_none() && self.funcs.is_empty() && self.locals.is_empty()
    }
}

impl Encode for Names<'_> {
    fn encode(&self, dst: &mut Vec<u8>) {
        let mut tmp = Vec::new();

        let mut subsec = |id: u8, data: &mut Vec<u8>| {
            dst.push(id);
            data.encode(dst);
            data.truncate(0);
        };

        if let Some(id) = self.module {
            id.encode(&mut tmp);
            subsec(0, &mut tmp);
        }
        if self.funcs.len() > 0 {
            self.funcs.encode(&mut tmp);
            subsec(1, &mut tmp);
        }
        if self.locals.len() > 0 {
            self.locals.encode(&mut tmp);
            subsec(2, &mut tmp);
        }
    }
}

impl Encode for Id<'_> {
    fn encode(&self, dst: &mut Vec<u8>) {
        self.name().encode(dst);
    }
}

impl Encode for V128Const {
    fn encode(&self, dst: &mut Vec<u8>) {
        dst.extend_from_slice(&self.to_le_bytes());
    }
}

impl Encode for V8x16Shuffle {
    fn encode(&self, dst: &mut Vec<u8>) {
        dst.extend_from_slice(&self.lanes);
    }
}

impl Encode for SelectTypes {
    fn encode(&self, dst: &mut Vec<u8>) {
        if self.tys.len() == 0 {
            dst.push(0x1b);
        } else {
            dst.push(0x1c);
            self.tys.encode(dst);
        }
    }
}
