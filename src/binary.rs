use crate::ast::*;

pub fn encode(module: &Module<'_>) -> Vec<u8> {
    let fields = match &module.kind {
        ModuleKind::Text(fields) => fields,
        ModuleKind::Quote(_) => panic!("unknown what to do here"),
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
    for start in start {
        section(8, start, &mut tmp, &mut wasm);
    }
    section_list(9, &elem, &mut tmp, &mut wasm);
    if contains_bulk_memory(&funcs) || true { // TODO: make this conditional
        section(12, data.len(), &mut tmp, &mut wasm);
    }
    section_list(10, &funcs, &mut tmp, &mut wasm);
    section_list(11, &data, &mut tmp, &mut wasm);

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
        tmp.len().encode(dst);
        dst.extend_from_slice(tmp);
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
            ValType::Anyref => e.push(0x00),
            ValType::Funcref => e.push(0x00),
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
            TableElemType::Funcref => e.push(0x70),
            TableElemType::Anyref => e.push(0x00),
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
        self.limits.encode(e);
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
        match &self.kind {
            ElemKind::Passive { ty } => {
                e.push(0x01);
                ty.encode(e);
            }
            ElemKind::Active { table, offset } => {
                if *table == Index::Num(0) {
                    e.push(0x00);
                    offset.encode(e);
                } else {
                    e.push(0x02);
                    table.encode(e);
                    offset.encode(e);
                }
            }
        }
        match &self.elems {
            Elems::Indices(list) => list.encode(e),
            Elems::Funcrefs(list) => list.encode(e),
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
                    offset.encode(e);
                } else {
                    e.push(0x02);
                    memory.encode(e);
                    offset.encode(e);
                }
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
        if let Some(ty) = &self.ty.ty {
            if ty.params.is_empty() && ty.results.is_empty() {
                return e.push(0x40);
            }
            if ty.params.is_empty() && ty.results.len() == 1 {
                return ty.results[0].encode(e);
            }
        }
        self.ty
            .index
            .as_ref()
            .expect("`TypeUse` should be filled in")
            .encode(e)
    }
}

impl Encode for MemArg {
    fn encode(&self, e: &mut Vec<u8>) {
        self.align.leading_zeros().encode(e);
        self.offset.encode(e);
    }
}

impl Encode for CallIndirect<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.ty.encode(e);
        match &self.table {
            Some(t) => t.encode(e),
            None => e.push(0x00),
        }
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

impl Encode for Float32<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x00);
    }
}

impl Encode for Float64<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x00);
    }
}
