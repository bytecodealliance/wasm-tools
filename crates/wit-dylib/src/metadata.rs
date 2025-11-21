use indexmap::IndexMap;
use std::mem;
use wasm_encoder::{Function, MemArg};
use wit_parser::TypeId;

const VERSION: u32 = 2;

#[derive(Default)]
pub struct Metadata {
    pub import_funcs: Vec<ImportFunc>,
    pub export_funcs: Vec<ExportFunc>,
    pub resources: Vec<Resource>,
    pub records: Vec<Record>,
    pub flags: Vec<Flags>,
    pub tuples: Vec<Tuple>,
    pub variants: Vec<Variant>,
    pub enums: Vec<Enum>,
    pub options: Vec<WitOption>,
    pub results: Vec<WitResult>,
    pub lists: Vec<List>,
    pub fixed_size_lists: Vec<FixedSizeList>,
    pub futures: Vec<Future>,
    pub streams: Vec<Stream>,
    pub aliases: Vec<Alias>,
}

pub struct ImportFunc {
    pub interface: Option<String>,
    pub name: String,
    pub sync_import_elem_index: Option<u32>,
    pub async_import_elem_index: Option<u32>,
    pub async_import_lift_results_elem_index: Option<u32>,
    pub args: Vec<Type>,
    pub result: Option<Type>,
    pub async_abi_area: Option<(usize, usize)>,
}

pub struct ExportFunc {
    pub interface: Option<String>,
    pub name: String,
    pub async_export_task_return_elem_index: Option<u32>,
    pub args: Vec<Type>,
    pub result: Option<Type>,
}

pub struct Resource {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: String,
    pub drop_elem_index: u32,
    pub new_elem_index: Option<u32>,
    pub rep_elem_index: Option<u32>,
}

pub struct Record {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

pub struct Flags {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: String,
    pub names: Vec<String>,
}

pub struct Tuple {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: Option<String>,
    pub types: Vec<Type>,
}

pub struct Variant {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: String,
    pub cases: Vec<(String, Option<Type>)>,
}

pub struct Enum {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: String,
    pub names: Vec<String>,
}

pub struct WitOption {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: Option<String>,
    pub ty: Type,
}

pub struct WitResult {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: Option<String>,
    pub ok: Option<Type>,
    pub err: Option<Type>,
}

pub struct List {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: Option<String>,
    pub ty: Type,
}

pub struct FixedSizeList {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: Option<String>,
    pub len: u32,
    pub ty: Type,
}

pub struct Future {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: Option<String>,
    pub ty: Option<Type>,
    pub new_elem_index: u32,
    pub read_elem_index: u32,
    pub write_elem_index: u32,
    pub cancel_read_elem_index: u32,
    pub cancel_write_elem_index: u32,
    pub drop_readable_elem_index: u32,
    pub drop_writable_elem_index: u32,
    pub lift_elem_index: Option<u32>,
    pub lower_elem_index: Option<u32>,
    pub abi_payload_size: usize,
    pub abi_payload_align: usize,
}

pub struct Stream {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: Option<String>,
    pub ty: Option<Type>,
    pub new_elem_index: u32,
    pub read_elem_index: u32,
    pub write_elem_index: u32,
    pub cancel_read_elem_index: u32,
    pub cancel_write_elem_index: u32,
    pub drop_readable_elem_index: u32,
    pub drop_writable_elem_index: u32,
    pub lift_elem_index: Option<u32>,
    pub lower_elem_index: Option<u32>,
    pub abi_payload_size: usize,
    pub abi_payload_align: usize,
}

pub struct Alias {
    pub id: TypeId,
    pub interface: Option<String>,
    pub name: String,
    pub ty: Type,
}

#[derive(Copy, Clone)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    S8,
    S16,
    S32,
    S64,
    Bool,
    Char,
    F32,
    F64,
    String,
    ErrorContext,
    Record(usize),
    Own(usize),
    Borrow(usize),
    Flags(usize),
    Tuple(usize),
    Variant(usize),
    Enum(usize),
    Option(usize),
    Result(usize),
    List(usize),
    FixedSizeList(usize),
    Future(usize),
    Stream(usize),
    Alias(usize),
}

struct Encoder {
    data: Vec<u8>,
    table_base: u32,
    memory_base: u32,
    relocs: Vec<Reloc>,
    strings: IndexMap<String, SymbolId>,
    symbol_offsets: Vec<usize>,
}

struct Reloc {
    sym: SymbolId,
    offset: usize,
    addend: usize,
    kind: RelocKind,
}

enum RelocKind {
    Data,
    Table,
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct SymbolId(usize);

impl SymbolId {
    const TABLE: SymbolId = SymbolId(usize::MAX);
}

impl Metadata {
    pub fn encode(&self, table_base: u32, memory_base: u32) -> (u32, Vec<u8>, Option<Function>) {
        let mut encoder = Encoder {
            data: Vec::new(),
            table_base,
            memory_base,
            relocs: Vec::new(),
            strings: IndexMap::new(),
            symbol_offsets: Vec::new(),
        };

        let import_funcs = encoder.encode_list(&self.import_funcs, Encoder::encode_import_funcs);
        let export_funcs = encoder.encode_list(&self.export_funcs, Encoder::encode_export_funcs);
        let resources = encoder.encode_list(&self.resources, Encoder::encode_resources);
        let records = encoder.encode_list(&self.records, Encoder::encode_records);
        let flags = encoder.encode_list(&self.flags, Encoder::encode_flags);
        let tuples = encoder.encode_list(&self.tuples, Encoder::encode_tuples);
        let variants = encoder.encode_list(&self.variants, Encoder::encode_variants);
        let enums = encoder.encode_list(&self.enums, Encoder::encode_enums);
        let options = encoder.encode_list(&self.options, Encoder::encode_options);
        let results = encoder.encode_list(&self.results, Encoder::encode_results);
        let lists = encoder.encode_list(&self.lists, Encoder::encode_lists);
        let fixed_size_lists =
            encoder.encode_list(&self.fixed_size_lists, Encoder::encode_fixed_size_lists);
        let futures = encoder.encode_list(&self.futures, Encoder::encode_futures);
        let streams = encoder.encode_list(&self.streams, Encoder::encode_streams);
        let aliases = encoder.encode_list(&self.aliases, Encoder::encode_aliases);

        let sym_metadata = encoder.symbol();
        encoder.bind(sym_metadata);
        encoder.put_u32(VERSION);
        for (sym, len) in [
            import_funcs,
            export_funcs,
            resources,
            records,
            flags,
            tuples,
            variants,
            enums,
            options,
            results,
            lists,
            fixed_size_lists,
            futures,
            streams,
            aliases,
        ] {
            encoder.put_usize(len);
            if len > 0 {
                encoder.memory_ptr(sym);
            } else {
                encoder.put_usize(0);
            }
        }

        encoder.encode_strings();

        let apply_relocs = encoder.generate_apply_relocs();
        (
            u32::try_from(encoder.symbol_offsets[sym_metadata.0]).unwrap(),
            encoder.finish(),
            apply_relocs,
        )
    }
}

impl Encoder {
    fn encode_list<T>(
        &mut self,
        list: &[T],
        encode: impl Fn(&mut Self, &[T]),
    ) -> (SymbolId, usize) {
        let ret = self.symbol();
        self.bind(ret);
        encode(self, list);
        (ret, list.len())
    }

    fn encode_import_funcs(&mut self, funcs: &[ImportFunc]) {
        let mut deferred_args = Vec::new();
        for func in funcs {
            let ImportFunc {
                interface,
                name,
                sync_import_elem_index,
                async_import_elem_index,
                async_import_lift_results_elem_index,
                args,
                result,
                async_abi_area,
            } = func;
            self.opt_string_ptr(interface.as_deref());
            self.string_ptr(name);
            self.opt_elem_index(*sync_import_elem_index);
            self.opt_elem_index(*async_import_elem_index);
            self.opt_elem_index(*async_import_lift_results_elem_index);
            self.list(args, &mut deferred_args);
            self.opt_ty(result.as_ref());
            match async_abi_area {
                Some((size, align)) => {
                    self.put_usize(*size);
                    self.put_usize(*align);
                }
                None => {
                    self.put_usize(0);
                    self.put_usize(0);
                }
            }
        }

        for (sym, args) in deferred_args {
            self.bind(sym);
            for arg in args {
                self.ty(arg);
            }
        }
    }

    fn encode_export_funcs(&mut self, funcs: &[ExportFunc]) {
        let mut deferred_args = Vec::new();
        for func in funcs {
            let ExportFunc {
                interface,
                name,
                async_export_task_return_elem_index,
                args,
                result,
            } = func;
            self.opt_string_ptr(interface.as_deref());
            self.string_ptr(name);
            self.opt_elem_index(*async_export_task_return_elem_index);
            self.list(args, &mut deferred_args);
            self.opt_ty(result.as_ref());
        }

        for (sym, args) in deferred_args {
            self.bind(sym);
            for arg in args {
                self.ty(arg);
            }
        }
    }

    fn encode_resources(&mut self, resources: &[Resource]) {
        for resource in resources {
            let Resource {
                id: _,
                interface,
                name,
                drop_elem_index,
                new_elem_index,
                rep_elem_index,
            } = resource;
            self.opt_string_ptr(interface.as_deref());
            self.string_ptr(name);
            self.elem_index(*drop_elem_index);
            self.opt_elem_index(*new_elem_index);
            self.opt_elem_index(*rep_elem_index);
        }
    }

    fn encode_records(&mut self, records: &[Record]) {
        let mut deferred_fields = Vec::new();
        for record in records {
            let Record {
                id: _,
                interface,
                name,
                fields,
            } = record;
            self.opt_string_ptr(interface.as_deref());
            self.string_ptr(name);
            self.list(fields, &mut deferred_fields);
        }

        for (sym, fields) in deferred_fields {
            self.bind(sym);
            for (name, ty) in fields {
                self.string_ptr(name);
                self.ty(ty);
            }
        }
    }

    fn encode_flags(&mut self, flags: &[Flags]) {
        let mut deferred = Vec::new();
        for flags in flags {
            let Flags {
                id: _,
                interface,
                name,
                names,
            } = flags;
            self.opt_string_ptr(interface.as_deref());
            self.string_ptr(name);
            self.list(names, &mut deferred);
        }

        for (sym, names) in deferred {
            self.bind(sym);
            for name in names {
                self.string_ptr(name);
            }
        }
    }

    fn encode_tuples(&mut self, tuples: &[Tuple]) {
        let mut deferred = Vec::new();
        for tuple in tuples {
            let Tuple {
                id: _,
                interface,
                name,
                types,
            } = tuple;
            self.opt_string_ptr(interface.as_deref());
            self.opt_string_ptr(name.as_deref());
            self.list(types, &mut deferred);
        }

        for (sym, types) in deferred {
            self.bind(sym);
            for ty in types {
                self.ty(ty);
            }
        }
    }

    fn encode_variants(&mut self, variants: &[Variant]) {
        let mut deferred = Vec::new();
        for variant in variants {
            let Variant {
                id: _,
                interface,
                name,
                cases,
            } = variant;
            self.opt_string_ptr(interface.as_deref());
            self.string_ptr(name);
            self.list(cases, &mut deferred);
        }

        for (sym, cases) in deferred {
            self.bind(sym);
            for (name, ty) in cases {
                self.string_ptr(name);
                self.opt_ty(ty.as_ref());
            }
        }
    }

    fn encode_enums(&mut self, enums: &[Enum]) {
        let mut deferred = Vec::new();
        for e in enums {
            let Enum {
                id: _,
                interface,
                name,
                names,
            } = e;
            self.opt_string_ptr(interface.as_deref());
            self.string_ptr(name);
            self.list(names, &mut deferred);
        }

        for (sym, names) in deferred {
            self.bind(sym);
            for name in names {
                self.string_ptr(name);
            }
        }
    }

    fn encode_options(&mut self, options: &[WitOption]) {
        for option in options {
            let WitOption {
                id: _,
                interface,
                name,
                ty,
            } = option;
            self.opt_string_ptr(interface.as_deref());
            self.opt_string_ptr(name.as_deref());
            self.ty(ty);
        }
    }

    fn encode_results(&mut self, results: &[WitResult]) {
        for result in results {
            let WitResult {
                id: _,
                interface,
                name,
                ok,
                err,
            } = result;
            self.opt_string_ptr(interface.as_deref());
            self.opt_string_ptr(name.as_deref());
            self.opt_ty(ok.as_ref());
            self.opt_ty(err.as_ref());
        }
    }

    fn encode_lists(&mut self, lists: &[List]) {
        for list in lists {
            let List {
                id: _,
                interface,
                name,
                ty,
            } = list;
            self.opt_string_ptr(interface.as_deref());
            self.opt_string_ptr(name.as_deref());
            self.ty(ty);
        }
    }

    fn encode_fixed_size_lists(&mut self, lists: &[FixedSizeList]) {
        for list in lists {
            let FixedSizeList {
                id: _,
                interface,
                name,
                len,
                ty,
            } = list;
            self.opt_string_ptr(interface.as_deref());
            self.opt_string_ptr(name.as_deref());
            self.put_u32(*len);
            self.ty(ty);
        }
    }

    fn encode_futures(&mut self, futures: &[Future]) {
        for future in futures {
            let Future {
                id: _,
                interface,
                name,
                ty,
                new_elem_index,
                read_elem_index,
                write_elem_index,
                cancel_read_elem_index,
                cancel_write_elem_index,
                drop_readable_elem_index,
                drop_writable_elem_index,
                lift_elem_index,
                lower_elem_index,
                abi_payload_size,
                abi_payload_align,
            } = future;
            self.opt_string_ptr(interface.as_deref());
            self.opt_string_ptr(name.as_deref());
            self.opt_ty(ty.as_ref());
            self.elem_index(*new_elem_index);
            self.elem_index(*read_elem_index);
            self.elem_index(*write_elem_index);
            self.elem_index(*cancel_read_elem_index);
            self.elem_index(*cancel_write_elem_index);
            self.elem_index(*drop_readable_elem_index);
            self.elem_index(*drop_writable_elem_index);
            self.opt_elem_index(*lift_elem_index);
            self.opt_elem_index(*lower_elem_index);
            self.put_usize(*abi_payload_size);
            self.put_usize(*abi_payload_align);
        }
    }

    fn encode_streams(&mut self, streams: &[Stream]) {
        for stream in streams {
            let Stream {
                id: _,
                interface,
                name,
                ty,
                new_elem_index,
                read_elem_index,
                write_elem_index,
                cancel_read_elem_index,
                cancel_write_elem_index,
                drop_readable_elem_index,
                drop_writable_elem_index,
                lift_elem_index,
                lower_elem_index,
                abi_payload_size,
                abi_payload_align,
            } = stream;
            self.opt_string_ptr(interface.as_deref());
            self.opt_string_ptr(name.as_deref());
            self.opt_ty(ty.as_ref());
            self.elem_index(*new_elem_index);
            self.elem_index(*read_elem_index);
            self.elem_index(*write_elem_index);
            self.elem_index(*cancel_read_elem_index);
            self.elem_index(*cancel_write_elem_index);
            self.elem_index(*drop_readable_elem_index);
            self.elem_index(*drop_writable_elem_index);
            self.opt_elem_index(*lift_elem_index);
            self.opt_elem_index(*lower_elem_index);
            self.put_usize(*abi_payload_size);
            self.put_usize(*abi_payload_align);
        }
    }

    fn encode_aliases(&mut self, aliases: &[Alias]) {
        for alias in aliases {
            let Alias {
                id: _,
                interface,
                name,
                ty,
            } = alias;
            self.opt_string_ptr(interface.as_deref());
            self.string_ptr(name);
            self.ty(ty);
        }
    }

    fn encode_strings(&mut self) {
        for (string, sym) in mem::take(&mut self.strings) {
            self.bind(sym);
            self.data.extend_from_slice(string.as_bytes());
            self.data.push(0);
        }
    }

    /// Creates a new, as yet unresolved, symbol.
    ///
    /// Suitable to pass to `memory_ptr` but must be `bind`-ed at some point
    /// too.
    fn symbol(&mut self) -> SymbolId {
        let ret = SymbolId(self.symbol_offsets.len());
        self.symbol_offsets.push(usize::MAX);
        ret
    }

    /// Indicate that `sym` belongs at the current encoding offset.
    ///
    /// Cannot bind a symbol twice.
    fn bind(&mut self, sym: SymbolId) {
        assert_eq!(self.symbol_offsets[sym.0], usize::MAX);
        self.symbol_offsets[sym.0] = self.data.len();
    }

    fn put_u32(&mut self, value: u32) {
        self.data.extend_from_slice(&value.to_le_bytes());
    }

    fn put_usize(&mut self, value: usize) {
        self.put_u32(value.try_into().unwrap());
    }

    /// Encodes a `list` at this location as a size/ptr combo.
    ///
    /// If the `list` is non-empty then the actual contents are pushed to
    /// `deferred` with a symbol that should be bound to where the list starts.
    fn list<'a, T>(&mut self, list: &'a [T], deferred: &mut Vec<(SymbolId, &'a [T])>) {
        self.put_usize(list.len());
        if list.is_empty() {
            self.put_usize(0);
        } else {
            let sym = self.symbol();
            deferred.push((sym, list));
            self.memory_ptr(sym);
        }
    }

    /// Encodes a pointer to `sym` at the current location.
    fn memory_ptr(&mut self, sym: SymbolId) {
        self.add_reloc(sym, 0, RelocKind::Data);
        self.put_u32(0);
    }

    fn opt_elem_index(&mut self, value: Option<u32>) {
        match value {
            Some(name) => self.elem_index(name),
            None => self.put_u32(0),
        }
    }

    fn elem_index(&mut self, value: u32) {
        self.add_reloc(SymbolId::TABLE, value.try_into().unwrap(), RelocKind::Table);
        self.put_u32(0);
    }

    /// Encodes an optional string pointer at the current location.
    fn opt_string_ptr(&mut self, value: Option<&str>) {
        match value {
            Some(name) => self.string_ptr(name),
            None => self.put_u32(0),
        }
    }

    /// Encodes a string pointer at the current location.
    fn string_ptr(&mut self, value: &str) {
        let string_sym = if self.strings.contains_key(value) {
            self.strings[value]
        } else {
            let sym = self.symbol();
            self.strings.insert(value.to_string(), sym);
            sym
        };
        self.memory_ptr(string_sym);
    }

    fn ty(&mut self, ty: &Type) {
        let index = |discr: u32, index: &usize| {
            let index = u32::try_from(*index).unwrap();
            assert_eq!(index << 8 >> 8, index);
            (index << 8) | discr
        };
        let val = match ty {
            Type::U8 => 0,
            Type::U16 => 1,
            Type::U32 => 2,
            Type::U64 => 3,
            Type::S8 => 4,
            Type::S16 => 5,
            Type::S32 => 6,
            Type::S64 => 7,
            Type::Bool => 8,
            Type::Char => 9,
            Type::F32 => 10,
            Type::F64 => 11,
            Type::String => 12,
            Type::ErrorContext => 13,
            Type::Record(i) => index(14, i),
            Type::Own(i) => index(15, i),
            Type::Borrow(i) => index(16, i),
            Type::Flags(i) => index(17, i),
            Type::Tuple(i) => index(18, i),
            Type::Variant(i) => index(19, i),
            Type::Enum(i) => index(20, i),
            Type::Option(i) => index(21, i),
            Type::Result(i) => index(22, i),
            Type::List(i) => index(23, i),
            Type::FixedSizeList(i) => index(24, i),
            Type::Future(i) => index(25, i),
            Type::Stream(i) => index(26, i),
            Type::Alias(i) => index(27, i),
        };
        self.put_u32(val);
    }

    fn opt_ty(&mut self, ty: Option<&Type>) {
        match ty {
            Some(ty) => self.ty(ty),
            None => self.put_u32(u32::MAX),
        }
    }

    fn add_reloc(&mut self, sym: SymbolId, addend: usize, kind: RelocKind) {
        self.relocs.push(Reloc {
            sym,
            offset: self.data.len(),
            addend,
            kind,
        });
    }

    fn generate_apply_relocs(&mut self) -> Option<Function> {
        if self.relocs.is_empty() {
            return None;
        }
        let mut func = Function::new([]);
        let mut ins = func.instructions();
        for reloc in self.relocs.iter() {
            let addend_i32 = u32::try_from(reloc.addend).unwrap() as i32;

            ins.global_get(self.memory_base);
            match reloc.kind {
                RelocKind::Data => {
                    ins.global_get(self.memory_base);
                    let offset = self.symbol_offsets[reloc.sym.0];
                    assert!(
                        offset != usize::MAX,
                        "failed to bind symbol {}",
                        reloc.sym.0,
                    );

                    let sym_i32 = u32::try_from(offset).unwrap() as i32;
                    ins.i32_const(sym_i32);
                    ins.i32_add();
                }
                RelocKind::Table => {
                    ins.global_get(self.table_base);
                    assert_eq!(reloc.sym, SymbolId::TABLE);
                }
            }
            if addend_i32 != 0 {
                ins.i32_const(addend_i32);
                ins.i32_add();
            }
            ins.i32_store(MemArg {
                align: 2,
                memory_index: 0,
                offset: u64::try_from(reloc.offset).unwrap(),
            });
        }
        ins.end();
        Some(func)
    }

    fn finish(self) -> Vec<u8> {
        self.data
    }
}
