use anyhow::{bail, Result};
use std::fmt::Write as _;
use std::io::Write;
use wasmparser::*;

pub fn dump_wasm(bytes: &[u8]) -> Result<String> {
    let mut dst = vec![];
    {
        let mut d = Dump::new(bytes, &mut dst);
        d.run()?;
    }
    Ok(String::from_utf8(dst).unwrap())
}

pub fn dump_wasm_into(bytes: &[u8], into: impl Write) -> Result<()> {
    let mut d = Dump::new(bytes, into);
    d.run()?;
    Ok(())
}

struct Dump<'a> {
    bytes: &'a [u8],
    cur: usize,
    state: String,
    dst: Box<dyn Write + 'a>,
    nesting: u32,
    offset_width: usize,
}

#[derive(Default)]
struct Indices {
    // Core module indexes
    core_types: u32,
    core_funcs: u32,
    core_globals: u32,
    core_tables: u32,
    core_memories: u32,
    core_tags: u32,
    core_modules: u32,
    core_instances: u32,

    // Component indexes
    types: u32,
    funcs: u32,
    components: u32,
    instances: u32,
    values: u32,
}

enum ComponentTypeKind {
    Func,
    Component,
    Instance,
    DefinedType,
}

const NBYTES: usize = 4;

impl<'a> Dump<'a> {
    fn new(bytes: &'a [u8], dst: impl Write + 'a) -> Dump<'a> {
        Dump {
            bytes,
            cur: 0,
            nesting: 0,
            state: String::new(),
            dst: Box::new(dst) as _,
            offset_width: format!("{:x}", bytes.len()).len() + 1,
        }
    }

    fn run(&mut self) -> Result<()> {
        self.print_module()?;
        assert_eq!(self.cur, self.bytes.len());
        Ok(())
    }

    fn print_module(&mut self) -> Result<()> {
        let mut stack = Vec::new();
        let mut i = Indices::default();
        let mut component_types = Vec::new();
        self.nesting += 1;

        for item in Parser::new(0).parse_all(self.bytes) {
            match item? {
                Payload::Version {
                    num,
                    encoding,
                    range,
                } => {
                    write!(self.state, "version {} ({:?})", num, encoding)?;
                    self.print(range.end)?;
                }
                Payload::TypeSection(s) => self.section(s, "type", |me, end, t| {
                    write!(me.state, "[type {}] {:?}", inc(&mut i.core_types), t)?;
                    me.print(end)
                })?,
                Payload::ImportSection(s) => self.section(s, "import", |me, end, imp| {
                    write!(me.state, "import ")?;
                    match imp.ty {
                        TypeRef::Func(_) => write!(me.state, "[func {}]", inc(&mut i.core_funcs))?,
                        TypeRef::Memory(_) => {
                            write!(me.state, "[memory {}]", inc(&mut i.core_memories))?
                        }
                        TypeRef::Tag(_) => write!(me.state, "[tag {}]", inc(&mut i.core_tags))?,
                        TypeRef::Table(_) => {
                            write!(me.state, "[table {}]", inc(&mut i.core_tables))?
                        }
                        TypeRef::Global(_) => {
                            write!(me.state, "[global {}]", inc(&mut i.core_globals))?
                        }
                    }
                    write!(me.state, " {:?}", imp)?;
                    me.print(end)
                })?,
                Payload::FunctionSection(s) => {
                    let mut cnt = i.core_funcs;
                    self.section(s, "func", |me, end, f| {
                        write!(me.state, "[func {}] type {:?}", inc(&mut cnt), f)?;
                        me.print(end)
                    })?
                }
                Payload::TableSection(s) => self.section(s, "table", |me, end, t| {
                    write!(me.state, "[table {}] {:?}", inc(&mut i.core_tables), t)?;
                    me.print(end)
                })?,
                Payload::MemorySection(s) => self.section(s, "memory", |me, end, m| {
                    write!(me.state, "[memory {}] {:?}", inc(&mut i.core_memories), m)?;
                    me.print(end)
                })?,
                Payload::TagSection(s) => self.section(s, "tag", |me, end, m| {
                    write!(me.state, "[tag {}] {:?}", inc(&mut i.core_tags), m)?;
                    me.print(end)
                })?,
                Payload::ExportSection(s) => self.section(s, "export", |me, end, e| {
                    write!(me.state, "export {:?}", e)?;
                    me.print(end)
                })?,
                Payload::GlobalSection(s) => self.section(s, "global", |me, _end, g| {
                    write!(me.state, "[global {}] {:?}", inc(&mut i.core_globals), g.ty)?;
                    me.print(g.init_expr.get_binary_reader().original_position())?;
                    me.print_ops(g.init_expr.get_operators_reader())
                })?,
                Payload::StartSection { func, range } => {
                    write!(self.state, "start section")?;
                    self.print(range.start)?;
                    write!(self.state, "start function {}", func)?;
                    self.print(range.end)?;
                }
                Payload::DataCountSection { count, range } => {
                    write!(self.state, "data count section")?;
                    self.print(range.start)?;
                    write!(self.state, "data count {}", count)?;
                    self.print(range.end)?;
                }
                Payload::ElementSection(s) => self.section(s, "element", |me, _end, i| {
                    write!(me.state, "element {:?}", i.ty)?;
                    let mut items = i.items.get_items_reader()?;
                    match i.kind {
                        ElementKind::Passive => {
                            write!(me.state, " passive, {} items", items.get_count())?;
                        }
                        ElementKind::Active {
                            table_index,
                            offset_expr,
                        } => {
                            write!(me.state, " table[{}]", table_index)?;
                            me.print(offset_expr.get_binary_reader().original_position())?;
                            me.print_ops(offset_expr.get_operators_reader())?;
                            write!(me.state, "{} items", items.get_count())?;
                        }
                        ElementKind::Declared => {
                            write!(me.state, " declared {} items", items.get_count())?;
                        }
                    }
                    me.print(items.original_position())?;
                    for _ in 0..items.get_count() {
                        let item = items.read()?;
                        write!(me.state, "item {:?}", item)?;
                        me.print(items.original_position())?;
                    }
                    Ok(())
                })?,

                Payload::DataSection(s) => self.section(s, "data", |me, end, i| {
                    match i.kind {
                        DataKind::Passive => {
                            write!(me.state, "data passive")?;
                            me.print(end - i.data.len())?;
                        }
                        DataKind::Active {
                            memory_index,
                            offset_expr,
                        } => {
                            write!(me.state, "data memory[{}]", memory_index)?;
                            me.print(offset_expr.get_binary_reader().original_position())?;
                            me.print_ops(offset_expr.get_operators_reader())?;
                        }
                    }
                    me.print_byte_header()?;
                    for _ in 0..NBYTES {
                        write!(me.dst, "---")?;
                    }
                    writeln!(me.dst, "-| ... {} bytes of data", i.data.len())?;
                    me.cur = end;
                    Ok(())
                })?,

                Payload::CodeSectionStart { count, range, size } => {
                    write!(self.state, "code section")?;
                    self.print(range.start)?;
                    write!(self.state, "{} count", count)?;
                    self.print(range.end - size as usize)?;
                }

                Payload::CodeSectionEntry(body) => {
                    writeln!(
                        self.dst,
                        "============== func {} ====================",
                        inc(&mut i.core_funcs),
                    )?;
                    write!(self.state, "size of function")?;
                    self.print(body.get_binary_reader().original_position())?;
                    let mut locals = body.get_locals_reader()?;
                    write!(self.state, "{} local blocks", locals.get_count())?;
                    self.print(locals.original_position())?;
                    for _ in 0..locals.get_count() {
                        let (amt, ty) = locals.read()?;
                        write!(self.state, "{} locals of type {:?}", amt, ty)?;
                        self.print(locals.original_position())?;
                    }
                    self.print_ops(body.get_operators_reader()?)?;
                }

                // Component sections
                Payload::ModuleSection { range, .. } => {
                    write!(
                        self.state,
                        "[core module {}] inline size",
                        inc(&mut i.core_modules)
                    )?;
                    self.print(range.start)?;
                    self.nesting += 1;
                    stack.push(i);
                    i = Indices::default();
                }

                Payload::InstanceSection(s) => self.section(s, "core instance", |me, end, e| {
                    write!(
                        me.state,
                        "[core instance {}] {:?}",
                        inc(&mut i.core_instances),
                        e
                    )?;
                    me.print(end)
                })?,

                Payload::AliasSection(s) => self.section(s, "core alias", |me, end, a| {
                    let (kind, num) = match a {
                        Alias::InstanceExport { kind, .. } => match kind {
                            ExternalKind::Func => ("func", inc(&mut i.core_funcs)),
                            ExternalKind::Table => ("table", inc(&mut i.core_tables)),
                            ExternalKind::Memory => ("memory", inc(&mut i.core_memories)),
                            ExternalKind::Global => ("global", inc(&mut i.core_globals)),
                            ExternalKind::Tag => ("tag", inc(&mut i.core_tags)),
                        },
                        Alias::Outer { kind, .. } => match kind {
                            OuterAliasKind::Type => ("type", inc(&mut i.core_types)),
                        },
                    };
                    write!(me.state, "core alias [{} {}] {:?}", kind, num, a)?;
                    me.print(end)
                })?,

                Payload::CoreTypeSection(s) => self.section(s, "core type", |me, end, t| {
                    write!(me.state, "[core type {}] {:?}", inc(&mut i.core_types), t)?;
                    me.print(end)
                })?,

                Payload::ComponentSection { range, .. } => {
                    write!(
                        self.state,
                        "[component {}] inline size",
                        inc(&mut i.components)
                    )?;
                    self.print(range.start)?;
                    self.nesting += 1;
                    stack.push(i);
                    i = Indices::default();
                }

                Payload::ComponentInstanceSection(s) => {
                    self.section(s, "component instance", |me, end, e| {
                        write!(me.state, "[instance {}] {:?}", inc(&mut i.instances), e)?;
                        me.print(end)
                    })?
                }

                Payload::ComponentAliasSection(s) => {
                    self.section(s, "component alias", |me, end, a| {
                        let (kind, num) = match a {
                            ComponentAlias::InstanceExport {
                                kind: ComponentExternalKind::Module,
                                ..
                            }
                            | ComponentAlias::Outer {
                                kind: ComponentOuterAliasKind::CoreModule,
                                ..
                            } => ("module", inc(&mut i.core_modules)),
                            ComponentAlias::Outer {
                                kind: ComponentOuterAliasKind::CoreType,
                                ..
                            } => ("core type", inc(&mut i.core_types)),
                            ComponentAlias::InstanceExport {
                                kind: ComponentExternalKind::Func,
                                ..
                            } => ("func", inc(&mut i.funcs)),
                            ComponentAlias::InstanceExport {
                                kind: ComponentExternalKind::Value,
                                ..
                            } => ("value", inc(&mut i.values)),
                            ComponentAlias::InstanceExport {
                                kind: ComponentExternalKind::Type,
                                ..
                            }
                            | ComponentAlias::Outer {
                                kind: ComponentOuterAliasKind::Type,
                                ..
                            } => ("type", inc(&mut i.types)),
                            ComponentAlias::InstanceExport {
                                kind: ComponentExternalKind::Instance,
                                ..
                            } => ("instance", inc(&mut i.instances)),
                            ComponentAlias::InstanceExport {
                                kind: ComponentExternalKind::Component,
                                ..
                            }
                            | ComponentAlias::Outer {
                                kind: ComponentOuterAliasKind::Component,
                                ..
                            } => ("component", inc(&mut i.components)),
                        };

                        write!(me.state, "alias [{} {}] {:?}", kind, num, a)?;
                        me.print(end)
                    })?
                }

                Payload::ComponentTypeSection(s) => {
                    self.section(s, "component type", |me, end, t| {
                        write!(me.state, "[type {}] {:?}", inc(&mut i.types), t)?;
                        component_types.push(match t {
                            ComponentType::Defined(_) => ComponentTypeKind::DefinedType,
                            ComponentType::Func(_) => ComponentTypeKind::Func,
                            ComponentType::Component(_) => ComponentTypeKind::Component,
                            ComponentType::Instance(_) => ComponentTypeKind::Instance,
                        });
                        me.print(end)
                    })?
                }

                Payload::ComponentImportSection(s) => {
                    self.section(s, "component import", |me, end, item| {
                        let (desc, idx) = match item.ty {
                            ComponentTypeRef::Module(..) => ("module", inc(&mut i.core_modules)),
                            ComponentTypeRef::Func(..) => ("func", inc(&mut i.funcs)),
                            ComponentTypeRef::Value(..) => ("value", inc(&mut i.values)),
                            ComponentTypeRef::Type(..) => ("type", inc(&mut i.types)),
                            ComponentTypeRef::Instance(..) => ("instance", inc(&mut i.instances)),
                            ComponentTypeRef::Component(..) => {
                                ("component", inc(&mut i.components))
                            }
                        };
                        write!(me.state, "[{desc} {idx}] {item:?}")?;
                        me.print(end)
                    })?
                }

                Payload::ComponentCanonicalSection(s) => {
                    self.section(s, "canonical function", |me, end, f| {
                        let (name, col) = match &f {
                            CanonicalFunction::Lift { .. } => ("func", &mut i.funcs),
                            CanonicalFunction::Lower { .. } => ("core func", &mut i.core_funcs),
                        };

                        write!(me.state, "[{} {}] {:?}", name, inc(col), f)?;
                        me.print(end)
                    })?
                }

                Payload::ComponentExportSection(s) => {
                    self.section(s, "component export", |me, end, e| {
                        write!(me.state, "export {:?}", e)?;
                        me.print(end)
                    })?
                }

                Payload::ComponentStartSection(mut s) => {
                    write!(self.state, "start section")?;
                    self.print(s.range().start)?;
                    write!(self.state, "{:?}", s.read()?)?;
                    self.print(s.range().end)?;
                }

                Payload::CustomSection(c) => {
                    write!(self.state, "custom section")?;
                    self.print(c.range().start)?;
                    write!(self.state, "name: {:?}", c.name())?;
                    self.print(c.data_offset())?;
                    if c.name() == "name" {
                        let mut iter = NameSectionReader::new(c.data(), c.data_offset())?;
                        while !iter.eof() {
                            self.print_custom_name_section(iter.read()?, iter.original_position())?;
                        }
                    } else {
                        self.print_byte_header()?;
                        for _ in 0..NBYTES {
                            write!(self.dst, "---")?;
                        }
                        writeln!(self.dst, "-| ... {} bytes of data", c.data().len())?;
                        self.cur += c.data().len();
                    }
                }
                Payload::UnknownSection {
                    id,
                    range,
                    contents,
                } => {
                    write!(self.state, "unknown section: {}", id)?;
                    self.print(range.start)?;
                    self.print_byte_header()?;
                    for _ in 0..NBYTES {
                        write!(self.dst, "---")?;
                    }
                    writeln!(self.dst, "-| ... {} bytes of data", contents.len())?;
                    self.cur += contents.len();
                }
                Payload::End(_) => {
                    self.nesting -= 1;
                    if self.nesting > 0 {
                        i = stack.pop().unwrap();
                    }
                }
            }
        }

        Ok(())
    }

    fn print_name_map(&mut self, thing: &str, n: NameMap<'_>) -> Result<()> {
        write!(self.state, "{} names", thing)?;
        self.print(n.original_position())?;
        let mut map = n.get_map()?;
        write!(self.state, "{} count", map.get_count())?;
        self.print(map.original_position())?;
        for _ in 0..map.get_count() {
            write!(self.state, "{:?}", map.read()?)?;
            self.print(map.original_position())?;
        }
        Ok(())
    }

    fn print_indirect_name_map(
        &mut self,
        thing_a: &str,
        thing_b: &str,
        n: IndirectNameMap<'_>,
    ) -> Result<()> {
        write!(self.state, "{} names", thing_b)?;
        self.print(n.original_position())?;
        let mut outer_map = n.get_indirect_map()?;
        write!(self.state, "{} count", outer_map.get_indirect_count())?;
        self.print(outer_map.original_position())?;
        for _ in 0..outer_map.get_indirect_count() {
            let inner = outer_map.read()?;
            write!(
                self.state,
                "{} {} {}s",
                thing_a, inner.indirect_index, thing_b,
            )?;
            self.print(inner.original_position())?;
            let mut map = inner.get_map()?;
            write!(self.state, "{} count", map.get_count())?;
            self.print(map.original_position())?;
            for _ in 0..map.get_count() {
                write!(self.state, "{:?}", map.read()?)?;
                self.print(map.original_position())?;
            }
        }
        Ok(())
    }

    fn print_custom_name_section(&mut self, name: Name<'_>, end: usize) -> Result<()> {
        match name {
            Name::Module(n) => {
                write!(self.state, "module name")?;
                self.print(n.original_position())?;
                write!(self.state, "{:?}", n.get_name()?)?;
                self.print(end)?;
            }
            Name::Function(n) => self.print_name_map("function", n)?,
            Name::Local(n) => self.print_indirect_name_map("function", "local", n)?,
            Name::Label(n) => self.print_indirect_name_map("function", "label", n)?,
            Name::Type(n) => self.print_name_map("type", n)?,
            Name::Table(n) => self.print_name_map("table", n)?,
            Name::Memory(n) => self.print_name_map("memory", n)?,
            Name::Global(n) => self.print_name_map("global", n)?,
            Name::Element(n) => self.print_name_map("element", n)?,
            Name::Data(n) => self.print_name_map("data", n)?,
            Name::Unknown { ty, range, .. } => {
                write!(self.state, "unknown names: {}", ty)?;
                self.print(range.start)?;
                self.print(end)?;
            }
        }
        Ok(())
    }

    fn section<T>(
        &mut self,
        iter: T,
        name: &str,
        print: impl FnMut(&mut Self, usize, T::Item) -> Result<()>,
    ) -> Result<()>
    where
        T: SectionReader + SectionWithLimitedItems,
    {
        write!(self.state, "{} section", name)?;
        self.print(iter.range().start)?;
        self.print_iter(iter, print)
    }

    fn print_iter<T>(
        &mut self,
        mut iter: T,
        mut print: impl FnMut(&mut Self, usize, T::Item) -> Result<()>,
    ) -> Result<()>
    where
        T: SectionReader + SectionWithLimitedItems,
    {
        write!(self.state, "{} count", iter.get_count())?;
        self.print(iter.original_position())?;
        for _ in 0..iter.get_count() {
            let item = iter.read()?;
            print(self, iter.original_position(), item)?;
        }
        if !iter.eof() {
            bail!("too many bytes in section");
        }
        Ok(())
    }

    fn print_ops(&mut self, mut i: OperatorsReader) -> Result<()> {
        while !i.eof() {
            match i.visit_with_offset(self) {
                Ok(()) => {}
                Err(_) => write!(self.state, "??")?,
            }
            self.print(i.original_position())?;
        }
        Ok(())
    }

    fn print(&mut self, end: usize) -> Result<()> {
        assert!(
            self.cur < end,
            "{:#x} >= {:#x}\ntrying to print: {}",
            self.cur,
            end,
            self.state,
        );
        let bytes = &self.bytes[self.cur..end];
        self.print_byte_header()?;
        for (i, chunk) in bytes.chunks(NBYTES).enumerate() {
            if i > 0 {
                for _ in 0..self.nesting - 1 {
                    write!(self.dst, "  ")?;
                }
                for _ in 0..self.offset_width {
                    write!(self.dst, " ")?;
                }
                write!(self.dst, "   |")?;
            }
            for j in 0..NBYTES {
                match chunk.get(j) {
                    Some(b) => write!(self.dst, " {:02x}", b)?,
                    None => write!(self.dst, "   ")?,
                }
            }
            if i == 0 {
                write!(self.dst, " | ")?;
                write!(self.dst, "{}", &self.state)?;
                self.state.truncate(0);
            }
            writeln!(self.dst)?;
        }
        self.cur = end;
        Ok(())
    }

    fn print_byte_header(&mut self) -> Result<()> {
        for _ in 0..self.nesting - 1 {
            write!(self.dst, "  ")?;
        }
        write!(
            self.dst,
            "{:#width$x} |",
            self.cur,
            width = self.offset_width + 2
        )?;
        Ok(())
    }
}

fn inc(spot: &mut u32) -> u32 {
    let ret = *spot;
    *spot += 1;
    ret
}

macro_rules! dump {
    ($(fn $name:ident(&mut self, offset: usize $(, $arg:ident: $argty:ty)* $(,)?))*) => {
        $(
            fn $name(&mut self, _offset: usize $(, $arg: $argty)*) {
                write!(
                    self.state,
                    concat!(
                        "{}"
                        $(, " ", stringify!($arg), ":{:?}")*
                    ),
                    stringify!($name).strip_prefix("visit_").unwrap(),
                    $($arg,)*
                ).unwrap();
            }
        )*
    }
}

impl<'a> VisitOperator<'a> for Dump<'_> {
    type Output = ();
    dump! {
        fn visit_nop(&mut self, offset: usize)
        fn visit_unreachable(&mut self, offset: usize)
        fn visit_block(&mut self, offset: usize, ty: BlockType)
        fn visit_loop(&mut self, offset: usize, ty: BlockType)
        fn visit_if(&mut self, offset: usize, ty: BlockType)
        fn visit_else(&mut self, offset: usize)
        fn visit_try(&mut self, offset: usize, ty: BlockType)
        fn visit_catch(&mut self, offset: usize, index: u32)
        fn visit_throw(&mut self, offset: usize, index: u32)
        fn visit_rethrow(&mut self, offset: usize, relative_depth: u32)
        fn visit_delegate(&mut self, offset: usize, relative_depth: u32)
        fn visit_catch_all(&mut self, offset: usize)
        fn visit_end(&mut self, offset: usize)
        fn visit_br(&mut self, offset: usize, relative_depth: u32)
        fn visit_br_if(&mut self, offset: usize, relative_depth: u32)
        fn visit_br_table(&mut self, offset: usize, table: &BrTable<'a>)
        fn visit_return(&mut self, offset: usize)
        fn visit_call(&mut self, offset: usize, function_index: u32)
        fn visit_return_call(&mut self, offset: usize, function_index: u32)
        fn visit_call_indirect(
            &mut self,
            offset: usize,
            index: u32,
            table_index: u32,
            table_byte: u8,
        )
        fn visit_return_call_indirect(
            &mut self,
            offset: usize,
            index: u32,
            table_index: u32,
        )
        fn visit_drop(&mut self, offset: usize)
        fn visit_select(&mut self, offset: usize)
        fn visit_typed_select(&mut self, offset: usize, ty: ValType)
        fn visit_local_get(&mut self, offset: usize, local_index: u32)
        fn visit_local_set(&mut self, offset: usize, local_index: u32)
        fn visit_local_tee(&mut self, offset: usize, local_index: u32)
        fn visit_global_get(&mut self, offset: usize, global_index: u32)
        fn visit_global_set(&mut self, offset: usize, global_index: u32)
        fn visit_i32_load(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_load(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_f32_load(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_f64_load(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_load8_s(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_load8_u(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_load16_s(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_load16_u(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_load8_s(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_load8_u(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_load16_s(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_load16_u(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_load32_s(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_load32_u(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_store(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_store(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_f32_store(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_f64_store(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_store8(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_store16(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_store8(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_store16(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_store32(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_memory_size(&mut self, offset: usize, mem: u32, mem_byte: u8)
        fn visit_memory_grow(&mut self, offset: usize, mem: u32, mem_byte: u8)
        fn visit_i32_const(&mut self, offset: usize, value: i32)
        fn visit_i64_const(&mut self, offset: usize, value: i64)
        fn visit_f32_const(&mut self, offset: usize, value: Ieee32)
        fn visit_f64_const(&mut self, offset: usize, value: Ieee64)
        fn visit_i32_eqz(&mut self, offset: usize)
        fn visit_i32_eq(&mut self, offset: usize)
        fn visit_i32_ne(&mut self, offset: usize)
        fn visit_i32_lt_s(&mut self, offset: usize)
        fn visit_i32_lt_u(&mut self, offset: usize)
        fn visit_i32_gt_s(&mut self, offset: usize)
        fn visit_i32_gt_u(&mut self, offset: usize)
        fn visit_i32_le_s(&mut self, offset: usize)
        fn visit_i32_le_u(&mut self, offset: usize)
        fn visit_i32_ge_s(&mut self, offset: usize)
        fn visit_i32_ge_u(&mut self, offset: usize)
        fn visit_i64_eqz(&mut self, offset: usize)
        fn visit_i64_eq(&mut self, offset: usize)
        fn visit_i64_ne(&mut self, offset: usize)
        fn visit_i64_lt_s(&mut self, offset: usize)
        fn visit_i64_lt_u(&mut self, offset: usize)
        fn visit_i64_gt_s(&mut self, offset: usize)
        fn visit_i64_gt_u(&mut self, offset: usize)
        fn visit_i64_le_s(&mut self, offset: usize)
        fn visit_i64_le_u(&mut self, offset: usize)
        fn visit_i64_ge_s(&mut self, offset: usize)
        fn visit_i64_ge_u(&mut self, offset: usize)
        fn visit_f32_eq(&mut self, offset: usize)
        fn visit_f32_ne(&mut self, offset: usize)
        fn visit_f32_lt(&mut self, offset: usize)
        fn visit_f32_gt(&mut self, offset: usize)
        fn visit_f32_le(&mut self, offset: usize)
        fn visit_f32_ge(&mut self, offset: usize)
        fn visit_f64_eq(&mut self, offset: usize)
        fn visit_f64_ne(&mut self, offset: usize)
        fn visit_f64_lt(&mut self, offset: usize)
        fn visit_f64_gt(&mut self, offset: usize)
        fn visit_f64_le(&mut self, offset: usize)
        fn visit_f64_ge(&mut self, offset: usize)
        fn visit_i32_clz(&mut self, offset: usize)
        fn visit_i32_ctz(&mut self, offset: usize)
        fn visit_i32_popcnt(&mut self, offset: usize)
        fn visit_i32_add(&mut self, offset: usize)
        fn visit_i32_sub(&mut self, offset: usize)
        fn visit_i32_mul(&mut self, offset: usize)
        fn visit_i32_div_s(&mut self, offset: usize)
        fn visit_i32_div_u(&mut self, offset: usize)
        fn visit_i32_rem_s(&mut self, offset: usize)
        fn visit_i32_rem_u(&mut self, offset: usize)
        fn visit_i32_and(&mut self, offset: usize)
        fn visit_i32_or(&mut self, offset: usize)
        fn visit_i32_xor(&mut self, offset: usize)
        fn visit_i32_shl(&mut self, offset: usize)
        fn visit_i32_shr_s(&mut self, offset: usize)
        fn visit_i32_shr_u(&mut self, offset: usize)
        fn visit_i32_rotl(&mut self, offset: usize)
        fn visit_i32_rotr(&mut self, offset: usize)
        fn visit_i64_clz(&mut self, offset: usize)
        fn visit_i64_ctz(&mut self, offset: usize)
        fn visit_i64_popcnt(&mut self, offset: usize)
        fn visit_i64_add(&mut self, offset: usize)
        fn visit_i64_sub(&mut self, offset: usize)
        fn visit_i64_mul(&mut self, offset: usize)
        fn visit_i64_div_s(&mut self, offset: usize)
        fn visit_i64_div_u(&mut self, offset: usize)
        fn visit_i64_rem_s(&mut self, offset: usize)
        fn visit_i64_rem_u(&mut self, offset: usize)
        fn visit_i64_and(&mut self, offset: usize)
        fn visit_i64_or(&mut self, offset: usize)
        fn visit_i64_xor(&mut self, offset: usize)
        fn visit_i64_shl(&mut self, offset: usize)
        fn visit_i64_shr_s(&mut self, offset: usize)
        fn visit_i64_shr_u(&mut self, offset: usize)
        fn visit_i64_rotl(&mut self, offset: usize)
        fn visit_i64_rotr(&mut self, offset: usize)
        fn visit_f32_abs(&mut self, offset: usize)
        fn visit_f32_neg(&mut self, offset: usize)
        fn visit_f32_ceil(&mut self, offset: usize)
        fn visit_f32_floor(&mut self, offset: usize)
        fn visit_f32_trunc(&mut self, offset: usize)
        fn visit_f32_nearest(&mut self, offset: usize)
        fn visit_f32_sqrt(&mut self, offset: usize)
        fn visit_f32_add(&mut self, offset: usize)
        fn visit_f32_sub(&mut self, offset: usize)
        fn visit_f32_mul(&mut self, offset: usize)
        fn visit_f32_div(&mut self, offset: usize)
        fn visit_f32_min(&mut self, offset: usize)
        fn visit_f32_max(&mut self, offset: usize)
        fn visit_f32_copysign(&mut self, offset: usize)
        fn visit_f64_abs(&mut self, offset: usize)
        fn visit_f64_neg(&mut self, offset: usize)
        fn visit_f64_ceil(&mut self, offset: usize)
        fn visit_f64_floor(&mut self, offset: usize)
        fn visit_f64_trunc(&mut self, offset: usize)
        fn visit_f64_nearest(&mut self, offset: usize)
        fn visit_f64_sqrt(&mut self, offset: usize)
        fn visit_f64_add(&mut self, offset: usize)
        fn visit_f64_sub(&mut self, offset: usize)
        fn visit_f64_mul(&mut self, offset: usize)
        fn visit_f64_div(&mut self, offset: usize)
        fn visit_f64_min(&mut self, offset: usize)
        fn visit_f64_max(&mut self, offset: usize)
        fn visit_f64_copysign(&mut self, offset: usize)
        fn visit_i32_wrap_i64(&mut self, offset: usize)
        fn visit_i32_trunc_f32s(&mut self, offset: usize)
        fn visit_i32_trunc_f32u(&mut self, offset: usize)
        fn visit_i32_trunc_f64s(&mut self, offset: usize)
        fn visit_i32_trunc_f64u(&mut self, offset: usize)
        fn visit_i64_extend_i32s(&mut self, offset: usize)
        fn visit_i64_extend_i32u(&mut self, offset: usize)
        fn visit_i64_trunc_f32s(&mut self, offset: usize)
        fn visit_i64_trunc_f32u(&mut self, offset: usize)
        fn visit_i64_trunc_f64s(&mut self, offset: usize)
        fn visit_i64_trunc_f64u(&mut self, offset: usize)
        fn visit_f32_convert_i32s(&mut self, offset: usize)
        fn visit_f32_convert_i32u(&mut self, offset: usize)
        fn visit_f32_convert_i64s(&mut self, offset: usize)
        fn visit_f32_convert_i64u(&mut self, offset: usize)
        fn visit_f32_demote_f64(&mut self, offset: usize)
        fn visit_f64_convert_i32s(&mut self, offset: usize)
        fn visit_f64_convert_i32u(&mut self, offset: usize)
        fn visit_f64_convert_i64s(&mut self, offset: usize)
        fn visit_f64_convert_i64u(&mut self, offset: usize)
        fn visit_f64_promote_f32(&mut self, offset: usize)
        fn visit_i32_reinterpret_f32(&mut self, offset: usize)
        fn visit_i64_reinterpret_f64(&mut self, offset: usize)
        fn visit_f32_reinterpret_i32(&mut self, offset: usize)
        fn visit_f64_reinterpret_i64(&mut self, offset: usize)
        fn visit_i32_trunc_sat_f32s(&mut self, offset: usize)
        fn visit_i32_trunc_sat_f32u(&mut self, offset: usize)
        fn visit_i32_trunc_sat_f64s(&mut self, offset: usize)
        fn visit_i32_trunc_sat_f64u(&mut self, offset: usize)
        fn visit_i64_trunc_sat_f32s(&mut self, offset: usize)
        fn visit_i64_trunc_sat_f32u(&mut self, offset: usize)
        fn visit_i64_trunc_sat_f64s(&mut self, offset: usize)
        fn visit_i64_trunc_sat_f64u(&mut self, offset: usize)
        fn visit_i32_extend8_s(&mut self, offset: usize)
        fn visit_i32_extend16_s(&mut self, offset: usize)
        fn visit_i64_extend8_s(&mut self, offset: usize)
        fn visit_i64_extend16_s(&mut self, offset: usize)
        fn visit_i64_extend32_s(&mut self, offset: usize)
        fn visit_i32_atomic_load(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_atomic_load16_u(&mut self, offset: usize, memarg: MemoryImmediate)

        fn visit_i32_atomic_load8_u(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_atomic_load(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_atomic_load32_u(&mut self, offset: usize, memarg: MemoryImmediate)

        fn visit_i64_atomic_load16_u(&mut self, offset: usize, memarg: MemoryImmediate)

        fn visit_i64_atomic_load8_u(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_atomic_store(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_atomic_store16(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_atomic_store8(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_atomic_store(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_atomic_store32(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_atomic_store16(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_atomic_store8(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_atomic_rmw_add(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_atomic_rmw_sub(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_atomic_rmw_and(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_atomic_rmw_or(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_atomic_rmw_xor(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i32_atomic_rmw16_add_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw16_sub_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw16_and_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw16_or_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw16_xor_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw8_add_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw8_sub_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw8_and_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw8_or_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw8_xor_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw_add(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_atomic_rmw_sub(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_atomic_rmw_and(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_atomic_rmw_or(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_atomic_rmw_xor(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_i64_atomic_rmw32_add_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw32_sub_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw32_and_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw32_or_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw32_xor_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw16_add_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw16_sub_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw16_and_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw16_or_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw16_xor_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw8_add_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw8_sub_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw8_and_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw8_or_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw8_xor_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw_xchg(&mut self, offset: usize, memarg: MemoryImmediate)

        fn visit_i32_atomic_rmw16_xchg_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw8_xchg_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw_cmpxchg(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw16_cmpxchg_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i32_atomic_rmw8_cmpxchg_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw_xchg(&mut self, offset: usize, memarg: MemoryImmediate)

        fn visit_i64_atomic_rmw32_xchg_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw16_xchg_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw8_xchg_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw_cmpxchg(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw32_cmpxchg_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw16_cmpxchg_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_i64_atomic_rmw8_cmpxchg_u(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_memory_atomic_notify(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_memory_atomic_wait32(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_memory_atomic_wait64(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
        )
        fn visit_atomic_fence(&mut self, offset: usize, flags: u8)
        fn visit_ref_null(&mut self, offset: usize, ty: ValType)
        fn visit_ref_is_null(&mut self, offset: usize)
        fn visit_ref_func(&mut self, offset: usize, function_index: u32)
        fn visit_v128_load(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_store(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_const(&mut self, offset: usize, value: V128)
        fn visit_i8x16_splat(&mut self, offset: usize)
        fn visit_i16x8_splat(&mut self, offset: usize)
        fn visit_i32x4_splat(&mut self, offset: usize)
        fn visit_i64x2_splat(&mut self, offset: usize)
        fn visit_f32x4_splat(&mut self, offset: usize)
        fn visit_f64x2_splat(&mut self, offset: usize)
        fn visit_i8x16_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_i8x16_extract_lane_u(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_i16x8_extract_lane_s(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_i16x8_extract_lane_u(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_i32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_i8x16_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_i16x8_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_i32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_i64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_i64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_f32x4_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_f32x4_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_f64x2_extract_lane(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_f64x2_replace_lane(&mut self, offset: usize, lane: SIMDLaneIndex)
        fn visit_f32x4_eq(&mut self, offset: usize)
        fn visit_f32x4_ne(&mut self, offset: usize)
        fn visit_f32x4_lt(&mut self, offset: usize)
        fn visit_f32x4_gt(&mut self, offset: usize)
        fn visit_f32x4_le(&mut self, offset: usize)
        fn visit_f32x4_ge(&mut self, offset: usize)
        fn visit_f64x2_eq(&mut self, offset: usize)
        fn visit_f64x2_ne(&mut self, offset: usize)
        fn visit_f64x2_lt(&mut self, offset: usize)
        fn visit_f64x2_gt(&mut self, offset: usize)
        fn visit_f64x2_le(&mut self, offset: usize)
        fn visit_f64x2_ge(&mut self, offset: usize)
        fn visit_f32x4_add(&mut self, offset: usize)
        fn visit_f32x4_sub(&mut self, offset: usize)
        fn visit_f32x4_mul(&mut self, offset: usize)
        fn visit_f32x4_div(&mut self, offset: usize)
        fn visit_f32x4_min(&mut self, offset: usize)
        fn visit_f32x4_max(&mut self, offset: usize)
        fn visit_f32x4_p_min(&mut self, offset: usize)
        fn visit_f32x4_p_max(&mut self, offset: usize)
        fn visit_f64x2_add(&mut self, offset: usize)
        fn visit_f64x2_sub(&mut self, offset: usize)
        fn visit_f64x2_mul(&mut self, offset: usize)
        fn visit_f64x2_div(&mut self, offset: usize)
        fn visit_f64x2_min(&mut self, offset: usize)
        fn visit_f64x2_max(&mut self, offset: usize)
        fn visit_f64x2_p_min(&mut self, offset: usize)
        fn visit_f64x2_p_max(&mut self, offset: usize)
        fn visit_f32x4_relaxed_min(&mut self, offset: usize)
        fn visit_f32x4_relaxed_max(&mut self, offset: usize)
        fn visit_f64x2_relaxed_min(&mut self, offset: usize)
        fn visit_f64x2_relaxed_max(&mut self, offset: usize)
        fn visit_i8x16_eq(&mut self, offset: usize)
        fn visit_i8x16_ne(&mut self, offset: usize)
        fn visit_i8x16_lt_s(&mut self, offset: usize)
        fn visit_i8x16_lt_u(&mut self, offset: usize)
        fn visit_i8x16_gt_s(&mut self, offset: usize)
        fn visit_i8x16_gt_u(&mut self, offset: usize)
        fn visit_i8x16_le_s(&mut self, offset: usize)
        fn visit_i8x16_le_u(&mut self, offset: usize)
        fn visit_i8x16_ge_s(&mut self, offset: usize)
        fn visit_i8x16_ge_u(&mut self, offset: usize)
        fn visit_i16x8_eq(&mut self, offset: usize)
        fn visit_i16x8_ne(&mut self, offset: usize)
        fn visit_i16x8_lt_s(&mut self, offset: usize)
        fn visit_i16x8_lt_u(&mut self, offset: usize)
        fn visit_i16x8_gt_s(&mut self, offset: usize)
        fn visit_i16x8_gt_u(&mut self, offset: usize)
        fn visit_i16x8_le_s(&mut self, offset: usize)
        fn visit_i16x8_le_u(&mut self, offset: usize)
        fn visit_i16x8_ge_s(&mut self, offset: usize)
        fn visit_i16x8_ge_u(&mut self, offset: usize)
        fn visit_i32x4_eq(&mut self, offset: usize)
        fn visit_i32x4_ne(&mut self, offset: usize)
        fn visit_i32x4_lt_s(&mut self, offset: usize)
        fn visit_i32x4_lt_u(&mut self, offset: usize)
        fn visit_i32x4_gt_s(&mut self, offset: usize)
        fn visit_i32x4_gt_u(&mut self, offset: usize)
        fn visit_i32x4_le_s(&mut self, offset: usize)
        fn visit_i32x4_le_u(&mut self, offset: usize)
        fn visit_i32x4_ge_s(&mut self, offset: usize)
        fn visit_i32x4_ge_u(&mut self, offset: usize)
        fn visit_i64x2_eq(&mut self, offset: usize)
        fn visit_i64x2_ne(&mut self, offset: usize)
        fn visit_i64x2_lt_s(&mut self, offset: usize)
        fn visit_i64x2_gt_s(&mut self, offset: usize)
        fn visit_i64x2_le_s(&mut self, offset: usize)
        fn visit_i64x2_ge_s(&mut self, offset: usize)
        fn visit_v128_and(&mut self, offset: usize)
        fn visit_v128_and_not(&mut self, offset: usize)
        fn visit_v128_or(&mut self, offset: usize)
        fn visit_v128_xor(&mut self, offset: usize)
        fn visit_i8x16_add(&mut self, offset: usize)
        fn visit_i8x16_add_sat_s(&mut self, offset: usize)
        fn visit_i8x16_add_sat_u(&mut self, offset: usize)
        fn visit_i8x16_sub(&mut self, offset: usize)
        fn visit_i8x16_sub_sat_s(&mut self, offset: usize)
        fn visit_i8x16_sub_sat_u(&mut self, offset: usize)
        fn visit_i8x16_min_s(&mut self, offset: usize)
        fn visit_i8x16_min_u(&mut self, offset: usize)
        fn visit_i8x16_max_s(&mut self, offset: usize)
        fn visit_i8x16_max_u(&mut self, offset: usize)
        fn visit_i16x8_add(&mut self, offset: usize)
        fn visit_i16x8_add_sat_s(&mut self, offset: usize)
        fn visit_i16x8_add_sat_u(&mut self, offset: usize)
        fn visit_i16x8_sub(&mut self, offset: usize)
        fn visit_i16x8_sub_sat_s(&mut self, offset: usize)
        fn visit_i16x8_sub_sat_u(&mut self, offset: usize)
        fn visit_i16x8_mul(&mut self, offset: usize)
        fn visit_i16x8_min_s(&mut self, offset: usize)
        fn visit_i16x8_min_u(&mut self, offset: usize)
        fn visit_i16x8_max_s(&mut self, offset: usize)
        fn visit_i16x8_max_u(&mut self, offset: usize)
        fn visit_i32x4_add(&mut self, offset: usize)
        fn visit_i32x4_sub(&mut self, offset: usize)
        fn visit_i32x4_mul(&mut self, offset: usize)
        fn visit_i32x4_min_s(&mut self, offset: usize)
        fn visit_i32x4_min_u(&mut self, offset: usize)
        fn visit_i32x4_max_s(&mut self, offset: usize)
        fn visit_i32x4_max_u(&mut self, offset: usize)
        fn visit_i32x4_dot_i16x8_s(&mut self, offset: usize)
        fn visit_i64x2_add(&mut self, offset: usize)
        fn visit_i64x2_sub(&mut self, offset: usize)
        fn visit_i64x2_mul(&mut self, offset: usize)
        fn visit_i8x16_rounding_average_u(&mut self, offset: usize)
        fn visit_i16x8_rounding_average_u(&mut self, offset: usize)
        fn visit_i8x16_narrow_i16x8_s(&mut self, offset: usize)
        fn visit_i8x16_narrow_i16x8_u(&mut self, offset: usize)
        fn visit_i16x8_narrow_i32x4_s(&mut self, offset: usize)
        fn visit_i16x8_narrow_i32x4_u(&mut self, offset: usize)
        fn visit_i16x8_ext_mul_low_i8x16_s(&mut self, offset: usize)
        fn visit_i16x8_ext_mul_high_i8x16_s(&mut self, offset: usize)
        fn visit_i16x8_ext_mul_low_i8x16_u(&mut self, offset: usize)
        fn visit_i16x8_ext_mul_high_i8x16_u(&mut self, offset: usize)
        fn visit_i32x4_ext_mul_low_i16x8_s(&mut self, offset: usize)
        fn visit_i32x4_ext_mul_high_i16x8_s(&mut self, offset: usize)
        fn visit_i32x4_ext_mul_low_i16x8_u(&mut self, offset: usize)
        fn visit_i32x4_ext_mul_high_i16x8_u(&mut self, offset: usize)
        fn visit_i64x2_ext_mul_low_i32x4_s(&mut self, offset: usize)
        fn visit_i64x2_ext_mul_high_i32x4_s(&mut self, offset: usize)
        fn visit_i64x2_ext_mul_low_i32x4_u(&mut self, offset: usize)
        fn visit_i64x2_ext_mul_high_i32x4_u(&mut self, offset: usize)
        fn visit_i16x8_q15_mulr_sat_s(&mut self, offset: usize)
        fn visit_f32x4_ceil(&mut self, offset: usize)
        fn visit_f32x4_floor(&mut self, offset: usize)
        fn visit_f32x4_trunc(&mut self, offset: usize)
        fn visit_f32x4_nearest(&mut self, offset: usize)
        fn visit_f64x2_ceil(&mut self, offset: usize)
        fn visit_f64x2_floor(&mut self, offset: usize)
        fn visit_f64x2_trunc(&mut self, offset: usize)
        fn visit_f64x2_nearest(&mut self, offset: usize)
        fn visit_f32x4_abs(&mut self, offset: usize)
        fn visit_f32x4_neg(&mut self, offset: usize)
        fn visit_f32x4_sqrt(&mut self, offset: usize)
        fn visit_f64x2_abs(&mut self, offset: usize)
        fn visit_f64x2_neg(&mut self, offset: usize)
        fn visit_f64x2_sqrt(&mut self, offset: usize)
        fn visit_f32x4_demote_f64x2_zero(&mut self, offset: usize)
        fn visit_f64x2_promote_low_f32x4(&mut self, offset: usize)
        fn visit_f64x2_convert_low_i32x4_s(&mut self, offset: usize)
        fn visit_f64x2_convert_low_i32x4_u(&mut self, offset: usize)
        fn visit_i32x4_trunc_sat_f32x4_s(&mut self, offset: usize)
        fn visit_i32x4_trunc_sat_f32x4_u(&mut self, offset: usize)
        fn visit_i32x4_trunc_sat_f64x2_s_zero(&mut self, offset: usize)
        fn visit_i32x4_trunc_sat_f64x2_u_zero(&mut self, offset: usize)
        fn visit_f32x4_convert_i32x4_s(&mut self, offset: usize)
        fn visit_f32x4_convert_i32x4_u(&mut self, offset: usize)
        fn visit_v128_not(&mut self, offset: usize)
        fn visit_i8x16_abs(&mut self, offset: usize)
        fn visit_i8x16_neg(&mut self, offset: usize)
        fn visit_i8x16_popcnt(&mut self, offset: usize)
        fn visit_i16x8_abs(&mut self, offset: usize)
        fn visit_i16x8_neg(&mut self, offset: usize)
        fn visit_i32x4_abs(&mut self, offset: usize)
        fn visit_i32x4_neg(&mut self, offset: usize)
        fn visit_i64x2_abs(&mut self, offset: usize)
        fn visit_i64x2_neg(&mut self, offset: usize)
        fn visit_i16x8_extend_low_i8x16_s(&mut self, offset: usize)
        fn visit_i16x8_extend_high_i8x16_s(&mut self, offset: usize)
        fn visit_i16x8_extend_low_i8x16_u(&mut self, offset: usize)
        fn visit_i16x8_extend_high_i8x16_u(&mut self, offset: usize)
        fn visit_i32x4_extend_low_i16x8_s(&mut self, offset: usize)
        fn visit_i32x4_extend_high_i16x8_s(&mut self, offset: usize)
        fn visit_i32x4_extend_low_i16x8_u(&mut self, offset: usize)
        fn visit_i32x4_extend_high_i16x8_u(&mut self, offset: usize)
        fn visit_i64x2_extend_low_i32x4_s(&mut self, offset: usize)
        fn visit_i64x2_extend_high_i32x4_s(&mut self, offset: usize)
        fn visit_i64x2_extend_low_i32x4_u(&mut self, offset: usize)
        fn visit_i64x2_extend_high_i32x4_u(&mut self, offset: usize)
        fn visit_i16x8_ext_add_pairwise_i8x16_s(&mut self, offset: usize)
        fn visit_i16x8_ext_add_pairwise_i8x16_u(&mut self, offset: usize)
        fn visit_i32x4_ext_add_pairwise_i16x8_s(&mut self, offset: usize)
        fn visit_i32x4_ext_add_pairwise_i16x8_u(&mut self, offset: usize)
        fn visit_i32x4_relaxed_trunc_sat_f32x4_s(&mut self, offset: usize)
        fn visit_i32x4_relaxed_trunc_sat_f32x4_u(&mut self, offset: usize)
        fn visit_i32x4_relaxed_trunc_sat_f64x2_s_zero(&mut self, offset: usize)
        fn visit_i32x4_relaxed_trunc_sat_f64x2_u_zero(&mut self, offset: usize)
        fn visit_v128_bitselect(&mut self, offset: usize)
        fn visit_f32x4_fma(&mut self, offset: usize)
        fn visit_f32x4_fms(&mut self, offset: usize)
        fn visit_f64x2_fma(&mut self, offset: usize)
        fn visit_f64x2_fms(&mut self, offset: usize)
        fn visit_i8x16_lane_select(&mut self, offset: usize)
        fn visit_i16x8_lane_select(&mut self, offset: usize)
        fn visit_i32x4_lane_select(&mut self, offset: usize)
        fn visit_i64x2_lane_select(&mut self, offset: usize)
        fn visit_v128_any_true(&mut self, offset: usize)
        fn visit_i8x16_all_true(&mut self, offset: usize)
        fn visit_i8x16_bitmask(&mut self, offset: usize)
        fn visit_i16x8_all_true(&mut self, offset: usize)
        fn visit_i16x8_bitmask(&mut self, offset: usize)
        fn visit_i32x4_all_true(&mut self, offset: usize)
        fn visit_i32x4_bitmask(&mut self, offset: usize)
        fn visit_i64x2_all_true(&mut self, offset: usize)
        fn visit_i64x2_bitmask(&mut self, offset: usize)
        fn visit_i8x16_shl(&mut self, offset: usize)
        fn visit_i8x16_shr_s(&mut self, offset: usize)
        fn visit_i8x16_shr_u(&mut self, offset: usize)
        fn visit_i16x8_shl(&mut self, offset: usize)
        fn visit_i16x8_shr_s(&mut self, offset: usize)
        fn visit_i16x8_shr_u(&mut self, offset: usize)
        fn visit_i32x4_shl(&mut self, offset: usize)
        fn visit_i32x4_shr_s(&mut self, offset: usize)
        fn visit_i32x4_shr_u(&mut self, offset: usize)
        fn visit_i64x2_shl(&mut self, offset: usize)
        fn visit_i64x2_shr_s(&mut self, offset: usize)
        fn visit_i64x2_shr_u(&mut self, offset: usize)
        fn visit_i8x16_swizzle(&mut self, offset: usize)
        fn visit_i8x16_relaxed_swizzle(&mut self, offset: usize)
        fn visit_i8x16_shuffle(&mut self, offset: usize, lanes: [SIMDLaneIndex; 16])
        fn visit_v128_load8_splat(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load16_splat(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load32_splat(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load32_zero(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load64_splat(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load64_zero(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load8x8_s(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load8x8_u(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load16x4_s(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load16x4_u(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load32x2_s(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load32x2_u(&mut self, offset: usize, memarg: MemoryImmediate)
        fn visit_v128_load8_lane(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
            lane: SIMDLaneIndex,
        )
        fn visit_v128_load16_lane(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
            lane: SIMDLaneIndex,
        )
        fn visit_v128_load32_lane(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
            lane: SIMDLaneIndex,
        )
        fn visit_v128_load64_lane(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
            lane: SIMDLaneIndex,
        )
        fn visit_v128_store8_lane(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
            lane: SIMDLaneIndex,
        )
        fn visit_v128_store16_lane(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
            lane: SIMDLaneIndex,
        )
        fn visit_v128_store32_lane(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
            lane: SIMDLaneIndex,
        )
        fn visit_v128_store64_lane(
            &mut self,
            offset: usize,
            memarg: MemoryImmediate,
            lane: SIMDLaneIndex,
        )
        fn visit_memory_init(&mut self, offset: usize, segment: u32, mem: u32)
        fn visit_data_drop(&mut self, offset: usize, segment: u32)
        fn visit_memory_copy(&mut self, offset: usize, dst: u32, src: u32)
        fn visit_memory_fill(&mut self, offset: usize, mem: u32)
        fn visit_table_init(&mut self, offset: usize, segment: u32, table: u32)
        fn visit_elem_drop(&mut self, offset: usize, segment: u32)
        fn visit_table_copy(&mut self, offset: usize, dst_table: u32, src_table: u32)
        fn visit_table_get(&mut self, offset: usize, table: u32)
        fn visit_table_set(&mut self, offset: usize, table: u32)
        fn visit_table_grow(&mut self, offset: usize, table: u32)
        fn visit_table_size(&mut self, offset: usize, table: u32)
        fn visit_table_fill(&mut self, offset: usize, table: u32)
    }
}
