use anyhow::{bail, Result};
use std::fmt::Write;
use wasmparser::*;

pub fn dump_wasm(bytes: &[u8]) -> Result<String> {
    let mut d = Dump::new(bytes);
    d.run()?;
    Ok(d.dst)
}

struct Dump<'a> {
    bytes: &'a [u8],
    cur: usize,
    state: String,
    dst: String,
    nesting: u32,
}

#[derive(Default)]
struct Indices {
    funcs: u32,
    globals: u32,
    tables: u32,
    memories: u32,
    tags: u32,
    modules: u32,
    instances: u32,
    types: u32,
}

const NBYTES: usize = 4;

impl<'a> Dump<'a> {
    fn new(bytes: &'a [u8]) -> Dump<'a> {
        Dump {
            bytes,
            cur: 0,
            nesting: 0,
            state: String::new(),
            dst: String::new(),
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
        self.nesting += 1;

        for item in Parser::new(0).parse_all(self.bytes) {
            match item? {
                Payload::Version { num, range } => {
                    write!(self.state, "version {}", num)?;
                    self.print(range.end)?;
                }
                Payload::TypeSection(s) => self.section(s, "type", |me, end, t| {
                    write!(me.state, "[type {}] {:?}", i.types, t)?;
                    i.types += 1;
                    me.print(end)
                })?,
                Payload::ImportSection(s) => self.section(s, "import", |me, end, imp| {
                    write!(me.state, "import ")?;
                    match imp.ty {
                        ImportSectionEntryType::Function(_) => {
                            write!(me.state, "[func {}]", i.funcs)?;
                            i.funcs += 1;
                        }
                        ImportSectionEntryType::Memory(_) => {
                            write!(me.state, "[memory {}]", i.memories)?;
                            i.memories += 1;
                        }
                        ImportSectionEntryType::Tag(_) => {
                            write!(me.state, "[tag {}]", i.tags)?;
                            i.tags += 1;
                        }
                        ImportSectionEntryType::Table(_) => {
                            write!(me.state, "[table {}]", i.tables)?;
                            i.tables += 1;
                        }
                        ImportSectionEntryType::Global(_) => {
                            write!(me.state, "[global {}]", i.globals)?;
                            i.globals += 1;
                        }
                        ImportSectionEntryType::Instance(_) => {
                            write!(me.state, "[instance {}]", i.instances)?;
                            i.instances += 1;
                        }
                        ImportSectionEntryType::Module(_) => {
                            write!(me.state, "[module {}]", i.modules)?;
                            i.modules += 1;
                        }
                    }
                    write!(me.state, " {:?}", imp)?;
                    me.print(end)
                })?,
                Payload::FunctionSection(s) => {
                    let mut cnt = 0;
                    self.section(s, "func", |me, end, f| {
                        write!(me.state, "[func {}] type {:?}", cnt + i.funcs, f)?;
                        cnt += 1;
                        me.print(end)
                    })?
                }
                Payload::TableSection(s) => self.section(s, "table", |me, end, t| {
                    write!(me.state, "[table {}] {:?}", i.tables, t)?;
                    i.tables += 1;
                    me.print(end)
                })?,
                Payload::MemorySection(s) => self.section(s, "memory", |me, end, m| {
                    write!(me.state, "[memory {}] {:?}", i.memories, m)?;
                    i.memories += 1;
                    me.print(end)
                })?,
                Payload::TagSection(s) => self.section(s, "tag", |me, end, m| {
                    write!(me.state, "[tag {}] {:?}", i.tags, m)?;
                    i.tags += 1;
                    me.print(end)
                })?,
                Payload::ExportSection(s) => self.section(s, "export", |me, end, e| {
                    write!(me.state, "export {:?}", e)?;
                    me.print(end)
                })?,
                Payload::GlobalSection(s) => self.section(s, "global", |me, _end, g| {
                    write!(me.state, "[global {}] {:?}", i.globals, g.ty)?;
                    i.globals += 1;
                    me.print(g.init_expr.get_binary_reader().original_position())?;
                    me.print_ops(g.init_expr.get_operators_reader())
                })?,
                Payload::AliasSection(s) => self.section(s, "alias", |me, end, a| {
                    write!(me.state, "[alias] {:?}", a)?;
                    match a {
                        Alias::InstanceExport { kind, .. } => match kind {
                            ExternalKind::Function => i.funcs += 1,
                            ExternalKind::Global => i.globals += 1,
                            ExternalKind::Module => i.modules += 1,
                            ExternalKind::Table => i.tables += 1,
                            ExternalKind::Instance => i.instances += 1,
                            ExternalKind::Memory => i.memories += 1,
                            ExternalKind::Tag => i.tags += 1,
                            ExternalKind::Type => i.types += 1,
                        },
                        Alias::OuterType { .. } => i.types += 1,
                        Alias::OuterModule { .. } => i.modules += 1,
                    }
                    me.print(end)
                })?,
                Payload::InstanceSection(s) => {
                    self.section(s, "instance", |me, _end, instance| {
                        write!(
                            me.state,
                            "[instance {}] instantiate module:{}",
                            i.instances,
                            instance.module()
                        )?;
                        me.print(instance.original_position())?;
                        i.instances += 1;
                        me.print_iter(instance.args()?, |me, end, arg| {
                            write!(me.state, "[instantiate arg] {:?}", arg)?;
                            me.print(end)
                        })
                    })?
                }
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
                            init_expr,
                        } => {
                            write!(me.state, " table[{}]", table_index)?;
                            me.print(init_expr.get_binary_reader().original_position())?;
                            me.print_ops(init_expr.get_operators_reader())?;
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
                            init_expr,
                        } => {
                            write!(me.state, "data memory[{}]", memory_index)?;
                            me.print(init_expr.get_binary_reader().original_position())?;
                            me.print_ops(init_expr.get_operators_reader())?;
                        }
                    }
                    write!(me.dst, "0x{:04x} |", me.cur)?;
                    for _ in 0..NBYTES {
                        write!(me.dst, "---")?;
                    }
                    write!(me.dst, "-| ... {} bytes of data\n", i.data.len())?;
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
                    write!(
                        self.dst,
                        "============== func {} ====================\n",
                        i.funcs
                    )?;
                    i.funcs += 1;
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

                Payload::ModuleSectionStart { count, range, size } => {
                    write!(self.state, "module section")?;
                    self.print(range.start)?;
                    write!(self.state, "{} count", count)?;
                    self.print(range.end - size as usize)?;
                }
                Payload::ModuleSectionEntry { parser: _, range } => {
                    write!(self.state, "inline module size")?;
                    self.print(range.start)?;
                    self.nesting += 1;
                    stack.push(i);
                    i = Indices::default();
                }

                Payload::CustomSection {
                    name,
                    data_offset,
                    data,
                    range,
                } => {
                    write!(self.state, "custom section")?;
                    self.print(range.start)?;
                    write!(self.state, "name: {:?}", name)?;
                    self.print(data_offset)?;
                    if name == "name" {
                        let mut iter = NameSectionReader::new(data, data_offset)?;
                        while !iter.eof() {
                            self.print_custom_name_section(iter.read()?, iter.original_position())?;
                        }
                    } else {
                        write!(self.dst, "0x{:04x} |", self.cur)?;
                        for _ in 0..NBYTES {
                            write!(self.dst, "---")?;
                        }
                        write!(self.dst, "-| ... {} bytes of data\n", data.len())?;
                        self.cur += data.len();
                    }
                }
                Payload::UnknownSection {
                    id,
                    range,
                    contents,
                } => {
                    write!(self.state, "unknown section: {}", id)?;
                    self.print(range.start)?;
                    write!(self.dst, "0x{:04x} |", self.cur)?;
                    for _ in 0..NBYTES {
                        write!(self.dst, "---")?;
                    }
                    write!(self.dst, "-| ... {} bytes of data\n", contents.len())?;
                    self.cur += contents.len();
                }
                Payload::End => {
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
            match i.read() {
                Ok(op) => write!(self.state, "{:?}", op)?,
                Err(_) => write!(self.state, "??")?,
            }
            self.print(i.original_position())?;
        }
        Ok(())
    }

    fn print(&mut self, end: usize) -> Result<()> {
        assert!(
            self.cur < end,
            "{:#x} >= {:#x}\ntrying to print: {}\n{}",
            self.cur,
            end,
            self.state,
            self.dst
        );
        let bytes = &self.bytes[self.cur..end];
        for _ in 0..self.nesting - 1 {
            write!(self.dst, "  ")?;
        }
        write!(self.dst, "0x{:04x} |", self.cur)?;
        for (i, chunk) in bytes.chunks(NBYTES).enumerate() {
            if i > 0 {
                for _ in 0..self.nesting - 1 {
                    write!(self.dst, "  ")?;
                }
                self.dst.push_str("       |");
            }
            for j in 0..NBYTES {
                match chunk.get(j) {
                    Some(b) => write!(self.dst, " {:02x}", b)?,
                    None => write!(self.dst, "   ")?,
                }
            }
            if i == 0 {
                self.dst.push_str(" | ");
                self.dst.push_str(&self.state);
                self.state.truncate(0);
            }
            self.dst.push_str("\n");
        }
        self.cur = end;
        Ok(())
    }
}
