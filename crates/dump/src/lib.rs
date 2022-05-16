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
    funcs: u32,
    globals: u32,
    tables: u32,
    memories: u32,
    tags: u32,
    types: u32,
    instances: u32,
    components: u32,
    modules: u32,
    values: u32,
}

enum ComponentTypeKind {
    Func,
    Component,
    Instance,
    Module,
    Value,
    Type,
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
                    write!(me.state, "[type {}] {:?}", inc(&mut i.types), t)?;
                    me.print(end)
                })?,
                Payload::ImportSection(s) => self.section(s, "import", |me, end, imp| {
                    write!(me.state, "import ")?;
                    match imp.ty {
                        TypeRef::Func(_) => write!(me.state, "[func {}]", inc(&mut i.funcs))?,
                        TypeRef::Memory(_) => {
                            write!(me.state, "[memory {}]", inc(&mut i.memories))?
                        }
                        TypeRef::Tag(_) => write!(me.state, "[tag {}]", inc(&mut i.tags))?,
                        TypeRef::Table(_) => write!(me.state, "[table {}]", inc(&mut i.tables))?,
                        TypeRef::Global(_) => write!(me.state, "[global {}]", inc(&mut i.globals))?,
                    }
                    write!(me.state, " {:?}", imp)?;
                    me.print(end)
                })?,
                Payload::FunctionSection(s) => {
                    let mut cnt = i.funcs;
                    self.section(s, "func", |me, end, f| {
                        write!(me.state, "[func {}] type {:?}", inc(&mut cnt), f)?;
                        me.print(end)
                    })?
                }
                Payload::TableSection(s) => self.section(s, "table", |me, end, t| {
                    write!(me.state, "[table {}] {:?}", inc(&mut i.tables), t)?;
                    me.print(end)
                })?,
                Payload::MemorySection(s) => self.section(s, "memory", |me, end, m| {
                    write!(me.state, "[memory {}] {:?}", inc(&mut i.memories), m)?;
                    me.print(end)
                })?,
                Payload::TagSection(s) => self.section(s, "tag", |me, end, m| {
                    write!(me.state, "[tag {}] {:?}", inc(&mut i.tags), m)?;
                    me.print(end)
                })?,
                Payload::ExportSection(s) => self.section(s, "export", |me, end, e| {
                    write!(me.state, "export {:?}", e)?;
                    me.print(end)
                })?,
                Payload::GlobalSection(s) => self.section(s, "global", |me, _end, g| {
                    write!(me.state, "[global {}] {:?}", inc(&mut i.globals), g.ty)?;
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
                    me.print_byte_header()?;
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
                        inc(&mut i.funcs),
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
                Payload::ComponentTypeSection(s) => self.section(s, "type", |me, end, t| {
                    write!(me.state, "[type {}] {:?}", inc(&mut i.types), t)?;
                    component_types.push(match t {
                        ComponentTypeDef::Module(_) => ComponentTypeKind::Module,
                        ComponentTypeDef::Component(_) => ComponentTypeKind::Component,
                        ComponentTypeDef::Instance(_) => ComponentTypeKind::Instance,
                        ComponentTypeDef::Function(_) => ComponentTypeKind::Func,
                        ComponentTypeDef::Value(_) => ComponentTypeKind::Value,
                        ComponentTypeDef::Interface(_) => ComponentTypeKind::Type,
                    });
                    me.print(end)
                })?,

                Payload::ComponentImportSection(s) => {
                    self.section(s, "import", |me, end, item| {
                        let (desc, idx) = match component_types.get(item.ty as usize) {
                            Some(ComponentTypeKind::Func) => ("func", inc(&mut i.funcs)),
                            Some(ComponentTypeKind::Component) => {
                                ("component", inc(&mut i.components))
                            }
                            Some(ComponentTypeKind::Module) => ("module", inc(&mut i.modules)),
                            Some(ComponentTypeKind::Instance) => {
                                ("instance", inc(&mut i.instances))
                            }
                            Some(ComponentTypeKind::Value) => ("value", inc(&mut i.values)),
                            Some(ComponentTypeKind::Type) => ("type", inc(&mut i.types)),
                            None => ("???", 0),
                        };
                        write!(me.state, "[{desc} {idx}] {item:?}")?;
                        me.print(end)
                    })?
                }

                Payload::ComponentFunctionSection(s) => {
                    self.section(s, "component function", |me, end, f| {
                        write!(me.state, "[func {}] {:?}", inc(&mut i.funcs), f)?;
                        me.print(end)
                    })?
                }

                Payload::ModuleSection { range, .. } => {
                    write!(self.state, "[module {}] inline size", inc(&mut i.modules))?;
                    self.print(range.start)?;
                    self.nesting += 1;
                    stack.push(i);
                    i = Indices::default();
                }

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

                Payload::InstanceSection(s) => self.section(s, "instance", |me, end, e| {
                    write!(me.state, "[instance {}] {:?}", inc(&mut i.instances), e)?;
                    me.print(end)
                })?,

                Payload::ComponentExportSection(s) => {
                    self.section(s, "component-export", |me, end, e| {
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

                Payload::AliasSection(s) => self.section(s, "alias", |me, end, a| {
                    let (kind, num) = match a {
                        Alias::InstanceExport { kind, .. } => match kind {
                            AliasKind::Module => ("module", inc(&mut i.modules)),
                            AliasKind::Component => ("component", inc(&mut i.components)),
                            AliasKind::Instance => ("instance", inc(&mut i.instances)),
                            AliasKind::ComponentFunc => ("component func", inc(&mut i.funcs)),
                            AliasKind::Value => ("value", inc(&mut i.values)),
                            AliasKind::Func => ("func", inc(&mut i.funcs)),
                            AliasKind::Table => ("table", inc(&mut i.tables)),
                            AliasKind::Memory => ("memory", inc(&mut i.memories)),
                            AliasKind::Global => ("global", inc(&mut i.globals)),
                            AliasKind::Tag => ("tag", inc(&mut i.tags)),
                        },
                        Alias::OuterModule { .. } => ("module", inc(&mut i.modules)),
                        Alias::OuterComponent { .. } => ("component", inc(&mut i.components)),
                        Alias::OuterType { .. } => ("type", inc(&mut i.types)),
                    };
                    write!(me.state, "alias [{} {}] {:?}", kind, num, a)?;
                    me.print(end)
                })?,

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
                        write!(self.dst, "-| ... {} bytes of data\n", c.data().len())?;
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
                    write!(self.dst, "-| ... {} bytes of data\n", contents.len())?;
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
            write!(self.dst, "\n")?;
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
    return ret;
}
