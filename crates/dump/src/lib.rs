use anyhow::Result;
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
                    let item_count = match &i.items {
                        ElementItems::Functions(reader) => reader.count(),
                        ElementItems::Expressions(reader) => reader.count(),
                    };
                    match i.kind {
                        ElementKind::Passive => {
                            write!(me.state, " passive, {} items", item_count)?;
                        }
                        ElementKind::Active {
                            table_index,
                            offset_expr,
                        } => {
                            write!(me.state, " table[{}]", table_index)?;
                            me.print(offset_expr.get_binary_reader().original_position())?;
                            me.print_ops(offset_expr.get_operators_reader())?;
                            write!(me.state, "{} items", item_count)?;
                        }
                        ElementKind::Declared => {
                            write!(me.state, " declared {} items", item_count)?;
                        }
                    }
                    match i.items {
                        ElementItems::Functions(reader) => {
                            let mut iter = reader.into_iter();
                            me.print(iter.original_position())?;
                            while let Some(item) = iter.next() {
                                write!(me.state, "item {:?}", item?)?;
                                me.print(iter.original_position())?;
                            }
                        }
                        ElementItems::Expressions(reader) => {
                            let mut iter = reader.into_iter();
                            me.print(iter.original_position())?;
                            while let Some(item) = iter.next() {
                                write!(me.state, "item {:?}", item?)?;
                                me.print(iter.original_position())?;
                            }
                        }
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
                            ComponentAlias::CoreInstanceExport { kind, .. } => match kind {
                                ExternalKind::Func => ("core func", inc(&mut i.core_funcs)),
                                ExternalKind::Table => ("core table", inc(&mut i.core_tables)),
                                ExternalKind::Memory => ("core memory", inc(&mut i.core_memories)),
                                ExternalKind::Global => ("core global", inc(&mut i.core_globals)),
                                ExternalKind::Tag => ("core tag", inc(&mut i.core_tags)),
                            },
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

                Payload::ComponentStartSection { start, range } => {
                    write!(self.state, "start section")?;
                    self.print(range.start)?;
                    write!(self.state, "{:?}", start)?;
                    self.print(range.end)?;
                }

                Payload::CustomSection(c) => {
                    write!(self.state, "custom section")?;
                    self.print(c.range().start)?;
                    write!(self.state, "name: {:?}", c.name())?;
                    self.print(c.data_offset())?;
                    if c.name() == "name" {
                        let mut iter = NameSectionReader::new(c.data(), c.data_offset());
                        while let Some(section) = iter.next() {
                            self.print_custom_name_section(section?, iter.original_position())?;
                        }
                    } else if c.name() == "component-name" {
                        let mut iter = ComponentNameSectionReader::new(c.data(), c.data_offset());
                        while let Some(section) = iter.next() {
                            self.print_custom_component_name_section(
                                section?,
                                iter.original_position(),
                            )?;
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
        self.section(n, thing, |me, end, naming| {
            write!(me.state, "{:?}", naming)?;
            me.print(end)
        })
    }

    fn print_indirect_name_map(
        &mut self,
        thing_a: &str,
        thing_b: &str,
        n: IndirectNameMap<'_>,
    ) -> Result<()> {
        self.section(n, thing_b, |me, _end, naming| {
            write!(me.state, "{} {} ", thing_a, naming.index)?;
            me.print_name_map(thing_b, naming.names)
        })
    }

    fn print_custom_name_section(&mut self, name: Name<'_>, end: usize) -> Result<()> {
        match name {
            Name::Module { name, name_range } => {
                write!(self.state, "module name")?;
                self.print(name_range.start)?;
                write!(self.state, "{:?}", name)?;
                self.print(name_range.end)?;
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

    fn print_custom_component_name_section(
        &mut self,
        name: ComponentName<'_>,
        end: usize,
    ) -> Result<()> {
        match name {
            ComponentName::Component { name, name_range } => {
                write!(self.state, "component name")?;
                self.print(name_range.start)?;
                write!(self.state, "{:?}", name)?;
                self.print(name_range.end)?;
            }
            ComponentName::CoreFuncs(n) => self.print_name_map("core func", n)?,
            ComponentName::CoreTables(n) => self.print_name_map("core table", n)?,
            ComponentName::CoreGlobals(n) => self.print_name_map("core global", n)?,
            ComponentName::CoreMemories(n) => self.print_name_map("core memory", n)?,
            ComponentName::CoreInstances(n) => self.print_name_map("core instance", n)?,
            ComponentName::CoreModules(n) => self.print_name_map("core module", n)?,
            ComponentName::CoreTypes(n) => self.print_name_map("core type", n)?,
            ComponentName::Types(n) => self.print_name_map("type", n)?,
            ComponentName::Instances(n) => self.print_name_map("instance", n)?,
            ComponentName::Components(n) => self.print_name_map("component", n)?,
            ComponentName::Funcs(n) => self.print_name_map("func", n)?,
            ComponentName::Values(n) => self.print_name_map("value", n)?,
            ComponentName::Unknown { ty, range, .. } => {
                write!(self.state, "unknown names: {}", ty)?;
                self.print(range.start)?;
                self.print(end)?;
            }
        }
        Ok(())
    }

    fn section<'b, T>(
        &mut self,
        iter: SectionLimited<'b, T>,
        name: &str,
        print: impl FnMut(&mut Self, usize, T) -> Result<()>,
    ) -> Result<()>
    where
        T: FromReader<'b>,
    {
        write!(self.state, "{} section", name)?;
        self.print(iter.range().start)?;
        self.print_iter(iter, print)
    }

    fn print_iter<'b, T>(
        &mut self,
        iter: SectionLimited<'b, T>,
        mut print: impl FnMut(&mut Self, usize, T) -> Result<()>,
    ) -> Result<()>
    where
        T: FromReader<'b>,
    {
        write!(self.state, "{} count", iter.count())?;
        let mut iter = iter.into_iter();
        self.print(iter.original_position())?;
        while let Some(item) = iter.next() {
            print(self, iter.original_position(), item?)?;
        }
        Ok(())
    }

    fn print_ops(&mut self, mut i: OperatorsReader) -> Result<()> {
        while !i.eof() {
            match i.visit_operator(self) {
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

macro_rules! define_visit_operator {
    ($(@$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident)*) => {
        $(
            fn $visit(&mut self $($(,$arg: $argty)*)?) {
                write!(
                    self.state,
                    concat!(
                        "{}"
                        $( $(, " ", stringify!($arg), ":{:?}")* )?
                    ),
                    stringify!($visit).strip_prefix("visit_").unwrap(),
                    $( $($arg,)* )?
                ).unwrap();
            }
        )*
    }
}

impl<'a> VisitOperator<'a> for Dump<'_> {
    type Output = ();

    wasmparser::for_each_operator!(define_visit_operator);
}
