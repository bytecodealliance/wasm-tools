//! A test suite to parse everything in `dump` and assert that it matches
//! the `*.dump` file it generates.
//!
//! Use `BLESS=1` in the environment to auto-update `*.err` files. Be sure to
//! look at the diff!

use anyhow::{bail, Result};
use rayon::prelude::*;
use std::env;
use std::fmt::Write;
use std::path::{Path, PathBuf};
use wasmparser::*;

fn main() {
    let mut tests = Vec::new();
    find_tests("tests/dump".as_ref(), &mut tests);
    let filter = std::env::args().nth(1);

    let bless = env::var("BLESS").is_ok();
    let tests = tests
        .iter()
        .filter(|test| {
            if let Some(filter) = &filter {
                if let Some(s) = test.file_name().and_then(|s| s.to_str()) {
                    if !s.contains(filter) {
                        return false;
                    }
                }
            }
            true
        })
        .collect::<Vec<_>>();

    println!("running {} tests\n", tests.len());

    let errors = tests
        .par_iter()
        .filter_map(|test| run_test(test, bless).err())
        .collect::<Vec<_>>();

    if !errors.is_empty() {
        for msg in errors.iter() {
            eprintln!("{:?}", msg);
        }

        panic!("{} tests failed", errors.len())
    }

    println!("test result: ok. {} passed\n", tests.len());
}

fn run_test(test: &Path, bless: bool) -> Result<()> {
    let wasm = wat::parse_file(test)?;
    let assert = test.with_extension("wat.dump");
    let dump = dump_wasm(&wasm)?;
    if bless {
        std::fs::write(assert, &dump)?;
        return Ok(());
    }

    // Ignore CRLF line ending and force always `\n`
    let assert = std::fs::read_to_string(assert)
        .unwrap_or(String::new())
        .replace("\r\n", "\n");

    let mut bad = false;
    let mut result = String::new();
    for diff in diff::lines(&assert, &dump) {
        match diff {
            diff::Result::Left(s) => {
                bad = true;
                result.push_str("-");
                result.push_str(s);
            }
            diff::Result::Right(s) => {
                bad = true;
                result.push_str("+");
                result.push_str(s);
            }
            diff::Result::Both(s, _) => {
                result.push_str(" ");
                result.push_str(s);
            }
        }
        result.push_str("\n");
    }
    if bad {
        bail!(
            "expected != actual for test `{}`\n\n{}",
            test.display(),
            result
        );
    } else {
        Ok(())
    }
}

fn find_tests(path: &Path, tests: &mut Vec<PathBuf>) {
    for f in path.read_dir().unwrap() {
        let f = f.unwrap();
        if f.file_type().unwrap().is_dir() {
            find_tests(&f.path(), tests);
            continue;
        }
        match f.path().extension().and_then(|s| s.to_str()) {
            Some("wat") => {}
            _ => continue,
        }
        tests.push(f.path());
    }
}
fn dump_wasm(bytes: &[u8]) -> Result<String> {
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
        self.print_module(ModuleReader::new(self.bytes)?)?;
        assert_eq!(self.cur, self.bytes.len());
        Ok(())
    }

    fn print_module(&mut self, mut parser: ModuleReader<'a>) -> Result<()> {
        self.nesting += 1;
        write!(self.state, "version {}", parser.get_version())?;
        self.print(parser.original_position())?;

        let mut funcs = 0;
        let mut globals = 0;
        let mut tables = 0;
        let mut memories = 0;
        let mut modules = 0;
        let mut instances = 0;
        let mut types = 0;

        while !parser.eof() {
            let section = parser.read()?;
            write!(self.state, "section {:?}", section.code)?;
            self.print(section.range().start)?;
            match section.code {
                SectionCode::Type => {
                    self.print_iter(section.get_type_section_reader()?, |me, end, i| {
                        write!(me.state, "[type {}] {:?}", types, i)?;
                        types += 1;
                        me.print(end)
                    })?
                }
                SectionCode::Import => {
                    self.print_iter(section.get_import_section_reader()?, |me, end, i| {
                        write!(me.state, "import ")?;
                        match i.ty {
                            ImportSectionEntryType::Function(_) => {
                                write!(me.state, "[func {}]", funcs)?;
                                funcs += 1;
                            }
                            ImportSectionEntryType::Memory(_) => {
                                write!(me.state, "[memory {}]", memories)?;
                                memories += 1;
                            }
                            ImportSectionEntryType::Table(_) => {
                                write!(me.state, "[table {}]", tables)?;
                                tables += 1;
                            }
                            ImportSectionEntryType::Global(_) => {
                                write!(me.state, "[global {}]", globals)?;
                                globals += 1;
                            }
                            ImportSectionEntryType::Instance(_) => {
                                write!(me.state, "[instance {}]", instances)?;
                                instances += 1;
                            }
                            ImportSectionEntryType::Module(_) => {
                                write!(me.state, "[module {}]", modules)?;
                                modules += 1;
                            }
                        }
                        write!(me.state, " {:?}", i)?;
                        me.print(end)
                    })?
                }
                SectionCode::Function => {
                    let mut cnt = 0;
                    self.print_iter(section.get_function_section_reader()?, |me, end, i| {
                        write!(me.state, "[func {}] type {:?}", cnt + funcs, i)?;
                        cnt += 1;
                        me.print(end)
                    })?
                }
                SectionCode::Table => {
                    self.print_iter(section.get_table_section_reader()?, |me, end, i| {
                        write!(me.state, "[table {}] {:?}", tables, i)?;
                        tables += 1;
                        me.print(end)
                    })?
                }
                SectionCode::Memory => {
                    self.print_iter(section.get_memory_section_reader()?, |me, end, i| {
                        write!(me.state, "[memory {}] {:?}", memories, i)?;
                        memories += 1;
                        me.print(end)
                    })?
                }
                SectionCode::Export => {
                    self.print_iter(section.get_export_section_reader()?, |me, end, i| {
                        write!(me.state, "export {:?}", i)?;
                        me.print(end)
                    })?
                }
                SectionCode::Global => {
                    self.print_iter(section.get_global_section_reader()?, |me, _end, i| {
                        write!(me.state, "[global {}] {:?}", globals, i.ty)?;
                        globals += 1;
                        me.print(i.init_expr.get_binary_reader().original_position())?;
                        me.print_ops(i.init_expr.get_operators_reader())
                    })?
                }
                SectionCode::Alias => {
                    self.print_iter(section.get_alias_section_reader()?, |me, end, i| {
                        write!(me.state, "[alias] {:?}", i)?;
                        match i.kind {
                            ExternalKind::Function => funcs += 1,
                            ExternalKind::Global => globals += 1,
                            ExternalKind::Module => modules += 1,
                            ExternalKind::Table => tables += 1,
                            ExternalKind::Instance => instances += 1,
                            ExternalKind::Memory => memories += 1,
                            ExternalKind::Type => types += 1,
                        }
                        me.print(end)
                    })?
                }
                SectionCode::Module => {
                    let mut cnt = 0;
                    self.print_iter(section.get_module_section_reader()?, |me, end, i| {
                        write!(me.state, "[module {}] type {:?}", cnt + modules, i)?;
                        cnt += 1;
                        me.print(end)
                    })?
                }
                SectionCode::Instance => {
                    self.print_iter(section.get_instance_section_reader()?, |me, _end, i| {
                        write!(me.state, "[instance {}] module:{}", instances, i.module())?;
                        me.print(i.original_position())?;
                        instances += 1;
                        me.print_iter(i.args()?, |me, end, (kind, index)| {
                            write!(me.state, "[instantiate arg] {:?} {}", kind, index)?;
                            me.print(end)
                        })
                    })?
                }
                SectionCode::ModuleCode => {
                    self.print_iter(section.get_module_code_section_reader()?, |me, _end, i| {
                        write!(me.state, "inline module")?;
                        me.print(i.original_position())?;
                        modules += 1;
                        me.print_module(i.module()?)
                    })?
                }
                SectionCode::Start => {
                    let start = section.get_start_section_content()?;
                    write!(self.state, "start function {}", start)?;
                    self.print(section.range().end)?;
                }
                SectionCode::DataCount => {
                    let start = section.get_data_count_section_content()?;
                    write!(self.state, "data count {}", start)?;
                    self.print(section.range().end)?;
                }
                SectionCode::Element => {
                    self.print_iter(section.get_element_section_reader()?, |me, _end, i| {
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
                    })?
                }

                SectionCode::Data => {
                    self.print_iter(section.get_data_section_reader()?, |me, end, i| {
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
                    })?
                }

                SectionCode::Code => {
                    self.print_iter(section.get_code_section_reader()?, |me, _end, i| {
                        write!(
                            me.dst,
                            "============== func {} ====================\n",
                            funcs
                        )?;
                        funcs += 1;
                        write!(me.state, "size of function")?;
                        me.print(i.get_binary_reader().original_position())?;
                        let mut locals = i.get_locals_reader()?;
                        write!(me.state, "{} local blocks", locals.get_count())?;
                        me.print(locals.original_position())?;
                        for _ in 0..locals.get_count() {
                            let (amt, ty) = locals.read()?;
                            write!(me.state, "{} locals of type {:?}", amt, ty)?;
                            me.print(locals.original_position())?;
                        }
                        me.print_ops(i.get_operators_reader()?)?;
                        Ok(())
                    })?
                }

                SectionCode::Custom { kind, name: _ } => match kind {
                    CustomSectionKind::Name => {
                        let mut iter = section.get_name_section_reader()?;
                        while !iter.eof() {
                            self.print_custom_name_section(iter.read()?, iter.original_position())?;
                        }
                    }
                    _ => {
                        write!(self.dst, "0x{:04x} |", self.cur)?;
                        for _ in 0..NBYTES {
                            write!(self.dst, "---")?;
                        }
                        write!(
                            self.dst,
                            "-| ... {} bytes of data\n",
                            section.get_binary_reader().bytes_remaining()
                        )?;
                        self.cur = section.range().end;
                    }
                },
            }
        }

        self.nesting -= 1;
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
            Name::Function(n) => {
                write!(self.state, "function names")?;
                self.print(n.original_position())?;
                let mut map = n.get_map()?;
                write!(self.state, "{} count", map.get_count())?;
                self.print(map.original_position())?;
                for _ in 0..map.get_count() {
                    write!(self.state, "{:?}", map.read()?)?;
                    self.print(map.original_position())?;
                }
            }
            Name::Local(n) => {
                write!(self.state, "local names")?;
                self.print(n.original_position())?;
                let mut function_map = n.get_function_local_reader()?;
                write!(self.state, "{} count", function_map.get_count())?;
                self.print(function_map.original_position())?;
                for _ in 0..function_map.get_count() {
                    let function_names = function_map.read()?;
                    write!(self.state, "function {} locals", function_names.func_index)?;
                    self.print(function_names.original_position())?;
                    let mut map = function_names.get_map()?;
                    write!(self.state, "{} count", map.get_count())?;
                    self.print(map.original_position())?;
                    for _ in 0..map.get_count() {
                        write!(self.state, "{:?}", map.read()?)?;
                        self.print(map.original_position())?;
                    }
                }
            }
        }
        Ok(())
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
            "{:#x} >= {:#x}\n{}",
            self.cur,
            end,
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
