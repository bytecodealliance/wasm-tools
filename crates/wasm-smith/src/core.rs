//! Generating arbitary core Wasm modules.

mod code_builder;
mod encode;
mod terminate;

use crate::{arbitrary_loop, limited_string, unique_string, Config, DefaultConfig};
use arbitrary::{Arbitrary, Result, Unstructured};
use code_builder::CodeBuilderAllocations;
use flagset::{flags, FlagSet};
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::marker;
use std::ops::Range;
use std::rc::Rc;
use std::str::{self, FromStr};
use wasm_encoder::{BlockType, Export, GlobalType, MemoryType, TableType, ValType};

// NB: these constants are used to control the rate at which various events
// occur. For more information see where these constants are used. Their values
// are somewhat random in the sense that they're not scientifically determined
// or anything like that, I just threw a bunch of random data at wasm-smith and
// measured various rates of ooms/traps/etc and adjusted these so abnormal
// events were ~1% of the time.
const CHANCE_OFFSET_INBOUNDS: usize = 10; // bigger = less traps
const CHANCE_SEGMENT_ON_EMPTY: usize = 10; // bigger = less traps
const PCT_INBOUNDS: f64 = 0.995; // bigger = less traps

type Instruction = wasm_encoder::Instruction<'static>;

/// A pseudo-random WebAssembly module.
///
/// Construct instances of this type with [the `Arbitrary`
/// trait](https://docs.rs/arbitrary/*/arbitrary/trait.Arbitrary.html).
///
/// ## Configuring Generated Modules
///
/// This uses the [`DefaultConfig`][crate::DefaultConfig] configuration. If you
/// want to customize the shape of generated modules, define your own
/// configuration type, implement the [`Config`][crate::Config] trait for it,
/// and use [`ConfiguredModule<YourConfigType>`][crate::ConfiguredModule]
/// instead of plain `Module`.
#[derive(Debug)]
pub struct Module {
    config: Rc<dyn Config>,
    valtypes: Vec<ValType>,

    /// The initial sections of this wasm module, including types and imports.
    /// This is stored as a list-of-lists where each `InitialSection` represents
    /// a whole section, so this `initial_sections` list represents a list of
    /// sections.
    ///
    /// With the module linking proposal, types, imports, module, instance,
    /// and alias sections can come in any order and occur repeatedly at the
    /// start of a Wasm module. We want to generate interesting entities --
    /// entities that require multiple, interspersed occurrences of these
    /// sections -- and we don't want to always generate the "same shape" of
    /// these initial sections. Each entry in this initializers list is one of
    /// these initial sections, and we will directly encode each entry as a
    /// section when we serialize this module to bytes, which allows us to
    /// easily model the flexibility of the module linking proposal.
    initial_sections: Vec<InitialSection>,

    /// A map of what import names have been generated. The key here is the
    /// name of the import and the value is `None` if it's a single-level
    /// import or `Some` if it's a two-level import with the set of
    /// second-level import names that have been generated so far.
    import_names: HashMap<String, Option<HashSet<String>>>,

    /// All types locally defined in this module (available in the type index
    /// space).
    types: Vec<LocalType>,

    /// Indices within `types` that are function types.
    func_types: Vec<u32>,

    /// Number of imported items into this module.
    num_imports: usize,

    /// The number of tags defined in this module (not imported or
    /// aliased).
    num_defined_tags: usize,

    /// The number of functions defined in this module (not imported or
    /// aliased).
    num_defined_funcs: usize,

    /// The number of tables defined in this module (not imported or
    /// aliased).
    num_defined_tables: usize,

    /// The number of memories defined in this module (not imported or
    /// aliased).
    num_defined_memories: usize,

    /// The indexes and initialization expressions of globals defined in this
    /// module.
    defined_globals: Vec<(u32, Instruction)>,

    /// All tags available to this module, sorted by their index. The list
    /// entry is the type of each tag.
    tags: Vec<TagType>,

    /// All functions available to this module, sorted by their index. The list
    /// entry points to the index in this module where the function type is
    /// defined (if available) and provides the type of the function.
    ///
    /// Note that aliased functions may have types not defined in this module,
    /// hence the optional index type. All defined functions in this module,
    /// however, will have an index type specified.
    funcs: Vec<(Option<u32>, Rc<FuncType>)>,

    /// All tables available to this module, sorted by their index. The list
    /// entry is the type of each table.
    tables: Vec<TableType>,

    /// All globals available to this module, sorted by their index. The list
    /// entry is the type of each global.
    globals: Vec<GlobalType>,

    /// All memories available to this module, sorted by their index. The list
    /// entry is the type of each memory.
    memories: Vec<MemoryType>,

    exports: Vec<(String, Export)>,
    start: Option<u32>,
    elems: Vec<ElementSegment>,
    code: Vec<Code>,
    data: Vec<DataSegment>,

    /// The predicted size of the effective type of this module, based on this
    /// module's size of the types of imports/exports.
    type_size: u32,
}

impl<'a> Arbitrary<'a> for Module {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ConfiguredModule::<DefaultConfig>::arbitrary(u)?.module)
    }
}

/// A pseudo-random generated WebAssembly file with custom configuration.
///
/// If you don't care about custom configuration, use [`Module`][crate::Module]
/// instead.
///
/// For details on configuring, see the [`Config`][crate::Config] trait.
#[derive(Debug)]
pub struct ConfiguredModule<C> {
    /// The generated module, controlled by the configuration of `C` in the
    /// `Arbitrary` implementation.
    pub module: Module,
    _marker: marker::PhantomData<C>,
}

impl Module {
    /// Returns a reference to the internal configuration.
    pub fn config(&self) -> &dyn Config {
        &*self.config
    }

    /// Creates a new `Module` with the specified `config` for
    /// configuration and `Unstructured` for the DNA of this module.
    pub fn new(config: impl Config, u: &mut Unstructured<'_>) -> Result<Self> {
        let mut module = Module::empty(Rc::new(config));
        module.build(u, false)?;
        Ok(module)
    }

    fn empty(config: Rc<dyn Config>) -> Self {
        Module {
            config,
            valtypes: Vec::new(),
            initial_sections: Vec::new(),
            import_names: HashMap::new(),
            types: Vec::new(),
            func_types: Vec::new(),
            num_imports: 0,
            num_defined_tags: 0,
            num_defined_funcs: 0,
            num_defined_tables: 0,
            num_defined_memories: 0,
            defined_globals: Vec::new(),
            tags: Vec::new(),
            funcs: Vec::new(),
            tables: Vec::new(),
            globals: Vec::new(),
            memories: Vec::new(),
            exports: Vec::new(),
            start: None,
            elems: Vec::new(),
            code: Vec::new(),
            data: Vec::new(),
            type_size: 0,
        }
    }
}

impl<'a, C: Config + Arbitrary<'a>> Arbitrary<'a> for ConfiguredModule<C> {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ConfiguredModule {
            module: Module::new(C::arbitrary(u)?, u)?,
            _marker: marker::PhantomData,
        })
    }
}

/// Same as [`Module`], but may be invalid.
///
/// This module generates function bodies differnetly than `Module` to try to
/// better explore wasm decoders and such.
#[derive(Debug)]
pub struct MaybeInvalidModule {
    module: Module,
}

impl MaybeInvalidModule {
    /// Encode this Wasm module into bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        self.module.to_bytes()
    }
}

impl<'a> Arbitrary<'a> for MaybeInvalidModule {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let mut module = Module::empty(Rc::new(DefaultConfig));
        module.build(u, true)?;
        Ok(MaybeInvalidModule { module })
    }
}

#[derive(Debug)]
enum InitialSection {
    Type(Vec<Type>),
    Import(Vec<Import>),
}

#[derive(Clone, Debug)]
pub(crate) enum Type {
    Func(Rc<FuncType>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct FuncType {
    params: Vec<ValType>,
    results: Vec<ValType>,
}

#[derive(Clone, Debug)]
pub(crate) struct Import(pub(crate) String, pub(crate) String, pub(crate) EntityType);

#[derive(Clone, Debug)]
pub(crate) enum EntityType {
    Global(GlobalType),
    Table(TableType),
    Memory(MemoryType),
    Tag(TagType),
    Func(u32, Rc<FuncType>),
}

#[derive(Clone, Debug)]
pub(crate) struct TagType {
    func_type_idx: u32,
    func_type: Rc<FuncType>,
}

#[derive(Debug)]
struct ElementSegment {
    kind: ElementKind,
    ty: ValType,
    items: Elements,
}

#[derive(Debug)]
enum ElementKind {
    Passive,
    Declared,
    Active {
        table: Option<u32>, // None == table 0 implicitly
        offset: Instruction,
    },
}

#[derive(Debug)]
enum Elements {
    Functions(Vec<u32>),
    Expressions(Vec<Option<u32>>),
}

#[derive(Debug)]
struct Code {
    locals: Vec<ValType>,
    instructions: Instructions,
}

#[derive(Debug)]
enum Instructions {
    Generated(Vec<Instruction>),
    Arbitrary(Vec<u8>),
}

#[derive(Debug)]
struct DataSegment {
    kind: DataSegmentKind,
    init: Vec<u8>,
}

#[derive(Debug)]
enum DataSegmentKind {
    Passive,
    Active {
        memory_index: u32,
        offset: Instruction,
    },
}

impl Module {
    fn build(&mut self, u: &mut Unstructured, allow_invalid: bool) -> Result<()> {
        self.valtypes.push(ValType::I32);
        self.valtypes.push(ValType::I64);
        self.valtypes.push(ValType::F32);
        self.valtypes.push(ValType::F64);
        if self.config.simd_enabled() {
            self.valtypes.push(ValType::V128);
        }
        if self.config.reference_types_enabled() {
            self.valtypes.push(ValType::ExternRef);
            self.valtypes.push(ValType::FuncRef);
        }
        self.arbitrary_initial_sections(u)?;
        self.arbitrary_tags(u)?;
        self.arbitrary_funcs(u)?;
        self.arbitrary_tables(u)?;
        self.arbitrary_memories(u)?;
        self.arbitrary_globals(u)?;
        self.arbitrary_exports(u)?;
        self.arbitrary_start(u)?;
        self.arbitrary_elems(u)?;
        self.arbitrary_data(u)?;
        self.arbitrary_code(u, allow_invalid)?;
        Ok(())
    }

    fn arbitrary_initial_sections(&mut self, u: &mut Unstructured) -> Result<()> {
        self.arbitrary_types(self.config.min_types(), u)?;
        self.arbitrary_imports(self.config.min_imports(), u)?;
        Ok(())
    }

    fn arbitrary_types(&mut self, min: usize, u: &mut Unstructured) -> Result<()> {
        // Note that we push to `self.initializers` immediately because types
        // can mention any previous types, so we need to ensure that after each
        // type is generated it's listed in the module's types so indexing will
        // succeed.
        let section_idx = self.initial_sections.len();
        self.initial_sections.push(InitialSection::Type(Vec::new()));
        arbitrary_loop(u, min, self.config.max_types() - self.types.len(), |u| {
            let ty = self.arbitrary_type(u)?;
            self.record_type(&ty);
            let types = match self.initial_sections.last_mut().unwrap() {
                InitialSection::Type(list) => list,
                _ => unreachable!(),
            };
            self.types.push(LocalType::Defined {
                section: section_idx,
                nth: types.len(),
            });
            types.push(ty);
            Ok(true)
        })?;
        let types = match self.initial_sections.last_mut().unwrap() {
            InitialSection::Type(list) => list,
            _ => unreachable!(),
        };
        if types.is_empty() && !u.arbitrary()? {
            self.initial_sections.pop();
        }
        Ok(())
    }

    fn record_type(&mut self, ty: &Type) {
        let list = match &ty {
            Type::Func(_) => &mut self.func_types,
        };
        list.push(self.types.len() as u32);
    }

    fn arbitrary_type(&mut self, u: &mut Unstructured) -> Result<Type> {
        Ok(Type::Func(self.arbitrary_func_type(u)?))
    }

    fn arbitrary_func_type(&mut self, u: &mut Unstructured) -> Result<Rc<FuncType>> {
        arbitrary_func_type(u, &self.valtypes)
    }

    fn can_add_local_or_import_tag(&self) -> bool {
        self.config.exceptions_enabled()
            && self.has_tag_func_types()
            && self.tags.len() < self.config.max_tags()
    }

    fn can_add_local_or_import_func(&self) -> bool {
        self.func_types.len() > 0 && self.funcs.len() < self.config.max_funcs()
    }

    fn can_add_local_or_import_table(&self) -> bool {
        self.tables.len() < self.config.max_tables()
    }

    fn can_add_local_or_import_global(&self) -> bool {
        self.globals.len() < self.config.max_globals()
    }

    fn can_add_local_or_import_memory(&self) -> bool {
        self.memories.len() < self.config.max_memories()
    }

    fn arbitrary_imports(&mut self, min: usize, u: &mut Unstructured) -> Result<()> {
        if self.config.max_type_size() < self.type_size {
            return Ok(());
        }

        let mut choices: Vec<fn(&mut Unstructured, &mut Module) -> Result<EntityType>> =
            Vec::with_capacity(4);

        let mut imports = Vec::new();
        arbitrary_loop(u, min, self.config.max_imports() - self.num_imports, |u| {
            choices.clear();
            if self.can_add_local_or_import_tag() {
                choices.push(|u, m| {
                    let ty = m.arbitrary_tag_type(u)?;
                    Ok(EntityType::Tag(ty))
                });
            }
            if self.can_add_local_or_import_func() {
                choices.push(|u, m| {
                    let idx = *u.choose(&m.func_types)?;
                    let ty = m.func_type(idx).clone();
                    Ok(EntityType::Func(idx, ty))
                });
            }
            if self.can_add_local_or_import_global() {
                choices.push(|u, m| {
                    let ty = m.arbitrary_global_type(u)?;
                    Ok(EntityType::Global(ty))
                });
            }
            if self.can_add_local_or_import_memory() {
                choices.push(|u, m| {
                    let ty = m.arbitrary_memtype(u)?;
                    Ok(EntityType::Memory(ty))
                });
            }
            if self.can_add_local_or_import_table() {
                choices.push(|u, m| {
                    let ty = m.arbitrary_table_type(u)?;
                    Ok(EntityType::Table(ty))
                });
            }

            if choices.is_empty() {
                // We are out of choices. If we have not have reached the minimum yet, then we
                // have no way to satisfy the constraint, but we follow max-constraints before
                // the min-import constraint.
                return Ok(false);
            }

            // Generate a type to import, but only actually add the item if the
            // type size budget allows us to.
            let f = u.choose(&choices)?;
            let ty = f(u, self)?;
            let budget = self.config.max_type_size() - self.type_size;
            if ty.size() + 1 > budget {
                return Ok(false);
            }
            self.type_size += ty.size() + 1;

            // Generate an arbitrary module/name pair to name this import. Note
            // that if module-linking is enabled and `name` is present, then we
            // might be implicitly generating an instance. If that's the case
            // then we need to record the type of this instance.
            let (module, name) = unique_import_strings(1_000, u)?;

            // Once our name is determined, and if module linking is enabled
            // we've inserted the implicit instance, then we push the typed item
            // into the appropriate namespace.
            match &ty {
                EntityType::Tag(ty) => self.tags.push(ty.clone()),
                EntityType::Func(idx, ty) => self.funcs.push((Some(*idx), ty.clone())),
                EntityType::Global(ty) => self.globals.push(ty.clone()),
                EntityType::Table(ty) => self.tables.push(ty.clone()),
                EntityType::Memory(ty) => self.memories.push(ty.clone()),
            }

            self.num_imports += 1;
            imports.push(Import(module, name, ty));
            Ok(true)
        })?;
        if !imports.is_empty() || u.arbitrary()? {
            self.initial_sections.push(InitialSection::Import(imports));
        }

        // After an import section we can no longer update previously-defined
        // pseudo-instance imports, so set them all to `None` indicating that
        // the bare name is imported and finalized.
        for val in self.import_names.values_mut() {
            *val = None;
        }
        Ok(())
    }

    fn type_of(&self, item: &Export) -> EntityType {
        match *item {
            Export::Global(idx) => EntityType::Global(self.globals[idx as usize].clone()),
            Export::Memory(idx) => EntityType::Memory(self.memories[idx as usize].clone()),
            Export::Table(idx) => EntityType::Table(self.tables[idx as usize].clone()),
            Export::Function(idx) => {
                let (_idx, ty) = &self.funcs[idx as usize];
                EntityType::Func(u32::max_value(), ty.clone())
            }
            Export::Tag(idx) => EntityType::Tag(self.tags[idx as usize].clone()),
        }
    }

    fn ty(&self, idx: u32) -> &Type {
        match &self.types[idx as usize] {
            LocalType::Defined { section, nth } => {
                if let InitialSection::Type(list) = &self.initial_sections[*section] {
                    return &list[*nth];
                }
                panic!("looked up a type with the wrong index")
            }
        }
    }

    fn func_types<'a>(&'a self) -> impl Iterator<Item = (u32, &'a FuncType)> + 'a {
        self.func_types
            .iter()
            .copied()
            .map(move |type_i| (type_i, &**self.func_type(type_i)))
    }

    fn func_type(&self, idx: u32) -> &Rc<FuncType> {
        match self.ty(idx) {
            Type::Func(f) => f,
        }
    }

    fn tags<'a>(&'a self) -> impl Iterator<Item = (u32, &'a TagType)> + 'a {
        self.tags
            .iter()
            .enumerate()
            .map(move |(i, ty)| (i as u32, ty))
    }

    fn funcs<'a>(&'a self) -> impl Iterator<Item = (u32, &'a Rc<FuncType>)> + 'a {
        self.funcs
            .iter()
            .enumerate()
            .map(move |(i, (_, ty))| (i as u32, ty))
    }

    fn has_tag_func_types(&self) -> bool {
        self.tag_func_types().next().is_some()
    }

    fn tag_func_types<'a>(&'a self) -> impl Iterator<Item = u32> + 'a {
        self.func_types
            .iter()
            .copied()
            .filter(move |i| self.func_type(*i).results.len() == 0)
    }

    fn arbitrary_valtype(&self, u: &mut Unstructured) -> Result<ValType> {
        Ok(*u.choose(&self.valtypes)?)
    }

    fn arbitrary_global_type(&self, u: &mut Unstructured) -> Result<GlobalType> {
        Ok(GlobalType {
            val_type: self.arbitrary_valtype(u)?,
            mutable: u.arbitrary()?,
        })
    }

    fn arbitrary_table_type(&self, u: &mut Unstructured) -> Result<TableType> {
        // We don't want to generate tables that are too large on average, so
        // keep the "inbounds" limit here a bit smaller.
        let max_inbounds = 10_000;
        let (minimum, maximum) = self.arbitrary_limits32(u, 1_000_000, false, max_inbounds)?;
        Ok(TableType {
            element_type: if self.config.reference_types_enabled() {
                *u.choose(&[ValType::FuncRef, ValType::ExternRef])?
            } else {
                ValType::FuncRef
            },
            minimum,
            maximum,
        })
    }

    fn arbitrary_tag_type(&self, u: &mut Unstructured) -> Result<TagType> {
        let candidate_func_types = self.tag_func_types().collect::<Vec<_>>();
        let max = candidate_func_types.len() - 1;
        let ty = candidate_func_types[u.int_in_range(0..=max)?];
        Ok(TagType {
            func_type_idx: ty,
            func_type: self.func_type(ty).clone(),
        })
    }

    fn arbitrary_tags(&mut self, u: &mut Unstructured) -> Result<()> {
        if !self.config.exceptions_enabled() || !self.has_tag_func_types() {
            return Ok(());
        }

        arbitrary_loop(u, self.config.min_tags(), self.config.max_tags(), |u| {
            if !self.can_add_local_or_import_tag() {
                return Ok(false);
            }
            self.tags.push(self.arbitrary_tag_type(u)?);
            self.num_defined_tags += 1;
            Ok(true)
        })
    }

    fn arbitrary_funcs(&mut self, u: &mut Unstructured) -> Result<()> {
        if self.func_types.is_empty() {
            return Ok(());
        }

        arbitrary_loop(u, self.config.min_funcs(), self.config.max_funcs(), |u| {
            if !self.can_add_local_or_import_func() {
                return Ok(false);
            }
            let max = self.func_types.len() - 1;
            let ty = self.func_types[u.int_in_range(0..=max)?];
            self.funcs.push((Some(ty), self.func_type(ty).clone()));
            self.num_defined_funcs += 1;
            Ok(true)
        })
    }

    fn arbitrary_tables(&mut self, u: &mut Unstructured) -> Result<()> {
        arbitrary_loop(
            u,
            self.config.min_tables() as usize,
            self.config.max_tables() as usize,
            |u| {
                if !self.can_add_local_or_import_table() {
                    return Ok(false);
                }
                self.num_defined_tables += 1;
                let ty = self.arbitrary_table_type(u)?;
                self.tables.push(ty);
                Ok(true)
            },
        )
    }

    fn arbitrary_memtype(&self, u: &mut Unstructured) -> Result<MemoryType> {
        let memory64 = self.config.memory64_enabled() && u.arbitrary()?;
        // We want to favor memories <= 1gb in size, allocate at most 16k pages,
        // depending on the maximum number of memories.
        let max_inbounds = 16 * 1024 / u64::try_from(self.config.max_memories()).unwrap();
        let max_pages = self.config.max_memory_pages(memory64);
        let (minimum, maximum) = self.arbitrary_limits64(
            u,
            max_pages,
            self.config.memory_max_size_required(),
            max_inbounds.min(max_pages),
        )?;
        Ok(MemoryType {
            minimum,
            maximum,
            memory64,
        })
    }

    fn arbitrary_memories(&mut self, u: &mut Unstructured) -> Result<()> {
        arbitrary_loop(
            u,
            self.config.min_memories() as usize,
            self.config.max_memories() as usize,
            |u| {
                if !self.can_add_local_or_import_memory() {
                    return Ok(false);
                }
                self.num_defined_memories += 1;
                self.memories.push(self.arbitrary_memtype(u)?);
                Ok(true)
            },
        )
    }

    fn arbitrary_globals(&mut self, u: &mut Unstructured) -> Result<()> {
        let mut choices: Vec<Box<dyn Fn(&mut Unstructured, ValType) -> Result<Instruction>>> =
            vec![];
        let num_imported_globals = self.globals.len();

        arbitrary_loop(
            u,
            self.config.min_globals(),
            self.config.max_globals(),
            |u| {
                if !self.can_add_local_or_import_global() {
                    return Ok(false);
                }

                let ty = self.arbitrary_global_type(u)?;

                choices.clear();
                let num_funcs = self.funcs.len() as u32;
                choices.push(Box::new(move |u, ty| {
                    Ok(match ty {
                        ValType::I32 => Instruction::I32Const(u.arbitrary()?),
                        ValType::I64 => Instruction::I64Const(u.arbitrary()?),
                        ValType::F32 => Instruction::F32Const(u.arbitrary()?),
                        ValType::F64 => Instruction::F64Const(u.arbitrary()?),
                        ValType::V128 => Instruction::V128Const(u.arbitrary()?),
                        ValType::ExternRef => Instruction::RefNull(ValType::ExternRef),
                        ValType::FuncRef => {
                            if num_funcs > 0 && u.arbitrary()? {
                                let func = u.int_in_range(0..=num_funcs - 1)?;
                                Instruction::RefFunc(func)
                            } else {
                                Instruction::RefNull(ValType::FuncRef)
                            }
                        }
                    })
                }));

                for (i, g) in self.globals[..num_imported_globals].iter().enumerate() {
                    if !g.mutable && g.val_type == ty.val_type {
                        choices.push(Box::new(move |_, _| Ok(Instruction::GlobalGet(i as u32))));
                    }
                }

                let f = u.choose(&choices)?;
                let expr = f(u, ty.val_type)?;
                let global_idx = self.globals.len() as u32;
                self.globals.push(ty);
                self.defined_globals.push((global_idx, expr));
                Ok(true)
            },
        )
    }

    fn arbitrary_exports(&mut self, u: &mut Unstructured) -> Result<()> {
        if self.config.max_type_size() < self.type_size {
            return Ok(());
        }

        // Build up a list of candidates for each class of import
        let mut choices: Vec<Vec<Export>> = Vec::with_capacity(6);
        choices.push(
            (0..self.funcs.len())
                .map(|i| Export::Function(i as u32))
                .collect(),
        );
        choices.push(
            (0..self.tables.len())
                .map(|i| Export::Table(i as u32))
                .collect(),
        );
        choices.push(
            (0..self.memories.len())
                .map(|i| Export::Memory(i as u32))
                .collect(),
        );
        choices.push(
            (0..self.globals.len())
                .map(|i| Export::Global(i as u32))
                .collect(),
        );

        let mut export_names = HashSet::new();
        arbitrary_loop(
            u,
            self.config.min_exports(),
            self.config.max_exports(),
            |u| {
                // Remove all candidates for export whose type size exceeds our
                // remaining budget for type size. Then also remove any classes
                // of exports which no longer have any candidates.
                //
                // If there's nothing remaining after this, then we're done.
                let max_size = self.config.max_type_size() - self.type_size;
                for list in choices.iter_mut() {
                    list.retain(|c| self.type_of(c).size() + 1 < max_size);
                }
                choices.retain(|list| list.len() > 0);
                if choices.len() == 0 {
                    return Ok(false);
                }

                // Pick a name, then pick the export, and then we can record
                // information about the chosen export.
                let name = unique_string(1_000, &mut export_names, u)?;
                let list = u.choose(&choices)?;
                let export = u.choose(list)?;
                let ty = self.type_of(export);
                self.type_size += 1 + ty.size();
                self.exports.push((name, *export));
                Ok(true)
            },
        )
    }

    fn arbitrary_start(&mut self, u: &mut Unstructured) -> Result<()> {
        if !self.config.allow_start_export() {
            return Ok(());
        }

        let mut choices = Vec::with_capacity(self.funcs.len() as usize);

        for (func_idx, ty) in self.funcs() {
            if ty.params.is_empty() && ty.results.is_empty() {
                choices.push(func_idx);
            }
        }

        if !choices.is_empty() && u.arbitrary().unwrap_or(false) {
            let f = *u.choose(&choices)?;
            self.start = Some(f);
        }

        Ok(())
    }

    fn arbitrary_elems(&mut self, u: &mut Unstructured) -> Result<()> {
        let func_max = self.funcs.len() as u32;

        // Create a helper closure to choose an arbitrary offset.
        let mut offset_global_choices = vec![];
        for (i, g) in self.globals[..self.globals.len() - self.defined_globals.len()]
            .iter()
            .enumerate()
        {
            if !g.mutable && g.val_type == ValType::I32 {
                offset_global_choices.push(i as u32);
            }
        }
        let arbitrary_active_elem = |u: &mut Unstructured, min: u32, table: Option<u32>| {
            let (offset, max_size_hint) = if !offset_global_choices.is_empty() && u.arbitrary()? {
                let g = u.choose(&offset_global_choices)?;
                (Instruction::GlobalGet(*g), None)
            } else {
                let offset = arbitrary_offset(u, min.into(), u32::MAX.into(), 0)? as u32;
                let max_size_hint =
                    if offset <= min && u.int_in_range(0..=CHANCE_OFFSET_INBOUNDS)? != 0 {
                        Some(min - offset)
                    } else {
                        None
                    };
                (Instruction::I32Const(offset as i32), max_size_hint)
            };
            Ok((ElementKind::Active { table, offset }, max_size_hint))
        };

        type GenElemSegment<'a> =
            dyn Fn(&mut Unstructured) -> Result<(ElementKind, Option<u32>)> + 'a;
        let mut funcrefs: Vec<Box<GenElemSegment>> = Vec::new();
        let mut externrefs: Vec<Box<GenElemSegment>> = Vec::new();

        for (i, ty) in self.tables.iter().enumerate() {
            // If this table starts with no capacity then any non-empty element
            // segment placed onto it will immediately trap, which isn't too
            // too interesting. If that's the case give it an unlikely chance
            // of proceeding.
            if ty.minimum == 0 && u.int_in_range(0..=CHANCE_SEGMENT_ON_EMPTY)? != 0 {
                continue;
            }

            let dst = if ty.element_type == ValType::FuncRef {
                &mut funcrefs
            } else {
                &mut externrefs
            };
            let minimum = ty.minimum;
            // If the first table is a funcref table then it's a candidate for
            // the MVP encoding of element segments.
            if i == 0 && ty.element_type == ValType::FuncRef {
                dst.push(Box::new(move |u| arbitrary_active_elem(u, minimum, None)));
            }
            dst.push(Box::new(move |u| {
                arbitrary_active_elem(u, minimum, Some(i as u32))
            }));
        }

        // Reference types allows us to create passive and declared element
        // segments.
        if self.config.reference_types_enabled() {
            funcrefs.push(Box::new(|_| Ok((ElementKind::Passive, None))));
            externrefs.push(Box::new(|_| Ok((ElementKind::Passive, None))));
            funcrefs.push(Box::new(|_| Ok((ElementKind::Declared, None))));
            externrefs.push(Box::new(|_| Ok((ElementKind::Declared, None))));
        }

        let mut choices = Vec::new();
        if !funcrefs.is_empty() {
            choices.push((&funcrefs, ValType::FuncRef));
        }
        if !externrefs.is_empty() {
            choices.push((&externrefs, ValType::ExternRef));
        }

        if choices.is_empty() {
            return Ok(());
        }
        arbitrary_loop(
            u,
            self.config.min_element_segments(),
            self.config.max_element_segments(),
            |u| {
                // Choose whether to generate a segment whose elements are initialized via
                // expressions, or one whose elements are initialized via function indices.
                let (kind_candidates, ty) = *u.choose(&choices)?;

                // Select a kind for this segment now that we know the number of
                // items the segment will hold.
                let (kind, max_size_hint) = u.choose(&kind_candidates)?(u)?;
                let max = max_size_hint
                    .map(|i| usize::try_from(i).unwrap())
                    .unwrap_or(self.config.max_elements());

                // Pick whether we're going to use expression elements or
                // indices. Note that externrefs must use expressions,
                // and functions without reference types must use indices.
                let items = if ty == ValType::ExternRef
                    || (self.config.reference_types_enabled() && u.arbitrary()?)
                {
                    let mut init = vec![];
                    arbitrary_loop(u, self.config.min_elements(), max, |u| {
                        init.push(
                            if ty == ValType::ExternRef || func_max == 0 || u.arbitrary()? {
                                None
                            } else {
                                Some(u.int_in_range(0..=func_max - 1)?)
                            },
                        );
                        Ok(true)
                    })?;
                    Elements::Expressions(init)
                } else {
                    let mut init = vec![];
                    if func_max > 0 {
                        arbitrary_loop(u, self.config.min_elements(), max, |u| {
                            let func_idx = u.int_in_range(0..=func_max - 1)?;
                            init.push(func_idx);
                            Ok(true)
                        })?;
                    }
                    Elements::Functions(init)
                };

                self.elems.push(ElementSegment { kind, ty, items });
                Ok(true)
            },
        )
    }

    fn arbitrary_code(&mut self, u: &mut Unstructured, allow_invalid: bool) -> Result<()> {
        self.code.reserve(self.num_defined_funcs);
        let mut allocs = CodeBuilderAllocations::new(self);
        for (_, ty) in self.funcs[self.funcs.len() - self.num_defined_funcs..].iter() {
            let body = self.arbitrary_func_body(u, ty, &mut allocs, allow_invalid)?;
            self.code.push(body);
        }
        Ok(())
    }

    fn arbitrary_func_body(
        &self,
        u: &mut Unstructured,
        ty: &FuncType,
        allocs: &mut CodeBuilderAllocations,
        allow_invalid: bool,
    ) -> Result<Code> {
        let mut locals = self.arbitrary_locals(u)?;
        let builder = allocs.builder(ty, &mut locals);
        let instructions = if allow_invalid && u.arbitrary().unwrap_or(false) {
            Instructions::Arbitrary(arbitrary_vec_u8(u)?)
        } else {
            Instructions::Generated(builder.arbitrary(u, self)?)
        };

        Ok(Code {
            locals,
            instructions,
        })
    }

    fn arbitrary_locals(&self, u: &mut Unstructured) -> Result<Vec<ValType>> {
        let mut ret = Vec::new();
        arbitrary_loop(u, 0, 100, |u| {
            ret.push(self.arbitrary_valtype(u)?);
            Ok(true)
        })?;
        Ok(ret)
    }

    fn arbitrary_data(&mut self, u: &mut Unstructured) -> Result<()> {
        // With bulk-memory we can generate passive data, otherwise if there are
        // no memories we can't generate any data.
        let memories = self.memories.len() as u32;
        if memories == 0 && !self.config.bulk_memory_enabled() {
            return Ok(());
        }

        let mut choices32: Vec<Box<dyn Fn(&mut Unstructured, u64, usize) -> Result<Instruction>>> =
            vec![];
        choices32.push(Box::new(|u, min_size, data_len| {
            Ok(Instruction::I32Const(arbitrary_offset(
                u,
                u32::try_from(min_size.saturating_mul(64 * 1024))
                    .unwrap_or(u32::MAX)
                    .into(),
                u32::MAX.into(),
                data_len,
            )? as i32))
        }));
        let mut choices64: Vec<Box<dyn Fn(&mut Unstructured, u64, usize) -> Result<Instruction>>> =
            vec![];
        choices64.push(Box::new(|u, min_size, data_len| {
            Ok(Instruction::I64Const(arbitrary_offset(
                u,
                min_size.saturating_mul(64 * 1024),
                u64::MAX,
                data_len,
            )? as i64))
        }));

        for (i, g) in self.globals[..self.globals.len() - self.defined_globals.len()]
            .iter()
            .enumerate()
        {
            if g.mutable {
                continue;
            }
            if g.val_type == ValType::I32 {
                choices32.push(Box::new(move |_, _, _| {
                    Ok(Instruction::GlobalGet(i as u32))
                }));
            } else if g.val_type == ValType::I64 {
                choices64.push(Box::new(move |_, _, _| {
                    Ok(Instruction::GlobalGet(i as u32))
                }));
            }
        }

        // Build a list of candidate memories that we'll add data initializers
        // for. If a memory doesn't have an initial size then any initializers
        // for that memory will trap instantiation, which isn't too
        // interesting. Try to make this happen less often by making it less
        // likely that a memory with 0 size will have a data segment.
        let mut memories = Vec::new();
        for (i, mem) in self.memories.iter().enumerate() {
            if mem.minimum > 0 || u.int_in_range(0..=CHANCE_SEGMENT_ON_EMPTY)? == 0 {
                memories.push(i as u32);
            }
        }

        // With memories we can generate data segments, and with bulk memory we
        // can generate passive segments. Without these though we can't create
        // a valid module with data segments.
        if memories.len() == 0 && !self.config.bulk_memory_enabled() {
            return Ok(());
        }

        arbitrary_loop(
            u,
            self.config.min_data_segments(),
            self.config.max_data_segments(),
            |u| {
                let init: Vec<u8> = u.arbitrary()?;

                // Passive data can only be generated if bulk memory is enabled.
                // Otherwise if there are no memories we *only* generate passive
                // data. Finally if all conditions are met we use an input byte to
                // determine if it should be passive or active.
                let kind = if self.config.bulk_memory_enabled()
                    && (memories.is_empty() || u.arbitrary()?)
                {
                    DataSegmentKind::Passive
                } else {
                    let memory_index = *u.choose(&memories)?;
                    let mem = &self.memories[memory_index as usize];
                    let f = if mem.memory64 {
                        u.choose(&choices64)?
                    } else {
                        u.choose(&choices32)?
                    };
                    let offset = f(u, mem.minimum, init.len())?;
                    DataSegmentKind::Active {
                        offset,
                        memory_index,
                    }
                };
                self.data.push(DataSegment { kind, init });
                Ok(true)
            },
        )
    }

    fn arbitrary_limits32(
        &self,
        u: &mut Unstructured,
        max_minimum: u32,
        max_required: bool,
        max_inbounds: u32,
    ) -> Result<(u32, Option<u32>)> {
        let (min, max) =
            self.arbitrary_limits64(u, max_minimum.into(), max_required, max_inbounds.into())?;
        Ok((
            u32::try_from(min).unwrap(),
            max.map(|i| u32::try_from(i).unwrap()),
        ))
    }

    fn arbitrary_limits64(
        &self,
        u: &mut Unstructured,
        max_minimum: u64,
        max_required: bool,
        max_inbounds: u64,
    ) -> Result<(u64, Option<u64>)> {
        let min = gradually_grow(u, 0, max_inbounds, max_minimum)?;
        let max = if max_required || u.arbitrary().unwrap_or(false) {
            Some(u.int_in_range(min..=max_minimum)?)
        } else {
            None
        };
        Ok((min, max))
    }

    fn params_results(&self, ty: &BlockType) -> (Vec<ValType>, Vec<ValType>) {
        match ty {
            BlockType::Empty => (vec![], vec![]),
            BlockType::Result(t) => (vec![], vec![*t]),
            BlockType::FunctionType(ty) => {
                let ty = self.func_type(*ty);
                (ty.params.clone(), ty.results.clone())
            }
        }
    }
}

pub(crate) fn arbitrary_func_type(
    u: &mut Unstructured,
    valtypes: &[ValType],
) -> Result<Rc<FuncType>> {
    let mut params = vec![];
    let mut results = vec![];
    arbitrary_loop(u, 0, 20, |u| {
        params.push(arbitrary_valtype(u, valtypes)?);
        Ok(true)
    })?;
    arbitrary_loop(u, 0, 20, |u| {
        results.push(arbitrary_valtype(u, valtypes)?);
        Ok(true)
    })?;
    Ok(Rc::new(FuncType { params, results }))
}

fn arbitrary_valtype(u: &mut Unstructured, valtypes: &[ValType]) -> Result<ValType> {
    Ok(*u.choose(valtypes)?)
}

/// This function generates a number between `min` and `max`, favoring values
/// between `min` and `max_inbounds`.
///
/// The thinking behind this function is that it's used for things like offsets
/// and minimum sizes which, when very large, can trivially make the wasm oom or
/// abort with a trap. This isn't the most interesting thing to do so it tries
/// to favor numbers in the `min..max_inbounds` range to avoid immediate ooms.
fn gradually_grow(u: &mut Unstructured, min: u64, max_inbounds: u64, max: u64) -> Result<u64> {
    if min == max {
        return Ok(min);
    }
    let min = min as f64;
    let max = max as f64;
    let max_inbounds = max_inbounds as f64;
    let x = u.arbitrary::<u32>()?;
    let x = f64::from(x);
    let x = map_custom(
        x,
        f64::from(u32::MIN)..f64::from(u32::MAX),
        min..max_inbounds,
        min..max,
    );
    return Ok(x.round() as u64);

    /// Map a value from within the input range to the output range(s).
    ///
    /// This will first map the input range into the `0..1` input range, and
    /// then depending on the value it will either map it exponentially
    /// (favoring small values) into the `output_inbounds` range or it will map
    /// it into the `output` range.
    fn map_custom(
        value: f64,
        input: Range<f64>,
        output_inbounds: Range<f64>,
        output: Range<f64>,
    ) -> f64 {
        assert!(!value.is_nan(), "{}", value);
        assert!(value.is_finite(), "{}", value);
        assert!(input.start < input.end, "{} < {}", input.start, input.end);
        assert!(
            output.start < output.end,
            "{} < {}",
            output.start,
            output.end
        );
        assert!(value >= input.start, "{} >= {}", value, input.start);
        assert!(value <= input.end, "{} <= {}", value, input.end);
        assert!(
            output.start <= output_inbounds.start,
            "{} <= {}",
            output.start,
            output_inbounds.start
        );
        assert!(
            output_inbounds.end <= output.end,
            "{} <= {}",
            output_inbounds.end,
            output.end
        );

        let x = map_linear(value, input.clone(), 0.0..1.0);
        let result = if x < PCT_INBOUNDS {
            if output_inbounds.start == output_inbounds.end {
                output_inbounds.start
            } else {
                let unscaled = x * x * x * x * x * x;
                map_linear(unscaled, 0.0..1.0, output_inbounds.clone())
            }
        } else {
            map_linear(x, 0.0..1.0, output.clone())
        };

        assert!(result >= output.start, "{} >= {}", result, output.start);
        assert!(result <= output.end, "{} <= {}", result, output.end);
        result
    }

    /// Map a value from within the input range linearly to the output range.
    ///
    /// For example, mapping `0.5` from the input range `0.0..1.0` to the output
    /// range `1.0..3.0` produces `2.0`.
    fn map_linear(
        value: f64,
        Range {
            start: in_low,
            end: in_high,
        }: Range<f64>,
        Range {
            start: out_low,
            end: out_high,
        }: Range<f64>,
    ) -> f64 {
        assert!(!value.is_nan(), "{}", value);
        assert!(value.is_finite(), "{}", value);
        assert!(in_low < in_high, "{} < {}", in_low, in_high);
        assert!(out_low < out_high, "{} < {}", out_low, out_high);
        assert!(value >= in_low, "{} >= {}", value, in_low);
        assert!(value <= in_high, "{} <= {}", value, in_high);

        let dividend = out_high - out_low;
        let divisor = in_high - in_low;
        let slope = dividend / divisor;
        let result = out_low + (slope * (value - in_low));

        assert!(result >= out_low, "{} >= {}", result, out_low);
        assert!(result <= out_high, "{} <= {}", result, out_high);
        result
    }
}

/// Selects a reasonable offset for an element or data segment. This favors
/// having the segment being in-bounds, but it may still generate
/// any offset.
fn arbitrary_offset(u: &mut Unstructured, min: u64, max: u64, size: usize) -> Result<u64> {
    let size = u64::try_from(size).unwrap();

    // If the segment is too big for the whole memory, just give it any
    // offset.
    if size > min {
        u.int_in_range(0..=max)
    } else {
        gradually_grow(u, 0, min - size, max)
    }
}

fn unique_import_strings(max_size: usize, u: &mut Unstructured) -> Result<(String, String)> {
    let module = limited_string(max_size, u)?;
    let field = limited_string(max_size, u)?;
    Ok((module, field))
}

fn arbitrary_vec_u8(u: &mut Unstructured) -> Result<Vec<u8>> {
    let size = u.arbitrary_len::<u8>()?;
    Ok(u.bytes(size)?.to_vec())
}

impl EntityType {
    fn size(&self) -> u32 {
        match self {
            EntityType::Tag(_)
            | EntityType::Global(_)
            | EntityType::Table(_)
            | EntityType::Memory(_) => 1,
            EntityType::Func(_, ty) => 1 + (ty.params.len() + ty.results.len()) as u32,
        }
    }
}

// A helper structure used when generating module/instance types to limit the
// amount of each kind of import created.
#[derive(Default, Clone, Copy, PartialEq)]
struct Entities {
    globals: usize,
    memories: usize,
    tables: usize,
    funcs: usize,
    tags: usize,
}

#[derive(Clone, Debug)]
enum LocalType {
    /// A type that's locally defined in a module via a type section.
    Defined {
        /// The section (index within `Module::initializers` that this
        /// type is defined.
        section: usize,
        /// Which element within the section definition this type corresponds
        /// to.
        nth: usize,
    },
}

/// A container for the kinds of instructions that wasm-smith is allowed to
/// emit.
///
/// # Example
///
/// ```
/// # use wasm_smith::{InstructionKinds, InstructionKind};
/// let kinds = InstructionKinds::new(&[InstructionKind::Numeric, InstructionKind::Memory]);
/// assert!(kinds.contains(InstructionKind::Memory));
/// ```
#[derive(Clone, Copy, Debug, Default)]
pub struct InstructionKinds(pub(crate) FlagSet<InstructionKind>);
impl InstructionKinds {
    /// Create a new container.
    pub fn new(kinds: &[InstructionKind]) -> Self {
        Self(kinds.iter().fold(FlagSet::default(), |ks, k| ks | *k))
    }

    /// Include all [InstructionKind]s.
    pub fn all() -> Self {
        Self(FlagSet::full())
    }

    /// Include no [InstructionKind]s.
    pub fn none() -> Self {
        Self(FlagSet::default())
    }

    /// Check if the [InstructionKind] is contained in this set.
    #[inline]
    pub fn contains(&self, kind: InstructionKind) -> bool {
        self.0.contains(kind)
    }
}

flags! {
    /// Enumerate the categories of instructions defined in the [WebAssembly
    /// specification](https://webassembly.github.io/spec/core/syntax/instructions.html).
    #[allow(missing_docs)]
    #[cfg_attr(feature = "_internal_cli", derive(serde::Deserialize))]
    pub enum InstructionKind: u16 {
        Numeric,
        Vector,
        Reference,
        Parametric,
        Variable,
        Table,
        Memory,
        Control,
    }
}

impl FromStr for InstructionKind {
    type Err = String;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "numeric" => Ok(InstructionKind::Numeric),
            "vector" => Ok(InstructionKind::Vector),
            "reference" => Ok(InstructionKind::Reference),
            "parametric" => Ok(InstructionKind::Parametric),
            "variable" => Ok(InstructionKind::Variable),
            "table" => Ok(InstructionKind::Table),
            "memory" => Ok(InstructionKind::Memory),
            "control" => Ok(InstructionKind::Control),
            _ => Err(format!("unknown instruction kind: {}", s)),
        }
    }
}
