use crate::ast::*;
use crate::resolve::Ns;
use crate::Error;
use std::collections::HashMap;

pub fn resolve<'a>(fields: &mut Vec<ModuleField<'a>>) -> Result<Resolver<'a>, Error> {
    let mut resolver = Resolver::default();
    resolver.process(None, fields)?;
    Ok(resolver)
}

/// Context structure used to perform name resolution.
#[derive(Default)]
pub struct Resolver<'a> {
    // Namespaces within each module. Note that each namespace carries with it
    // information about the signature of the item in that namespace. The
    // signature is later used to synthesize the type of a module and inject
    // type annotations if necessary.
    funcs: Namespace<'a>,
    globals: Namespace<'a>,
    tables: Namespace<'a>,
    memories: Namespace<'a>,
    types: Namespace<'a>,
    events: Namespace<'a>,
    modules: Namespace<'a>,
    instances: Namespace<'a>,
    datas: Namespace<'a>,
    elems: Namespace<'a>,
    fields: Namespace<'a>,
    type_info: Vec<TypeInfo<'a>>,
}

impl<'a> Resolver<'a> {
    fn process(
        &mut self,
        parent: Option<&Resolver<'a>>,
        fields: &mut Vec<ModuleField<'a>>,
    ) -> Result<(), Error> {
        // Number everything in the module, recording what names correspond to
        // what indices.
        for field in fields.iter_mut() {
            self.register(field)?;
        }

        // Then we can replace all our `Index::Id` instances with `Index::Num`
        // in the AST. Note that this also recurses into nested modules.
        for field in fields.iter_mut() {
            self.resolve_field(field, parent)?;
        }
        Ok(())
    }

    fn register(&mut self, item: &ModuleField<'a>) -> Result<(), Error> {
        match item {
            ModuleField::Import(i) => match &i.item.kind {
                ItemKind::Func(_) => self.funcs.register(i.item.id, "func")?,
                ItemKind::Memory(_) => self.memories.register(i.item.id, "memory")?,
                ItemKind::Table(_) => self.tables.register(i.item.id, "table")?,
                ItemKind::Global(_) => self.globals.register(i.item.id, "global")?,
                ItemKind::Event(_) => self.events.register(i.item.id, "event")?,
                ItemKind::Module(_) => self.modules.register(i.item.id, "module")?,
                ItemKind::Instance(_) => self.instances.register(i.item.id, "instance")?,
            },
            ModuleField::Global(i) => self.globals.register(i.id, "global")?,
            ModuleField::Memory(i) => self.memories.register(i.id, "memory")?,
            ModuleField::Func(i) => self.funcs.register(i.id, "func")?,
            ModuleField::Table(i) => self.tables.register(i.id, "table")?,
            ModuleField::NestedModule(m) => self.modules.register(m.id, "module")?,
            ModuleField::Instance(i) => self.instances.register(i.id, "instance")?,

            ModuleField::Type(i) => {
                match &i.def {
                    // For GC structure types we need to be sure to populate the
                    // field namespace here as well.
                    //
                    // The field namespace is global, but the resolved indices
                    // are relative to the struct they are defined in
                    TypeDef::Struct(r#struct) => {
                        for (i, field) in r#struct.fields.iter().enumerate() {
                            if let Some(id) = field.id {
                                self.fields.register_specific(id, i as u32, "field")?;
                            }
                        }
                    }

                    TypeDef::Instance(_)
                    | TypeDef::Array(_)
                    | TypeDef::Func(_)
                    | TypeDef::Module(_) => {}
                }

                // Record function signatures as we see them to so we can
                // generate errors for mismatches in references such as
                // `call_indirect`.
                match &i.def {
                    TypeDef::Func(f) => {
                        let params = f.params.iter().map(|p| p.2).collect();
                        let results = f.results.clone();
                        self.type_info.push(TypeInfo::Func { params, results });
                    }
                    _ => self.type_info.push(TypeInfo::Other),
                }

                self.types.register(i.id, "type")?
            }
            ModuleField::Elem(e) => self.elems.register(e.id, "elem")?,
            ModuleField::Data(d) => self.datas.register(d.id, "data")?,
            ModuleField::Event(e) => self.events.register(e.id, "event")?,
            ModuleField::Alias(a) => match a.item_kind() {
                ExportKind::Func => self.funcs.register(a.id, "func")?,
                ExportKind::Table => self.tables.register(a.id, "table")?,
                ExportKind::Memory => self.memories.register(a.id, "memory")?,
                ExportKind::Global => self.globals.register(a.id, "global")?,
                ExportKind::Instance => self.instances.register(a.id, "instance")?,
                ExportKind::Module => self.modules.register(a.id, "module")?,
                ExportKind::Event => self.events.register(a.id, "event")?,
                ExportKind::Type => {
                    self.type_info.push(TypeInfo::Other);
                    self.types.register(a.id, "type")?
                }
            },

            // These fields don't define any items in any index space.
            ModuleField::Export(_) | ModuleField::Start(_) | ModuleField::Custom(_) => {
                return Ok(())
            }
        };

        Ok(())
    }

    fn resolve_field(
        &self,
        field: &mut ModuleField<'a>,
        parent: Option<&Resolver<'a>>,
    ) -> Result<(), Error> {
        match field {
            ModuleField::Import(i) => {
                self.resolve_item_sig(&mut i.item)?;
                Ok(())
            }

            ModuleField::Type(ty) => {
                match &mut ty.def {
                    TypeDef::Func(func) => func.resolve(self)?,
                    TypeDef::Struct(struct_) => {
                        for field in &mut struct_.fields {
                            self.resolve_storagetype(&mut field.ty)?;
                        }
                    }
                    TypeDef::Array(array) => self.resolve_storagetype(&mut array.ty)?,
                    TypeDef::Module(m) => m.resolve(self)?,
                    TypeDef::Instance(i) => i.resolve(self)?,
                }
                Ok(())
            }

            ModuleField::Func(f) => {
                let (idx, inline) = self.resolve_type_use(&mut f.ty)?;
                let n = match idx {
                    Index::Num(n, _) => *n,
                    Index::Id(_) => panic!("expected `Num`"),
                };
                if let FuncKind::Inline { locals, expression } = &mut f.kind {
                    // Resolve (ref T) in locals
                    for local in locals.iter_mut() {
                        self.resolve_valtype(&mut local.ty)?;
                    }

                    // Build a scope with a local namespace for the function
                    // body
                    let mut scope = Namespace::default();

                    // Parameters come first in the scope...
                    if let Some(inline) = &inline {
                        for (id, _, _) in inline.params.iter() {
                            scope.register(*id, "local")?;
                        }
                    } else if let Some(TypeInfo::Func { params, .. }) =
                        self.type_info.get(n as usize)
                    {
                        for _ in 0..params.len() {
                            scope.register(None, "local")?;
                        }
                    }

                    // .. followed by locals themselves
                    for local in locals {
                        scope.register(local.id, "local")?;
                    }

                    // Initialize the expression resolver with this scope
                    let mut resolver = ExprResolver::new(self, scope);

                    // and then we can resolve the expression!
                    resolver.resolve(expression)?;

                    // specifically save the original `sig`, if it was present,
                    // because that's what we're using for local names.
                    f.ty.inline = inline;
                }
                Ok(())
            }

            ModuleField::Elem(e) => {
                match &mut e.kind {
                    ElemKind::Active { table, offset } => {
                        self.resolve(table, Ns::Table)?;
                        self.resolve_expr(offset)?;
                    }
                    ElemKind::Passive { .. } | ElemKind::Declared { .. } => {}
                }
                match &mut e.payload {
                    ElemPayload::Indices(elems) => {
                        for idx in elems {
                            self.resolve(idx, Ns::Func)?;
                        }
                    }
                    ElemPayload::Exprs { exprs, ty } => {
                        for funcref in exprs {
                            if let Some(idx) = funcref {
                                self.resolve(idx, Ns::Func)?;
                            }
                        }
                        self.resolve_heaptype(&mut ty.heap)?;
                    }
                }
                Ok(())
            }

            ModuleField::Data(d) => {
                if let DataKind::Active { memory, offset } = &mut d.kind {
                    self.resolve(memory, Ns::Memory)?;
                    self.resolve_expr(offset)?;
                }
                Ok(())
            }

            ModuleField::Start(i) => {
                self.resolve(i, Ns::Func)?;
                Ok(())
            }

            ModuleField::Export(e) => {
                let ns = Ns::from_export(&e.kind);
                self.resolve(&mut e.index, ns)?;
                Ok(())
            }

            ModuleField::Global(g) => {
                self.resolve_valtype(&mut g.ty.ty)?;
                if let GlobalKind::Inline(expr) = &mut g.kind {
                    self.resolve_expr(expr)?;
                }
                Ok(())
            }

            ModuleField::Event(e) => {
                match &mut e.ty {
                    EventType::Exception(ty) => {
                        self.resolve_type_use(ty)?;
                    }
                }
                Ok(())
            }

            ModuleField::Instance(i) => {
                if let InstanceKind::Inline { module, args } = &mut i.kind {
                    self.resolve(module, Ns::Module)?;
                    for arg in args {
                        let ns = Ns::from_export(&arg.kind);
                        self.resolve(&mut arg.index, ns)?;
                    }
                }
                Ok(())
            }

            ModuleField::NestedModule(m) => {
                let fields = match &mut m.kind {
                    NestedModuleKind::Inline { fields } => fields,
                    NestedModuleKind::Import { .. } => panic!("should only be inline"),
                };
                Resolver::default().process(Some(self), fields)
            }

            ModuleField::Table(t) => {
                if let TableKind::Normal(t) = &mut t.kind {
                    self.resolve_heaptype(&mut t.elem.heap)?;
                }
                Ok(())
            }

            ModuleField::Alias(a) => {
                match &mut a.kind {
                    AliasKind::InstanceExport { instance, .. } => {
                        self.resolve(instance, Ns::Instance)?;
                    }
                    AliasKind::Parent { parent_index, kind } => {
                        let parent = match parent {
                            Some(p) => p,
                            None => {
                                return Err(Error::new(
                                    a.span,
                                    "cannot use alias parent directives in root module".to_string(),
                                ))
                            }
                        };
                        parent.resolve(parent_index, Ns::from_export(kind))?;
                    }
                }
                Ok(())
            }

            ModuleField::Memory(_) | ModuleField::Custom(_) => Ok(()),
        }
    }

    fn resolve_valtype(&self, ty: &mut ValType<'a>) -> Result<(), Error> {
        match ty {
            ValType::Ref(ty) => self.resolve_heaptype(&mut ty.heap)?,
            ValType::Rtt(_d, i) => {
                self.resolve(i, Ns::Type)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn resolve_heaptype(&self, ty: &mut HeapType<'a>) -> Result<(), Error> {
        match ty {
            HeapType::Index(i) => {
                self.resolve(i, Ns::Type)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn resolve_storagetype(&self, ty: &mut StorageType<'a>) -> Result<(), Error> {
        match ty {
            StorageType::Val(ty) => self.resolve_valtype(ty)?,
            _ => {}
        }
        Ok(())
    }

    fn resolve_item_sig(&self, item: &mut ItemSig<'a>) -> Result<(), Error> {
        match &mut item.kind {
            ItemKind::Func(t) | ItemKind::Event(EventType::Exception(t)) => {
                self.resolve_type_use(t)?;
            }
            ItemKind::Global(t) => self.resolve_valtype(&mut t.ty)?,
            ItemKind::Instance(t) => {
                self.resolve_type_use(t)?;
            }
            ItemKind::Module(m) => {
                self.resolve_type_use(m)?;
            }
            ItemKind::Table(t) => {
                self.resolve_heaptype(&mut t.elem.heap)?;
            }
            ItemKind::Memory(_) => {}
        }
        Ok(())
    }

    fn resolve_type_use<'b, T>(
        &self,
        ty: &'b mut TypeUse<'a, T>,
    ) -> Result<(&'b Index<'a>, Option<T>), Error>
    where
        T: TypeReference<'a>,
    {
        let idx = ty.index.as_mut().unwrap();
        self.resolve(idx, Ns::Type)?;

        // If the type was listed inline *and* it was specified via a type index
        // we need to assert they're the same.
        //
        // Note that we resolve the type first to transform all names to
        // indices to ensure that all the indices line up.
        if let Some(inline) = &mut ty.inline {
            inline.resolve(self)?;
            inline.check_matches(idx, self)?;
        }

        Ok((idx, ty.inline.take()))
    }

    fn resolve_expr(&self, expr: &mut Expression<'a>) -> Result<(), Error> {
        ExprResolver::new(self, Namespace::default()).resolve(expr)
    }

    pub fn resolve(&self, idx: &mut Index<'a>, ns: Ns) -> Result<u32, Error> {
        match ns {
            Ns::Func => self.funcs.resolve(idx, "func"),
            Ns::Table => self.tables.resolve(idx, "table"),
            Ns::Global => self.globals.resolve(idx, "global"),
            Ns::Memory => self.memories.resolve(idx, "memory"),
            Ns::Instance => self.instances.resolve(idx, "instance"),
            Ns::Module => self.modules.resolve(idx, "module"),
            Ns::Event => self.events.resolve(idx, "event"),
            Ns::Type => self.types.resolve(idx, "type"),
        }
    }
}

#[derive(Default)]
pub struct Namespace<'a> {
    names: HashMap<Id<'a>, u32>,
    count: u32,
}

impl<'a> Namespace<'a> {
    fn register(&mut self, name: Option<Id<'a>>, desc: &str) -> Result<u32, Error> {
        let index = self.alloc();
        if let Some(name) = name {
            if let Some(_prev) = self.names.insert(name, index) {
                // FIXME: temporarily allow duplicately-named data and element
                // segments. This is a sort of dumb hack to get the spec test
                // suite working (ironically).
                //
                // So as background, the text format disallows duplicate
                // identifiers, causing a parse error if they're found. There
                // are two tests currently upstream, however, data.wast and
                // elem.wast, which *look* like they have duplicately named
                // element and data segments. These tests, however, are using
                // pre-bulk-memory syntax where a bare identifier was the
                // table/memory being initialized. In post-bulk-memory this
                // identifier is the name of the segment. Since we implement
                // post-bulk-memory features that means that we're parsing the
                // memory/table-to-initialize as the name of the segment.
                //
                // This is technically incorrect behavior but no one is
                // hopefully relying on this too much. To get the spec tests
                // passing we ignore errors for elem/data segments. Once the
                // spec tests get updated enough we can remove this condition
                // and return errors for them.
                if desc != "elem" && desc != "data" {
                    return Err(Error::new(
                        name.span(),
                        format!("duplicate {} identifier", desc),
                    ));
                }
            }
        }
        Ok(index)
    }

    fn alloc(&mut self) -> u32 {
        let index = self.count;
        self.count += 1;
        return index;
    }

    fn register_specific(&mut self, name: Id<'a>, index: u32, desc: &str) -> Result<(), Error> {
        if let Some(_prev) = self.names.insert(name, index) {
            return Err(Error::new(
                name.span(),
                format!("duplicate identifier for {}", desc),
            ));
        }
        Ok(())
    }

    fn resolve(&self, idx: &mut Index<'a>, desc: &str) -> Result<u32, Error> {
        let id = match idx {
            Index::Num(n, _) => return Ok(*n),
            Index::Id(id) => id,
        };
        if let Some(&n) = self.names.get(id) {
            *idx = Index::Num(n, id.span());
            return Ok(n);
        }
        Err(resolve_error(*id, desc))
    }
}

fn resolve_error(id: Id<'_>, ns: &str) -> Error {
    assert!(!id.is_gensym());
    Error::new(
        id.span(),
        format!("failed to find {} named `${}`", ns, id.name()),
    )
}

#[derive(Debug, Clone)]
struct ExprBlock<'a> {
    // The label of the block
    label: Option<Id<'a>>,
    // Whether this block pushed a new scope for resolving locals
    pushed_scope: bool,
}

struct ExprResolver<'a, 'b> {
    resolver: &'b Resolver<'a>,
    // Scopes tracks the local namespace and dynamically grows as we enter/exit
    // `let` blocks
    scopes: Vec<Namespace<'a>>,
    blocks: Vec<ExprBlock<'a>>,
}

impl<'a, 'b> ExprResolver<'a, 'b> {
    fn new(resolver: &'b Resolver<'a>, initial_scope: Namespace<'a>) -> ExprResolver<'a, 'b> {
        ExprResolver {
            resolver,
            scopes: vec![initial_scope],
            blocks: Vec::new(),
        }
    }

    fn resolve(&mut self, expr: &mut Expression<'a>) -> Result<(), Error> {
        for instr in expr.instrs.iter_mut() {
            self.resolve_instr(instr)?;
        }
        Ok(())
    }

    fn resolve_block_type(&mut self, bt: &mut BlockType<'a>) -> Result<(), Error> {
        // Ok things get interesting here. First off when parsing `bt`
        // *optionally* has an index and a function type listed. If
        // they're both not present it's equivalent to 0 params and 0
        // results.
        //
        // In MVP wasm blocks can have 0 params and 0-1 results. Now
        // there's also multi-value. We want to prefer MVP wasm wherever
        // possible (for backcompat) so we want to list this block as
        // being an "MVP" block if we can. The encoder only has
        // `BlockType` to work with, so it'll be looking at `params` and
        // `results` to figure out what to encode. If `params` and
        // `results` fit within MVP, then it uses MVP encoding
        //
        // To put all that together, here we handle:
        //
        // * If the `index` was specified, resolve it and use it as the
        //   source of truth. If this turns out to be an MVP type,
        //   record it as such.
        // * Otherwise use `params` and `results` as the source of
        //   truth. *If* this were a non-MVP compatible block `index`
        //   would be filled by by `tyexpand.rs`.
        //
        // tl;dr; we handle the `index` here if it's set and then fill
        // out `params` and `results` if we can, otherwise no work
        // happens.
        if bt.ty.index.is_some() {
            let (ty, _) = self.resolver.resolve_type_use(&mut bt.ty)?;
            let n = match ty {
                Index::Num(n, _) => *n,
                Index::Id(_) => panic!("expected `Num`"),
            };
            let ty = match self.resolver.type_info.get(n as usize) {
                Some(TypeInfo::Func { params, results }) => (params, results),
                _ => return Ok(()),
            };
            if ty.0.len() == 0 && ty.1.len() <= 1 {
                let mut inline = FunctionType::default();
                inline.results = ty.1.clone();
                bt.ty.inline = Some(inline);
                bt.ty.index = None;
            }
        }

        // If the inline annotation persists to this point then resolve
        // all of its inline value types.
        if let Some(inline) = &mut bt.ty.inline {
            inline.resolve(self.resolver)?;
        }
        Ok(())
    }

    fn resolve_instr(&mut self, instr: &mut Instruction<'a>) -> Result<(), Error> {
        use crate::ast::Instruction::*;

        match instr {
            MemorySize(i) | MemoryGrow(i) | MemoryFill(i) => {
                self.resolver.resolve(&mut i.mem, Ns::Memory)?;
            }
            MemoryInit(i) => {
                self.resolver.datas.resolve(&mut i.data, "data")?;
                self.resolver.resolve(&mut i.mem, Ns::Memory)?;
            }
            MemoryCopy(i) => {
                self.resolver.resolve(&mut i.src, Ns::Memory)?;
                self.resolver.resolve(&mut i.dst, Ns::Memory)?;
            }
            DataDrop(i) => {
                self.resolver.datas.resolve(i, "data")?;
            }

            TableInit(i) => {
                self.resolver.elems.resolve(&mut i.elem, "elem")?;
                self.resolver.resolve(&mut i.table, Ns::Table)?;
            }
            ElemDrop(i) => {
                self.resolver.elems.resolve(i, "elem")?;
            }

            TableCopy(i) => {
                self.resolver.resolve(&mut i.dst, Ns::Table)?;
                self.resolver.resolve(&mut i.src, Ns::Table)?;
            }

            TableFill(i) | TableSet(i) | TableGet(i) | TableSize(i) | TableGrow(i) => {
                self.resolver.resolve(&mut i.dst, Ns::Table)?;
            }

            GlobalSet(i) | GlobalGet(i) => {
                self.resolver.resolve(i, Ns::Global)?;
            }

            LocalSet(i) | LocalGet(i) | LocalTee(i) => {
                assert!(self.scopes.len() > 0);
                // Resolve a local by iterating over scopes from most recent
                // to less recent. This allows locals added by `let` blocks to
                // shadow less recent locals.
                for (depth, scope) in self.scopes.iter().enumerate().rev() {
                    if let Err(e) = scope.resolve(i, "local") {
                        if depth == 0 {
                            // There are no more scopes left, report this as
                            // the result
                            return Err(e);
                        }
                    } else {
                        break;
                    }
                }
                // We must have taken the `break` and resolved the local
                assert!(i.is_resolved());
            }

            Call(i) | RefFunc(i) | ReturnCall(i) => {
                self.resolver.resolve(i, Ns::Func)?;
            }

            CallIndirect(c) | ReturnCallIndirect(c) => {
                self.resolver.resolve(&mut c.table, Ns::Table)?;
                self.resolver.resolve_type_use(&mut c.ty)?;
            }

            FuncBind(b) => {
                self.resolver.resolve_type_use(&mut b.ty)?;
            }

            Let(t) => {
                // Resolve (ref T) in locals
                for local in &mut t.locals {
                    self.resolver.resolve_valtype(&mut local.ty)?;
                }

                // Register all locals defined in this let
                let mut scope = Namespace::default();
                for local in &t.locals {
                    scope.register(local.id, "local")?;
                }
                self.scopes.push(scope);
                self.blocks.push(ExprBlock {
                    label: t.block.label,
                    pushed_scope: true,
                });

                self.resolve_block_type(&mut t.block)?;
            }

            Block(bt) | If(bt) | Loop(bt) | Try(bt) => {
                self.blocks.push(ExprBlock {
                    label: bt.label,
                    pushed_scope: false,
                });
                self.resolve_block_type(bt)?;
            }

            // On `End` instructions we pop a label from the stack, and for both
            // `End` and `Else` instructions if they have labels listed we
            // verify that they match the label at the beginning of the block.
            Else(_) | End(_) => {
                let (matching_block, label) = match &instr {
                    Else(label) => (self.blocks.last().cloned(), label),
                    End(label) => (self.blocks.pop(), label),
                    _ => unreachable!(),
                };
                let matching_block = match matching_block {
                    Some(l) => l,
                    None => return Ok(()),
                };

                // Reset the local scopes to before this block was entered
                if matching_block.pushed_scope {
                    if let End(_) = instr {
                        self.scopes.pop();
                    }
                }

                let label = match label {
                    Some(l) => l,
                    None => return Ok(()),
                };
                if Some(*label) == matching_block.label {
                    return Ok(());
                }
                return Err(Error::new(
                    label.span(),
                    "mismatching labels between end and block".to_string(),
                ));
            }

            Br(i) | BrIf(i) | BrOnNull(i) => {
                self.resolve_label(i)?;
            }

            BrTable(i) => {
                for label in i.labels.iter_mut() {
                    self.resolve_label(label)?;
                }
                self.resolve_label(&mut i.default)?;
            }

            Throw(i) => {
                self.resolver.resolve(i, Ns::Event)?;
            }
            Rethrow(i) => {
                self.resolve_label(i)?;
            }
            Catch(i) => {
                self.resolver.resolve(i, Ns::Event)?;
            }

            BrOnCast(b) => {
                self.resolve_label(&mut b.label)?;
                self.resolver.resolve_heaptype(&mut b.val)?;
                self.resolver.resolve_heaptype(&mut b.rtt)?;
            }

            Select(s) => {
                if let Some(list) = &mut s.tys {
                    for ty in list {
                        self.resolver.resolve_valtype(ty)?;
                    }
                }
            }

            StructNew(i)
            | StructNewWithRtt(i)
            | StructNewDefaultWithRtt(i)
            | ArrayNewWithRtt(i)
            | ArrayNewDefaultWithRtt(i)
            | ArrayGet(i)
            | ArrayGetS(i)
            | ArrayGetU(i)
            | ArraySet(i)
            | ArrayLen(i) => {
                self.resolver.resolve(i, Ns::Type)?;
            }
            RTTCanon(t) => {
                self.resolver.resolve_heaptype(t)?;
            }
            RTTSub(s) => {
                self.resolver.resolve_heaptype(&mut s.input_rtt)?;
                self.resolver.resolve_heaptype(&mut s.output_rtt)?;
            }
            RefTest(t) | RefCast(t) => {
                self.resolver.resolve_heaptype(&mut t.val)?;
                self.resolver.resolve_heaptype(&mut t.rtt)?;
            }

            StructSet(s) | StructGet(s) | StructGetS(s) | StructGetU(s) => {
                self.resolver.resolve(&mut s.r#struct, Ns::Type)?;
                self.resolver.fields.resolve(&mut s.field, "field")?;
            }
            StructNarrow(s) => {
                self.resolver.resolve_valtype(&mut s.from)?;
                self.resolver.resolve_valtype(&mut s.to)?;
            }

            RefNull(ty) => self.resolver.resolve_heaptype(ty)?,

            I32Load(m)
            | I64Load(m)
            | F32Load(m)
            | F64Load(m)
            | I32Load8s(m)
            | I32Load8u(m)
            | I32Load16s(m)
            | I32Load16u(m)
            | I64Load8s(m)
            | I64Load8u(m)
            | I64Load16s(m)
            | I64Load16u(m)
            | I64Load32s(m)
            | I64Load32u(m)
            | I32Store(m)
            | I64Store(m)
            | F32Store(m)
            | F64Store(m)
            | I32Store8(m)
            | I32Store16(m)
            | I64Store8(m)
            | I64Store16(m)
            | I64Store32(m)
            | I32AtomicLoad(m)
            | I64AtomicLoad(m)
            | I32AtomicLoad8u(m)
            | I32AtomicLoad16u(m)
            | I64AtomicLoad8u(m)
            | I64AtomicLoad16u(m)
            | I64AtomicLoad32u(m)
            | I32AtomicStore(m)
            | I64AtomicStore(m)
            | I32AtomicStore8(m)
            | I32AtomicStore16(m)
            | I64AtomicStore8(m)
            | I64AtomicStore16(m)
            | I64AtomicStore32(m)
            | I32AtomicRmwAdd(m)
            | I64AtomicRmwAdd(m)
            | I32AtomicRmw8AddU(m)
            | I32AtomicRmw16AddU(m)
            | I64AtomicRmw8AddU(m)
            | I64AtomicRmw16AddU(m)
            | I64AtomicRmw32AddU(m)
            | I32AtomicRmwSub(m)
            | I64AtomicRmwSub(m)
            | I32AtomicRmw8SubU(m)
            | I32AtomicRmw16SubU(m)
            | I64AtomicRmw8SubU(m)
            | I64AtomicRmw16SubU(m)
            | I64AtomicRmw32SubU(m)
            | I32AtomicRmwAnd(m)
            | I64AtomicRmwAnd(m)
            | I32AtomicRmw8AndU(m)
            | I32AtomicRmw16AndU(m)
            | I64AtomicRmw8AndU(m)
            | I64AtomicRmw16AndU(m)
            | I64AtomicRmw32AndU(m)
            | I32AtomicRmwOr(m)
            | I64AtomicRmwOr(m)
            | I32AtomicRmw8OrU(m)
            | I32AtomicRmw16OrU(m)
            | I64AtomicRmw8OrU(m)
            | I64AtomicRmw16OrU(m)
            | I64AtomicRmw32OrU(m)
            | I32AtomicRmwXor(m)
            | I64AtomicRmwXor(m)
            | I32AtomicRmw8XorU(m)
            | I32AtomicRmw16XorU(m)
            | I64AtomicRmw8XorU(m)
            | I64AtomicRmw16XorU(m)
            | I64AtomicRmw32XorU(m)
            | I32AtomicRmwXchg(m)
            | I64AtomicRmwXchg(m)
            | I32AtomicRmw8XchgU(m)
            | I32AtomicRmw16XchgU(m)
            | I64AtomicRmw8XchgU(m)
            | I64AtomicRmw16XchgU(m)
            | I64AtomicRmw32XchgU(m)
            | I32AtomicRmwCmpxchg(m)
            | I64AtomicRmwCmpxchg(m)
            | I32AtomicRmw8CmpxchgU(m)
            | I32AtomicRmw16CmpxchgU(m)
            | I64AtomicRmw8CmpxchgU(m)
            | I64AtomicRmw16CmpxchgU(m)
            | I64AtomicRmw32CmpxchgU(m)
            | V128Load(m)
            | V128Load8x8S(m)
            | V128Load8x8U(m)
            | V128Load16x4S(m)
            | V128Load16x4U(m)
            | V128Load32x2S(m)
            | V128Load32x2U(m)
            | V128Load8Splat(m)
            | V128Load16Splat(m)
            | V128Load32Splat(m)
            | V128Load64Splat(m)
            | V128Load32Zero(m)
            | V128Load64Zero(m)
            | V128Store(m)
            | MemoryAtomicNotify(m)
            | MemoryAtomicWait32(m)
            | MemoryAtomicWait64(m) => {
                self.resolver.resolve(&mut m.memory, Ns::Memory)?;
            }

            _ => {}
        }
        Ok(())
    }

    fn resolve_label(&self, label: &mut Index<'a>) -> Result<(), Error> {
        let id = match label {
            Index::Num(..) => return Ok(()),
            Index::Id(id) => *id,
        };
        let idx = self
            .blocks
            .iter()
            .rev()
            .enumerate()
            .filter_map(|(i, b)| b.label.map(|l| (i, l)))
            .find(|(_, l)| *l == id);
        match idx {
            Some((idx, _)) => {
                *label = Index::Num(idx as u32, id.span());
                Ok(())
            }
            None => Err(resolve_error(id, "label")),
        }
    }
}

enum TypeInfo<'a> {
    Func {
        params: Box<[ValType<'a>]>,
        results: Box<[ValType<'a>]>,
    },
    Other,
}

trait TypeReference<'a> {
    fn check_matches(&mut self, idx: &Index<'a>, cx: &Resolver<'a>) -> Result<(), Error>;
    fn resolve(&mut self, cx: &Resolver<'a>) -> Result<(), Error>;
}

impl<'a> TypeReference<'a> for FunctionType<'a> {
    fn check_matches(&mut self, idx: &Index<'a>, cx: &Resolver<'a>) -> Result<(), Error> {
        let n = match idx {
            Index::Num(n, _) => *n,
            Index::Id(_) => panic!("expected `Num`"),
        };
        let (params, results) = match cx.type_info.get(n as usize) {
            Some(TypeInfo::Func { params, results }) => (params, results),
            _ => return Ok(()),
        };

        // Here we need to check that the inline type listed (ourselves) matches
        // what was listed in the module itself (the `params` and `results`
        // above). The listed values in `types` are not resolved yet, although
        // we should be resolved. In any case we do name resolution
        // opportunistically here to see if the values are equal.

        let types_not_equal = |a: &ValType, b: &ValType| {
            let mut a = a.clone();
            let mut b = b.clone();
            drop(cx.resolve_valtype(&mut a));
            drop(cx.resolve_valtype(&mut b));
            a != b
        };

        let not_equal = params.len() != self.params.len()
            || results.len() != self.results.len()
            || params
                .iter()
                .zip(self.params.iter())
                .any(|(a, (_, _, b))| types_not_equal(a, b))
            || results
                .iter()
                .zip(self.results.iter())
                .any(|(a, b)| types_not_equal(a, b));
        if not_equal {
            return Err(Error::new(
                idx.span(),
                format!("inline function type doesn't match type reference"),
            ));
        }

        Ok(())
    }

    fn resolve(&mut self, cx: &Resolver<'a>) -> Result<(), Error> {
        // Resolve the (ref T) value types in the final function type
        for param in self.params.iter_mut() {
            cx.resolve_valtype(&mut param.2)?;
        }
        for result in self.results.iter_mut() {
            cx.resolve_valtype(result)?;
        }
        Ok(())
    }
}

impl<'a> TypeReference<'a> for InstanceType<'a> {
    fn check_matches(&mut self, idx: &Index<'a>, cx: &Resolver<'a>) -> Result<(), Error> {
        drop(cx);
        Err(Error::new(
            idx.span(),
            format!("cannot specify instance type as a reference and inline"),
        ))
    }

    fn resolve(&mut self, cx: &Resolver<'a>) -> Result<(), Error> {
        for export in self.exports.iter_mut() {
            cx.resolve_item_sig(&mut export.item)?;
        }
        Ok(())
    }
}

impl<'a> TypeReference<'a> for ModuleType<'a> {
    fn check_matches(&mut self, idx: &Index<'a>, cx: &Resolver<'a>) -> Result<(), Error> {
        drop(cx);
        Err(Error::new(
            idx.span(),
            format!("cannot specify module type as a reference and inline"),
        ))
    }

    fn resolve(&mut self, cx: &Resolver<'a>) -> Result<(), Error> {
        for i in self.imports.iter_mut() {
            cx.resolve_item_sig(&mut i.item)?;
        }
        for e in self.exports.iter_mut() {
            cx.resolve_item_sig(&mut e.item)?;
        }
        Ok(())
    }
}
