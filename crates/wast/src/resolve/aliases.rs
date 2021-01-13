use crate::ast::*;
use crate::resolve::gensym;
use std::collections::{hash_map::Entry, HashMap};

pub fn run(fields: &mut Vec<ModuleField>) {
    let mut cur = 0;
    let mut cx = Expander::default();
    while cur < fields.len() {
        cx.process(&mut fields[cur]);
        for item in cx.to_prepend.drain(..) {
            fields.insert(cur, item);
            cur += 1;
        }
        cur += 1;
    }
    assert!(cx.to_prepend.is_empty());
}

#[derive(Default)]
struct Expander<'a> {
    to_prepend: Vec<ModuleField<'a>>,
    instances: HashMap<(Index<'a>, &'a str, ExportKind), Index<'a>>,
    parents: HashMap<(Index<'a>, Index<'a>, ExportKind), Index<'a>>,
}

impl<'a> Expander<'a> {
    fn process(&mut self, field: &mut ModuleField<'a>) {
        assert!(self.to_prepend.is_empty());
        match field {
            ModuleField::Alias(a) => {
                let id = gensym::fill(a.span, &mut a.id);
                match &mut a.kind {
                    AliasKind::InstanceExport {
                        instance,
                        export,
                        kind,
                    } => {
                        self.expand(instance);
                        self.instances
                            .insert((*instance.unwrap_index(), export, *kind), id.into());
                    }
                    AliasKind::Outer {
                        module,
                        index,
                        kind,
                    } => {
                        self.parents.insert((*module, *index, *kind), id.into());
                    }
                }
            }

            ModuleField::Instance(i) => {
                if let InstanceKind::Inline { module, args } = &mut i.kind {
                    self.expand(module);
                    for arg in args {
                        self.expand(&mut arg.index);
                    }
                }
            }

            ModuleField::Elem(e) => {
                if let ElemKind::Active { table, .. } = &mut e.kind {
                    self.expand(table);
                }
                match &mut e.payload {
                    ElemPayload::Indices(funcs) => {
                        for func in funcs {
                            self.expand(func);
                        }
                    }
                    ElemPayload::Exprs { exprs, .. } => {
                        for func in exprs {
                            if let Some(func) = func {
                                self.expand(func);
                            }
                        }
                    }
                }
            }

            ModuleField::Data(e) => {
                if let DataKind::Active { memory, .. } = &mut e.kind {
                    self.expand(memory);
                }
            }

            ModuleField::Export(e) => self.expand(&mut e.index),

            ModuleField::Func(f) => {
                self.expand_type_use(&mut f.ty);
                if let FuncKind::Inline { expression, .. } = &mut f.kind {
                    self.expand_expr(expression);
                }
            }

            ModuleField::Import(i) => self.expand_item_sig(&mut i.item),

            ModuleField::Global(g) => {
                if let GlobalKind::Inline(expr) = &mut g.kind {
                    self.expand_expr(expr);
                }
            }

            ModuleField::Start(s) => self.expand(s),

            ModuleField::Event(e) => match &mut e.ty {
                EventType::Exception(t) => self.expand_type_use(t),
            },

            ModuleField::NestedModule(m) => match &mut m.kind {
                NestedModuleKind::Import { ty, .. } => self.expand_type_use(ty),
                NestedModuleKind::Inline { fields } => run(fields),
            },

            ModuleField::Custom(_)
            | ModuleField::Memory(_)
            | ModuleField::Table(_)
            | ModuleField::Type(_) => {}
        }
    }

    fn expand_item_sig(&mut self, sig: &mut ItemSig<'a>) {
        match &mut sig.kind {
            ItemKind::Func(t) => self.expand_type_use(t),
            ItemKind::Module(t) => self.expand_type_use(t),
            ItemKind::Instance(t) => self.expand_type_use(t),
            ItemKind::Table(_) => {}
            ItemKind::Memory(_) => {}
            ItemKind::Global(_) => {}
            ItemKind::Event(_) => {}
        }
    }

    fn expand_type_use<T>(&mut self, ty: &mut TypeUse<'a, T>) {
        if let Some(index) = &mut ty.index {
            self.expand(index);
        }
    }

    fn expand_expr(&mut self, expr: &mut Expression<'a>) {
        for instr in expr.instrs.iter_mut() {
            self.expand_instr(instr);
        }
    }

    fn expand_instr(&mut self, instr: &mut Instruction<'a>) {
        use Instruction::*;

        if let Some(m) = instr.memarg_mut() {
            self.expand(&mut m.memory);
        }

        match instr {
            Call(i) | ReturnCall(i) | RefFunc(i) => self.expand(&mut i.0),
            CallIndirect(i) | ReturnCallIndirect(i) => {
                self.expand(&mut i.table);
                self.expand_type_use(&mut i.ty);
            }
            TableInit(i) => self.expand(&mut i.table),
            MemoryInit(i) => self.expand(&mut i.mem),
            TableCopy(i) => {
                self.expand(&mut i.src);
                self.expand(&mut i.dst);
            }
            MemoryCopy(i) => {
                self.expand(&mut i.src);
                self.expand(&mut i.dst);
            }
            GlobalSet(g) | GlobalGet(g) => self.expand(&mut g.0),
            TableGet(t) | TableSet(t) | TableFill(t) | TableSize(t) | TableGrow(t) => {
                self.expand(&mut t.dst)
            }

            MemorySize(m) | MemoryGrow(m) | MemoryFill(m) => self.expand(&mut m.mem),

            _ => {}
        }
    }

    fn expand<T>(&mut self, item: &mut ItemRef<'a, T>)
    where
        T: Into<ExportKind> + Copy,
    {
        match item {
            ItemRef::Outer { kind, module, idx } => {
                let key = (*module, *idx, (*kind).into());
                let idx = match self.parents.entry(key) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(v) => {
                        let span = idx.span();
                        let id = gensym::gen(span);
                        self.to_prepend.push(ModuleField::Alias(Alias {
                            span,
                            id: Some(id),
                            name: None,
                            kind: AliasKind::Outer {
                                module: *module,
                                index: *idx,
                                kind: (*kind).into(),
                            },
                        }));
                        *v.insert(Index::Id(id))
                    }
                };
                *item = ItemRef::Item {
                    kind: *kind,
                    idx,
                    exports: Vec::new(),
                };
            }
            ItemRef::Item { kind, idx, exports } => {
                let mut cur = *idx;
                let len = exports.len();
                for (i, export) in exports.drain(..).enumerate() {
                    let kind = if i < len - 1 {
                        ExportKind::Instance
                    } else {
                        (*kind).into()
                    };
                    let key = (cur, export, kind);
                    cur = match self.instances.entry(key) {
                        Entry::Occupied(e) => *e.get(),
                        Entry::Vacant(v) => {
                            let span = idx.span();
                            let id = gensym::gen(span);
                            self.to_prepend.push(ModuleField::Alias(Alias {
                                span,
                                id: Some(id),
                                name: None,
                                kind: AliasKind::InstanceExport {
                                    kind,
                                    instance: ItemRef::Item {
                                        kind: kw::instance(span),
                                        idx: cur,
                                        exports: Vec::new(),
                                    },
                                    export,
                                },
                            }));
                            *v.insert(Index::Id(id))
                        }
                    };
                }
                *idx = cur;
            }
        }
    }
}
