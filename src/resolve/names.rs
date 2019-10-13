use crate::ast::*;
use crate::resolve::{ResolveError, ResolveErrorInner};
use std::collections::HashMap;

#[derive(Copy, Clone)]
enum Ns {
    Data,
    Elem,
    Func,
    Global,
    Memory,
    Table,
    Type,
}

impl Ns {
    fn desc(&self) -> &'static str {
        match self {
            Ns::Data => "data",
            Ns::Elem => "elem",
            Ns::Func => "func",
            Ns::Global => "global",
            Ns::Memory => "memory",
            Ns::Table => "table",
            Ns::Type => "type",
        }
    }
}

#[derive(Default)]
pub struct Resolver<'a> {
    ns: [Namespace<'a>; 7],
    ty_nargs: Vec<usize>,
}

#[derive(Default)]
struct Namespace<'a> {
    names: HashMap<Id<'a>, u32>,
    count: u32,
}

impl<'a> Resolver<'a> {
    pub fn register(&mut self, item: &ModuleField<'a>) {
        let mut register = |ns: Ns, name: Option<Id<'a>>| {
            self.ns_mut(ns).register(name);
        };
        match item {
            ModuleField::Import(i) => match i.kind {
                ImportKind::Func(_) => register(Ns::Func, i.id),
                ImportKind::Memory(_) => register(Ns::Memory, i.id),
                ImportKind::Table(_) => register(Ns::Table, i.id),
                ImportKind::Global(_) => register(Ns::Global, i.id),
            },
            ModuleField::Global(i) => register(Ns::Global, i.name),
            ModuleField::Memory(i) => register(Ns::Memory, i.name),
            ModuleField::Func(i) => register(Ns::Func, i.name),
            ModuleField::Table(i) => register(Ns::Table, i.name),
            ModuleField::Type(i) => {
                register(Ns::Type, i.name);
                self.ty_nargs.push(i.func.params.len());
            }
            ModuleField::Elem(e) => register(Ns::Elem, e.name),
            ModuleField::Data(d) => register(Ns::Data, d.name),
            ModuleField::Start(_) => {}
            ModuleField::Export(_) => {}
        }
    }

    fn ns_mut(&mut self, ns: Ns) -> &mut Namespace<'a> {
        &mut self.ns[ns as usize]
    }

    fn ns(&self, ns: Ns) -> &Namespace<'a> {
        &self.ns[ns as usize]
    }

    pub fn resolve(&self, field: &mut ModuleField<'a>) -> Result<(), ResolveError> {
        match field {
            ModuleField::Import(i) => {
                if let ImportKind::Func(f) = &mut i.kind {
                    self.resolve_type_use(f)?;
                }
                Ok(())
            }

            ModuleField::Func(f) => {
                let ty_idx = self.resolve_type_use(&mut f.ty)?;
                if let FuncKind::Inline { locals, expression } = &mut f.kind {
                    let mut resolver = ExprResolver::new(self);

                    // Parameters come first in the local namespace...
                    match &f.ty.ty {
                        Some(fty) => {
                            for (name, _) in fty.params.iter() {
                                resolver.locals.register(*name);
                            }
                        }
                        None => {
                            for _ in 0..self.ty_nargs[ty_idx as usize] {
                                resolver.locals.register(None);
                            }
                        }
                    }

                    // .. followed by locals themselves
                    for (name, _) in locals {
                        resolver.locals.register(*name);
                    }

                    // and then we can resolve the expression!
                    resolver.resolve(expression)?;
                }
                Ok(())
            }

            ModuleField::Elem(e) => {
                if let ElemKind::Active { table, offset } = &mut e.kind {
                    self.resolve_idx(table, Ns::Table)?;
                    self.resolve_expr(offset)?;
                }
                match &mut e.elems {
                    Elems::Indices(i) => {
                        for idx in i {
                            self.resolve_idx(idx, Ns::Func)?;
                        }
                    }
                    Elems::Funcrefs(i) => {
                        for expr in i {
                            self.resolve_expr(expr)?;
                        }
                    }
                }
                Ok(())
            }

            ModuleField::Data(d) => {
                if let DataKind::Active { memory, offset } = &mut d.kind {
                    self.resolve_idx(memory, Ns::Memory)?;
                    self.resolve_expr(offset)?;
                }
                Ok(())
            }

            ModuleField::Start(i) => {
                self.resolve_idx(i, Ns::Func)?;
                Ok(())
            }

            ModuleField::Export(e) => match &mut e.kind {
                ExportKind::Func(f) => self.resolve_idx(f, Ns::Func),
                ExportKind::Memory(f) => self.resolve_idx(f, Ns::Memory),
                ExportKind::Global(f) => self.resolve_idx(f, Ns::Global),
                ExportKind::Table(f) => self.resolve_idx(f, Ns::Table),
            },

            ModuleField::Table(_)
            | ModuleField::Global(_)
            | ModuleField::Memory(_)
            | ModuleField::Type(_) => Ok(()),
        }
    }

    fn resolve_type_use(&self, ty: &mut TypeUse<'a>) -> Result<u32, ResolveError> {
        assert!(ty.index.is_some());
        self.ns(Ns::Type)
            .resolve(ty.index.as_mut().unwrap())
            .map_err(|id| self.resolve_error(id, "type"))
    }

    fn resolve_expr(&self, expr: &mut Expression<'a>) -> Result<(), ResolveError> {
        ExprResolver::new(self).resolve(expr)
    }

    fn resolve_idx(&self, idx: &mut Index<'a>, ns: Ns) -> Result<(), ResolveError> {
        match self.ns(ns).resolve(idx) {
            Ok(_n) => Ok(()),
            Err(id) => Err(self.resolve_error(id, ns.desc())),
        }
    }

    fn resolve_error(&self, id: Id<'a>, ns: &str) -> ResolveError {
        let (line, col) = match id.orig {
            Some(orig) => {
                let pos = id.name().as_ptr() as usize - orig.as_ptr() as usize;
                crate::to_linecol(orig, pos)
            }
            None => (0, 0),
        };
        ResolveError {
            inner: Box::new(ResolveErrorInner {
                message: format!("failed to find {} named `${}`", ns, id.name()),
                line,
                col,
            }),
        }
    }
}

impl<'a> Namespace<'a> {
    fn register(&mut self, name: Option<Id<'a>>) {
        if let Some(name) = name {
            self.names.insert(name, self.count);
        }
        self.count += 1;
    }

    fn resolve(&self, idx: &mut Index<'a>) -> Result<u32, Id<'a>> {
        let id = match idx {
            Index::Num(n) => return Ok(*n),
            Index::Id(id) => id,
        };
        if let Some(&n) = self.names.get(id) {
            *idx = Index::Num(n);
            return Ok(n);
        }
        Err(*id)
    }
}

struct ExprResolver<'a, 'b> {
    resolver: &'b Resolver<'a>,
    locals: Namespace<'a>,
    labels: Vec<Option<Id<'a>>>,
}

impl<'a, 'b> ExprResolver<'a, 'b> {
    fn new(resolver: &'b Resolver<'a>) -> ExprResolver<'a, 'b> {
        ExprResolver {
            resolver,
            locals: Default::default(),
            labels: Vec::new(),
        }
    }

    fn resolve(&mut self, expr: &mut Expression<'a>) -> Result<(), ResolveError> {
        for instr in expr.instrs.iter_mut() {
            self.resolve_instr(instr)?;
        }
        Ok(())
    }

    fn resolve_instr(&mut self, instr: &mut Instruction<'a>) -> Result<(), ResolveError> {
        use crate::ast::Instruction::*;

        match instr {
            MemoryInit(i) => self.resolver.resolve_idx(i, Ns::Data),

            DataDrop(i) => self.resolver.resolve_idx(i, Ns::Data),

            ElemDrop(i) => self.resolver.resolve_idx(i, Ns::Elem),

            TableFill(i) | TableSet(i) | TableGet(i) | TableSize(i) | TableGrow(i) => {
                self.resolver.resolve_idx(i, Ns::Table)
            }

            GlobalSet(i) | GlobalGet(i) => self.resolver.resolve_idx(i, Ns::Global),

            LocalSet(i) | LocalGet(i) | LocalTee(i) => self
                .locals
                .resolve(i)
                .map(|_| ())
                .map_err(|id| self.resolver.resolve_error(id, "local")),

            Call(i) | RefFunc(i) => self.resolver.resolve_idx(i, Ns::Func),

            CallIndirect(c) => {
                if let Some(table) = &mut c.table {
                    self.resolver.resolve_idx(table, Ns::Table)?;
                }
                self.resolver.resolve_type_use(&mut c.ty)?;
                Ok(())
            }

            Block(bt) | If(bt) | Loop(bt) => {
                self.resolver.resolve_type_use(&mut bt.ty)?;
                self.labels.push(bt.label);
                Ok(())
            }

            End(_) => {
                self.labels.pop();
                Ok(())
            }

            Br(i) | BrIf(i) => self.resolve_label(i),

            BrTable(i) => {
                for label in i.labels.iter_mut() {
                    self.resolve_label(label)?;
                }
                self.resolve_label(&mut i.default)
            }

            _ => Ok(()),
        }
    }

    fn resolve_label(&self, label: &mut Index<'a>) -> Result<(), ResolveError> {
        let id = match label {
            Index::Num(_) => return Ok(()),
            Index::Id(id) => *id,
        };
        let idx = self
            .labels
            .iter()
            .rev()
            .enumerate()
            .filter_map(|(i, l)| l.map(|l| (i, l)))
            .find(|(_, l)| *l == id);
        match idx {
            Some((idx, _)) => {
                *label = Index::Num(idx as u32);
                Ok(())
            }
            None => Err(self.resolver.resolve_error(id, "label")),
        }
    }
}
