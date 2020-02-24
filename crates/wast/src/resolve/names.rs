use crate::ast::*;
use crate::Error;
use std::collections::HashMap;

#[derive(Copy, Clone)]
pub enum Ns {
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
    tys: Vec<Type<'a>>,
}

struct Type<'a> {
    params: Vec<(Option<Id<'a>>, Option<NameAnnotation<'a>>, ValType)>,
    results: Vec<ValType>,
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
            ModuleField::Global(i) => register(Ns::Global, i.id),
            ModuleField::Memory(i) => register(Ns::Memory, i.id),
            ModuleField::Func(i) => register(Ns::Func, i.id),
            ModuleField::Table(i) => register(Ns::Table, i.id),
            ModuleField::Type(i) => {
                register(Ns::Type, i.id);
                self.tys.push(Type {
                    params: i.func.params.clone(),
                    results: i.func.results.clone(),
                });
            }
            ModuleField::Elem(e) => register(Ns::Elem, e.id),
            ModuleField::Data(d) => register(Ns::Data, d.id),
            ModuleField::Start(_) => {}
            ModuleField::Export(_) => {}
            ModuleField::Custom(_) => {}
        }
    }

    fn ns_mut(&mut self, ns: Ns) -> &mut Namespace<'a> {
        &mut self.ns[ns as usize]
    }

    fn ns(&self, ns: Ns) -> &Namespace<'a> {
        &self.ns[ns as usize]
    }

    pub fn resolve(&self, field: &mut ModuleField<'a>) -> Result<(), Error> {
        match field {
            ModuleField::Import(i) => {
                if let ImportKind::Func(f) = &mut i.kind {
                    self.resolve_type_use(i.span, f)?;
                }
                Ok(())
            }

            ModuleField::Func(f) => {
                self.resolve_type_use(f.span, &mut f.ty)?;
                if let FuncKind::Inline { locals, expression } = &mut f.kind {
                    let mut resolver = ExprResolver::new(self, f.span);

                    // Parameters come first in the local namespace...
                    for (id, _, _) in f.ty.ty.params.iter() {
                        resolver.locals.register(*id);
                    }

                    // .. followed by locals themselves
                    for (id, _, _) in locals {
                        resolver.locals.register(*id);
                    }

                    // and then we can resolve the expression!
                    resolver.resolve(expression)?;
                }
                Ok(())
            }

            ModuleField::Elem(e) => {
                match &mut e.kind {
                    ElemKind::Active { table, offset } => {
                        self.resolve_idx(table, Ns::Table)?;
                        self.resolve_expr(e.span, offset)?;
                    }
                    ElemKind::Passive { .. } | ElemKind::Declared { .. } => {}
                }
                match &mut e.payload {
                    ElemPayload::Indices(elems) => {
                        for idx in elems {
                            self.resolve_idx(idx, Ns::Func)?;
                        }
                    }
                    ElemPayload::Exprs { exprs, .. } => {
                        for funcref in exprs {
                            if let Some(idx) = funcref {
                                self.resolve_idx(idx, Ns::Func)?;
                            }
                        }
                    }
                }
                Ok(())
            }

            ModuleField::Data(d) => {
                if let DataKind::Active { memory, offset } = &mut d.kind {
                    self.resolve_idx(memory, Ns::Memory)?;
                    self.resolve_expr(d.span, offset)?;
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

            ModuleField::Global(g) => {
                if let GlobalKind::Inline(expr) = &mut g.kind {
                    self.resolve_expr(g.span, expr)?;
                }
                Ok(())
            }

            ModuleField::Table(_)
            | ModuleField::Memory(_)
            | ModuleField::Type(_)
            | ModuleField::Custom(_) => Ok(()),
        }
    }

    fn resolve_type_use(&self, span: Span, ty: &mut TypeUse<'a>) -> Result<u32, Error> {
        assert!(ty.index.is_some());
        let idx = self
            .ns(Ns::Type)
            .resolve(ty.index.as_mut().unwrap())
            .map_err(|id| self.resolve_error(id, "type"))?;

        // If the type was listed inline *and* it was specified via a type index
        // we need to assert they're the same.
        let expected = match self.tys.get(idx as usize) {
            Some(ty) => ty,
            None => return Ok(idx),
        };
        if ty.ty.params.len() > 0 || ty.ty.results.len() > 0 {
            let params_not_equal = expected.params.iter().map(|t| t.2).ne(ty
                .ty
                .params
                .iter()
                .map(|t| t.2));
            if params_not_equal || expected.results != ty.ty.results {
                let span = ty.index_span.unwrap_or(span);
                return Err(Error::new(
                    span,
                    format!("inline function type doesn't match type reference"),
                ));
            }
        } else {
            ty.ty.params = expected.params.clone();
            ty.ty.results = expected.results.clone();
        }

        Ok(idx)
    }

    fn resolve_expr(&self, span: Span, expr: &mut Expression<'a>) -> Result<(), Error> {
        ExprResolver::new(self, span).resolve(expr)
    }

    pub fn resolve_idx(&self, idx: &mut Index<'a>, ns: Ns) -> Result<(), Error> {
        match self.ns(ns).resolve(idx) {
            Ok(_n) => Ok(()),
            Err(id) => Err(self.resolve_error(id, ns.desc())),
        }
    }

    fn resolve_error(&self, id: Id<'a>, ns: &str) -> Error {
        Error::new(
            id.span(),
            format!("failed to find {} named `${}`", ns, id.name()),
        )
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
    span: Span,
}

impl<'a, 'b> ExprResolver<'a, 'b> {
    fn new(resolver: &'b Resolver<'a>, span: Span) -> ExprResolver<'a, 'b> {
        ExprResolver {
            resolver,
            locals: Default::default(),
            labels: Vec::new(),
            span,
        }
    }

    fn resolve(&mut self, expr: &mut Expression<'a>) -> Result<(), Error> {
        for instr in expr.instrs.iter_mut() {
            self.resolve_instr(instr)?;
        }
        Ok(())
    }

    fn resolve_instr(&mut self, instr: &mut Instruction<'a>) -> Result<(), Error> {
        use crate::ast::Instruction::*;

        match instr {
            MemoryInit(i) => self.resolver.resolve_idx(&mut i.data, Ns::Data),
            DataDrop(i) => self.resolver.resolve_idx(i, Ns::Data),

            TableInit(i) => {
                self.resolver.resolve_idx(&mut i.elem, Ns::Elem)?;
                self.resolver.resolve_idx(&mut i.table, Ns::Table)
            }
            ElemDrop(i) => self.resolver.resolve_idx(i, Ns::Elem),

            TableCopy(i) => {
                self.resolver.resolve_idx(&mut i.dst, Ns::Table)?;
                self.resolver.resolve_idx(&mut i.src, Ns::Table)
            }

            TableFill(i) | TableSet(i) | TableGet(i) | TableSize(i) | TableGrow(i) => {
                self.resolver.resolve_idx(i, Ns::Table)
            }

            GlobalSet(i) | GlobalGet(i) => self.resolver.resolve_idx(i, Ns::Global),

            LocalSet(i) | LocalGet(i) | LocalTee(i) => self
                .locals
                .resolve(i)
                .map(|_| ())
                .map_err(|id| self.resolver.resolve_error(id, "local")),

            Call(i) | RefFunc(i) | ReturnCall(i) => self.resolver.resolve_idx(i, Ns::Func),

            CallIndirect(c) | ReturnCallIndirect(c) => {
                self.resolver.resolve_idx(&mut c.table, Ns::Table)?;
                self.resolver.resolve_type_use(self.span, &mut c.ty)?;
                Ok(())
            }

            Block(bt) | If(bt) | Loop(bt) => {
                self.labels.push(bt.label);

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
                    let ty = self.resolver.resolve_type_use(self.span, &mut bt.ty)?;
                    let ty = match self.resolver.tys.get(ty as usize) {
                        Some(ty) => ty,
                        None => return Ok(()),
                    };
                    if ty.params.len() == 0 && ty.results.len() <= 1 {
                        bt.ty.ty.params.truncate(0);
                        bt.ty.ty.results = ty.results.clone();
                        bt.ty.index = None;
                    }
                }
                Ok(())
            }

            // On `End` instructions we pop a label from the stack, and for both
            // `End` and `Else` instructions if they have labels listed we
            // verify that they match the label at the beginning of the block.
            Else(_) | End(_) => {
                let (matching_label, label) = match instr {
                    Else(label) => (self.labels.last().cloned(), label),
                    End(label) => (self.labels.pop(), label),
                    _ => unreachable!(),
                };
                let matching_label = match matching_label {
                    Some(l) => l,
                    None => return Ok(()),
                };
                let label = match label {
                    Some(l) => l,
                    None => return Ok(()),
                };
                if Some(*label) == matching_label {
                    return Ok(());
                }
                return Err(Error::new(
                    label.span(),
                    "mismatching labels between end and block".to_string(),
                ));
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

    fn resolve_label(&self, label: &mut Index<'a>) -> Result<(), Error> {
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
