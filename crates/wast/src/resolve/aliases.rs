use crate::ast::*;
use crate::resolve::gensym;
use std::collections::HashMap;

pub fn run(fields: &mut Vec<ModuleField>) {
    let mut cur = 0;
    let mut cx = Expander::default();

    // Note that insertion here is somewhat tricky. We're injecting aliases
    // which will affect the index spaces for each kind of item being aliased.
    // In the final binary aliases will come before all locally defined items,
    // notably via the sorting in binary emission of this crate. To account for
    // this index space behavior we need to ensure that aliases all appear at
    // the right place in the module.
    //
    // The general algorithm here is that aliases discovered in the "header" of
    // the module, e.g. imports/aliases/types/etc, are all inserted preceding
    // the field that the alias is found within. After the header, however, the
    // position of the header is recorded and all future aliases will be
    // inserted at that location.
    //
    // This ends up meaning that aliases found in globals/functions/tables/etc
    // will precede all of those definitions, being positioned at a point that
    // should come after all the instances that are defined as well. Overall
    // this isn't the cleanest algorithm and probably isn't the final form of
    // those. It's hoped that discussion on WebAssembly/module-linking#25 might
    // lead to a good solution.
    let mut insertion_point = None;
    while cur < fields.len() {
        let field = &mut fields[cur];
        match field {
            ModuleField::Alias(_)
            | ModuleField::Type(_)
            | ModuleField::Import(_)
            | ModuleField::NestedModule(_)
            | ModuleField::Instance(_) => {}
            _ if insertion_point.is_none() => insertion_point = Some(cur),
            _ => {}
        }
        cx.process(field);
        if insertion_point.is_none() {
            for item in cx.to_prepend.drain(..) {
                fields.insert(cur, item);
                cur += 1;
            }
        }
        cur += 1;
    }
    if let Some(mut i) = insertion_point {
        for item in cx.to_prepend.drain(..) {
            fields.insert(i, item);
            i += 1;
        }
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
        match field {
            ModuleField::Alias(a) => {
                let id = gensym::fill(a.span, &mut a.id);
                match &mut a.source {
                    AliasSource::InstanceExport { instance, export } => {
                        self.expand(instance);
                        self.instances
                            .insert((instance.idx, export, a.kind), id.into());
                    }
                    AliasSource::Outer { module, index } => {
                        self.parents.insert((*module, *index, a.kind), id.into());
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
                if let ElemKind::Active { table, offset, .. } = &mut e.kind {
                    self.expand(table);
                    self.expand_expr(offset);
                }
                match &mut e.payload {
                    ElemPayload::Indices(funcs) => {
                        for func in funcs {
                            self.expand(func);
                        }
                    }
                    ElemPayload::Exprs { exprs, .. } => {
                        for expr in exprs {
                            self.expand_expr(expr);
                        }
                    }
                }
            }

            ModuleField::Data(e) => {
                if let DataKind::Active { memory, offset, .. } = &mut e.kind {
                    self.expand(memory);
                    self.expand_expr(offset);
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

            ModuleField::Tag(t) => match &mut t.ty {
                TagType::Exception(t) => self.expand_type_use(t),
            },

            ModuleField::NestedModule(m) => match &mut m.kind {
                NestedModuleKind::Import { ty, .. } => self.expand_type_use(ty),
                NestedModuleKind::Inline { fields } => run(fields),
            },

            ModuleField::Type(t) => match &mut t.def {
                TypeDef::Func(f) => f.expand(self),
                TypeDef::Struct(_) => {}
                TypeDef::Array(_) => {}
                TypeDef::Module(m) => m.expand(self),
                TypeDef::Instance(i) => i.expand(self),
            },

            ModuleField::Custom(_) | ModuleField::Memory(_) | ModuleField::Table(_) => {}
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
            ItemKind::Tag(t) => match t {
                TagType::Exception(t) => self.expand_type_use(t),
            },
        }
    }

    fn expand_type_use<T: Expand<'a>>(&mut self, ty: &mut TypeUse<'a, T>) {
        if let Some(index) = &mut ty.index {
            self.expand(index);
        }
        if let Some(inline) = &mut ty.inline {
            inline.expand(self);
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

            Let(t) => self.expand_type_use(&mut t.block.ty),

            Block(bt) | If(bt) | Loop(bt) | Try(bt) => self.expand_type_use(&mut bt.ty),

            FuncBind(t) => self.expand_type_use(&mut t.ty),

            _ => {}
        }
    }

    fn expand<T>(&mut self, item: &mut ItemRef<'a, T>)
    where
        T: Into<ExportKind> + Copy,
    {
        #[cfg(wast_check_exhaustive)]
        {
            item.visited = true;
        }
    }
}

trait Expand<'a> {
    fn expand(&mut self, cx: &mut Expander<'a>);
}

impl<'a> Expand<'a> for FunctionType<'a> {
    fn expand(&mut self, _cx: &mut Expander<'a>) {}
}

impl<'a> Expand<'a> for ModuleType<'a> {
    fn expand(&mut self, cx: &mut Expander<'a>) {
        for i in self.imports.iter_mut() {
            cx.expand_item_sig(&mut i.item);
        }
        for e in self.exports.iter_mut() {
            cx.expand_item_sig(&mut e.item);
        }
    }
}

impl<'a> Expand<'a> for InstanceType<'a> {
    fn expand(&mut self, cx: &mut Expander<'a>) {
        for e in self.exports.iter_mut() {
            cx.expand_item_sig(&mut e.item);
        }
    }
}
