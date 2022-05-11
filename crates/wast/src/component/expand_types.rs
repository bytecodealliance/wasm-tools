use crate::component::*;
use crate::core;
use crate::gensym;
use crate::kw;
use crate::token::{Index, ItemRef, Span};
use std::collections::HashMap;
use std::mem;

pub fn expand(fields: &mut Vec<ComponentField<'_>>) {
    let mut expander = Expander::default();
    expander.process(fields, Expander::expand);
}

#[derive(Default)]
struct Expander<'a> {
    /// Fields, during processing, which should be prepended to the
    /// currently-being-processed field. This should always be empty after
    /// processing is complete.
    to_prepend: Vec<TypeField<'a>>,
}

impl<'a> Expander<'a> {
    fn process<T>(&mut self, fields: &mut Vec<T>, expand: fn(&mut Self, &mut T))
    where
        T: From<TypeField<'a>>,
    {
        let mut cur = 0;
        while cur < fields.len() {
            expand(self, &mut fields[cur]);
            for item in self.to_prepend.drain(..) {
                fields.insert(cur, item.into());
                cur += 1;
            }
            cur += 1;
        }
    }

    fn expand(&mut self, item: &mut ComponentField<'a>) {
        match item {
            ComponentField::Type(t) => self.expand_type_field(t),
            ComponentField::Import(t) => {
                self.expand_item_sig(&mut t.item);
            }
            ComponentField::Component(c) => match &mut c.kind {
                NestedComponentKind::Inline(fields) => expand(fields),
                NestedComponentKind::Import { .. } => {}
            },

            ComponentField::Func(f) => {
                self.expand_component_type_use(&mut f.ty);
            }

            // These fields don't have any nested `ComponentTypeUse` or
            // `TypeUse` fields.
            ComponentField::Alias(_)
            | ComponentField::Module(_)
            | ComponentField::Start(_)
            | ComponentField::Export(_)
            | ComponentField::Instance(_) => {}
        }
    }

    fn expand_type_field(&mut self, field: &mut TypeField<'a>) {
        match &mut field.def {
            ComponentTypeDef::DefType(t) => self.expand_deftype(t),
            ComponentTypeDef::InterType(t) => self.expand_intertype(t),
        }
        let name = gensym::fill(field.span, &mut field.id);
        let name = Index::Id(name);
        match &field.def {
            ComponentTypeDef::DefType(DefType::Func(t)) => t.key().insert(self, name),
            ComponentTypeDef::DefType(DefType::Module(t)) => t.key().insert(self, name),
            ComponentTypeDef::DefType(DefType::Component(t)) => t.key().insert(self, name),
            ComponentTypeDef::DefType(DefType::Instance(t)) => t.key().insert(self, name),
            ComponentTypeDef::DefType(DefType::Value(t)) => {
                // TODO: is `value` really a type definition?
                drop(t);
            }
            ComponentTypeDef::InterType(t) => t.key().insert(self, name),
        }
    }

    fn expand_deftype(&mut self, ty: &mut DefType<'a>) {
        match ty {
            DefType::Func(t) => self.expand_func_ty(t),
            DefType::Module(m) => self.expand_module_ty(m),
            DefType::Component(c) => self.expand_component_ty(c),
            DefType::Instance(i) => self.expand_instance_ty(i),
            DefType::Value(v) => self.expand_value_ty(v),
        }
    }

    fn expand_func_ty(&mut self, ty: &mut ComponentFunctionType<'a>) {
        for param in ty.params.iter_mut() {
            self.expand_component_type_use(&mut param.type_);
        }
        self.expand_component_type_use(&mut ty.result);
    }

    fn expand_module_ty(&mut self, ty: &mut ModuleType<'a>) {
        use crate::core::resolve::types::{FuncKey, TypeKey, TypeReference};

        // Note that this is a custom implementation from everything else in
        // this file since this is using core wasm types instead of component
        // types, so a small part of the core wasm expansion process is
        // inlined here to handle the `TypeUse` from core wasm.

        let mut func_type_to_idx = HashMap::new();
        let mut to_prepend = Vec::new();
        let mut i = 0;
        while i < ty.defs.len() {
            match &mut ty.defs[i] {
                ModuleTypeDef::Type(ty) => match &ty.def {
                    core::TypeDef::Func(f) => {
                        let id = gensym::fill(ty.span, &mut ty.id);
                        func_type_to_idx.insert(f.key(), Index::Id(id));
                    }
                    core::TypeDef::Struct(_) => {}
                    core::TypeDef::Array(_) => {}
                },
                ModuleTypeDef::Import(ty) => {
                    expand_sig(&mut ty.item, &mut to_prepend, &mut func_type_to_idx);
                }
                ModuleTypeDef::Export(_, item) => {
                    expand_sig(item, &mut to_prepend, &mut func_type_to_idx);
                }
            }
            ty.defs.splice(i..i, to_prepend.drain(..));
            i += 1;
        }

        fn expand_sig<'a>(
            item: &mut core::ItemSig<'a>,
            to_prepend: &mut Vec<ModuleTypeDef<'a>>,
            func_type_to_idx: &mut HashMap<FuncKey<'a>, Index<'a>>,
        ) {
            match &mut item.kind {
                core::ItemKind::Func(t) | core::ItemKind::Tag(core::TagType::Exception(t)) => {
                    // If the index is already filled in then this is skipped
                    if t.index.is_some() {
                        return;
                    }

                    // Otherwise the inline type information is used to
                    // generate a type into this module if necessary. If the
                    // function type already exists we reuse the same key,
                    // otherwise a fresh type definition is created and we use
                    // that one instead.
                    let ty = t.inline.take().unwrap_or_default();
                    let key = ty.key();
                    if let Some(idx) = func_type_to_idx.get(&key) {
                        t.index = Some(ItemRef {
                            idx: idx.clone(),
                            kind: kw::r#type(item.span),
                        });
                        return;
                    }
                    let id = gensym::gen(item.span);
                    to_prepend.push(ModuleTypeDef::Type(core::Type {
                        span: item.span,
                        id: Some(id),
                        name: None,
                        def: key.to_def(item.span),
                    }));
                    let idx = Index::Id(id);
                    t.index = Some(ItemRef {
                        idx,
                        kind: kw::r#type(item.span),
                    });
                }
                core::ItemKind::Global(_)
                | core::ItemKind::Table(_)
                | core::ItemKind::Memory(_) => {}
            }
        }
    }

    fn expand_component_ty(&mut self, ty: &mut ComponentType<'a>) {
        Expander::default().process(&mut ty.fields, |e, field| match field {
            ComponentTypeField::Type(t) => e.expand_type_field(t),
            ComponentTypeField::Alias(_) => {}
            ComponentTypeField::Export(t) => e.expand_item_sig(&mut t.item),
            ComponentTypeField::Import(t) => e.expand_item_sig(&mut t.item),
        })
    }

    fn expand_instance_ty(&mut self, ty: &mut InstanceType<'a>) {
        Expander::default().process(&mut ty.fields, |e, field| match field {
            InstanceTypeField::Type(t) => e.expand_type_field(t),
            InstanceTypeField::Alias(_) => {}
            InstanceTypeField::Export(t) => e.expand_item_sig(&mut t.item),
        })
    }

    fn expand_item_sig(&mut self, sig: &mut ItemSig<'a>) {
        match &mut sig.kind {
            ItemKind::Component(t) => {
                self.expand_component_type_use(t);
            }
            ItemKind::Module(t) => {
                self.expand_component_type_use(t);
            }
            ItemKind::Instance(t) => {
                self.expand_component_type_use(t);
            }
            ItemKind::Value(t) => self.expand_value_ty(t),
            ItemKind::Func(t) => {
                self.expand_component_type_use(t);
            }
        };
    }

    fn expand_value_ty(&mut self, ty: &mut ValueType<'a>) {
        self.expand_component_type_use(&mut ty.value_type);
    }

    fn expand_intertype(&mut self, ty: &mut InterType<'a>) {
        match ty {
            InterType::Unit
            | InterType::Bool
            | InterType::S8
            | InterType::U8
            | InterType::S16
            | InterType::U16
            | InterType::S32
            | InterType::U32
            | InterType::S64
            | InterType::U64
            | InterType::Float32
            | InterType::Float64
            | InterType::Char
            | InterType::String
            | InterType::Flags(_)
            | InterType::Enum(_) => {}
            InterType::Record(r) => {
                for field in r.fields.iter_mut() {
                    self.expand_component_type_use(&mut field.type_);
                }
            }
            InterType::Variant(v) => {
                for case in v.cases.iter_mut() {
                    self.expand_component_type_use(&mut case.type_);
                }
            }
            InterType::List(t) => {
                self.expand_component_type_use(&mut t.element);
            }
            InterType::Tuple(t) => {
                for field in t.fields.iter_mut() {
                    self.expand_component_type_use(field);
                }
            }
            InterType::Union(u) => {
                for arm in u.arms.iter_mut() {
                    self.expand_component_type_use(arm);
                }
            }
            InterType::Option(t) => {
                self.expand_component_type_use(&mut t.element);
            }
            InterType::Expected(e) => {
                self.expand_component_type_use(&mut e.ok);
                self.expand_component_type_use(&mut e.err);
            }
        }
    }

    fn expand_component_type_use<T>(&mut self, item: &mut ComponentTypeUse<'a, T>) -> Index<'a>
    where
        T: TypeReference<'a>,
    {
        let span = Span::from_offset(0); // FIXME: don't manufacture
        let dummy = ComponentTypeUse::Ref(ItemRef {
            idx: Index::Num(0, span),
            kind: kw::r#type(span),
        });
        let mut inline = match mem::replace(item, dummy) {
            // If this type-use was already a reference to an existing type
            // then we put it back the way it was and return the corresponding
            // index.
            ComponentTypeUse::Ref(idx) => {
                *item = ComponentTypeUse::Ref(idx.clone());
                return idx.idx;
            }

            // ... otherwise with an inline type definition we go into
            // processing below.
            ComponentTypeUse::Inline(inline) => inline,
        };
        inline.expand(self);

        // If this inline type has already been defined within this context
        // then reuse the previously defined type to avoid injecting too many
        // types into the type index space.
        if let Some(idx) = inline.key().lookup(self) {
            *item = ComponentTypeUse::Ref(ItemRef {
                idx,
                kind: kw::r#type(span),
            });
            return idx;
        }

        // And if this type isn't already defined we append it to the index
        // space with a fresh and unique name.
        let id = gensym::gen(span);
        self.to_prepend.push(TypeField {
            span,
            id: Some(id),
            name: None,
            def: inline.into_def(),
        });
        let idx = Index::Id(id);
        *item = ComponentTypeUse::Ref(ItemRef {
            idx,
            kind: kw::r#type(span),
        });
        return idx;
    }
}

trait TypeReference<'a> {
    type Key: TypeKey<'a>;
    fn key(&self) -> Self::Key;
    fn expand(&mut self, cx: &mut Expander<'a>);
    fn into_def(self) -> ComponentTypeDef<'a>;
}

impl<'a> TypeReference<'a> for InterType<'a> {
    type Key = Todo; // TODO: should implement this

    fn key(&self) -> Self::Key {
        Todo
    }

    fn expand(&mut self, cx: &mut Expander<'a>) {
        cx.expand_intertype(self)
    }

    fn into_def(self) -> ComponentTypeDef<'a> {
        ComponentTypeDef::InterType(self)
    }
}

impl<'a> TypeReference<'a> for ComponentType<'a> {
    type Key = Todo; // TODO: should implement this

    fn key(&self) -> Self::Key {
        Todo
    }

    fn expand(&mut self, cx: &mut Expander<'a>) {
        cx.expand_component_ty(self)
    }

    fn into_def(self) -> ComponentTypeDef<'a> {
        ComponentTypeDef::DefType(DefType::Component(self))
    }
}

impl<'a> TypeReference<'a> for ModuleType<'a> {
    type Key = Todo; // TODO: should implement this

    fn key(&self) -> Self::Key {
        Todo
    }

    fn expand(&mut self, cx: &mut Expander<'a>) {
        cx.expand_module_ty(self)
    }

    fn into_def(self) -> ComponentTypeDef<'a> {
        ComponentTypeDef::DefType(DefType::Module(self))
    }
}

impl<'a> TypeReference<'a> for InstanceType<'a> {
    type Key = Todo; // TODO: should implement this

    fn key(&self) -> Self::Key {
        Todo
    }

    fn expand(&mut self, cx: &mut Expander<'a>) {
        cx.expand_instance_ty(self)
    }

    fn into_def(self) -> ComponentTypeDef<'a> {
        ComponentTypeDef::DefType(DefType::Instance(self))
    }
}

impl<'a> TypeReference<'a> for ComponentFunctionType<'a> {
    type Key = Todo; // TODO: should implement this

    fn key(&self) -> Self::Key {
        Todo
    }

    fn expand(&mut self, cx: &mut Expander<'a>) {
        cx.expand_func_ty(self)
    }

    fn into_def(self) -> ComponentTypeDef<'a> {
        ComponentTypeDef::DefType(DefType::Func(self))
    }
}

trait TypeKey<'a> {
    fn lookup(&self, cx: &Expander<'a>) -> Option<Index<'a>>;
    fn insert(&self, cx: &mut Expander<'a>, id: Index<'a>);
}

struct Todo;

impl<'a> TypeKey<'a> for Todo {
    fn lookup(&self, _cx: &Expander<'a>) -> Option<Index<'a>> {
        None
    }
    fn insert(&self, cx: &mut Expander<'a>, id: Index<'a>) {
        drop((cx, id));
    }
}
