use crate::Error;
use crate::component::*;
use crate::core::{self, ValType, resolve::ResolveCoreType};
use crate::gensym;
use crate::kw;
use crate::names::Namespace;
use crate::token::Span;
use crate::token::{Id, Index};

/// Resolve the fields of a component and everything nested within it, changing
/// `Index::Id` to `Index::Num` and expanding alias syntax sugar.
pub fn resolve(component: &mut Component<'_>) -> Result<(), Error> {
    let fields = match &mut component.kind {
        ComponentKind::Text(fields) => fields,
        ComponentKind::Binary(_) => return Ok(()),
    };
    let mut resolver = Resolver::default();
    resolver.fields(component.id, fields)
}

impl<'a> From<Alias<'a>> for ComponentField<'a> {
    fn from(a: Alias<'a>) -> Self {
        Self::Alias(a)
    }
}

impl<'a> From<Alias<'a>> for ModuleTypeDecl<'a> {
    fn from(a: Alias<'a>) -> Self {
        Self::Alias(a)
    }
}

impl<'a> From<Alias<'a>> for ComponentTypeDecl<'a> {
    fn from(a: Alias<'a>) -> Self {
        Self::Alias(a)
    }
}

impl<'a> From<Alias<'a>> for InstanceTypeDecl<'a> {
    fn from(a: Alias<'a>) -> Self {
        Self::Alias(a)
    }
}

#[derive(Default)]
struct Resolver<'a> {
    stack: Vec<ComponentState<'a>>,

    // Current resolver phase, see `resolve_group` for more information.
    phase: Phase,

    // When a name refers to a definition in an outer scope, we'll need to
    // insert an outer alias before it. This collects the aliases to be
    // inserted during resolution.
    aliases_to_insert: Vec<Alias<'a>>,
}

/// The phases of resolution when processing a group of fields.
///
/// See `resolve_group` for more information.
#[derive(Default, PartialEq, Debug)]
enum Phase {
    ExpandAliases,
    #[default]
    Resolve,
}

/// Context structure used to perform name resolution.
#[derive(Default)]
struct ComponentState<'a> {
    id: Option<Id<'a>>,

    // Namespaces within each component. Note that each namespace carries
    // with it information about the signature of the item in that namespace.
    // The signature is later used to synthesize the type of a component and
    // inject type annotations if necessary.
    core_funcs: Namespace<'a>,
    core_globals: Namespace<'a>,
    core_tables: Namespace<'a>,
    core_memories: Namespace<'a>,
    core_types: Namespace<'a>,
    core_tags: Namespace<'a>,
    core_instances: Namespace<'a>,
    core_modules: Namespace<'a>,

    funcs: Namespace<'a>,
    types: Namespace<'a>,
    instances: Namespace<'a>,
    components: Namespace<'a>,
    values: Namespace<'a>,
}

impl<'a> ComponentState<'a> {
    fn new(id: Option<Id<'a>>) -> ComponentState<'a> {
        ComponentState {
            id,
            ..ComponentState::default()
        }
    }

    fn register_item_sig(&mut self, sig: &ItemSig<'a>) -> Result<u32, Error> {
        match &sig.kind {
            ItemSigKind::CoreModule(_) => self.core_modules.register(sig.id, "core module"),
            ItemSigKind::Func(_) => self.funcs.register(sig.id, "func"),
            ItemSigKind::Component(_) => self.components.register(sig.id, "component"),
            ItemSigKind::Instance(_) => self.instances.register(sig.id, "instance"),
            ItemSigKind::Value(_) => self.values.register(sig.id, "value"),
            ItemSigKind::Type(_) => self.types.register(sig.id, "type"),
        }
    }
}

impl<'a> Resolver<'a> {
    fn current(&mut self) -> &mut ComponentState<'a> {
        self.stack
            .last_mut()
            .expect("should have at least one component state")
    }

    fn fields(
        &mut self,
        id: Option<Id<'a>>,
        fields: &mut Vec<ComponentField<'a>>,
    ) -> Result<(), Error> {
        self.resolve_group(id, fields, Resolver::field, ComponentState::register)?;
        Ok(())
    }

    /// Helper function to resolve a group of `fields` together within their own
    /// index space.
    ///
    /// The `process` function is the means by which each field is recursively
    /// traversed and visited internally to resolve all of its names that it
    /// references. The `register` function inserts the name of the field into
    /// the `ComponentState` if it's defined.
    ///
    /// This will internally handle injection of aliases as necessary.
    fn resolve_group<T>(
        &mut self,
        group_id: Option<Id<'a>>,
        fields: &mut Vec<T>,
        process: fn(&mut Self, &mut T) -> Result<(), Error>,
        register: fn(&mut ComponentState<'a>, &T) -> Result<(), Error>,
    ) -> Result<(), Error>
    where
        T: From<Alias<'a>>,
    {
        // During the expansion phase there's no need to recurse into
        // groups-within-that-container, so this is skipped entirely.
        if let Phase::ExpandAliases = self.phase {
            return Ok(());
        }

        assert!(self.aliases_to_insert.is_empty());

        // The first phase of resolution is done to expand and inject aliases
        // into the AST. This will resolve aliased exports and outer aliases as
        // necessary. The `self.phase` tracker is used to configure minor bits
        // of behavior in core functions below.
        //
        // Note that this phase still starts with a `register` to build up the
        // namespace of all known items. The actual indices here, however, are
        // known to be incorrect because they don't take into account injected
        // aliases. That's ok though because this phase doesn't actually rewrite
        // any name references in the AST. The `state` here is discarded
        // entirely before moving on to the next phase to represent how these
        // invalid indices are all thrown away.
        //
        // The namespaces here are primarily populated to handle resolution of
        // outer aliases. Outer aliases are injected when there's otherwise
        // nothing to refer to within a component, but we only know what's able
        // to be referred to if the namespaces are registered.
        {
            self.phase = Phase::ExpandAliases;
            let mut state = ComponentState::new(group_id);

            for field in fields.iter() {
                register(&mut state, field)?;
            }

            self.stack.push(state);

            // Process each field, and then handle the `aliases_to_insert` to
            // prepend all those aliases ust before the field being processed.
            // Then the field, and the aliases, are all skipped and the next
            // field is processed.
            let mut i = 0;
            while i < fields.len() {
                process(self, &mut fields[i])?;
                let amt = self.aliases_to_insert.len();
                fields.splice(i..i, self.aliases_to_insert.drain(..).map(T::from));
                i += amt + 1;
            }

            self.stack.pop();
        }

        assert!(self.aliases_to_insert.is_empty());

        // The second phase of resolution is where everything is now final and
        // we're ready to rewrite the AST from names to indices. This is modeled
        // after the core wasm resolution where everything is registered to
        // assign indexes an then everything is processed.
        //
        // Note that this explicitly allows forward-references which aren't
        // actually valid in the component model. This makes printing/parsing
        // invalid components easier to work with and additionally ensures that
        // `wasmprinter`'s printing of an invalid component isn't something that
        // then can't parse. Basically it makes tooling easier to allow
        // forward/backward references at the same time, even if only one of
        // those is valid.
        {
            self.phase = Phase::Resolve;
            let mut state = ComponentState::new(group_id);

            for field in fields.iter() {
                register(&mut state, field)?;
            }

            self.stack.push(state);

            for field in fields.iter_mut() {
                process(self, field)?;
            }

            assert_eq!(self.phase, Phase::Resolve);
            self.stack.pop();
        }

        assert!(self.aliases_to_insert.is_empty());

        Ok(())
    }

    fn field(&mut self, field: &mut ComponentField<'a>) -> Result<(), Error> {
        match field {
            ComponentField::CoreModule(m) => self.core_module(m),
            ComponentField::CoreInstance(i) => self.core_instance(i),
            ComponentField::CoreType(t) => self.core_ty(t),
            ComponentField::CoreRec(t) => self.core_rec(t),
            ComponentField::Component(c) => self.component(c),
            ComponentField::Instance(i) => self.instance(i),
            ComponentField::Alias(a) => self.alias(a),
            ComponentField::Type(t) => self.ty(t),
            ComponentField::CanonicalFunc(f) => self.canonical_func(f),
            ComponentField::CoreFunc(_) => unreachable!("should be expanded already"),
            ComponentField::Func(_) => unreachable!("should be expanded already"),
            ComponentField::Start(s) => self.start(s),
            ComponentField::Import(i) => self.item_sig(&mut i.item),
            ComponentField::Export(e) => {
                if let Some(ty) = &mut e.ty {
                    self.item_sig(&mut ty.0)?;
                }
                self.export(&mut e.kind)
            }
            ComponentField::Custom(_) | ComponentField::Producers(_) => Ok(()),
        }
    }

    fn core_module(&mut self, module: &mut CoreModule) -> Result<(), Error> {
        match &mut module.kind {
            CoreModuleKind::Inline { fields } => {
                if let Phase::Resolve = self.phase {
                    crate::core::resolve::resolve(fields)?;
                }
            }

            CoreModuleKind::Import { .. } => {
                unreachable!("should be expanded already")
            }
        }

        Ok(())
    }

    fn component(&mut self, component: &mut NestedComponent<'a>) -> Result<(), Error> {
        match &mut component.kind {
            NestedComponentKind::Import { .. } => unreachable!("should be expanded already"),
            NestedComponentKind::Inline(fields) => {
                self.fields(component.id, fields)?;
                Ok(())
            }
        }
    }

    fn core_instance(&mut self, instance: &mut CoreInstance<'a>) -> Result<(), Error> {
        match &mut instance.kind {
            CoreInstanceKind::Instantiate { module, args } => {
                self.component_item_ref(module)?;
                for arg in args {
                    match &mut arg.kind {
                        CoreInstantiationArgKind::Instance(i) => {
                            self.core_item_ref(i)?;
                        }
                        CoreInstantiationArgKind::BundleOfExports(..) => {
                            unreachable!("should be expanded already");
                        }
                    }
                }
            }
            CoreInstanceKind::BundleOfExports(exports) => {
                for export in exports {
                    self.core_item_ref(&mut export.item)?;
                }
            }
        }
        Ok(())
    }

    fn instance(&mut self, instance: &mut Instance<'a>) -> Result<(), Error> {
        match &mut instance.kind {
            InstanceKind::Instantiate { component, args } => {
                self.component_item_ref(component)?;
                for arg in args {
                    match &mut arg.kind {
                        InstantiationArgKind::Item(e) => {
                            self.export(e)?;
                        }
                        InstantiationArgKind::BundleOfExports(..) => {
                            unreachable!("should be expanded already")
                        }
                    }
                }
            }
            InstanceKind::BundleOfExports(exports) => {
                for export in exports {
                    self.export(&mut export.kind)?;
                }
            }
            InstanceKind::Import { .. } => {
                unreachable!("should be expanded already")
            }
        }
        Ok(())
    }

    fn item_sig(&mut self, item: &mut ItemSig<'a>) -> Result<(), Error> {
        match &mut item.kind {
            // Here we must be explicit otherwise the module type reference will
            // be assumed to be in the component type namespace
            ItemSigKind::CoreModule(t) => self.core_type_use(t),
            ItemSigKind::Func(t) => self.component_type_use(t),
            ItemSigKind::Component(t) => self.component_type_use(t),
            ItemSigKind::Instance(t) => self.component_type_use(t),
            ItemSigKind::Value(t) => self.component_val_type(&mut t.0),
            ItemSigKind::Type(b) => match b {
                TypeBounds::Eq(i) => self.resolve_ns(i, Ns::Type),
                TypeBounds::SubResource => Ok(()),
            },
        }
    }

    fn export(&mut self, kind: &mut ComponentExportKind<'a>) -> Result<(), Error> {
        match kind {
            // Here we do *not* have to be explicit as the item ref is to a core module
            ComponentExportKind::CoreModule(r) => self.component_item_ref(r),
            ComponentExportKind::Func(r) => self.component_item_ref(r),
            ComponentExportKind::Value(r) => self.component_item_ref(r),
            ComponentExportKind::Type(r) => self.component_item_ref(r),
            ComponentExportKind::Component(r) => self.component_item_ref(r),
            ComponentExportKind::Instance(r) => self.component_item_ref(r),
        }
    }

    fn start(&mut self, start: &mut Start<'a>) -> Result<(), Error> {
        self.resolve_ns(&mut start.func, Ns::Func)?;
        for arg in start.args.iter_mut() {
            self.component_item_ref(arg)?;
        }
        Ok(())
    }

    fn outer_alias<T: Into<Ns>>(
        &mut self,
        outer: &mut Index<'a>,
        index: &mut Index<'a>,
        kind: T,
        span: Span,
    ) -> Result<(), Error> {
        // Nothing here injects an alias, so skip this function on this phase.
        if let Phase::ExpandAliases = self.phase {
            return Ok(());
        }

        // Short-circuit when both indices are already resolved as this
        // helps to write tests for invalid modules where wasmparser should
        // be the one returning the error.
        if let Index::Num(..) = outer {
            if let Index::Num(..) = index {
                return Ok(());
            }
        }

        // Resolve `outer`, and compute the depth at which to look up
        // `index`.
        let depth = match outer {
            Index::Id(id) => {
                let mut depth = 0;
                for resolver in self.stack.iter().rev() {
                    if resolver.id == Some(*id) {
                        break;
                    }
                    depth += 1;
                }
                if depth as usize == self.stack.len() {
                    return Err(Error::new(
                        span,
                        format!("outer component `{}` not found", id.name()),
                    ));
                }
                depth
            }
            Index::Num(n, _span) => *n,
        };

        if depth as usize >= self.stack.len() {
            return Err(Error::new(
                span,
                format!("outer count of `{depth}` is too large"),
            ));
        }

        *outer = Index::Num(depth, span);

        // Resolve `index` within the computed scope depth.
        let computed = self.stack.len() - 1 - depth as usize;
        self.stack[computed].resolve(kind.into(), index)?;

        Ok(())
    }

    fn alias(&mut self, alias: &mut Alias<'a>) -> Result<(), Error> {
        match &mut alias.target {
            AliasTarget::Export {
                instance,
                name: _,
                kind: _,
            } => {
                self.resolve_ns(instance, Ns::Instance)?;
            }
            AliasTarget::CoreExport {
                instance,
                name: _,
                kind: _,
            } => {
                self.resolve_ns(instance, Ns::CoreInstance)?;
            }
            AliasTarget::Outer { outer, index, kind } => {
                self.outer_alias(outer, index, *kind, alias.span)?;
            }
        }
        Ok(())
    }

    fn canonical_func(&mut self, func: &mut CanonicalFunc<'a>) -> Result<(), Error> {
        match &mut func.kind {
            CanonicalFuncKind::Lift { ty, info } => {
                self.component_type_use(ty)?;
                self.core_item_ref(&mut info.func)?;
                self.canon_opts(&mut info.opts)?;
            }
            CanonicalFuncKind::Core(core) => match core {
                CoreFuncKind::Alias(_) => {
                    panic!("should have been removed during expansion")
                }
                CoreFuncKind::Lower(info) => {
                    self.component_item_ref(&mut info.func)?;
                    self.canon_opts(&mut info.opts)?;
                }
                CoreFuncKind::ResourceNew(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::ResourceRep(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::ResourceDrop(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::ThreadSpawnRef(info) => {
                    self.core_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::ThreadSpawnIndirect(info) => {
                    self.core_item_ref(&mut info.ty)?;
                    self.core_item_ref(&mut info.table)?;
                }
                CoreFuncKind::ThreadAvailableParallelism(_)
                | CoreFuncKind::BackpressureInc
                | CoreFuncKind::BackpressureDec
                | CoreFuncKind::TaskCancel
                | CoreFuncKind::SubtaskDrop
                | CoreFuncKind::SubtaskCancel(_)
                | CoreFuncKind::ErrorContextDrop => {}
                CoreFuncKind::TaskReturn(info) => {
                    if let Some(ty) = &mut info.result {
                        self.component_val_type(ty)?;
                    }
                    self.canon_opts(&mut info.opts)?;
                }
                CoreFuncKind::ContextGet(ty, _) => self.ref_type(ty)?,
                CoreFuncKind::ContextSet(ty, _) => self.ref_type(ty)?,
                CoreFuncKind::StreamNew(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::StreamRead(info) => {
                    self.component_item_ref(&mut info.ty)?;
                    self.canon_opts(&mut info.opts)?;
                }
                CoreFuncKind::StreamWrite(info) => {
                    self.component_item_ref(&mut info.ty)?;
                    self.canon_opts(&mut info.opts)?;
                }
                CoreFuncKind::StreamCancelRead(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::StreamCancelWrite(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::StreamDropReadable(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::StreamDropWritable(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::FutureNew(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::FutureRead(info) => {
                    self.component_item_ref(&mut info.ty)?;
                    self.canon_opts(&mut info.opts)?;
                }
                CoreFuncKind::FutureWrite(info) => {
                    self.component_item_ref(&mut info.ty)?;
                    self.canon_opts(&mut info.opts)?;
                }
                CoreFuncKind::FutureCancelRead(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::FutureCancelWrite(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::FutureDropReadable(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::FutureDropWritable(info) => {
                    self.component_item_ref(&mut info.ty)?;
                }
                CoreFuncKind::ErrorContextNew(info) => {
                    self.canon_opts(&mut info.opts)?;
                }
                CoreFuncKind::ErrorContextDebugMessage(info) => {
                    self.canon_opts(&mut info.opts)?;
                }
                CoreFuncKind::WaitableSetNew => {}
                CoreFuncKind::WaitableSetWait(info) => {
                    self.core_item_ref(&mut info.memory)?;
                }
                CoreFuncKind::WaitableSetPoll(info) => {
                    self.core_item_ref(&mut info.memory)?;
                }
                CoreFuncKind::WaitableSetDrop => {}
                CoreFuncKind::WaitableJoin => {}
                CoreFuncKind::ThreadIndex => {}
                CoreFuncKind::ThreadNewIndirect(info) => {
                    self.core_item_ref(&mut info.ty)?;
                    self.core_item_ref(&mut info.table)?;
                }
                CoreFuncKind::ThreadResumeLater => {}
                CoreFuncKind::ThreadSuspend => {}
                CoreFuncKind::ThreadYield => {}
                CoreFuncKind::ThreadSuspendThenResume => {}
                CoreFuncKind::ThreadYieldThenResume => {}
                CoreFuncKind::ThreadSuspendThenPromote => {}
                CoreFuncKind::ThreadYieldThenPromote => {}
            },
        }

        Ok(())
    }

    fn ref_type(&mut self, ty: &mut ValType<'a>) -> Result<(), Error> {
        Ok(match ty {
            ValType::I32 | ValType::I64 | ValType::F32 | ValType::F64 | ValType::V128 => {}
            ValType::Ref(r) => match &mut r.heap {
                core::HeapType::Abstract { .. } => {}
                core::HeapType::Concrete(id) | core::HeapType::Exact(id) => {
                    self.resolve_ns(id, Ns::Type)?;
                }
            },
        })
    }

    fn canon_opts(&mut self, opts: &mut [CanonOpt<'a>]) -> Result<(), Error> {
        for opt in opts {
            match opt {
                CanonOpt::StringUtf8
                | CanonOpt::StringUtf16
                | CanonOpt::StringLatin1Utf16
                | CanonOpt::Async
                | CanonOpt::Gc => {}
                CanonOpt::Memory(r) => self.core_item_ref(r)?,
                CanonOpt::Realloc(r) | CanonOpt::PostReturn(r) | CanonOpt::Callback(r) => {
                    self.core_item_ref(r)?
                }
                CanonOpt::CoreType(t) => self.core_item_ref(t)?,
            }
        }

        Ok(())
    }

    fn core_type_use<T>(&mut self, ty: &mut CoreTypeUse<'a, T>) -> Result<(), Error> {
        let item = match ty {
            CoreTypeUse::Ref(r) => r,
            CoreTypeUse::Inline(_) => {
                unreachable!("inline type-use should be expanded by now")
            }
        };
        self.core_item_ref(item)
    }

    fn component_type_use<T>(&mut self, ty: &mut ComponentTypeUse<'a, T>) -> Result<(), Error> {
        let item = match ty {
            ComponentTypeUse::Ref(r) => r,
            ComponentTypeUse::Inline(_) => {
                unreachable!("inline type-use should be expanded by now")
            }
        };
        self.component_item_ref(item)
    }

    fn defined_type(&mut self, ty: &mut ComponentDefinedType<'a>) -> Result<(), Error> {
        match ty {
            ComponentDefinedType::Primitive(_) => {}
            ComponentDefinedType::Flags(_) => {}
            ComponentDefinedType::Enum(_) => {}
            ComponentDefinedType::Record(r) => {
                for field in r.fields.iter_mut() {
                    self.component_val_type(&mut field.ty)?;
                }
            }
            ComponentDefinedType::Variant(v) => {
                // Namespace for case identifier resolution
                let mut ns = Namespace::default();
                for case in v.cases.iter_mut() {
                    if self.phase == Phase::Resolve {
                        ns.register(case.id, "variant case")?;
                    }

                    if let Some(ty) = &mut case.ty {
                        self.component_val_type(ty)?;
                    }
                }
            }
            ComponentDefinedType::List(List { element: t })
            | ComponentDefinedType::FixedLengthList(FixedLengthList {
                element: t,
                elements: _,
            }) => {
                self.component_val_type(t)?;
            }
            ComponentDefinedType::Map(Map { key: k, value: v }) => {
                self.component_val_type(k)?;
                self.component_val_type(v)?;
            }
            ComponentDefinedType::Tuple(t) => {
                for field in t.fields.iter_mut() {
                    self.component_val_type(field)?;
                }
            }
            ComponentDefinedType::Option(o) => {
                self.component_val_type(&mut o.element)?;
            }
            ComponentDefinedType::Result(r) => {
                if let Some(ty) = &mut r.ok {
                    self.component_val_type(ty)?;
                }

                if let Some(ty) = &mut r.err {
                    self.component_val_type(ty)?;
                }
            }
            ComponentDefinedType::Own(t) | ComponentDefinedType::Borrow(t) => {
                self.resolve_ns(t, Ns::Type)?;
            }
            ComponentDefinedType::Stream(s) => {
                if let Some(ty) = &mut s.element {
                    self.component_val_type(ty)?;
                }
            }
            ComponentDefinedType::Future(f) => {
                if let Some(ty) = &mut f.element {
                    self.component_val_type(ty)?;
                }
            }
        }
        Ok(())
    }

    fn component_val_type(&mut self, ty: &mut ComponentValType<'a>) -> Result<(), Error> {
        match ty {
            ComponentValType::Ref(idx) => {
                self.resolve_ns(idx, Ns::Type)?;
                Ok(())
            }
            ComponentValType::Inline(ComponentDefinedType::Primitive(_)) => Ok(()),
            ComponentValType::Inline(_) => unreachable!("should be expanded by now"),
        }
    }

    fn core_ty(&mut self, field: &mut CoreType<'a>) -> Result<(), Error> {
        match &mut field.def {
            CoreTypeDef::Def(ty) => {
                if let Phase::Resolve = self.phase {
                    self.current().resolve_type_def(ty)?;
                }
            }
            CoreTypeDef::Module(t) => {
                self.module_type(field.id, t)?;
            }
        }
        Ok(())
    }

    fn core_rec(&mut self, rec: &mut core::Rec<'a>) -> Result<(), Error> {
        if let Phase::Resolve = self.phase {
            for ty in rec.types.iter_mut() {
                self.current().resolve_type(ty)?;
            }
        }
        Ok(())
    }

    fn ty(&mut self, field: &mut Type<'a>) -> Result<(), Error> {
        match &mut field.def {
            TypeDef::Defined(t) => {
                self.defined_type(t)?;
            }
            TypeDef::Func(f) => {
                for param in f.params.iter_mut() {
                    self.component_val_type(&mut param.ty)?;
                }

                if let Some(result) = &mut f.result {
                    self.component_val_type(result)?;
                }
            }
            TypeDef::Component(c) => {
                self.component_type(field.id, c)?;
            }
            TypeDef::Instance(i) => {
                self.instance_type(field.id, i)?;
            }
            TypeDef::Resource(r) => {
                self.ref_type(&mut r.rep)?;
                if let Some(dtor) = &mut r.dtor {
                    self.core_item_ref(dtor)?;
                }
            }
        }
        Ok(())
    }

    fn component_type(
        &mut self,
        id: Option<Id<'a>>,
        c: &mut ComponentType<'a>,
    ) -> Result<(), Error> {
        self.resolve_group(
            id,
            &mut c.decls,
            |resolver, decl| match decl {
                ComponentTypeDecl::Alias(alias) => resolver.alias(alias),
                ComponentTypeDecl::CoreType(ty) => resolver.core_ty(ty),
                ComponentTypeDecl::Type(ty) => resolver.ty(ty),
                ComponentTypeDecl::Import(import) => resolver.item_sig(&mut import.item),
                ComponentTypeDecl::Export(export) => resolver.item_sig(&mut export.item),
            },
            |state, decl| {
                match decl {
                    ComponentTypeDecl::Alias(alias) => {
                        state.register_alias(alias)?;
                    }
                    ComponentTypeDecl::CoreType(ty) => {
                        state.core_types.register(ty.id, "core type")?;
                    }
                    ComponentTypeDecl::Type(ty) => {
                        state.types.register(ty.id, "type")?;
                    }
                    ComponentTypeDecl::Export(e) => {
                        state.register_item_sig(&e.item)?;
                    }
                    ComponentTypeDecl::Import(i) => {
                        state.register_item_sig(&i.item)?;
                    }
                }
                Ok(())
            },
        )
    }

    fn instance_type(&mut self, id: Option<Id<'a>>, c: &mut InstanceType<'a>) -> Result<(), Error> {
        self.resolve_group(
            id,
            &mut c.decls,
            |resolver, decl| match decl {
                InstanceTypeDecl::Alias(alias) => resolver.alias(alias),
                InstanceTypeDecl::CoreType(ty) => resolver.core_ty(ty),
                InstanceTypeDecl::Type(ty) => resolver.ty(ty),
                InstanceTypeDecl::Export(export) => resolver.item_sig(&mut export.item),
            },
            |state, decl| {
                match decl {
                    InstanceTypeDecl::Alias(alias) => {
                        state.register_alias(alias)?;
                    }
                    InstanceTypeDecl::CoreType(ty) => {
                        state.core_types.register(ty.id, "core type")?;
                    }
                    InstanceTypeDecl::Type(ty) => {
                        state.types.register(ty.id, "type")?;
                    }
                    InstanceTypeDecl::Export(export) => {
                        state.register_item_sig(&export.item)?;
                    }
                }
                Ok(())
            },
        )
    }

    fn core_item_ref<K>(&mut self, item: &mut CoreItemRef<'a, K>) -> Result<(), Error>
    where
        K: CoreItem + Copy,
    {
        match self.phase {
            Phase::ExpandAliases => {
                // If this has an export name then an alias will be injected,
                // but otherwise delegate to `resolve_ns` logic.
                let name = match item.export_name {
                    Some(name) => name,
                    None => return self.resolve_ns(&mut item.idx, item.kind.ns()),
                };
                let span = item.idx.span();
                let kind = match item.kind.ns() {
                    ns @ (Ns::CoreFunc
                    | Ns::CoreTable
                    | Ns::CoreGlobal
                    | Ns::CoreMemory
                    | Ns::CoreTag) => ns.into(),
                    Ns::CoreType
                    | Ns::CoreInstance
                    | Ns::CoreModule
                    | Ns::Func
                    | Ns::Type
                    | Ns::Instance
                    | Ns::Component
                    | Ns::Value => {
                        return Err(Error::new(
                            span,
                            "core instances cannot export this kind of item, \
                             so an export name cannot be used to resolve this \
                             reference"
                                .to_string(),
                        ));
                    }
                };

                // Record an alias to reference the export
                let id = gensym::generate(span);
                let alias = Alias {
                    span,
                    id: Some(id),
                    name: None,
                    target: AliasTarget::CoreExport {
                        instance: item.idx,
                        name,
                        kind,
                    },
                };

                self.aliases_to_insert.push(alias);

                item.idx = Index::Id(id);
                item.export_name = None;
            }

            // During this phase any optional aliases should have been procesed,
            // so proceed directly to normal resolution.
            Phase::Resolve => {
                assert!(item.export_name.is_none());
                self.resolve_ns(&mut item.idx, item.kind.ns())?;
            }
        }
        Ok(())
    }

    fn component_item_ref<K>(&mut self, item: &mut ItemRef<'a, K>) -> Result<(), Error>
    where
        K: ComponentItem + Copy,
    {
        match self.phase {
            Phase::ExpandAliases => {
                let span = item.idx.span();
                let names = item.export_names.len();
                for (pos, export_name) in item.export_names.drain(..).enumerate() {
                    // Record an alias to reference the export
                    let id = gensym::generate(span);
                    let alias = Alias {
                        span,
                        id: Some(id),
                        name: None,
                        target: AliasTarget::Export {
                            instance: item.idx,
                            name: export_name,
                            kind: if pos == names - 1 {
                                item.kind.ns().into()
                            } else {
                                ComponentExportAliasKind::Instance
                            },
                        },
                    };

                    item.idx = Index::Id(id);
                    self.aliases_to_insert.push(alias);
                }
            }
            Phase::Resolve => {
                assert!(item.export_names.is_empty());
            }
        }

        self.resolve_ns(&mut item.idx, item.kind.ns())?;
        Ok(())
    }

    fn resolve_ns(&mut self, idx: &mut Index<'a>, ns: Ns) -> Result<(), Error> {
        match self.phase {
            Phase::ExpandAliases => {
                let mut idx_clone = *idx;
                for (depth, resolver) in self.stack.iter_mut().rev().enumerate() {
                    let depth = depth as u32;
                    // Try to find `idx_clone` within `resolver`, but failing
                    // that move on to the next parent.
                    //
                    // Note that this implicitly relies on all resolution
                    // registering all names first, which is part of the
                    // processing in `resolve_group`.
                    let (r, _) = resolver.resolver(ns);
                    let found = match r.try_resolve(&mut idx_clone) {
                        Some(idx) => idx,
                        None => continue,
                    };

                    // If this is the current component then no extra alias is
                    // necessary, so return success.
                    if depth == 0 {
                        return Ok(());
                    }
                    let id = match idx {
                        Index::Id(id) => *id,
                        Index::Num(..) => unreachable!(),
                    };

                    // When resolution succeeds in a parent then an outer alias
                    // is automatically inserted here in this component.
                    let span = idx.span();
                    let alias = Alias {
                        span,
                        id: Some(id),
                        name: None,
                        target: AliasTarget::Outer {
                            outer: Index::Num(depth, span),
                            index: Index::Num(found, span),
                            kind: match ns {
                                Ns::CoreModule => ComponentOuterAliasKind::CoreModule,
                                Ns::CoreType => ComponentOuterAliasKind::CoreType,
                                Ns::Type => ComponentOuterAliasKind::Type,
                                Ns::Component => ComponentOuterAliasKind::Component,
                                _ => {
                                    return Err(Error::new(
                                        span,
                                        format!(
                                            "outer item `{}` is not a module, type, or component",
                                            id.name(),
                                        ),
                                    ));
                                }
                            },
                        },
                    };
                    // Register `alias` with the well-known-name `id` so future
                    // outer aliases to the same type reuse this injected alias.
                    self.current().register_alias(&alias)?;

                    self.aliases_to_insert.push(alias);
                    break;
                }
            }

            // During the resovle phase of processing it's required that `idx`
            // lives within the current namespace, so just resolve it there.
            Phase::Resolve => {
                self.current().resolve(ns, idx)?;
            }
        }
        Ok(())
    }

    fn module_type(&mut self, id: Option<Id<'a>>, ty: &mut ModuleType<'a>) -> Result<(), Error> {
        return self.resolve_group(
            id,
            &mut ty.decls,
            |resolver, decl| match decl {
                ModuleTypeDecl::Alias(alias) => resolver.alias(alias),
                ModuleTypeDecl::Type(t) => {
                    if let Phase::Resolve = resolver.phase {
                        resolver.current().resolve_type(t)?;
                    }
                    Ok(())
                }
                ModuleTypeDecl::Rec(t) => {
                    if let Phase::Resolve = resolver.phase {
                        for t in t.types.iter_mut() {
                            resolver.current().resolve_type(t)?;
                        }
                    }
                    Ok(())
                }

                ModuleTypeDecl::Import(imports) => {
                    if let Phase::Resolve = resolver.phase {
                        for sig in imports.unique_sigs_mut() {
                            resolve_item_sig(resolver, sig)?;
                        }
                    }
                    Ok(())
                }
                ModuleTypeDecl::Export(_, item) => {
                    if let Phase::Resolve = resolver.phase {
                        resolve_item_sig(resolver, item)?;
                    }
                    Ok(())
                }
            },
            |state, decl| {
                match decl {
                    ModuleTypeDecl::Alias(alias) => {
                        state.register_alias(alias)?;
                    }
                    ModuleTypeDecl::Type(t) => {
                        state.core_types.register(t.id, "type")?;
                    }
                    ModuleTypeDecl::Rec(t) => {
                        for t in t.types.iter() {
                            state.core_types.register(t.id, "type")?;
                        }
                    }
                    // Only the type namespace is populated within the module type
                    // namespace so these are ignored here.
                    ModuleTypeDecl::Import(_) | ModuleTypeDecl::Export(..) => {}
                }
                Ok(())
            },
        );

        fn resolve_item_sig<'a>(
            resolver: &Resolver<'a>,
            sig: &mut core::ItemSig<'a>,
        ) -> Result<(), Error> {
            match &mut sig.kind {
                core::ItemKind::Func(ty)
                | core::ItemKind::FuncExact(ty)
                | core::ItemKind::Tag(core::TagType::Exception(ty)) => {
                    let idx = ty.index.as_mut().expect("index should be filled in");
                    resolver
                        .stack
                        .last()
                        .unwrap()
                        .core_types
                        .resolve(idx, "type")?;
                }
                core::ItemKind::Memory(_)
                | core::ItemKind::Global(_)
                | core::ItemKind::Table(_) => {}
            }
            Ok(())
        }
    }
}

impl<'a> ComponentState<'a> {
    fn resolve(&self, ns: Ns, idx: &mut Index<'a>) -> Result<u32, Error> {
        let (ns, desc) = self.resolver(ns);
        ns.resolve(idx, desc)
    }

    fn resolver(&self, ns: Ns) -> (&Namespace<'a>, &'static str) {
        match ns {
            Ns::CoreFunc => (&self.core_funcs, "core func"),
            Ns::CoreGlobal => (&self.core_globals, "core global"),
            Ns::CoreTable => (&self.core_tables, "core table"),
            Ns::CoreMemory => (&self.core_memories, "core memory"),
            Ns::CoreType => (&self.core_types, "core type"),
            Ns::CoreTag => (&self.core_tags, "core tag"),
            Ns::CoreInstance => (&self.core_instances, "core instance"),
            Ns::CoreModule => (&self.core_modules, "core module"),
            Ns::Func => (&self.funcs, "func"),
            Ns::Type => (&self.types, "type"),
            Ns::Instance => (&self.instances, "instance"),
            Ns::Component => (&self.components, "component"),
            Ns::Value => (&self.values, "value"),
        }
    }

    /// Assign an index to the given field.
    fn register(&mut self, item: &ComponentField<'a>) -> Result<(), Error> {
        match item {
            ComponentField::CoreModule(m) => self.core_modules.register(m.id, "core module")?,
            ComponentField::CoreInstance(i) => {
                self.core_instances.register(i.id, "core instance")?
            }
            ComponentField::CoreType(ty) => match &ty.def {
                CoreTypeDef::Def(_) => self.core_types.register(ty.id, "core type")?,
                CoreTypeDef::Module(_) => self.core_types.register(ty.id, "core type")?,
            },
            ComponentField::CoreRec(ty) => {
                for ty in ty.types.iter() {
                    self.core_types.register(ty.id, "core type")?;
                }
                0
            }
            ComponentField::Component(c) => self.components.register(c.id, "component")?,
            ComponentField::Instance(i) => self.instances.register(i.id, "instance")?,
            ComponentField::Alias(a) => self.register_alias(a)?,
            ComponentField::Type(t) => self.types.register(t.id, "type")?,
            ComponentField::CanonicalFunc(f) => match &f.kind {
                CanonicalFuncKind::Lift { .. } => self.funcs.register(f.id, "func")?,
                CanonicalFuncKind::Core(_) => self.core_funcs.register(f.id, "core func")?,
            },
            ComponentField::CoreFunc(_) | ComponentField::Func(_) => {
                unreachable!("should be expanded already")
            }
            ComponentField::Start(s) => {
                for r in &s.results {
                    self.values.register(*r, "value")?;
                }
                return Ok(());
            }
            ComponentField::Import(i) => self.register_item_sig(&i.item)?,
            ComponentField::Export(e) => match &e.kind {
                ComponentExportKind::CoreModule(_) => {
                    self.core_modules.register(e.id, "core module")?
                }
                ComponentExportKind::Func(_) => self.funcs.register(e.id, "func")?,
                ComponentExportKind::Instance(_) => self.instances.register(e.id, "instance")?,
                ComponentExportKind::Value(_) => self.values.register(e.id, "value")?,
                ComponentExportKind::Component(_) => self.components.register(e.id, "component")?,
                ComponentExportKind::Type(_) => self.types.register(e.id, "type")?,
            },
            ComponentField::Custom(_) | ComponentField::Producers(_) => return Ok(()),
        };

        Ok(())
    }

    fn register_alias(&mut self, alias: &Alias<'a>) -> Result<u32, Error> {
        match alias.target {
            AliasTarget::Export { kind, .. } => match kind {
                ComponentExportAliasKind::CoreModule => {
                    self.core_modules.register(alias.id, "core module")
                }
                ComponentExportAliasKind::Func => self.funcs.register(alias.id, "func"),
                ComponentExportAliasKind::Value => self.values.register(alias.id, "value"),
                ComponentExportAliasKind::Type => self.types.register(alias.id, "type"),
                ComponentExportAliasKind::Component => {
                    self.components.register(alias.id, "component")
                }
                ComponentExportAliasKind::Instance => self.instances.register(alias.id, "instance"),
            },
            AliasTarget::CoreExport { kind, .. } => match kind {
                core::ExportKind::Func => self.core_funcs.register(alias.id, "core func"),
                core::ExportKind::Table => self.core_tables.register(alias.id, "core table"),
                core::ExportKind::Memory => self.core_memories.register(alias.id, "core memory"),
                core::ExportKind::Global => self.core_globals.register(alias.id, "core global"),
                core::ExportKind::Tag => self.core_tags.register(alias.id, "core tag"),
            },
            AliasTarget::Outer { kind, .. } => match kind {
                ComponentOuterAliasKind::CoreModule => {
                    self.core_modules.register(alias.id, "core module")
                }
                ComponentOuterAliasKind::CoreType => {
                    self.core_types.register(alias.id, "core type")
                }
                ComponentOuterAliasKind::Type => self.types.register(alias.id, "type"),
                ComponentOuterAliasKind::Component => {
                    self.components.register(alias.id, "component")
                }
            },
        }
    }
}

impl<'a> ResolveCoreType<'a> for ComponentState<'a> {
    fn resolve_type_name(&mut self, name: &mut Index<'a>) -> Result<u32, Error> {
        self.resolve(Ns::CoreType, name)
    }
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
enum Ns {
    CoreFunc,
    CoreGlobal,
    CoreTable,
    CoreMemory,
    CoreType,
    CoreTag,
    CoreInstance,
    CoreModule,
    Func,
    Type,
    Instance,
    Component,
    Value,
}

trait ComponentItem {
    fn ns(&self) -> Ns;
}

trait CoreItem {
    fn ns(&self) -> Ns;
}

macro_rules! component_item {
    ($kw:path, $kind:ident) => {
        impl ComponentItem for $kw {
            fn ns(&self) -> Ns {
                Ns::$kind
            }
        }
    };
}

macro_rules! core_item {
    ($kw:path, $kind:ident) => {
        impl CoreItem for $kw {
            fn ns(&self) -> Ns {
                Ns::$kind
            }
        }
    };
}

component_item!(kw::func, Func);
component_item!(kw::r#type, Type);
component_item!(kw::r#instance, Instance);
component_item!(kw::component, Component);
component_item!(kw::value, Value);
component_item!(kw::module, CoreModule);

core_item!(kw::func, CoreFunc);
core_item!(kw::memory, CoreMemory);
core_item!(kw::table, CoreTable);
core_item!(kw::r#type, CoreType);
core_item!(kw::r#instance, CoreInstance);

impl From<Ns> for ComponentExportAliasKind {
    fn from(ns: Ns) -> Self {
        match ns {
            Ns::CoreModule => Self::CoreModule,
            Ns::Func => Self::Func,
            Ns::Type => Self::Type,
            Ns::Instance => Self::Instance,
            Ns::Component => Self::Component,
            Ns::Value => Self::Value,
            _ => unreachable!("not a component exportable namespace"),
        }
    }
}

impl From<Ns> for core::ExportKind {
    fn from(ns: Ns) -> Self {
        match ns {
            Ns::CoreFunc => Self::Func,
            Ns::CoreTable => Self::Table,
            Ns::CoreGlobal => Self::Global,
            Ns::CoreMemory => Self::Memory,
            Ns::CoreTag => Self::Tag,
            _ => unreachable!("not a core exportable namespace"),
        }
    }
}

impl From<ComponentOuterAliasKind> for Ns {
    fn from(kind: ComponentOuterAliasKind) -> Self {
        match kind {
            ComponentOuterAliasKind::CoreModule => Self::CoreModule,
            ComponentOuterAliasKind::CoreType => Self::CoreType,
            ComponentOuterAliasKind::Type => Self::Type,
            ComponentOuterAliasKind::Component => Self::Component,
        }
    }
}

impl CoreItem for core::ExportKind {
    fn ns(&self) -> Ns {
        match self {
            Self::Func => Ns::CoreFunc,
            Self::Table => Ns::CoreTable,
            Self::Global => Ns::CoreGlobal,
            Self::Memory => Ns::CoreMemory,
            Self::Tag => Ns::CoreTag,
        }
    }
}
