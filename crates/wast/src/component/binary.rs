use crate::component::*;
use crate::core;
use crate::encode::Encode;
use crate::token::{Id, NameAnnotation};

pub fn encode(component: &Component<'_>) -> Vec<u8> {
    match &component.kind {
        ComponentKind::Text(fields) => encode_fields(&component.id, &component.name, fields),
        ComponentKind::Binary(bytes) => bytes.iter().flat_map(|b| b.iter().cloned()).collect(),
    }
}

fn encode_fields(
    component_id: &Option<Id<'_>>,
    component_name: &Option<NameAnnotation<'_>>,
    fields: &[ComponentField<'_>],
) -> Vec<u8> {
    let mut e = Encoder {
        wasm: Vec::new(),
        tmp: Vec::new(),
        last_section: None,
        last_section_contents: Vec::new(),
        last_section_count: 0,
    };
    e.wasm.extend(b"\0asm");
    e.wasm.extend(b"\x0a\0\x01\0");

    for field in fields {
        match field {
            ComponentField::Type(i) => e.append(1, i),
            ComponentField::Import(i) => e.append(2, i),
            ComponentField::Func(i) => e.append(3, i),
            ComponentField::Module(i) => e.section(4, i),
            ComponentField::Component(i) => e.section(5, i),
            ComponentField::Instance(i) => e.append(6, i),
            ComponentField::Export(i) => e.append(7, i),
            ComponentField::Start(i) => e.section(8, i),
            ComponentField::Alias(i) => e.append(9, i),
        }
    }

    let names = find_names(component_id, component_name, fields);
    if !names.is_empty() {
        e.section(0, &("name", names));
    }

    e.flush();

    return e.wasm;
}

struct Encoder {
    wasm: Vec<u8>,
    tmp: Vec<u8>,

    last_section: Option<u8>,
    last_section_contents: Vec<u8>,
    last_section_count: usize,
}

impl Encoder {
    /// Appends an entire section represented by the `section` provided
    fn section(&mut self, id: u8, section: &dyn Encode) {
        self.flush();
        self.tmp.truncate(0);
        section.encode(&mut self.tmp);
        self.wasm.push(id);
        self.tmp.encode(&mut self.wasm);
    }

    /// Appends an `item` specified within the section `id` specified.
    fn append(&mut self, id: u8, item: &dyn Encode) {
        // Flush if necessary to start a new section
        if self.last_section != Some(id) {
            self.flush();
        }
        // ... and afterwards start building up this section incrementally
        // in case the next item encoded is also part of this section.
        item.encode(&mut self.last_section_contents);
        self.last_section_count += 1;
        self.last_section = Some(id);
    }

    fn flush(&mut self) {
        let id = match self.last_section.take() {
            Some(id) => id,
            None => return,
        };
        self.wasm.push(id);
        self.tmp.truncate(0);
        self.last_section_count.encode(&mut self.tmp);
        self.last_section_count = 0;
        self.tmp.extend(self.last_section_contents.drain(..));
        self.tmp.encode(&mut self.wasm);
    }
}

impl Encode for NestedComponent<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        let fields = match &self.kind {
            NestedComponentKind::Import { .. } => panic!("imports should be gone by now"),
            NestedComponentKind::Inline(fields) => fields,
        };
        e.extend(encode_fields(&self.id, &self.name, fields));
    }
}

impl Encode for Module<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match &self.kind {
            ModuleKind::Import { .. } => panic!("imports should be gone by now"),
            ModuleKind::Inline { fields } => {
                e.extend(crate::core::binary::encode(&self.id, &self.name, fields));
            }
        }
    }
}

impl Encode for Instance<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match &self.kind {
            InstanceKind::Module { module, args } => {
                e.push(0x00);
                e.push(0x00);
                module.idx.encode(e);
                args.encode(e);
            }
            InstanceKind::Component { component, args } => {
                e.push(0x00);
                e.push(0x01);
                component.idx.encode(e);
                args.encode(e);
            }
            InstanceKind::BundleOfComponentExports { args } => {
                e.push(0x01);
                args.encode(e);
            }
            InstanceKind::BundleOfExports { args } => {
                e.push(0x02);
                args.encode(e);
            }
        }
    }
}

impl Encode for NamedModuleArg<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.name.encode(e);
        e.push(0x02);
        self.arg.encode(e);
    }
}

impl Encode for ModuleArg<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            ModuleArg::Def(def) => {
                def.idx.encode(e);
            }
            ModuleArg::BundleOfExports(_) => {
                todo!("ModuleArg::BundleOfExports should be desugared")
            }
        }
    }
}

impl Encode for NamedComponentArg<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.name.encode(e);
        self.arg.encode(e);
    }
}

impl Encode for ComponentArg<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            ComponentArg::Def(def) => {
                match def.kind {
                    DefTypeKind::Module => e.push(0x00),
                    DefTypeKind::Component => e.push(0x01),
                    DefTypeKind::Instance => e.push(0x02),
                    DefTypeKind::Func => e.push(0x03),
                    DefTypeKind::Value => e.push(0x04),
                }
                def.idx.encode(e);
            }
            ComponentArg::Type(ty) => {
                e.push(0x05);
                ty.idx.encode(e);
            }
            ComponentArg::BundleOfExports(_) => {
                todo!("ComponentArg::BundleOfExports should be desugared")
            }
        }
    }
}

impl Encode for Start<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.func.encode(e);
        self.args.encode(e);
    }
}

impl Encode for Alias<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self.target {
            AliasTarget::Export { instance, export } => {
                match self.kind {
                    AliasKind::Module => {
                        e.push(0x00);
                        e.push(0x00);
                    }
                    AliasKind::Component => {
                        e.push(0x00);
                        e.push(0x01);
                    }
                    AliasKind::Instance => {
                        e.push(0x00);
                        e.push(0x02);
                    }
                    AliasKind::Value => {
                        e.push(0x00);
                        e.push(0x04);
                    }
                    AliasKind::ExportKind(export_kind) => {
                        e.push(0x01);
                        export_kind.encode(e);
                    }
                }
                instance.encode(e);
                export.encode(e);
            }
            AliasTarget::Outer { outer, index } => {
                e.push(0x02);
                match self.kind {
                    AliasKind::Module => e.push(0x00),
                    AliasKind::Component => e.push(0x01),
                    AliasKind::ExportKind(core::ExportKind::Type) => e.push(0x05),
                    _ => todo!("Unexpected outer alias kind"),
                }
                outer.encode(e);
                index.encode(e);
            }
        }
    }
}

impl Encode for CanonLower<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x01);
        self.opts.encode(e);
        self.func.encode(e);
    }
}

impl Encode for CanonLift<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x00);
        self.type_.encode(e);
        self.opts.encode(e);
        self.func.encode(e);
    }
}

impl Encode for CanonOpt<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            CanonOpt::StringUtf8 => e.push(0x00),
            CanonOpt::StringUtf16 => e.push(0x01),
            CanonOpt::StringLatin1Utf16 => e.push(0x02),
            CanonOpt::Into(index) => {
                e.push(0x03);
                index.encode(e);
            }
        }
    }
}

impl Encode for ModuleType<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x4f);
        self.defs.encode(e);
    }
}

impl Encode for ModuleTypeDef<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            ModuleTypeDef::Type(f) => {
                e.push(0x01);
                f.encode(e);
            }
            ModuleTypeDef::Import(i) => {
                e.push(0x02);
                i.encode(e);
            }
            ModuleTypeDef::Export(name, x) => {
                e.push(0x07);
                name.encode(e);
                x.encode(e);
            }
        }
    }
}

impl Encode for ComponentType<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x4e);
        self.fields.encode(e);
    }
}

impl Encode for ComponentTypeField<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            ComponentTypeField::Type(ty_) => {
                e.push(0x01);
                ty_.encode(e);
            }
            ComponentTypeField::Alias(alias) => {
                e.push(0x09);
                alias.encode(e);
            }
            ComponentTypeField::Export(export) => {
                e.push(0x07);
                export.encode(e);
            }
            ComponentTypeField::Import(import) => {
                e.push(0x02);
                import.encode(e);
            }
        }
    }
}

impl Encode for InstanceType<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x4d);
        self.fields.encode(e);
    }
}

impl Encode for InstanceTypeField<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            InstanceTypeField::Type(ty_) => {
                e.push(0x01);
                ty_.encode(e);
            }
            InstanceTypeField::Alias(alias) => {
                e.push(0x09);
                alias.encode(e);
            }
            InstanceTypeField::Export(export) => {
                e.push(0x07);
                export.encode(e);
            }
        }
    }
}

impl Encode for ComponentExportType<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.name.encode(e);
        self.item.encode(e);
    }
}
impl Encode for TypeField<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match &self.def {
            ComponentTypeDef::DefType(d) => d.encode(e),
            ComponentTypeDef::InterType(i) => i.encode(e),
        }
    }
}

impl<'a> Encode for InterType<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            InterType::Unit => e.push(0x7f),
            InterType::Bool => e.push(0x7e),
            InterType::S8 => e.push(0x7d),
            InterType::U8 => e.push(0x7c),
            InterType::S16 => e.push(0x7b),
            InterType::U16 => e.push(0x7a),
            InterType::S32 => e.push(0x79),
            InterType::U32 => e.push(0x78),
            InterType::S64 => e.push(0x77),
            InterType::U64 => e.push(0x76),
            InterType::Float32 => e.push(0x75),
            InterType::Float64 => e.push(0x74),
            InterType::Char => e.push(0x73),
            InterType::String => e.push(0x72),
            InterType::Record(r) => r.encode(e),
            InterType::Variant(v) => v.encode(e),
            InterType::List(l) => l.encode(e),
            InterType::Tuple(t) => t.encode(e),
            InterType::Flags(f) => f.encode(e),
            InterType::Enum(n) => n.encode(e),
            InterType::Union(u) => u.encode(e),
            InterType::Option(o) => o.encode(e),
            InterType::Expected(x) => x.encode(e),
        }
    }
}

impl<'a> Encode for DefType<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            DefType::Func(f) => f.encode(e),
            DefType::Module(m) => m.encode(e),
            DefType::Component(c) => c.encode(e),
            DefType::Instance(i) => i.encode(e),
            DefType::Value(v) => v.encode(e),
        }
    }
}

impl<'a> Encode for Record<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x71);
        self.fields.encode(e);
    }
}

impl<'a> Encode for Field<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.name.encode(e);
        self.type_.encode(e);
    }
}

impl<'a> Encode for Variant<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x70);
        self.cases.encode(e);
    }
}

impl<'a> Encode for Case<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.name.encode(e);
        self.type_.encode(e);
        if let Some(defaults_to) = self.defaults_to {
            e.push(0x01);
            defaults_to.encode(e);
        } else {
            e.push(0x00);
        }
    }
}

impl<'a> Encode for List<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x6f);
        self.element.encode(e);
    }
}

impl<'a> Encode for Tuple<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x6e);
        self.fields.encode(e);
    }
}

impl<'a> Encode for Flags<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x6d);
        self.flag_names.encode(e);
    }
}

impl<'a> Encode for Enum<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x6c);
        self.arms.encode(e);
    }
}

impl<'a> Encode for Union<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x6b);
        self.arms.encode(e);
    }
}

impl<'a> Encode for OptionType<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x6a);
        self.element.encode(e);
    }
}

impl<'a> Encode for Expected<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x69);
        self.ok.encode(e);
        self.err.encode(e);
    }
}

impl<'a> Encode for ComponentFunctionType<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x4c);
        self.params.encode(e);
        self.result.encode(e);
    }
}

impl<'a> Encode for ComponentFunctionParam<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        if let Some(id) = self.id {
            e.push(0x01);
            id.encode(e);
        } else {
            e.push(0x00);
        }
        self.type_.encode(e);
    }
}

impl<'a> Encode for ValueType<'a> {
    fn encode(&self, e: &mut Vec<u8>) {
        e.push(0x4b);
        self.value_type.encode(e)
    }
}
impl<T: Encode> Encode for ComponentTypeUse<'_, T> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            ComponentTypeUse::Inline(inline) => inline.encode(e),
            ComponentTypeUse::Ref(index) => index.encode(e),
        }
    }
}
impl Encode for ComponentExport<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.name.encode(e);
        match &self.arg {
            ComponentArg::Def(item_ref) => {
                match item_ref.kind {
                    DefTypeKind::Module => e.push(0x00),
                    DefTypeKind::Component => e.push(0x01),
                    DefTypeKind::Instance => e.push(0x02),
                    DefTypeKind::Func => e.push(0x03),
                    DefTypeKind::Value => e.push(0x04),
                }
                item_ref.idx.encode(e);
            }
            ComponentArg::Type(_item_ref) => todo!("Encode for ComponentArg::Type"),
            ComponentArg::BundleOfExports(_exports) => {
                todo!("Encode for ComponentArg::BundleOfExports")
            }
        }
    }
}

impl Encode for ComponentFunc<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match &self.kind {
            ComponentFuncKind::Import { .. } => todo!("Imported component function"),
            ComponentFuncKind::Inline { body } => {
                body.encode(e);
            }
        }
    }
}

impl Encode for ComponentFuncBody<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match self {
            ComponentFuncBody::CanonLift(lift) => lift.encode(e),
            ComponentFuncBody::CanonLower(lower) => lower.encode(e),
        }
    }
}

#[derive(Default)]
struct ComponentNames<'a> {
    component: Option<&'a str>,
    funcs: Vec<(u32, &'a str)>,
    func_idx: u32,
    modules: Vec<(u32, &'a str)>,
    module_idx: u32,
    components: Vec<(u32, &'a str)>,
    component_idx: u32,
    values: Vec<(u32, &'a str)>,
    value_idx: u32,
    instances: Vec<(u32, &'a str)>,
    instance_idx: u32,
    types: Vec<(u32, &'a str)>,
    type_idx: u32,
    tables: Vec<(u32, &'a str)>,
    table_idx: u32,
    globals: Vec<(u32, &'a str)>,
    global_idx: u32,
    memories: Vec<(u32, &'a str)>,
    memory_idx: u32,
    tags: Vec<(u32, &'a str)>,
    tag_idx: u32,
}

// TODO: should ideally deduplicate this with similar code in `core/binary.rs`
fn find_names<'a>(
    component_id: &Option<Id<'a>>,
    component_name: &Option<NameAnnotation<'a>>,
    fields: &[ComponentField<'a>],
) -> ComponentNames<'a> {
    fn get_name<'a>(id: &Option<Id<'a>>, name: &Option<NameAnnotation<'a>>) -> Option<&'a str> {
        name.as_ref().map(|n| n.name).or(id.and_then(|id| {
            if id.is_gensym() {
                None
            } else {
                Some(id.name())
            }
        }))
    }

    enum Name {
        Type,
        Func,
        Module,
        Component,
        Instance,
        Value,
        Table,
        Global,
        Memory,
        Tag,
    }

    let mut ret = ComponentNames::default();
    ret.component = get_name(component_id, component_name);
    for field in fields {
        // Extract the kind/id/name from whatever kind of field this is...
        let (kind, id, name) = match field {
            ComponentField::Import(_i) => {
                // TODO: Extract the kind/id/name for the name section from ComponentField::Import
                continue;
            }
            ComponentField::Module(m) => (Name::Module, &m.id, &m.name),
            ComponentField::Component(c) => (Name::Component, &c.id, &c.name),
            ComponentField::Instance(i) => (Name::Instance, &i.id, &i.name),
            ComponentField::Type(t) => (Name::Type, &t.id, &t.name),
            ComponentField::Func(f) => (Name::Func, &f.id, &f.name),
            ComponentField::Alias(a) => match a.target {
                AliasTarget::Export { .. } => {
                    // TODO: Extract the kind/id/name for the name section from export aliases
                    continue;
                }
                AliasTarget::Outer { .. } => match a.kind {
                    AliasKind::Module => (Name::Module, &a.id, &a.name),
                    AliasKind::Component => (Name::Component, &a.id, &a.name),
                    AliasKind::Instance => (Name::Instance, &a.id, &a.name),
                    AliasKind::Value => (Name::Value, &a.id, &a.name),
                    AliasKind::ExportKind(core::ExportKind::Func) => (Name::Func, &a.id, &a.name),
                    AliasKind::ExportKind(core::ExportKind::Table) => (Name::Table, &a.id, &a.name),
                    AliasKind::ExportKind(core::ExportKind::Global) => {
                        (Name::Global, &a.id, &a.name)
                    }
                    AliasKind::ExportKind(core::ExportKind::Memory) => {
                        (Name::Memory, &a.id, &a.name)
                    }
                    AliasKind::ExportKind(core::ExportKind::Tag) => (Name::Tag, &a.id, &a.name),
                    AliasKind::ExportKind(core::ExportKind::Type) => (Name::Type, &a.id, &a.name),
                },
            },
            ComponentField::Export(_) | ComponentField::Start(_) => continue,
        };

        // .. and using the kind we can figure out where to place this name
        let (list, idx) = match kind {
            Name::Func => (&mut ret.funcs, &mut ret.func_idx),
            Name::Module => (&mut ret.modules, &mut ret.module_idx),
            Name::Component => (&mut ret.components, &mut ret.component_idx),
            Name::Instance => (&mut ret.instances, &mut ret.instance_idx),
            Name::Type => (&mut ret.types, &mut ret.type_idx),
            Name::Value => (&mut ret.values, &mut ret.value_idx),
            Name::Table => (&mut ret.tables, &mut ret.table_idx),
            Name::Global => (&mut ret.globals, &mut ret.global_idx),
            Name::Memory => (&mut ret.memories, &mut ret.memory_idx),
            Name::Tag => (&mut ret.tags, &mut ret.tag_idx),
        };
        if let Some(name) = get_name(id, name) {
            list.push((*idx, name));
        }

        *idx += 1;
    }

    return ret;
}

impl Encode for ComponentImport<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        self.name.encode(e);
        self.item.encode(e);
    }
}

impl Encode for ItemSig<'_> {
    fn encode(&self, e: &mut Vec<u8>) {
        match &self.kind {
            ItemKind::Component(t) => t.encode(e),
            ItemKind::Module(t) => t.encode(e),
            ItemKind::Func(t) => t.encode(e),
            ItemKind::Instance(t) => t.encode(e),
            ItemKind::Value(t) => t.encode(e),
        }
    }
}

impl ComponentNames<'_> {
    fn is_empty(&self) -> bool {
        // TODO: when an encoding is implemented this should be something that's
        // not always `true`
        true
    }
}

impl Encode for ComponentNames<'_> {
    fn encode(&self, _dst: &mut Vec<u8>) {
        // TODO: names section for components
    }
}
