use id_arena::Id;

use crate::{
    Enum, Flags, Interface, InterfaceItem, Package, PackageName, Params, Record, Resource,
    ResourceFunc, Result_, Results, StandaloneFunc, Tuple, Type, TypeDef, TypeDefKind, Variant,
    World, WorldItem,
};

pub fn packages_from_parsed(resolve: &wit_parser::Resolve) -> Vec<Package> {
    let converter = Converter::new(resolve);
    converter.convert()
}

struct Converter<'a> {
    resolve: &'a wit_parser::Resolve,
}

impl<'a> Converter<'a> {
    fn new(resolve: &'a wit_parser::Resolve) -> Self {
        Self { resolve }
    }

    fn convert(&self) -> Vec<Package> {
        self.resolve
            .packages
            .iter()
            .map(|(_, p)| self.convert_package(p))
            .collect()
    }

    fn convert_package(&self, package: &wit_parser::Package) -> Package {
        let mut output = Package::new(self.convert_package_name(&package.name));
        for (_, id) in &package.interfaces {
            let interface = self.resolve.interfaces.get(*id).unwrap();
            output.interface(self.convert_interface(
                interface,
                None,
                wit_parser::TypeOwner::Interface(*id),
            ));
        }
        for (_, id) in &package.worlds {
            let world = self.resolve.worlds.get(*id).unwrap();
            output.world(self.convert_world(world, wit_parser::TypeOwner::World(*id)));
        }
        output
    }

    fn convert_package_name(&self, package: &wit_parser::PackageName) -> PackageName {
        PackageName::new(
            package.namespace.clone(),
            package.name.clone(),
            package.version.clone(),
        )
    }

    fn convert_world(&self, world: &wit_parser::World, owner: wit_parser::TypeOwner) -> World {
        let mut output = World::new(world.name.clone());

        for (key, item) in &world.imports {
            match item {
                wit_parser::WorldItem::Interface { id, .. } => {
                    let interface = self.resolve.interfaces.get(*id).unwrap();
                    output.item(match &interface.name {
                        Some(name) => {
                            // standalone
                            WorldItem::named_interface_import(name.clone())
                        }
                        None => {
                            // inlined
                            let name = match key {
                                wit_parser::WorldKey::Name(name) => name.clone(),
                                wit_parser::WorldKey::Interface(_) => {
                                    unreachable!("inlined interface must have a kye name")
                                }
                            };
                            WorldItem::inline_interface_import(self.convert_interface(
                                interface,
                                Some(name),
                                owner,
                            ))
                        }
                    });
                }
                wit_parser::WorldItem::Function(func) => {
                    if let Some(func) = self.standalone_func_convert(func) {
                        output.item(WorldItem::function_import(func));
                    }
                }
                wit_parser::WorldItem::Type(_) => {
                    todo!();
                }
            }
        }
        for (key, item) in &world.exports {
            match item {
                wit_parser::WorldItem::Interface { id, .. } => {
                    let interface = self.resolve.interfaces.get(*id).unwrap();
                    output.item(match &interface.name {
                        Some(name) => {
                            // standalone
                            WorldItem::named_interface_export(name.clone())
                        }
                        None => {
                            // inlined
                            let name = match key {
                                wit_parser::WorldKey::Name(name) => name.clone(),
                                wit_parser::WorldKey::Interface(_) => {
                                    unreachable!("inlined interface must have a kye name")
                                }
                            };
                            WorldItem::inline_interface_export(self.convert_interface(
                                interface,
                                Some(name),
                                owner,
                            ))
                        }
                    });
                }
                wit_parser::WorldItem::Function(func) => {
                    if let Some(func) = self.standalone_func_convert(func) {
                        output.item(WorldItem::function_export(func));
                    }
                }
                wit_parser::WorldItem::Type(_) => {
                    todo!();
                }
            }
        }

        output
    }

    fn convert_interface(
        &self,
        interface: &wit_parser::Interface,
        inlined_name: Option<String>,
        owner: wit_parser::TypeOwner,
    ) -> Interface {
        let mut output = Interface::new(interface.name.clone().unwrap_or_else(|| {
            inlined_name
                .clone()
                .expect("inlined interface must pass in inlined_name")
        }));

        for (_, func) in &interface.functions {
            if let Some(func) = self.standalone_func_convert(func) {
                output.items.push(InterfaceItem::Function(func));
            }
        }
        for (_, type_id) in &interface.types {
            let type_def = self.resolve.types.get(*type_id).unwrap();

            let underlying_type_def = match &type_def.kind {
                wit_parser::TypeDefKind::Type(type_) => match &type_ {
                    wit_parser::Type::Id(type_id) => {
                        let type_def = self.resolve.types.get(*type_id).unwrap();
                        type_def
                    }
                    _ => type_def,
                },
                _ => type_def,
            };

            if underlying_type_def.owner == owner {
                if let Some(type_def) = self.convert_type_def(type_def, *type_id) {
                    output.item(InterfaceItem::TypeDef(type_def));
                }
            } else {
                let interface_name = match underlying_type_def.owner {
                    wit_parser::TypeOwner::Interface(id) => self
                        .resolve
                        .interfaces
                        .get(id)
                        .unwrap()
                        .name
                        .clone()
                        .expect("can't use type from inline interface"),
                    _ => panic!("Type not part of an interface"),
                };
                let local_type_name = type_def.name.clone().unwrap();
                let underlying_local_type_name = underlying_type_def.name.clone().unwrap();
                if underlying_local_type_name == local_type_name {
                    output.use_type(interface_name, local_type_name, None);
                } else {
                    output.use_type(
                        interface_name,
                        underlying_local_type_name,
                        Some(local_type_name.into()),
                    );
                }
            }
        }

        output
    }

    fn convert_type_def(
        &self,
        type_def: &wit_parser::TypeDef,
        type_def_id: Id<wit_parser::TypeDef>,
    ) -> Option<TypeDef> {
        match &type_def.name {
            None => None,
            Some(name) => {
                let kind = match &type_def.kind {
                    wit_parser::TypeDefKind::Record(record) => {
                        let output = self.convert_record(record);
                        TypeDefKind::Record(output)
                    }
                    wit_parser::TypeDefKind::Resource => {
                        let output = self.convert_resource(type_def_id, name, &type_def.owner);
                        TypeDefKind::Resource(output)
                    }
                    wit_parser::TypeDefKind::Flags(flags) => {
                        let output = self.convert_flags(flags);
                        TypeDefKind::Flags(output)
                    }
                    wit_parser::TypeDefKind::Variant(variant) => {
                        let output = self.convert_variant(variant);
                        TypeDefKind::Variant(output)
                    }
                    wit_parser::TypeDefKind::Enum(enum_) => {
                        let output = self.convert_enum(enum_);
                        TypeDefKind::Enum(output)
                    }
                    wit_parser::TypeDefKind::Future(_) => {
                        todo!("Enable once wit-encoder supports `future`")
                    }
                    wit_parser::TypeDefKind::Stream(_) => {
                        todo!("Enable once wit-encoder supports `stream`")
                    }
                    // all the following are just `type` declarations
                    wit_parser::TypeDefKind::Option(ty) => {
                        let output = Type::option(self.convert_type(ty));
                        TypeDefKind::Type(output)
                    }
                    wit_parser::TypeDefKind::List(ty) => {
                        let output = Type::list(self.convert_type(ty));
                        TypeDefKind::Type(output)
                    }
                    wit_parser::TypeDefKind::Handle(handle) => {
                        let output = self.handle_to_type(handle);
                        TypeDefKind::Type(output)
                    }
                    wit_parser::TypeDefKind::Result(result) => {
                        let output = Type::result(self.convert_result(result));
                        TypeDefKind::Type(output)
                    }
                    wit_parser::TypeDefKind::Tuple(tuple) => {
                        let output = Type::Tuple(self.convert_tuple(tuple));
                        TypeDefKind::Type(output)
                    }
                    wit_parser::TypeDefKind::Type(ty) => {
                        let output = self.convert_type(ty);
                        TypeDefKind::Type(output)
                    }
                    wit_parser::TypeDefKind::Unknown => unreachable!(),
                };

                Some(TypeDef::new(name.clone(), kind))
            }
        }
    }

    fn convert_type(&self, type_: &wit_parser::Type) -> Type {
        match type_ {
            wit_parser::Type::Bool => Type::Bool,
            wit_parser::Type::U8 => Type::U8,
            wit_parser::Type::U16 => Type::U16,
            wit_parser::Type::U32 => Type::U32,
            wit_parser::Type::U64 => Type::U64,
            wit_parser::Type::S8 => Type::S8,
            wit_parser::Type::S16 => Type::S16,
            wit_parser::Type::S32 => Type::S32,
            wit_parser::Type::S64 => Type::S64,
            wit_parser::Type::F32 => Type::F32,
            wit_parser::Type::F64 => Type::F64,
            wit_parser::Type::Char => Type::Char,
            wit_parser::Type::String => Type::String,
            wit_parser::Type::Id(id) => {
                let type_def = self.resolve.types.get(*id).expect("Type not found");
                match &type_def.name {
                    Some(name) => Type::named(name.clone()),
                    None => match &type_def.kind {
                        wit_parser::TypeDefKind::Tuple(tuple) => {
                            Type::Tuple(self.convert_tuple(tuple))
                        }
                        wit_parser::TypeDefKind::Option(type_) => {
                            Type::option(self.convert_type(type_))
                        }
                        wit_parser::TypeDefKind::Result(result) => {
                            Type::result(self.convert_result(result))
                        }
                        wit_parser::TypeDefKind::List(type_) => {
                            Type::list(self.convert_type(type_))
                        }
                        wit_parser::TypeDefKind::Handle(handle) => self.handle_to_type(handle),
                        wit_parser::TypeDefKind::Future(_) => {
                            todo!("Enable once wit-encoder supports `future`")
                        }
                        wit_parser::TypeDefKind::Stream(_) => {
                            todo!("Enable once wit-encoder supports `stream`")
                        }
                        wit_parser::TypeDefKind::Record(_)
                        | wit_parser::TypeDefKind::Resource
                        | wit_parser::TypeDefKind::Flags(_)
                        | wit_parser::TypeDefKind::Variant(_)
                        | wit_parser::TypeDefKind::Enum(_)
                        | wit_parser::TypeDefKind::Type(_) => {
                            panic!("type doesn't have a name, and can't be inlined")
                        }
                        wit_parser::TypeDefKind::Unknown => unreachable!(),
                    },
                }
            }
        }
    }

    fn convert_enum(&self, enum_: &wit_parser::Enum) -> Enum {
        let mut output = Enum::empty();
        for case in &enum_.cases {
            output.case(case.name.clone())
        }
        output
    }
    fn convert_record(&self, record: &wit_parser::Record) -> Record {
        Record::new(record.fields.iter().map(|field| {
            let name = field.name.clone();
            let type_ = self.convert_type(&field.ty);
            (name, type_)
        }))
    }
    fn convert_variant(&self, variant: &wit_parser::Variant) -> Variant {
        let mut output = Variant::empty();
        for case in &variant.cases {
            match &case.ty {
                Some(ty) => {
                    let ty = self.convert_type(ty);
                    output.case((case.name.clone(), ty))
                }
                None => output.case((case.name.clone(),)),
            }
        }
        output
    }
    fn convert_flags(&self, flags: &wit_parser::Flags) -> Flags {
        Flags::new(flags.flags.iter().map(|flag| (flag.name.clone(),)))
    }

    fn convert_resource(
        &self,
        resource_id: Id<wit_parser::TypeDef>,
        name: &str,
        owner: &wit_parser::TypeOwner,
    ) -> Resource {
        let functions = match owner {
            wit_parser::TypeOwner::World(_) => {
                todo!("Enable once win-encoder supports `include`")
            }
            wit_parser::TypeOwner::Interface(id) => {
                &self.resolve.interfaces.get(*id).unwrap().functions
            }
            wit_parser::TypeOwner::None => panic!("Resource has to have an owner"),
        };

        let mut output = Resource::empty();
        for (_, func) in functions {
            if let Some(method) = self.convert_resource_func(resource_id, name, func) {
                output.func(method);
            }
        }
        output
    }

    fn convert_resource_func(
        &self,
        resource_id: Id<wit_parser::TypeDef>,
        resource_name: &str,
        func: &wit_parser::Function,
    ) -> Option<ResourceFunc> {
        // skip first argument for methods, as they're just `self`.
        let mut skip_first_param = false;
        // constructors can't return anything
        let mut with_returns = true;
        let mut method = match func.kind {
            wit_parser::FunctionKind::Freestanding => return None,
            wit_parser::FunctionKind::Method(id) => {
                if id != resource_id {
                    return None;
                }
                skip_first_param = true;
                let name = clean_func_name(resource_name, &func.name);
                ResourceFunc::method(name)
            }
            wit_parser::FunctionKind::Static(id) => {
                if id != resource_id {
                    return None;
                }
                let name = clean_func_name(resource_name, &func.name);
                ResourceFunc::static_(name)
            }
            wit_parser::FunctionKind::Constructor(id) => {
                if id != resource_id {
                    return None;
                }
                with_returns = false;
                ResourceFunc::constructor()
            }
        };

        method.set_params(self.convert_params(&func.params));
        if skip_first_param {
            method.params_mut().items_mut().remove(0);
        }

        if with_returns {
            method.set_results(self.convert_results(&func.results));
        }

        Some(method)
    }

    fn standalone_func_convert(&self, func: &wit_parser::Function) -> Option<StandaloneFunc> {
        match func.kind {
            wit_parser::FunctionKind::Method(_)
            | wit_parser::FunctionKind::Static(_)
            | wit_parser::FunctionKind::Constructor(_) => None,
            wit_parser::FunctionKind::Freestanding => {
                let mut output = StandaloneFunc::new(func.name.clone());

                output.set_params(self.convert_params(&func.params));
                output.set_results(self.convert_results(&func.results));

                Some(output)
            }
        }
    }

    fn convert_params(&self, params: &wit_parser::Params) -> Params {
        let mut output = Params::empty();
        for (name, ty) in params.iter() {
            let name = name.to_string();
            let ty = self.convert_type(ty);
            output.push(name, ty);
        }
        output
    }

    fn convert_results(&self, results: &wit_parser::Results) -> Results {
        match results {
            wit_parser::Results::Named(named) => Results::named(named.iter().map(|(name, ty)| {
                let ty = self.convert_type(ty);
                (name.to_owned(), ty)
            })),
            wit_parser::Results::Anon(ty) => Results::Anon(self.convert_type(ty)),
        }
    }

    fn handle_to_type(&self, handle: &wit_parser::Handle) -> Type {
        let id = match handle {
            wit_parser::Handle::Own(id) => id,
            wit_parser::Handle::Borrow(id) => id,
        };
        let type_def = self.resolve.types.get(*id).expect("Type not found");
        let name = type_def
            .name
            .clone()
            .expect("Handles should only be for resources, and resources should have names");
        match handle {
            wit_parser::Handle::Own(_) => Type::named(name),
            wit_parser::Handle::Borrow(_) => Type::borrow(name),
        }
    }

    fn convert_result(&self, result: &wit_parser::Result_) -> Result_ {
        match (&result.ok, &result.err) {
            (None, None) => Result_::empty(),
            (Some(ok), None) => Result_::ok(self.convert_type(ok)),
            (None, Some(err)) => Result_::err(self.convert_type(err)),
            (Some(ok), Some(err)) => Result_::both(self.convert_type(ok), self.convert_type(err)),
        }
    }

    fn convert_tuple(&self, tuple: &wit_parser::Tuple) -> Tuple {
        let mut output = Tuple::empty();
        for ty in tuple.types.iter() {
            output.types_mut().push(self.convert_type(ty));
        }
        output
    }
}

fn clean_func_name(resource_name: &str, method_name: &str) -> String {
    const METHOD_PREFIX: &str = "[method]";
    const STATIC_PREFIX: &str = "[static]";

    let method_name = method_name
        .strip_prefix(METHOD_PREFIX)
        .unwrap_or(method_name);
    let method_name = method_name
        .strip_prefix(STATIC_PREFIX)
        .unwrap_or(method_name);
    let method_name = method_name.strip_prefix(resource_name).unwrap();
    let method_name = method_name.strip_prefix(".").unwrap();
    method_name.to_string()
}
