#![no_main]

use libfuzzer_sys::fuzz_target;
use std::path::Path;
use wit_component::*;

fuzz_target!(|data: &[u8]| {
    drop(env_logger::try_init());

    let mut u = arbitrary::Unstructured::new(data);
    let doc = match generate::document(&mut u) {
        Ok(doc) => doc,
        Err(_) => return,
    };
    let text = DocumentPrinter::default().print(&doc).unwrap();
    write_file("doc.wit", &text);
    let world = match doc.default_world() {
        Ok(world) => world,
        Err(_) => return,
    };

    let types_only_binary = ComponentEncoder::default()
        .validate(true)
        .types_only(true)
        .document(doc.clone(), StringEncoding::UTF8)
        .unwrap()
        .encode()
        .unwrap();
    write_file("doc.types-only.wasm", &types_only_binary);
    let (types_only_doc, _) = decode_world(&doc.worlds[world].name, &types_only_binary).unwrap();
    let text_from_types_only = DocumentPrinter::default().print(&types_only_doc).unwrap();
    write_file("doc.types-only.wit", &text_from_types_only);

    let dummy = test_helpers::dummy_module(&doc);
    write_file("doc.dummy.wasm", &dummy);
    let normal_binary = ComponentEncoder::default()
        .validate(true)
        .module(&dummy)
        .unwrap()
        .document(doc.clone(), StringEncoding::UTF8)
        .unwrap()
        .encode()
        .unwrap();
    write_file("doc.normal.wasm", &normal_binary);
    let (normal_doc, _) = decode_world(&doc.worlds[world].name, &normal_binary).unwrap();
    let text_from_normal = DocumentPrinter::default().print(&normal_doc).unwrap();
    write_file("doc.normal.wit", &text_from_normal);

    if text_from_normal != text_from_types_only {
        panic!("text not equal");
    }
});

fn write_file(path: &str, contents: impl AsRef<[u8]>) {
    if !log::log_enabled!(log::Level::Debug) {
        return;
    }
    log::debug!("writing file {path}");
    let contents = contents.as_ref();
    let path = Path::new(path);
    std::fs::write(path, contents).unwrap();
    if path.extension().and_then(|s| s.to_str()) == Some("wasm") {
        let path = path.with_extension("wat");
        log::debug!("writing file {}", path.display());
        std::fs::write(path, wasmprinter::print_bytes(&contents).unwrap()).unwrap();
    }
}

mod generate {
    use arbitrary::{Arbitrary, Result, Unstructured};
    use std::collections::HashSet;
    use std::str;
    use wit_parser::*;

    #[derive(Default)]
    struct Generator {
        doc: Document,
        type_sizes: Vec<usize>,
        unique_names: HashSet<String>,
        types_in_interface: Vec<TypeId>,
        named_types: Vec<TypeId>,
    }

    pub fn document(u: &mut Unstructured<'_>) -> Result<Document> {
        let mut gen = Generator::default();
        gen.gen(u)?;
        Ok(gen.doc)
    }

    impl Generator {
        fn gen(&mut self, u: &mut Unstructured<'_>) -> Result<()> {
            #[derive(Arbitrary)]
            enum Generate {
                World,
                Interface,
                Done,
            }

            while !u.is_empty() {
                match u.arbitrary()? {
                    Generate::World => {
                        self.gen_world(u)?;
                    }
                    Generate::Interface => {
                        self.gen_interface(u)?;
                    }
                    Generate::Done => break,
                }
            }
            Ok(())
        }

        fn gen_world(&mut self, u: &mut Unstructured<'_>) -> Result<WorldId> {
            let mut world = World::default();
            world.name = self.gen_name(u)?;

            let mut interfaces = self
                .doc
                .interfaces
                .iter()
                .map(|(id, _)| id)
                .collect::<Vec<_>>();

            #[derive(Arbitrary)]
            enum Generate {
                Import,
                // TODO: this is buggy right now due to use-through-export not
                // being implemented. Should fix and re-enable this.
                // Export,
            }

            let mut imports = Vec::new();
            let mut exports = Vec::new();
            while interfaces.len() > 0 && u.arbitrary()? {
                let dst = match u.arbitrary()? {
                    Generate::Import => &mut imports,
                    // Generate::Export => &mut exports,
                };
                if dst.len() > 10 {
                    continue;
                }
                let i = u.int_in_range(0..=interfaces.len() - 1)?;
                let interface = interfaces.swap_remove(i);
                dst.push(interface);
            }
            if interfaces.len() > 0 && u.arbitrary()? {
                world.default = Some(*u.choose(&interfaces)?);
            }

            // TODO: most of this probably doesn't make sense. One hope is that
            // this all goes away and can be replaced with the text-based
            // resolution pass by generating a text file instead of an AST
            // directly. Unsure how to best clean this up.
            let mut visited = HashSet::new();
            let mut import_order = Vec::new();
            for id in imports {
                self.topo_visit(id, &mut visited, &mut import_order);
            }
            for id in exports.iter() {
                self.topo_visit(*id, &mut visited, &mut import_order);
                import_order.pop();
            }
            if let Some(default) = world.default {
                self.topo_visit(default, &mut visited, &mut import_order);
                import_order.pop();
            }

            for i in import_order {
                world.imports.insert(self.gen_unique_name(u)?, i);
            }
            for e in exports {
                world.exports.insert(self.gen_unique_name(u)?, e);
            }

            self.unique_names.clear();
            return Ok(self.doc.worlds.alloc(world));
        }

        fn topo_visit(
            &self,
            id: InterfaceId,
            visited: &mut HashSet<InterfaceId>,
            order: &mut Vec<InterfaceId>,
        ) {
            if !visited.insert(id) {
                return;
            }
            let interface = &self.doc.interfaces[id];
            for ty in interface.types.iter() {
                let other_id = match self.doc.types[*ty].kind {
                    TypeDefKind::Type(Type::Id(id)) => id,
                    _ => continue,
                };
                let other_interface = match self.doc.types[other_id].interface {
                    Some(i) => i,
                    None => continue,
                };
                if other_interface == id {
                    continue;
                }
                self.topo_visit(other_interface, visited, order);
            }
            order.push(id);
        }

        fn gen_interface(&mut self, u: &mut Unstructured<'_>) -> Result<InterfaceId> {
            let mut iface = Interface::default();
            iface.name = self.gen_name(u)?;

            #[derive(Arbitrary)]
            enum Generate {
                Type,
                Function,
            }

            while u.arbitrary()? {
                match u.arbitrary()? {
                    Generate::Type => {
                        let (size, typedef) = self.gen_typedef(u)?;
                        let id = self.doc.types.alloc(typedef);
                        self.type_sizes.push(size);
                        if self.doc.types[id].name.is_some() {
                            iface.types.push(id);
                            self.named_types.push(id);
                        }
                        self.types_in_interface.push(id);
                    }
                    Generate::Function => {
                        let f = self.gen_func(u)?;
                        iface.functions.push(f);
                    }
                }
            }

            self.types_in_interface.clear();
            Ok(self.doc.interfaces.alloc(iface))
        }

        fn gen_typedef(&mut self, u: &mut Unstructured<'_>) -> Result<(usize, TypeDef)> {
            const MAX_PARTS: usize = 5;

            #[derive(Arbitrary)]
            pub enum Kind {
                Record,
                Tuple,
                Flags,
                Variant,
                Enum,
                Option,
                Result,
                Union,
                List,
                Type,
            }

            let mut size = 1;
            let kind = match u.arbitrary()? {
                Kind::Record => TypeDefKind::Record(Record {
                    fields: (0..u.int_in_range(0..=MAX_PARTS)?)
                        .map(|_| {
                            Ok(Field {
                                docs: Docs::default(),
                                name: self.gen_unique_name(u)?,
                                ty: self.gen_type(u, &mut size)?,
                            })
                        })
                        .collect::<Result<_>>()?,
                }),
                Kind::Variant => TypeDefKind::Variant(Variant {
                    cases: (0..u.int_in_range(1..=MAX_PARTS)?)
                        .map(|_| {
                            Ok(Case {
                                docs: Docs::default(),
                                name: self.gen_unique_name(u)?,
                                ty: if u.arbitrary()? {
                                    Some(self.gen_type(u, &mut size)?)
                                } else {
                                    None
                                },
                            })
                        })
                        .collect::<Result<_>>()?,
                }),
                Kind::Tuple => TypeDefKind::Tuple(Tuple {
                    types: (0..u.int_in_range(0..=MAX_PARTS)?)
                        .map(|_| self.gen_type(u, &mut size))
                        .collect::<Result<_>>()?,
                }),
                Kind::Union => TypeDefKind::Union(Union {
                    cases: (0..u.int_in_range(1..=MAX_PARTS)?)
                        .map(|_| {
                            Ok(UnionCase {
                                docs: Docs::default(),
                                ty: self.gen_type(u, &mut size)?,
                            })
                        })
                        .collect::<Result<_>>()?,
                }),
                Kind::List => TypeDefKind::List(self.gen_type(u, &mut size)?),
                Kind::Type => TypeDefKind::Type(self.gen_type_allow_use(u, true, &mut size)?),
                Kind::Option => TypeDefKind::Option(self.gen_type(u, &mut size)?),
                Kind::Result => TypeDefKind::Result(Result_ {
                    ok: if u.arbitrary()? {
                        Some(self.gen_type(u, &mut size)?)
                    } else {
                        None
                    },
                    err: if u.arbitrary()? {
                        Some(self.gen_type(u, &mut size)?)
                    } else {
                        None
                    },
                }),
                Kind::Flags => TypeDefKind::Flags(Flags {
                    flags: (0..u.int_in_range(0..=MAX_PARTS)?)
                        .map(|_| {
                            Ok(Flag {
                                name: self.gen_unique_name(u)?,
                                docs: Docs::default(),
                            })
                        })
                        .collect::<Result<_>>()?,
                }),
                Kind::Enum => TypeDefKind::Enum(Enum {
                    cases: (0..u.int_in_range(1..=MAX_PARTS)?)
                        .map(|_| {
                            Ok(EnumCase {
                                name: self.gen_unique_name(u)?,
                                docs: Docs::default(),
                            })
                        })
                        .collect::<Result<_>>()?,
                }),
            };

            // Determine if this kind is allowed to be anonymous or not, and if
            // it can be optionally annotate it with an interface and a name.
            // Note that non-anonymous types must have an interface and a name.
            let can_be_anonymous = match kind {
                TypeDefKind::Type(_)
                | TypeDefKind::Record(_)
                | TypeDefKind::Enum(_)
                | TypeDefKind::Flags(_)
                | TypeDefKind::Variant(_)
                | TypeDefKind::Future(_)
                | TypeDefKind::Stream(_)
                | TypeDefKind::Union(_) => false,
                TypeDefKind::Tuple(_)
                | TypeDefKind::Option(_)
                | TypeDefKind::List(_)
                | TypeDefKind::Result(_) => true,
            };

            // Choose a name, either because we are forced to or because the
            // fuzz input says we should name this.
            let name = if !can_be_anonymous || u.arbitrary()? {
                Some(self.gen_unique_name(u)?)
            } else {
                None
            };

            // Determine if the `interface` field will be set. This is required
            // for named or non-anonymous types since they're attached to an
            // interface. Otherwise let the fuzz input determine that.
            let interface = if name.is_some() || !can_be_anonymous || u.arbitrary()? {
                Some(self.doc.interfaces.next_id())
            } else {
                None
            };

            Ok((
                size,
                TypeDef {
                    docs: Docs::default(),
                    kind,
                    name,
                    interface,
                },
            ))
        }

        fn gen_type(&mut self, u: &mut Unstructured<'_>, size: &mut usize) -> Result<Type> {
            self.gen_type_allow_use(u, false, size)
        }

        fn gen_type_allow_use(
            &mut self,
            u: &mut Unstructured<'_>,
            allow_use: bool,
            size: &mut usize,
        ) -> Result<Type> {
            const MAX_SIZE: usize = 100;

            #[derive(Arbitrary)]
            enum Kind {
                Bool,
                U8,
                U16,
                U32,
                U64,
                S8,
                S16,
                S32,
                S64,
                Float32,
                Float64,
                Char,
                String,
                Id,
            }

            *size += 1;
            loop {
                break match u.arbitrary()? {
                    Kind::Bool => Ok(Type::Bool),
                    Kind::U8 => Ok(Type::U8),
                    Kind::S8 => Ok(Type::S8),
                    Kind::U16 => Ok(Type::U16),
                    Kind::S16 => Ok(Type::S16),
                    Kind::U32 => Ok(Type::U32),
                    Kind::S32 => Ok(Type::S32),
                    Kind::U64 => Ok(Type::U64),
                    Kind::S64 => Ok(Type::S64),
                    Kind::Float32 => Ok(Type::Float32),
                    Kind::Float64 => Ok(Type::Float64),
                    Kind::Char => Ok(Type::Char),
                    Kind::String => Ok(Type::String),
                    Kind::Id => {
                        let types = if allow_use {
                            &self.named_types
                        } else {
                            &self.types_in_interface
                        };
                        if types.is_empty() {
                            continue;
                        }
                        let id = *u.choose(&types)?;
                        if *size + self.type_sizes[id.index()] > MAX_SIZE {
                            continue;
                        }
                        *size += self.type_sizes[id.index()];
                        Ok(Type::Id(id))
                    }
                };
            }
        }

        fn gen_func(&mut self, u: &mut Unstructured<'_>) -> Result<Function> {
            Ok(Function {
                docs: Docs::default(),
                name: self.gen_unique_name(u)?,
                kind: FunctionKind::Freestanding,
                params: self.gen_params(u)?,
                results: if u.arbitrary()? {
                    Results::Anon(self.gen_type(u, &mut 1)?)
                } else {
                    Results::Named(self.gen_params(u)?)
                },
            })
        }

        fn gen_params(&mut self, u: &mut Unstructured<'_>) -> Result<Vec<(String, Type)>> {
            (0..u.int_in_range(0..=5)?)
                .map(|_| Ok((self.gen_unique_name(u)?, self.gen_type(u, &mut 1)?)))
                .collect()
        }

        fn gen_name(&self, u: &mut Unstructured<'_>) -> Result<String> {
            let size = u.arbitrary_len::<u8>()?;
            let size = std::cmp::min(size, 20);
            let name = match str::from_utf8(u.peek_bytes(size).unwrap()) {
                Ok(s) => {
                    u.bytes(size).unwrap();
                    s.to_string()
                }
                Err(e) => {
                    let i = e.valid_up_to();
                    let valid = u.bytes(i).unwrap();
                    str::from_utf8(valid).unwrap().to_string()
                }
            };
            let name = name
                .chars()
                .map(|x| if x.is_ascii_lowercase() { x } else { 'x' })
                .collect::<String>();
            Ok(if name.is_empty() {
                "name".to_string()
            } else {
                name
            })
        }

        fn gen_unique_name(&mut self, u: &mut Unstructured<'_>) -> Result<String> {
            use std::fmt::Write;
            let mut name = self.gen_name(u)?;
            while !self.unique_names.insert(name.clone()) {
                write!(&mut name, "{}", self.unique_names.len()).unwrap();
            }
            Ok(name)
        }
    }
}
