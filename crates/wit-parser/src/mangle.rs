//! Canonical ABI name mangling.
//!
//! This file implements the name mangling scheme defined [here]
//! [here]: https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#canonical-module-type

use crate::{
    Case, EnumCase, Field, Flag, Function, Interface, ResourceId, Stream, Type, TypeDefKind,
    UnionCase,
};

const CABI_VERSION: &str = "0.1";

enum PreSpace {
    False,
}

impl Interface {
    pub fn mangle_funcname(&self, func: &Function) -> String {
        self.mangle_funcname_with_name(&func.name, func)
    }

    pub fn mangle_start_funcname(&self, func: &Function) -> String {
        self.mangle_funcname_with_name(&format!("cabi_start{{cabi={}}}", CABI_VERSION), func)
    }

    fn mangle_funcname_with_name(&self, name: &str, func: &Function) -> String {
        format!(
            "{}: func{} -> {}",
            name,
            self.mangle_funcvec(&func.params, PreSpace::False),
            self.mangle_valtype(func.result)
        )
    }

    fn mangle_funcvec(&self, es: &[(String, Type)], _pre_space: PreSpace) -> String {
        format!(
            "({})",
            es.iter()
                .map(|e| format!("{}: {}", e.0, self.mangle_valtype(e.1)))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    fn mangle_valtype(&self, t: Type) -> String {
        match t {
            Type::Unit => "unit".to_owned(),
            Type::Bool => "bool".to_owned(),
            Type::S8 => "s8".to_owned(),
            Type::U8 => "u8".to_owned(),
            Type::S16 => "s16".to_owned(),
            Type::U16 => "u16".to_owned(),
            Type::S32 => "s32".to_owned(),
            Type::U32 => "u32".to_owned(),
            Type::S64 => "s64".to_owned(),
            Type::U64 => "u64".to_owned(),
            Type::Float32 => "float32".to_owned(),
            Type::Float64 => "float64".to_owned(),
            Type::Char => "char".to_owned(),
            Type::String => "string".to_owned(),
            Type::Handle(id) => self.mangle_handletype(id),
            Type::Id(id) => self.mangle_valtypedef(&self.types[id].kind),
        }
    }

    fn mangle_valtypedef(&self, kind: &TypeDefKind) -> String {
        match kind {
            TypeDefKind::List(t) => format!("list<{}>", self.mangle_valtype(*t)),
            TypeDefKind::Record(r) => self.mangle_recordtype(&r.fields),
            TypeDefKind::Tuple(t) => self.mangle_tupletype(&t.types),
            TypeDefKind::Flags(f) => self.mangle_flags(&f.flags),
            TypeDefKind::Variant(v) => self.mangle_varianttype(&v.cases),
            TypeDefKind::Enum(e) => self.mangle_enumtype(&e.cases),
            TypeDefKind::Union(u) => self.mangle_uniontype(&u.cases),
            TypeDefKind::Option(t) => self.mangle_optiontype(*t),
            TypeDefKind::Result(r) => self.mangle_resulttype(r.ok, r.err),
            TypeDefKind::Future(t) => self.mangle_futuretype(*t),
            TypeDefKind::Stream(s) => self.mangle_streamtype(s),
            TypeDefKind::Type(t) => self.mangle_valtype(*t),
        }
    }

    fn mangle_recordtype(&self, fields: &[Field]) -> String {
        if fields.is_empty() {
            "record {}".to_owned()
        } else {
            format!(
                "record {{ {} }}",
                fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, self.mangle_valtype(f.ty)))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }

    fn mangle_tupletype(&self, ts: &[Type]) -> String {
        format!(
            "tuple<{}>",
            ts.iter()
                .map(|t| self.mangle_valtype(*t))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    fn mangle_flags(&self, labels: &[Flag]) -> String {
        if labels.is_empty() {
            "flags {}".to_owned()
        } else {
            format!(
                "flags {{ {} }}",
                labels
                    .iter()
                    .map(|f| f.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }

    fn mangle_varianttype(&self, cases: &[Case]) -> String {
        if cases.is_empty() {
            "variant {}".to_owned()
        } else {
            format!(
                "variant {{ {} }}",
                cases
                    .iter()
                    .map(|c| format!("{}{}", c.name, format!("({})", self.mangle_valtype(c.ty)),))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }

    fn mangle_enumtype(&self, labels: &[EnumCase]) -> String {
        if labels.is_empty() {
            "enum {}".to_owned()
        } else {
            format!(
                "enum {{ {} }}",
                labels
                    .iter()
                    .map(|l| l.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }

    fn mangle_uniontype(&self, cases: &[UnionCase]) -> String {
        if cases.is_empty() {
            "union {}".to_owned()
        } else {
            format!(
                "union {{ {} }}",
                cases
                    .iter()
                    .map(|case| self.mangle_valtype(case.ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }

    fn mangle_optiontype(&self, t: Type) -> String {
        format!("option<{}>", self.mangle_valtype(t))
    }

    fn mangle_resulttype(&self, ok: Type, error: Type) -> String {
        format!(
            "result<{}, {}>",
            self.mangle_valtype(ok),
            self.mangle_valtype(error)
        )
    }

    fn mangle_handletype(&self, id: ResourceId) -> String {
        format!("handle<{}>", self.resources[id].name)
    }

    fn mangle_futuretype(&self, ty: Type) -> String {
        format!("future<{}>", self.mangle_valtype(ty))
    }

    fn mangle_streamtype(&self, stream: &Stream) -> String {
        format!(
            "stream<{}, {}>",
            self.mangle_valtype(stream.element),
            self.mangle_valtype(stream.end)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        Docs, Enum, Flag, Flags, FunctionKind, Record, Resource, Result_, Tuple, Union, Variant,
    };

    #[test]
    fn test_funcname() {
        let interface = Interface::default();

        assert_eq!(
            interface.mangle_funcname(&Function {
                docs: Docs::default(),
                name: "foo".to_owned(),
                kind: FunctionKind::Freestanding,
                params: Vec::new(),
                result: Type::Unit
            }),
            "foo: func() -> unit"
        );
        assert_eq!(
            interface.mangle_funcname(&Function {
                docs: Docs::default(),
                name: "foo".to_owned(),
                kind: FunctionKind::Freestanding,
                params: vec![("a".to_owned(), Type::S64)],
                result: Type::S32
            }),
            "foo: func(a: s64) -> s32"
        );
        assert_eq!(
            interface.mangle_funcname(&Function {
                docs: Docs::default(),
                name: "foo".to_owned(),
                kind: FunctionKind::Freestanding,
                params: vec![("a".to_owned(), Type::S64), ("b".to_owned(), Type::U64)],
                result: Type::S32
            }),
            "foo: func(a: s64, b: u64) -> s32"
        );
    }

    #[test]
    fn test_start_funcname() {
        let interface = Interface::default();

        assert_eq!(
            interface.mangle_start_funcname(&Function {
                docs: Docs::default(),
                name: "foo".to_owned(),
                kind: FunctionKind::Freestanding,
                params: Vec::new(),
                result: Type::Unit
            }),
            format!("cabi_start{{cabi={}}}: func() -> unit", CABI_VERSION)
        );
    }

    #[test]
    fn test_types() {
        let iface = Interface::default();
        assert_eq!(iface.mangle_valtype(Type::Unit), "unit");
        assert_eq!(iface.mangle_valtype(Type::Bool), "bool");
        assert_eq!(iface.mangle_valtype(Type::S8), "s8");
        assert_eq!(iface.mangle_valtype(Type::U8), "u8");
        assert_eq!(iface.mangle_valtype(Type::S16), "s16");
        assert_eq!(iface.mangle_valtype(Type::U16), "u16");
        assert_eq!(iface.mangle_valtype(Type::S32), "s32");
        assert_eq!(iface.mangle_valtype(Type::U32), "u32");
        assert_eq!(iface.mangle_valtype(Type::S64), "s64");
        assert_eq!(iface.mangle_valtype(Type::U64), "u64");
        assert_eq!(iface.mangle_valtype(Type::Float32), "float32");
        assert_eq!(iface.mangle_valtype(Type::Float64), "float64");
        assert_eq!(iface.mangle_valtype(Type::Char), "char");
        assert_eq!(iface.mangle_valtype(Type::String), "string");
    }

    #[test]
    fn test_listtype() {
        let iface = Interface::default();
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::List(Type::U16)),
            "list<u16>"
        );
    }

    #[test]
    fn test_recordtype() {
        let iface = Interface::default();
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Record(Record { fields: Vec::new() })),
            "record {}"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Record(Record {
                fields: vec![Field {
                    name: "x".to_owned(),
                    docs: Docs::default(),
                    ty: Type::Float32
                }]
            })),
            "record { x: float32 }"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Record(Record {
                fields: vec![
                    Field {
                        name: "x".to_owned(),
                        docs: Docs::default(),
                        ty: Type::Float32
                    },
                    Field {
                        name: "y".to_owned(),
                        docs: Docs::default(),
                        ty: Type::Float64
                    }
                ]
            })),
            "record { x: float32, y: float64 }"
        );
    }

    #[test]
    fn test_tupletype() {
        let iface = Interface::default();
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Tuple(Tuple { types: Vec::new() })),
            "tuple<>"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Tuple(Tuple {
                types: vec![Type::Float32]
            })),
            "tuple<float32>"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Tuple(Tuple {
                types: vec![Type::Float32, Type::Float64]
            })),
            "tuple<float32, float64>"
        );
    }

    #[test]
    fn test_flags() {
        let iface = Interface::default();
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Flags(Flags { flags: Vec::new() })),
            "flags {}"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Flags(Flags {
                flags: vec![Flag {
                    name: "red".to_owned(),
                    docs: Docs::default()
                }]
            })),
            "flags { red }"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Flags(Flags {
                flags: vec![
                    Flag {
                        name: "red".to_owned(),
                        docs: Docs::default()
                    },
                    Flag {
                        name: "green".to_owned(),
                        docs: Docs::default()
                    }
                ]
            })),
            "flags { red, green }"
        );
    }

    #[test]
    fn test_varianttype() {
        let iface = Interface::default();
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Variant(Variant { cases: Vec::new() })),
            "variant {}"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Variant(Variant {
                cases: vec![Case {
                    name: "x".to_owned(),
                    docs: Docs::default(),
                    ty: Type::Float32
                }]
            })),
            "variant { x(float32) }"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Variant(Variant {
                cases: vec![
                    Case {
                        name: "x".to_owned(),
                        docs: Docs::default(),
                        ty: Type::Float32
                    },
                    Case {
                        name: "y".to_owned(),
                        docs: Docs::default(),
                        ty: Type::Float64
                    }
                ]
            })),
            "variant { x(float32), y(float64) }"
        );
    }

    #[test]
    fn test_enumtype() {
        let iface = Interface::default();
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Enum(Enum { cases: Vec::new() })),
            "enum {}"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Enum(Enum {
                cases: vec![EnumCase {
                    name: "x".to_owned(),
                    docs: Docs::default(),
                }]
            })),
            "enum { x }"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Enum(Enum {
                cases: vec![
                    EnumCase {
                        name: "x".to_owned(),
                        docs: Docs::default(),
                    },
                    EnumCase {
                        name: "y".to_owned(),
                        docs: Docs::default(),
                    }
                ]
            })),
            "enum { x, y }"
        );
    }

    #[test]
    fn test_uniontype() {
        let iface = Interface::default();
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Union(Union { cases: Vec::new() })),
            "union {}"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Union(Union {
                cases: vec![UnionCase {
                    docs: Docs::default(),
                    ty: Type::Float32
                }]
            })),
            "union { float32 }"
        );
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Union(Union {
                cases: vec![
                    UnionCase {
                        docs: Docs::default(),
                        ty: Type::Float32
                    },
                    UnionCase {
                        docs: Docs::default(),
                        ty: Type::Float64
                    }
                ]
            })),
            "union { float32, float64 }"
        );
    }

    #[test]
    fn test_optiontype() {
        let iface = Interface::default();
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Option(Type::S8)),
            "option<s8>"
        );
    }

    #[test]
    fn test_resulttype() {
        let iface = Interface::default();
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Result(Result_ {
                ok: Type::S32,
                err: Type::U32
            })),
            "result<s32, u32>"
        );
    }

    #[test]
    fn test_handletype() {
        let mut iface = Interface::default();
        let id = iface.resources.alloc(Resource {
            name: "thing".to_owned(),
            docs: Docs::default(),
            foreign_module: None,
            supertype: None,
        });
        assert_eq!(iface.mangle_valtype(Type::Handle(id)), "handle<thing>");
    }

    #[test]
    fn test_futuretype() {
        let iface = Interface::default();
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Future(Type::S8)),
            "future<s8>"
        );
    }

    #[test]
    fn test_streamtype() {
        let iface = Interface::default();
        assert_eq!(
            iface.mangle_valtypedef(&TypeDefKind::Stream(Stream {
                element: Type::S8,
                end: Type::U8
            })),
            "stream<s8, u8>"
        );
    }
}
