//! You can run this test suite with:
//!
//!     cargo test --test all
//!
//! An argument can be passed as well to filter, based on filename, which test
//! to run
//!
//!     cargo test --test all foo.wit

use anyhow::{bail, Context, Result};
use rayon::prelude::*;
use serde::Serialize;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::str;
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};
use wit_parser::*;

fn main() {
    let tests = find_tests();
    let filter = std::env::args().nth(1);

    let tests = tests
        .par_iter()
        .filter_map(|test| {
            if let Some(filter) = &filter {
                if let Some(s) = test.to_str() {
                    if !s.contains(filter) {
                        return None;
                    }
                }
            }
            let contents = fs::read(test).unwrap();
            Some((test, contents))
        })
        .collect::<Vec<_>>();

    println!("running {} test files\n", tests.len());

    let ntests = AtomicUsize::new(0);
    let errors = tests
        .par_iter()
        .filter_map(|(test, contents)| {
            Runner { ntests: &ntests }
                .run(test, contents)
                .context(format!("test {:?} failed", test))
                .err()
        })
        .collect::<Vec<_>>();

    if !errors.is_empty() {
        for msg in errors.iter() {
            eprintln!("{:?}", msg);
        }

        panic!("{} tests failed", errors.len())
    }

    println!(
        "test result: ok. {} directives passed\n",
        ntests.load(SeqCst)
    );
}

/// Recursively finds all tests in a whitelisted set of directories which we
/// then load up and test in parallel.
fn find_tests() -> Vec<PathBuf> {
    let mut tests = Vec::new();
    find_tests("tests/ui".as_ref(), &mut tests);
    tests.sort();
    return tests;

    fn find_tests(path: &Path, tests: &mut Vec<PathBuf>) {
        for f in path.read_dir().unwrap() {
            let f = f.unwrap();
            if f.file_type().unwrap().is_dir() {
                find_tests(&f.path(), tests);
                continue;
            }

            match f.path().extension().and_then(|s| s.to_str()) {
                Some("md") => {}
                Some("wit") => {}
                _ => continue,
            }
            tests.push(f.path());
        }
    }
}

struct Runner<'a> {
    ntests: &'a AtomicUsize,
}

impl Runner<'_> {
    fn run(&mut self, test: &Path, contents: &[u8]) -> Result<()> {
        let contents = str::from_utf8(contents)?;

        let result = Document::parse(test, contents);

        let result = if contents.contains("// parse-fail") {
            match result {
                Ok(_) => bail!("expected test to not parse but it did"),
                Err(mut e) => {
                    if let Some(err) = e.downcast_mut::<io::Error>() {
                        *err = io::Error::new(
                            io::ErrorKind::Other,
                            "some generic platform-agnostic error message",
                        );
                    }
                    normalize(test, &format!("{:?}", e))
                }
            }
        } else {
            let document = result?;
            test_document(&document);
            to_json(&document)
        };

        // "foo.wit" => "foo.wit.result"
        // "foo.wit.md" => "foo.wit.md.result"
        let result_file = if test.extension() == Some(OsStr::new("md"))
            && test
                .file_stem()
                .and_then(|path| Path::new(path).extension())
                == Some(OsStr::new("wit"))
        {
            test.with_extension("md.result")
        } else {
            test.with_extension("wit.result")
        };
        if env::var_os("BLESS").is_some() {
            fs::write(&result_file, result)?;
        } else {
            let expected = fs::read_to_string(&result_file).context(format!(
                "failed to read test expectation file {:?}\nthis can be fixed with BLESS=1",
                result_file
            ))?;
            let expected = normalize(test, &expected);
            if expected != result {
                bail!(
                    "failed test: expected `{:?}` but found `{:?}`",
                    expected,
                    result
                );
            }
        }
        self.bump_ntests();
        return Ok(());

        fn normalize(test: &Path, s: &str) -> String {
            s.replace(
                &test.display().to_string(),
                &test.display().to_string().replace('\\', "/"),
            )
            .replace("\\parse-fail\\", "/parse-fail/")
            .replace("\r\n", "\n")
        }
    }

    fn bump_ntests(&self) {
        self.ntests.fetch_add(1, SeqCst);
    }
}

fn to_json(document: &Document) -> String {
    #[derive(Serialize)]
    struct Document {
        #[serde(skip_serializing_if = "Vec::is_empty")]
        worlds: Vec<World>,
        #[serde(skip_serializing_if = "Vec::is_empty")]
        interfaces: Vec<Interface>,
    }

    #[derive(Serialize)]
    struct World {
        name: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<Interface>,
        #[serde(skip_serializing_if = "Vec::is_empty")]
        imports: Vec<(String, Interface)>,
        #[serde(skip_serializing_if = "Vec::is_empty")]
        exports: Vec<(String, Interface)>,
    }

    #[derive(Serialize)]
    struct Interface {
        name: String,
        #[serde(skip_serializing_if = "Vec::is_empty")]
        types: Vec<TypeDef>,
        #[serde(skip_serializing_if = "Vec::is_empty")]
        functions: Vec<Function>,
        #[serde(skip_serializing_if = "Vec::is_empty")]
        globals: Vec<Global>,
    }

    #[derive(Serialize)]
    struct Resource {
        name: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        supertype: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        foreign_module: Option<String>,
    }

    #[derive(Serialize)]
    struct TypeDef {
        idx: usize,
        #[serde(skip_serializing_if = "Option::is_none")]
        name: Option<String>,
        #[serde(flatten)]
        ty: Type,
        #[serde(skip_serializing_if = "Option::is_none")]
        foreign_module: Option<String>,
    }

    #[derive(Serialize)]
    #[serde(rename_all = "kebab-case")]
    enum Type {
        Primitive(String),
        Record {
            fields: Vec<(String, String)>,
        },
        Flags {
            flags: Vec<String>,
        },
        Enum {
            cases: Vec<String>,
        },
        Variant {
            cases: Vec<(String, Option<String>)>,
        },
        Tuple {
            types: Vec<String>,
        },
        Option(String),
        Result {
            ok: Option<String>,
            err: Option<String>,
        },
        Future(Option<String>),
        Stream {
            element: Option<String>,
            end: Option<String>,
        },
        List(String),
        Union {
            cases: Vec<String>,
        },
    }

    #[derive(Serialize)]
    struct Function {
        name: String,
        params: Vec<String>,
        results: Vec<String>,
    }

    #[derive(Serialize)]
    struct Global {
        name: String,
        ty: String,
    }

    let document = Document {
        worlds: document.worlds.iter().map(translate_world).collect(),
        interfaces: document
            .interfaces
            .iter()
            .map(translate_interface)
            .collect(),
    };

    return serde_json::to_string_pretty(&document).unwrap();

    fn translate_world(w: &wit_parser::World) -> World {
        World {
            name: w.name.clone(),
            default: w.default.as_ref().map(translate_interface),
            imports: w
                .imports
                .iter()
                .map(|(name, iface)| (name.clone(), translate_interface(iface)))
                .collect(),
            exports: w
                .exports
                .iter()
                .map(|(name, iface)| (name.clone(), translate_interface(iface)))
                .collect(),
        }
    }

    fn translate_interface(i: &wit_parser::Interface) -> Interface {
        let types = i
            .types
            .iter()
            .map(|(i, r)| TypeDef {
                idx: i.index(),
                name: r.name.clone(),
                ty: translate_typedef(r),
                foreign_module: r.foreign_module.clone(),
            })
            .collect::<Vec<_>>();
        let functions = i
            .functions
            .iter()
            .map(|f| Function {
                name: f.name.clone(),
                params: f.params.iter().map(|(_, ty)| translate_type(ty)).collect(),
                results: f.results.iter_types().map(translate_type).collect(),
            })
            .collect::<Vec<_>>();
        let globals = i
            .globals
            .iter()
            .map(|g| Global {
                name: g.name.clone(),
                ty: translate_type(&g.ty),
            })
            .collect::<Vec<_>>();

        Interface {
            name: i.name.clone(),
            types,
            functions,
            globals,
        }
    }

    fn translate_typedef(ty: &wit_parser::TypeDef) -> Type {
        match &ty.kind {
            TypeDefKind::Type(t) => Type::Primitive(translate_type(t)),
            TypeDefKind::Record(r) => Type::Record {
                fields: r
                    .fields
                    .iter()
                    .map(|f| (f.name.clone(), translate_type(&f.ty)))
                    .collect(),
            },
            TypeDefKind::Tuple(t) => Type::Tuple {
                types: t.types.iter().map(translate_type).collect(),
            },
            TypeDefKind::Flags(r) => Type::Flags {
                flags: r.flags.iter().map(|f| f.name.clone()).collect(),
            },
            TypeDefKind::Enum(r) => Type::Enum {
                cases: r.cases.iter().map(|f| f.name.clone()).collect(),
            },
            TypeDefKind::Variant(v) => Type::Variant {
                cases: v
                    .cases
                    .iter()
                    .map(|f| (f.name.clone(), translate_optional_type(f.ty.as_ref())))
                    .collect(),
            },
            TypeDefKind::Option(t) => Type::Option(translate_type(t)),
            TypeDefKind::Result(r) => Type::Result {
                ok: translate_optional_type(r.ok.as_ref()),
                err: translate_optional_type(r.err.as_ref()),
            },
            TypeDefKind::Future(t) => Type::Future(translate_optional_type(t.as_ref())),
            TypeDefKind::Stream(s) => Type::Stream {
                element: translate_optional_type(s.element.as_ref()),
                end: translate_optional_type(s.end.as_ref()),
            },
            TypeDefKind::List(ty) => Type::List(translate_type(ty)),
            TypeDefKind::Union(u) => Type::Union {
                cases: u.cases.iter().map(|c| translate_type(&c.ty)).collect(),
            },
        }
    }

    fn translate_type(ty: &wit_parser::Type) -> String {
        use wit_parser::Type;
        match ty {
            Type::Bool => "bool".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::S8 => "s8".to_string(),
            Type::S16 => "s16".to_string(),
            Type::S32 => "s32".to_string(),
            Type::S64 => "s64".to_string(),
            Type::Float32 => "float32".to_string(),
            Type::Float64 => "float64".to_string(),
            Type::Char => "char".to_string(),
            Type::String => "string".to_string(),
            Type::Id(id) => format!("type-{}", id.index()),
        }
    }

    fn translate_optional_type(ty: Option<&wit_parser::Type>) -> Option<String> {
        ty.map(translate_type)
    }
}

fn test_document(document: &Document) {
    for interface in &document.interfaces {
        test_interface(interface);
    }

    for world in &document.worlds {
        test_world(world);
    }
}

fn test_world(world: &World) {
    for (_, interface) in world.imports.iter() {
        test_interface(interface);
    }
    for (_, interface) in world.exports.iter() {
        test_interface(interface);
    }
    if let Some(default) = &world.default {
        test_interface(default);
    }
}

fn test_interface(interface: &Interface) {
    let mut sizes = SizeAlign::default();
    sizes.fill(interface);
}
