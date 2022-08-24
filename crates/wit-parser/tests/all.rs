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

        let result = Interface::parse_file(test);

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
            let instance = result?;
            to_json(&instance)
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
                &test.display().to_string().replace("\\", "/"),
            )
            .replace("\\parse-fail\\", "/parse-fail/")
            .replace("\r\n", "\n")
        }
    }

    fn bump_ntests(&self) {
        self.ntests.fetch_add(1, SeqCst);
    }
}

fn to_json(i: &Interface) -> String {
    #[derive(Serialize)]
    struct Interface {
        #[serde(skip_serializing_if = "Vec::is_empty")]
        resources: Vec<Resource>,
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
        Record { fields: Vec<(String, String)> },
        Flags { flags: Vec<String> },
        Enum { cases: Vec<String> },
        Variant { cases: Vec<(String, String)> },
        Tuple { types: Vec<String> },
        Option(String),
        Expected { ok: String, err: String },
        Future(String),
        Stream { element: String, end: String },
        List(String),
        Union { cases: Vec<String> },
    }

    #[derive(Serialize)]
    struct Function {
        name: String,
        #[serde(rename = "async", skip_serializing_if = "Option::is_none")]
        is_async: Option<bool>,
        params: Vec<String>,
        result: String,
    }

    #[derive(Serialize)]
    struct Global {
        name: String,
        ty: String,
    }

    let resources = i
        .resources
        .iter()
        .map(|(_, r)| Resource {
            name: r.name.clone(),
            supertype: r.supertype.as_ref().map(|supertype| supertype.clone()),
            foreign_module: r.foreign_module.clone(),
        })
        .collect::<Vec<_>>();

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
            is_async: if f.is_async { Some(f.is_async) } else { None },
            params: f.params.iter().map(|(_, ty)| translate_type(ty)).collect(),
            result: translate_type(&f.result),
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

    let iface = Interface {
        resources,
        types,
        functions,
        globals,
    };
    return serde_json::to_string_pretty(&iface).unwrap();

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
                types: t.types.iter().map(|ty| translate_type(ty)).collect(),
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
                    .map(|f| (f.name.clone(), translate_type(&f.ty)))
                    .collect(),
            },
            TypeDefKind::Option(t) => Type::Option(translate_type(t)),
            TypeDefKind::Expected(e) => Type::Expected {
                ok: translate_type(&e.ok),
                err: translate_type(&e.err),
            },
            TypeDefKind::Future(t) => Type::Future(translate_type(t)),
            TypeDefKind::Stream(s) => Type::Stream {
                element: translate_type(&s.element),
                end: translate_type(&s.end),
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
            Type::Unit => format!("unit"),
            Type::Bool => format!("bool"),
            Type::U8 => format!("u8"),
            Type::U16 => format!("u16"),
            Type::U32 => format!("u32"),
            Type::U64 => format!("u64"),
            Type::S8 => format!("s8"),
            Type::S16 => format!("s16"),
            Type::S32 => format!("s32"),
            Type::S64 => format!("s64"),
            Type::Float32 => format!("float32"),
            Type::Float64 => format!("float64"),
            Type::Char => format!("char"),
            Type::String => format!("string"),
            Type::Handle(resource) => format!("handle-{}", resource.index()),
            Type::Id(id) => format!("type-{}", id.index()),
        }
    }
}
