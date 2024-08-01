use pretty_assertions::assert_eq;
use wit_encoder::packages_from_parsed;

const WIT: &str = r#"package foo:functions;

interface foo-interface {
  flags %flags {
    flag-a,
    flag-b,
  }
}

interface bar-interface {
  use foo-interface.{ %flags as external-type };
  enum some-enum {
    first-case,
    second-case,
  }
}

interface %interface {
  use bar-interface.{ some-enum, external-type };
  statndalone-func: func(a: u32, b: s32) -> f32;
  variant %variant {
    empty-case,
    valued-case(u32),
  }
  resource %resource {
    constructor();
    method: func(arg: list<u32>) -> char;
    static-method: static func(arg: tuple<u32, u32>) -> list<u32>;
  }
  resource other-resource {
    constructor(values: list<u32>);
    static-method: func(arg: tuple<u32, u32>);
  }
  record some-record {
    optinal: option<string>,
    %own: %resource,
    %borrow: borrow<%resource>,
    result-a: result,
    result-b: result<f64>,
    result-c: result<_, f64>,
    result-d: result<f64, f32>,
    result-e: result<string, list<u8>>,
  }
  type type-list = list<f64>;
  type type-option = option<f64>;
  type type-result-a = result;
  type type-result-b = result<f64>;
  type type-result-c = result<_, f64>;
  type type-result-d = result<f64, f32>;
  type type-result-e = result<string, list<u8>>;
  type type-handle-own = %resource;
  type type-handle-borrow = borrow<%resource>;
  type type-tuple = tuple<u32, list<u32>, borrow<%resource>>;
}

world my-world {
  import foo-interface;
  import bar-interface;
  import %interface;
  import inline-import: interface {
    do-something: func(a: string) -> string;
  }
  import import-func: func();
  export export-func: func(a: u32, b: s32) -> f32;
  export %interface;
  export inline-export: interface {
    do-nothing: func();
  }
}
"#;

#[test]
fn round_trip() {
    let mut resolve = wit_parser::Resolve::new();
    resolve.push_str("", WIT).unwrap();
    let packages = packages_from_parsed(&resolve);

    assert!(packages.len() == 1, "Should create exactly one package");
    let package = &packages[0];
    assert_eq!(WIT, package.to_string());
}
