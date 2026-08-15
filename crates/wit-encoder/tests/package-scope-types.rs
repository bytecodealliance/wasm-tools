use pretty_assertions::assert_eq;
use wit_encoder::packages_from_parsed;

const WIT: &str = r#"package local:demo;

/// Shared point.
record point {
  /// X coordinate
  x: u32,
  y: u32,
}

flags style {
  bold,
  italic,
}

interface api {
  move-to: func(p: point);
  style: func() -> style;
}

world w {
  export api;
}
"#;

#[test]
fn package_scope_types_round_trip() {
    let mut resolve = wit_parser::Resolve::new();
    resolve.push_str("demo.wit", WIT).unwrap();
    let packages = packages_from_parsed(&resolve);
    assert_eq!(packages.len(), 1);
    let rendered = packages[0].to_string();
    assert!(
        rendered.contains("record point"),
        "expected package-scope record in encoder output:\n{rendered}"
    );
    assert!(
        rendered.contains("flags style"),
        "expected package-scope flags in encoder output:\n{rendered}"
    );

    let mut resolve2 = wit_parser::Resolve::new();
    resolve2.push_str("demo.wit", &rendered).unwrap();
    let packages2 = packages_from_parsed(&resolve2);
    assert_eq!(packages[0].to_string(), packages2[0].to_string());
}

#[test]
fn package_scope_types_order() {
    let mut resolve = wit_parser::Resolve::new();
    resolve.push_str("demo.wit", WIT).unwrap();
    let packages = packages_from_parsed(&resolve);
    let rendered = packages[0].to_string();

    let point = rendered.find("record point").expect("missing point");
    let style = rendered.find("flags style").expect("missing style");
    let api = rendered.find("interface api").expect("missing api");
    let world = rendered.find("world w").expect("missing world");
    assert!(
        point < style && style < api && api < world,
        "expected package types before interfaces/worlds:\n{rendered}"
    );
}

#[test]
fn package_scope_foreign_compose_round_trip() {
    let mut resolve = wit_parser::Resolve::new();
    resolve
        .push_str(
            "types.wit",
            r#"
package local:types;

record point {
  x: u32,
  y: u32,
}

interface unused {}
"#,
        )
        .unwrap();
    resolve
        .push_str(
            "consumer.wit",
            r#"
package local:consumer;

use local:types/point;

record bin {
  p: point,
}

interface api {
  wrap: func(b: bin);
}

world w {
  export api;
}
"#,
        )
        .unwrap();

    let packages = packages_from_parsed(&resolve);
    let consumer = packages
        .iter()
        .find(|p| p.name().to_string() == "local:consumer")
        .expect("consumer package");
    let rendered = consumer.to_string();
    assert!(
        rendered.contains("use local:types/point;"),
        "expected toplevel use of foreign package type:\n{rendered}"
    );
    assert!(
        rendered.contains("record bin"),
        "expected consumer package-scope type:\n{rendered}"
    );

    // Push deps first, then consumer.
    let mut resolve2 = wit_parser::Resolve::new();
    let types = packages
        .iter()
        .find(|p| p.name().to_string() == "local:types")
        .unwrap();
    resolve2
        .push_str("types.wit", &types.to_string())
        .unwrap();
    resolve2
        .push_str("consumer.wit", &rendered)
        .unwrap();
    let packages2 = packages_from_parsed(&resolve2);
    let rendered2 = packages2
        .iter()
        .find(|p| p.name().to_string() == "local:consumer")
        .unwrap()
        .to_string();
    assert_eq!(rendered, rendered2);
}

#[test]
fn package_scope_nested_round_trip() {
    let wit = r#"package local:nested;

record point {
  x: u32,
  y: u32,
}

interface api {
  move-to: func(p: point);
}

world w {
  export api;
}
"#;
    let mut resolve = wit_parser::Resolve::new();
    resolve.push_str("nested.wit", wit).unwrap();
    let packages = packages_from_parsed(&resolve);
    assert_eq!(packages.len(), 1);
    let rendered = packages[0].to_string();
    assert!(
        rendered.contains("record point"),
        "expected nested package-scope type:\n{rendered}"
    );

    let mut resolve2 = wit_parser::Resolve::new();
    resolve2.push_str("nested.wit", &rendered).unwrap();
    let packages2 = packages_from_parsed(&resolve2);
    assert_eq!(packages[0].to_string(), packages2[0].to_string());
}

#[test]
fn package_scope_foreign_iface_only_round_trip() {
    let mut resolve = wit_parser::Resolve::new();
    resolve
        .push_str(
            "types.wit",
            r#"
package local:types;

record point {
  x: u32,
  y: u32,
}

interface unused {}
"#,
        )
        .unwrap();
    resolve
        .push_str(
            "consumer.wit",
            r#"
package local:consumer;

use local:types/point;

interface api {
  move-to: func(p: point);
}

world w {
  export api;
}
"#,
        )
        .unwrap();

    let packages = packages_from_parsed(&resolve);
    let consumer = packages
        .iter()
        .find(|p| p.name().to_string() == "local:consumer")
        .expect("consumer package");
    let rendered = consumer.to_string();
    assert!(
        rendered.contains("use local:types/point;"),
        "iface-only foreign refs must still emit toplevel use:\n{rendered}"
    );
    assert!(
        !rendered.contains("record point"),
        "consumer should not redefine foreign package type:\n{rendered}"
    );

    let mut resolve2 = wit_parser::Resolve::new();
    let types = packages
        .iter()
        .find(|p| p.name().to_string() == "local:types")
        .unwrap();
    resolve2
        .push_str("types.wit", &types.to_string())
        .unwrap();
    resolve2
        .push_str("consumer.wit", &rendered)
        .unwrap();
    let packages2 = packages_from_parsed(&resolve2);
    let rendered2 = packages2
        .iter()
        .find(|p| p.name().to_string() == "local:consumer")
        .unwrap()
        .to_string();
    assert_eq!(rendered, rendered2);
}

#[test]
fn package_scope_foreign_world_only_round_trip() {
    let mut resolve = wit_parser::Resolve::new();
    resolve
        .push_str(
            "types.wit",
            r#"
package local:types;

record point {
  x: u32,
  y: u32,
}

interface unused {}
"#,
        )
        .unwrap();
    resolve
        .push_str(
            "consumer.wit",
            r#"
package local:consumer;

use local:types/point;

world w {
  import move-to: func(p: point);
  export place: func(p: point);
}
"#,
        )
        .unwrap();

    let packages = packages_from_parsed(&resolve);
    let consumer = packages
        .iter()
        .find(|p| p.name().to_string() == "local:consumer")
        .expect("consumer package");
    let rendered = consumer.to_string();
    assert!(
        rendered.contains("use local:types/point;"),
        "world-only foreign refs must still emit toplevel use:\n{rendered}"
    );

    let mut resolve2 = wit_parser::Resolve::new();
    let types = packages
        .iter()
        .find(|p| p.name().to_string() == "local:types")
        .unwrap();
    resolve2
        .push_str("types.wit", &types.to_string())
        .unwrap();
    resolve2
        .push_str("consumer.wit", &rendered)
        .unwrap();
    let packages2 = packages_from_parsed(&resolve2);
    assert_eq!(
        consumer.to_string(),
        packages2
            .iter()
            .find(|p| p.name().to_string() == "local:consumer")
            .unwrap()
            .to_string()
    );
}
