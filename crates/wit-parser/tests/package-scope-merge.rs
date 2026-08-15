use wit_parser::{CloneMaps, Resolve, TypeOwner};

#[test]
fn package_scope_types_survive_merge() {
    let mut a = Resolve::new();
    let pkg_a = a
        .push_str(
            "a.wit",
            r#"
package local:a;

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
"#,
        )
        .unwrap();

    let mut b = Resolve::new();
    let pkg_b = b
        .push_str(
            "b.wit",
            r#"
package local:b;

enum direction {
  north,
  south,
}

interface api {
  heading: func() -> direction;
}

world w {
  export api;
}
"#,
        )
        .unwrap();

    let remap = a.merge(b).unwrap();
    let pkg_b = remap.packages[pkg_b.index()];

    assert_eq!(a.packages[pkg_a].types.len(), 1);
    assert!(a.packages[pkg_a].types.contains_key("point"));
    assert_eq!(a.packages[pkg_b].types.len(), 1);
    assert!(a.packages[pkg_b].types.contains_key("direction"));

    for (name, &id) in a.packages[pkg_a].types.iter() {
        assert_eq!(a.types[id].owner, TypeOwner::Package(pkg_a));
        assert_eq!(a.types[id].name.as_deref(), Some(name.as_str()));
    }
    for (name, &id) in a.packages[pkg_b].types.iter() {
        assert_eq!(a.types[id].owner, TypeOwner::Package(pkg_b));
        assert_eq!(a.types[id].name.as_deref(), Some(name.as_str()));
    }

    a.assert_valid();
}

#[test]
fn package_scope_types_merge_into_same_package() {
    let mut into = Resolve::new();
    into.push_str(
        "into.wit",
        r#"
package local:shared;

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
"#,
    )
    .unwrap();

    let mut from = Resolve::new();
    from.push_str(
        "from.wit",
        r#"
package local:shared;

record point {
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
"#,
    )
    .unwrap();

    into.merge(from).unwrap();
    let pkg = *into.package_names.values().next().unwrap();
    assert!(into.packages[pkg].types.contains_key("point"));
    assert!(into.packages[pkg].types.contains_key("style"));
    into.assert_valid();
}

#[test]
fn package_scope_types_survive_merge_worlds() {
    let mut resolve = Resolve::new();
    let pkg = resolve
        .push_str(
            "demo.wit",
            r#"
package local:demo;

record point {
  x: u32,
  y: u32,
}

interface a {
  move-to: func(p: point);
}

interface b {
  place: func(p: point);
}

world wa {
  export a;
}

world wb {
  export b;
}
"#,
        )
        .unwrap();

    let wa = resolve.packages[pkg].worlds["wa"];
    let wb = resolve.packages[pkg].worlds["wb"];
    let point = resolve.packages[pkg].types["point"];

    resolve
        .merge_worlds(wb, wa, &mut CloneMaps::default())
        .unwrap();

    assert_eq!(resolve.packages[pkg].types["point"], point);
    assert_eq!(resolve.types[point].owner, TypeOwner::Package(pkg));
    assert_eq!(resolve.packages[pkg].types.len(), 1);
    resolve.assert_valid();
}

#[test]
fn package_scope_type_vs_interface_merge_clash() {
    let mut into = Resolve::new();
    into.push_str(
        "into.wit",
        r#"
package local:shared;

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
"#,
    )
    .unwrap();

    let mut from = Resolve::new();
    from.push_str(
        "from.wit",
        r#"
package local:shared;

interface point {
  get: func() -> u32;
}

world w {
  export point;
}
"#,
    )
    .unwrap();

    let err = match into.merge(from) {
        Ok(_) => panic!("expected merge to fail on type vs interface name clash"),
        Err(e) => e,
    };
    let msg = format!("{err:#}");
    assert!(
        msg.contains("point"),
        "expected NS clash mentioning `point`, got: {msg}"
    );
}
