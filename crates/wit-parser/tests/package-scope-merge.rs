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

/// Two packages may each declare a package-scope type under the same local
/// name. Merging their worlds does not unify those types: a package-scope name
/// is unique within its package, so the two stay distinct and keep their own
/// owners.
#[test]
fn package_scope_same_local_name_survives_merge_worlds() {
    let mut resolve = Resolve::new();
    let pkg1 = resolve
        .push_str(
            "b1.wit",
            r#"
package a:b1;

record r {
  a: u32,
}

world w1 {
  export f1: func() -> r;
}
"#,
        )
        .unwrap();
    let pkg2 = resolve
        .push_str(
            "b2.wit",
            r#"
package a:b2;

record r {
  a: f32,
}

world w2 {
  export f2: func() -> r;
}
"#,
        )
        .unwrap();

    let w1 = resolve.packages[pkg1].worlds["w1"];
    let w2 = resolve.packages[pkg2].worlds["w2"];
    let r1 = resolve.packages[pkg1].types["r"];
    let r2 = resolve.packages[pkg2].types["r"];
    assert_ne!(r1, r2);

    resolve
        .merge_worlds(w2, w1, &mut CloneMaps::default())
        .unwrap();

    assert_eq!(resolve.packages[pkg1].types["r"], r1);
    assert_eq!(resolve.packages[pkg2].types["r"], r2);
    assert_eq!(resolve.types[r1].owner, TypeOwner::Package(pkg1));
    assert_eq!(resolve.types[r2].owner, TypeOwner::Package(pkg2));

    let f1 = match &resolve.worlds[w1].exports[&wit_parser::WorldKey::Name("f1".into())] {
        wit_parser::WorldItem::Function(f) => f,
        other => panic!("expected f1 to be a function, got {other:?}"),
    };
    let f2 = match &resolve.worlds[w1].exports[&wit_parser::WorldKey::Name("f2".into())] {
        wit_parser::WorldItem::Function(f) => f,
        other => panic!("expected f2 to be a function, got {other:?}"),
    };
    assert_eq!(f1.result, Some(wit_parser::Type::Id(r1)));
    assert_eq!(f2.result, Some(wit_parser::Type::Id(r2)));
    resolve.assert_valid();
}

/// When both worlds export the same kebab name, the shared item is merged
/// rather than added, and `MergeMap::build_type_id` does not compare type
/// structure (see its FIXME). So two `export f: func() -> r` worlds whose `r`
/// differs merge without error and `into`'s type wins. This is pre-existing
/// behavior for any named type, not specific to package scope; the test pins it
/// down so a future structural check is a deliberate change.
#[test]
fn package_scope_same_export_name_keeps_into_type_on_merge_worlds() {
    let mut resolve = Resolve::new();
    let pkg1 = resolve
        .push_str(
            "b1.wit",
            r#"
package a:b1;

record r {
  a: u32,
}

world w1 {
  export f: func() -> r;
}
"#,
        )
        .unwrap();
    let pkg2 = resolve
        .push_str(
            "b2.wit",
            r#"
package a:b2;

record r {
  a: f32,
}

world w2 {
  export f: func() -> r;
}
"#,
        )
        .unwrap();

    let w1 = resolve.packages[pkg1].worlds["w1"];
    let w2 = resolve.packages[pkg2].worlds["w2"];
    let r1 = resolve.packages[pkg1].types["r"];
    let r2 = resolve.packages[pkg2].types["r"];

    resolve
        .merge_worlds(w2, w1, &mut CloneMaps::default())
        .unwrap();

    // `f` already existed in `w1`, so it was merged, not added, and kept
    // pointing at `a:b1/r`. Both package-scope types still exist separately.
    let f = match &resolve.worlds[w1].exports[&wit_parser::WorldKey::Name("f".into())] {
        wit_parser::WorldItem::Function(f) => f,
        other => panic!("expected f to be a function, got {other:?}"),
    };
    assert_eq!(f.result, Some(wit_parser::Type::Id(r1)));
    assert_eq!(resolve.packages[pkg1].types["r"], r1);
    assert_eq!(resolve.packages[pkg2].types["r"], r2);
    assert_eq!(resolve.types[r1].owner, TypeOwner::Package(pkg1));
    assert_eq!(resolve.types[r2].owner, TypeOwner::Package(pkg2));
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
