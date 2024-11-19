use anyhow::Result;
use pretty_assertions::assert_eq;
use wit_encoder::{packages_from_parsed, Package};
use wit_parser::Resolve;

const MAIN_PACKAGE_SOURCE: &str = indoc::indoc! {"
    package foo:main;

    interface main-interface-a {
      use foo:dep-a/dep-a-interface-b.{ ra };
    }

    interface main-interface-b {
        record mb {
            x: f32,
        }
    }

    interface main-interface-c {}

    interface main-interface-d {}

    interface main-interface-e {}

    world main-world-a {
      use foo:dep-c/dep-c-interface-b.{ cb, cb as cbcb };
      import foo:dep-c/dep-c-interface-a;
      export foo:dep-c/dep-c-interface-b;
      include foo:dep-c/dep-c-world-a;

      use foo:dep-b/dep-b-interface-b@1.2.3.{ bb, bb as bbbb };
      import foo:dep-b/dep-b-interface-a@1.2.3;
      export foo:dep-b/dep-b-interface-b@1.2.3;
      include foo:dep-b/dep-b-world-b@1.2.3;

      use main-interface-b.{ mb, mb as mbmb };
      import main-interface-b;
      export main-interface-a;
      include main-world-b;
    }

    world main-world-b {
      export main-interface-d;
      import main-interface-d;
    }
"};

const MAIN_PACKAGE_RESOLVED_ENCODED: &str = indoc::indoc! {"
    package foo:main;

    interface main-interface-a {
      use foo:dep-a/dep-a-interface-b.{ ra };
    }

    interface main-interface-b {
      record mb {
        x: f32,
      }
    }

    interface main-interface-c {}

    interface main-interface-d {}

    interface main-interface-e {}

    world main-world-b {
      import main-interface-d;
      export main-interface-d;
    }

    world main-world-a {
      use foo:dep-b/dep-b-interface-b@1.2.3.{ bb, bb as bbbb };
      use foo:dep-c/dep-c-interface-b.{ cb, cb as cbcb };
      use main-interface-b.{ mb, mb as mbmb };
      import foo:dep-a/dep-a-interface-b;
      import foo:dep-b/dep-b-interface-a@1.2.3;
      import foo:dep-b/dep-b-interface-b@1.2.3;
      import foo:dep-c/dep-c-interface-a;
      import foo:dep-c/dep-c-interface-b;
      import foo:dep-c/dep-c-interface-c;
      import main-interface-b;
      import main-interface-d;
      export foo:dep-b/dep-b-interface-b@1.2.3;
      export foo:dep-c/dep-c-interface-b;
      export foo:dep-c/dep-c-interface-d;
      export main-interface-a;
      export main-interface-d;
    }
"};

const DEP_PACKAGE_A: &str = indoc::indoc! {"
    package foo:dep-a;

    interface dep-a-interface-b {
      use foo:dep-b/dep-b-interface-a@1.2.3.{a};
      type ra = a;
    }

    world dep-a-world {}
"};

const DEP_PACKAGE_B: &str = indoc::indoc! {"
    package foo:dep-b@1.2.3;

    interface dep-b-interface-a {
      record a {
        x: f32,
        y: f32,
      }
    }

    interface dep-b-interface-b {
      use foo:dep-c/dep-c-interface-a.{a};

      record bb {
        x: f32,
      }
    }

    world dep-b-world-a {
      export dep-b-interface-a;
    }

    world dep-b-world-b {}
"};

const DEP_PACKAGE_C: &str = indoc::indoc! {"
    package foo:dep-c;

    interface dep-c-interface-a {
      record a {
        x: f32,
        y: f32,
      }
    }

    interface dep-c-interface-b {
      record cb {
        x: f32,
      }
    }

    interface dep-c-interface-c {}

    interface dep-c-interface-d {}

    world dep-c-world-a {
      import dep-c-interface-c;
      export dep-c-interface-d;
    }
"};

#[test]
fn resolve_encode_resolve_round_trip_with_use_and_include() {
    let packages = packages_from_parsed(
        &resolve_packages(&[
            ("dep_c.wit", DEP_PACKAGE_C),
            ("dep_b.wit", DEP_PACKAGE_B),
            ("dep_a.wit", DEP_PACKAGE_A),
            ("main_foo.wit", MAIN_PACKAGE_SOURCE),
        ])
        .unwrap(),
    );

    assert_eq!(packages.len(), 4);

    assert_package_names(
        &packages,
        &["foo:dep-c", "foo:dep-b@1.2.3", "foo:dep-a", "foo:main"],
    );

    // Resolve should still work after rendering the encoded packages
    let packages = packages_from_parsed(
        &resolve_packages(&[
            ("dep_c.wit", &packages[0].to_string()),
            ("dep_b.wit", &packages[1].to_string()),
            ("dep_a.wit", &packages[2].to_string()),
            ("main_foo.wit", &packages[3].to_string()),
        ])
        .unwrap(),
    );

    // Check that no package is lost
    assert_package_names(
        &packages,
        &["foo:dep-c", "foo:dep-b@1.2.3", "foo:dep-a", "foo:main"],
    );

    // Check for the encoded main wit
    assert_eq!(MAIN_PACKAGE_RESOLVED_ENCODED, &packages[3].to_string());
}

fn resolve_packages(packages: &[(&str, &str)]) -> Result<Resolve> {
    let mut resolve = Resolve::new();
    for (name, wit) in packages {
        resolve.push_str(name, wit)?;
    }
    Ok(resolve)
}

// The order of converted packages currently follows the order of pushes to the parser's package arena.
// If this changes, then explicit sorting or search should be added to these tests
fn assert_package_names(packages: &[Package], package_names: &[&str]) {
    assert_eq!(
        packages
            .iter()
            .map(|p| p.name().to_string())
            .collect::<Vec<_>>(),
        package_names
    );
}
