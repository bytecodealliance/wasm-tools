use pretty_assertions::assert_eq;
use wit_parser::Resolve;

const MAIN_PACKAGE_CASE_1: &str = indoc::indoc! {"
    package foo:main;

    interface main-interface-a {
        use foo:dep-a/dep-a-interface-b.{ra};
    }

    interface main-interface-b {
    }

    interface main-interface-c {
    }

    world main-world-a {
        import foo:dep-c/dep-c-interface-a;
        export foo:dep-c/dep-c-interface-b;
        include foo:dep-c/dep-c-world-a;

        import foo:dep-b/dep-b-interface-a@1.2.3;
        export foo:dep-b/dep-b-interface-b@1.2.3;
        include foo:dep-b/dep-b-world-b@1.2.3;

        import main-interface-b;
        export main-interface-a;
        include main-world-b;
    }

    world main-world-b {
    }
"};

const DEP_PACKAGE_A: &str = indoc::indoc! {"
    package foo:dep-a;

    interface dep-a-interface-b {
        use foo:dep-b/dep-b-interface-a@1.2.3.{a};
        type ra = a;
    }

    world dep-a-world {
    }
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
    }

    world dep-b-world-a {
        export dep-b-interface-a;
    }

    world dep-b-world-b {
    }
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
    }

    interface dep-c-interface-c {
    }

    interface dep-c-interface-d {
    }

    world dep-c-world-a {
        import dep-c-interface-c;
        export dep-c-interface-d;
    }
"};

#[test]
fn resolve_encode_resolve_round_trip_with_use_and_include() {
    let packages = {
        let mut resolve = Resolve::new();
        resolve.push_str("dep_c.wit", DEP_PACKAGE_C).unwrap();
        resolve.push_str("dep_b.wit", DEP_PACKAGE_B).unwrap();
        resolve.push_str("dep_a.wit", DEP_PACKAGE_A).unwrap();
        resolve
            .push_str("main_foo.wit", MAIN_PACKAGE_CASE_1)
            .unwrap();

        wit_encoder::packages_from_parsed(&resolve)
    };
    assert_eq!(packages.len(), 4);

    // The order of converted packages currently follows the order of pushes to the parser's package arena.
    // If this changes, then explicit sorting or search is required in this test.
    assert_eq!(packages[0].name().name().raw_name(), "dep-c");
    assert_eq!(packages[1].name().name().raw_name(), "dep-b");
    assert_eq!(packages[2].name().name().raw_name(), "dep-a");
    assert_eq!(packages[3].name().name().raw_name(), "main");

    // Resolve should still work after rendering the encoded packages
    {
        let mut resolve = Resolve::new();
        resolve
            .push_str("dep_c.wit", &packages[0].to_string())
            .unwrap();
        resolve
            .push_str("dep_b.wit", &packages[1].to_string())
            .unwrap();
        resolve
            .push_str("dep_a.wit", &packages[2].to_string())
            .unwrap();
        resolve
            .push_str("main_foo.wit", &packages[3].to_string())
            .unwrap();
    }
}
