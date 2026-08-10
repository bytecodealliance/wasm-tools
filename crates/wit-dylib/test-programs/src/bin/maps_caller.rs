use std::collections::BTreeMap;
use test_programs::*;

export_test!(struct MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        wit: Wit,
        func: ExportFunction,
        args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), None);
        assert_eq!(func.name(), "run");
        assert_eq!(func.params().len(), 0);
        assert!(func.result().is_none());
        assert_eq!(args.len(), 0);

        {
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-u8-string",
                &[Val::Map(BTreeMap::new())],
            );
            assert_eq!(ret, Some(Val::Map(BTreeMap::new())));

            let map = b"abc"
                .iter()
                .map(|&v| (Key::U8(v), Val::String(v.to_string())))
                .collect::<BTreeMap<_, _>>();
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-u8-string",
                &[Val::Map(map.clone())],
            );
            assert_eq!(ret, Some(Val::Map(map)));

            let map = b"abc"
                .iter()
                .map(|&v| (Key::U8(v), Val::String("42".into())))
                .collect::<BTreeMap<_, _>>();
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-u8-string",
                &[Val::Map(map.clone())],
            );
            assert_eq!(ret, Some(Val::Map(map)));
        }

        {
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-u32-string",
                &[Val::Map(BTreeMap::new())],
            );
            assert_eq!(ret, Some(Val::Map(BTreeMap::new())));

            let map = [1, 2]
                .iter()
                .map(|&v| (Key::U32(v), Val::String(v.to_string())))
                .collect::<BTreeMap<_, _>>();
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-u32-string",
                &[Val::Map(map.clone())],
            );
            assert_eq!(ret, Some(Val::Map(map)));
        }

        {
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-string-u8",
                &[Val::Map(BTreeMap::new())],
            );
            assert_eq!(ret, Some(Val::Map(BTreeMap::new())));

            let map = [1, 2]
                .iter()
                .map(|&v| (Key::String(v.to_string()), Val::U8(v)))
                .collect::<BTreeMap<_, _>>();
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-string-u8",
                &[Val::Map(map.clone())],
            );
            assert_eq!(ret, Some(Val::Map(map)));
        }

        {
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "map-of-variants",
                &[Val::Map(
                    [
                        (
                            Key::U8(42),
                            Val::Map([(Key::U8(42), Val::Option(None))].into_iter().collect()),
                        ),
                        (
                            Key::U8(43),
                            Val::Map([(Key::U8(42), Val::Option(None))].into_iter().collect()),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                )],
            );
            assert_eq!(ret, None);
        }

        None
    }
}
