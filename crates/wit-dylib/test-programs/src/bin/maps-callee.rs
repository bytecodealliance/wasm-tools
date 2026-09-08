use test_programs::*;

export_test!(struct MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        _wit: Wit,
        func: ExportFunction,
        mut args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), Some("a:b/x"));
        match func.name() {
            name if name.starts_with("echo-") => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                let arg = args.next().unwrap();
                match arg {
                    Val::Map(_) => {}
                    _ => panic!(),
                }
                Some(arg)
            }

            "map-of-variants" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_none());
                assert_eq!(args.len(), 1);
                let arg = args.next().unwrap();
                assert_eq!(
                    arg,
                    Val::Map(
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
                    )
                );
                None
            }

            other => panic!("unknown function {other:?}"),
        }
    }
}
