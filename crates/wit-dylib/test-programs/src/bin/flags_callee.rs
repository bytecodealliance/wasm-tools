use test_programs::*;

export_test!(struct MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        _wit: Wit,
        func: Function,
        mut args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), Some("a:b/x"));
        match func.name() {
            "invert8" | "invert16" | "invert32" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                let Val::Flags(flags) = args.next().unwrap() else {
                    panic!()
                };
                Some(Val::Flags(!flags))
            }
            "ret3-v1" => {
                assert_eq!(func.params().len(), 0);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 0);
                Some(Val::Tuple(vec![
                    Val::Flags(0x12),
                    Val::Flags(0x3456),
                    Val::Flags(0x789abcde),
                ]))
            }
            "ret3-v2" => {
                assert_eq!(func.params().len(), 0);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 0);
                Some(Val::Tuple(vec![
                    Val::Flags(0x789abcde),
                    Val::Flags(0x3456),
                    Val::Flags(0x12),
                ]))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
