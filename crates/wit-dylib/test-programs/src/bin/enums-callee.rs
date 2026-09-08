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
            "echo-e1" | "echo-e2" | "echo-e3" | "echo-e257" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                let Val::Enum(e) = args.next().unwrap() else {
                    panic!()
                };
                Some(Val::Enum(e))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
