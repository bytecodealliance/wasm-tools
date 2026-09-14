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
            "echo-u32-u64" | "echo-u32-f32" | "echo-u32-f64" | "echo-u64-f64" | "echo-u64-f32" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                let Val::Result(e) = args.next().unwrap() else {
                    panic!()
                };
                Some(Val::Result(e))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
