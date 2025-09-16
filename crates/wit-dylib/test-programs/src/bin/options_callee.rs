use test_programs::test_util::*;
use test_programs::*;

struct MyInterpreter;

export!(MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        _wit: Wit,
        func: Function,
        mut args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), Some("a:b/x"));
        match func.name() {
            "echo-u32" | "echo-u64" | "echo-f32" | "echo-f64" | "echo-string" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                let Val::Option(e) = args.next().unwrap() else {
                    panic!()
                };
                Some(Val::Option(e))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
