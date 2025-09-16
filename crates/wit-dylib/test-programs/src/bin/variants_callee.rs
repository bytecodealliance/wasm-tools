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
            "echo-v1" | "echo-v2" | "echo-v3" | "echo-v4" | "echo-v5" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                let Val::Variant(discr, payload) = args.next().unwrap() else {
                    panic!()
                };
                Some(Val::Variant(discr, payload))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
