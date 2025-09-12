use test_programs::test_util::*;
use test_programs::*;

struct MyInterpreter;

export!(MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(_wit: Wit, func: Function, mut args: OwnVals<'_, Self>) -> Option<Box<Val>> {
        assert_eq!(func.interface(), Some("a:b/x"));
        match func.name() {
            "echo-u32-u64" | "echo-u32-f32" | "echo-u32-f64" | "echo-u64-f64" | "echo-u64-f32" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                let Val::Result(e) = *args.next().unwrap() else {
                    panic!()
                };
                Some(Box::new(Val::Result(e)))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
