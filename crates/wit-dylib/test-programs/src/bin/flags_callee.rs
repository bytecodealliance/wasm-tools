use test_programs::test_util::*;
use test_programs::*;

struct MyInterpreter;

export!(MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(_wit: Wit, func: Function, mut args: OwnVals<'_, Self>) -> Option<Box<Val>> {
        assert_eq!(func.interface(), Some("a:b/x"));
        match func.name() {
            "invert8" | "invert16" | "invert32" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                let Val::Flags(flags) = *args.next().unwrap() else {
                    panic!()
                };
                Some(Box::new(Val::Flags(!flags)))
            }
            "ret3-v1" => {
                assert_eq!(func.params().len(), 0);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 0);
                Some(Box::new(Val::Tuple(vec![
                    Box::new(Val::Flags(0x12)),
                    Box::new(Val::Flags(0x3456)),
                    Box::new(Val::Flags(0x789abcde)),
                ])))
            }
            "ret3-v2" => {
                assert_eq!(func.params().len(), 0);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 0);
                Some(Box::new(Val::Tuple(vec![
                    Box::new(Val::Flags(0x789abcde)),
                    Box::new(Val::Flags(0x3456)),
                    Box::new(Val::Flags(0x12)),
                ])))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
