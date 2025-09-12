use test_programs::test_util::*;
use test_programs::*;

struct MyInterpreter;

export!(MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(_wit: Wit, func: Function, mut args: OwnVals<'_, Self>) -> Option<Box<Val>> {
        assert_eq!(func.interface(), Some("a:b/x"));
        match func.name() {
            "set" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_none());
                assert_eq!(args.len(), 1);
                assert_eq!(
                    args.next(),
                    Some(Box::new(Val::String("hello".to_string())))
                );
                None
            }
            "get" => {
                assert_eq!(func.params().len(), 0);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 0);
                assert_eq!(args.next(), None);
                Some(Box::new(Val::String("world".to_string())))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
