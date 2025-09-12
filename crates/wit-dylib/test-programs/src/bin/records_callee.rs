use test_programs::test_util::*;
use test_programs::*;

struct MyInterpreter;

export!(MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(_wit: Wit, func: Function, mut args: OwnVals<'_, Self>) -> Option<Box<Val>> {
        assert_eq!(func.interface(), Some("a:b/x"));
        match func.name() {
            "echo-a" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                assert_eq!(
                    args.next(),
                    Some(Box::new(Val::Record(vec![Box::new(Val::U32(1))])))
                );
                Some(Box::new(Val::Record(vec![Box::new(Val::U32(2))])))
            }
            "echo-b" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                assert_eq!(
                    args.next(),
                    Some(Box::new(Val::Record(vec![Box::new(Val::String(
                        "hello".to_string()
                    ))])))
                );
                Some(Box::new(Val::Record(vec![Box::new(Val::String(
                    "world".to_string(),
                ))])))
            }
            "echo-c" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                assert_eq!(
                    args.next(),
                    Some(Box::new(Val::Record(vec![
                        Box::new(Val::F32(1.0)),
                        Box::new(Val::S64(2)),
                    ])))
                );
                Some(Box::new(Val::Record(vec![
                    Box::new(Val::F32(3.0)),
                    Box::new(Val::S64(4)),
                ])))
            }
            "echo-d" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                let d = Box::new(Val::Record(vec![
                    Box::new(Val::String("a1".to_string())),
                    Box::new(Val::String("a2".to_string())),
                    Box::new(Val::String("a3".to_string())),
                    Box::new(Val::String("a4".to_string())),
                ]));
                assert_eq!(
                    args.next(),
                    Some(Box::new(Val::Record(vec![
                        d.clone(),
                        d.clone(),
                        d.clone(),
                        d.clone(),
                    ])))
                );
                Some(Box::new(Val::Record(vec![
                    d.clone(),
                    d.clone(),
                    d.clone(),
                    d.clone(),
                ])))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
