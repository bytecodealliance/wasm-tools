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
            "echo-a" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                assert_eq!(args.next(), Some(Val::Record(vec![Val::U32(1)])));
                Some(Val::Record(vec![Val::U32(2)]))
            }
            "echo-b" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                assert_eq!(
                    args.next(),
                    Some(Val::Record(vec![Val::String("hello".to_string())]))
                );
                Some(Val::Record(vec![Val::String("world".to_string())]))
            }
            "echo-c" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                assert_eq!(
                    args.next(),
                    Some(Val::Record(vec![Val::F32(1.0), Val::S64(2),]))
                );
                Some(Val::Record(vec![Val::F32(3.0), Val::S64(4)]))
            }
            "echo-d" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                let d = Val::Record(vec![
                    Val::String("a1".to_string()),
                    Val::String("a2".to_string()),
                    Val::String("a3".to_string()),
                    Val::String("a4".to_string()),
                ]);
                assert_eq!(
                    args.next(),
                    Some(Val::Record(vec![
                        d.clone(),
                        d.clone(),
                        d.clone(),
                        d.clone(),
                    ]))
                );
                Some(Val::Record(vec![
                    d.clone(),
                    d.clone(),
                    d.clone(),
                    d.clone(),
                ]))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
