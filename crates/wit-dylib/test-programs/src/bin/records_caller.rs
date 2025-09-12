use test_programs::test_util::*;
use test_programs::*;

struct MyInterpreter;

export!(MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(wit: Wit, func: Function, args: OwnVals<'_, Self>) -> Option<Box<Val>> {
        assert_eq!(func.interface(), None);
        assert_eq!(func.name(), "run");
        assert_eq!(func.params().len(), 0);
        assert!(func.result().is_none());
        assert_eq!(args.len(), 0);

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-a",
            &[Val::Record(vec![Box::new(Val::U32(1))])],
        );
        assert_eq!(
            ret,
            Some(Box::new(Val::Record(vec![Box::new(Val::U32(2))])))
        );

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-b",
            &[Val::Record(vec![Box::new(Val::String(
                "hello".to_string(),
            ))])],
        );
        assert_eq!(
            ret,
            Some(Box::new(Val::Record(vec![Box::new(Val::String(
                "world".to_string()
            ))])))
        );

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-c",
            &[Val::Record(vec![
                Box::new(Val::F32(1.0)),
                Box::new(Val::S64(2)),
            ])],
        );
        assert_eq!(
            ret,
            Some(Box::new(Val::Record(vec![
                Box::new(Val::F32(3.0)),
                Box::new(Val::S64(4))
            ])))
        );

        let d = Box::new(Val::Record(vec![
            Box::new(Val::String("a1".to_string())),
            Box::new(Val::String("a2".to_string())),
            Box::new(Val::String("a3".to_string())),
            Box::new(Val::String("a4".to_string())),
        ]));
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-d",
            &[Val::Record(vec![
                d.clone(),
                d.clone(),
                d.clone(),
                d.clone(),
            ])],
        );
        assert_eq!(
            ret,
            Some(Box::new(Val::Record(vec![
                d.clone(),
                d.clone(),
                d.clone(),
                d.clone(),
            ])))
        );

        None
    }
}
