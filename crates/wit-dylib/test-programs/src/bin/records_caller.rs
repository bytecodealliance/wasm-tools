use test_programs::*;

export_test!(struct MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        wit: Wit,
        func: ExportFunction,
        args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), None);
        assert_eq!(func.name(), "run");
        assert_eq!(func.params().len(), 0);
        assert!(func.result().is_none());
        assert_eq!(args.len(), 0);

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-a",
            &[Val::Record(vec![Val::U32(1)])],
        );
        assert_eq!(ret, Some(Val::Record(vec![Val::U32(2)])));

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-b",
            &[Val::Record(vec![Val::String("hello".to_string())])],
        );
        assert_eq!(
            ret,
            Some(Val::Record(vec![Val::String("world".to_string())]))
        );

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-c",
            &[Val::Record(vec![Val::F32(1.0), Val::S64(2)])],
        );
        assert_eq!(ret, Some(Val::Record(vec![Val::F32(3.0), Val::S64(4)])));

        let d = Val::Record(vec![
            Val::String("a1".to_string()),
            Val::String("a2".to_string()),
            Val::String("a3".to_string()),
            Val::String("a4".to_string()),
        ]);
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
            Some(Val::Record(vec![
                d.clone(),
                d.clone(),
                d.clone(),
                d.clone(),
            ]))
        );

        None
    }
}
