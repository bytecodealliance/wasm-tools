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

        // u32/u64 union
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u32-u64",
            &[Val::Result(Ok(Some(Box::new(Val::U32(1)))))],
        );
        assert_eq!(ret, Some(Val::Result(Ok(Some(Box::new(Val::U32(1)))))));
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u32-u64",
            &[Val::Result(Err(Some(Box::new(Val::U64(2)))))],
        );
        assert_eq!(ret, Some(Val::Result(Err(Some(Box::new(Val::U64(2)))))));

        // u32/f32 union
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u32-f32",
            &[Val::Result(Ok(Some(Box::new(Val::U32(1)))))],
        );
        assert_eq!(ret, Some(Val::Result(Ok(Some(Box::new(Val::U32(1)))))));
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u32-f32",
            &[Val::Result(Err(Some(Box::new(Val::F32(2.)))))],
        );
        assert_eq!(ret, Some(Val::Result(Err(Some(Box::new(Val::F32(2.)))))));

        // u32/f64 union
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u32-f64",
            &[Val::Result(Ok(Some(Box::new(Val::U32(1)))))],
        );
        assert_eq!(ret, Some(Val::Result(Ok(Some(Box::new(Val::U32(1)))))));
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u32-f64",
            &[Val::Result(Err(Some(Box::new(Val::F64(2.)))))],
        );
        assert_eq!(ret, Some(Val::Result(Err(Some(Box::new(Val::F64(2.)))))));

        // u64/f64 union
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u64-f64",
            &[Val::Result(Ok(Some(Box::new(Val::U64(1)))))],
        );
        assert_eq!(ret, Some(Val::Result(Ok(Some(Box::new(Val::U64(1)))))));
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u64-f64",
            &[Val::Result(Err(Some(Box::new(Val::F64(2.)))))],
        );
        assert_eq!(ret, Some(Val::Result(Err(Some(Box::new(Val::F64(2.)))))));

        // u64/f32 union
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u64-f32",
            &[Val::Result(Ok(Some(Box::new(Val::U64(1)))))],
        );
        assert_eq!(ret, Some(Val::Result(Ok(Some(Box::new(Val::U64(1)))))));
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u64-f32",
            &[Val::Result(Err(Some(Box::new(Val::F32(2.)))))],
        );
        assert_eq!(ret, Some(Val::Result(Err(Some(Box::new(Val::F32(2.)))))));

        None
    }
}
