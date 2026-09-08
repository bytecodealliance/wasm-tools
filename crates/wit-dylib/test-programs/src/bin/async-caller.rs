use test_programs::*;

export_test!(struct MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        wit: Wit,
        func: ExportFunction,
        args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        let _ = (wit, func, args);
        unreachable!()
    }

    async fn call_export_async(
        wit: Wit,
        func: ExportFunction,
        args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), None);
        assert_eq!(func.name(), "run");
        assert_eq!(func.params().len(), 0);
        assert!(func.result().is_none());
        assert_eq!(args.len(), 0);

        let ret = Self::call_import_async(wit, Some("a:b/x"), "f", &[]).await;
        assert!(ret.is_none());

        let ret =
            Self::call_import_async(wit, Some("a:b/x"), "f-scalar-param", &[Val::U32(101)]).await;
        assert!(ret.is_none());

        let ret = Self::call_import_async(wit, Some("a:b/x"), "f-scalar-result", &[]).await;
        assert_eq!(ret, Some(Val::U32(202)));

        let ret = Self::call_import_async(
            wit,
            Some("a:b/x"),
            "aggregates",
            &[
                Val::Record(vec![
                    Val::Record(vec![Val::U32(2000), Val::Char('y')]),
                    Val::F32(32.0),
                ]),
                Val::Record(vec![Val::U32(1000), Val::Char('x')]),
            ],
        )
        .await;
        assert_eq!(
            ret,
            Some(Val::Record(vec![
                Val::Record(vec![Val::U32(3000), Val::Char('z'),]),
                Val::F32(64.0),
            ]))
        );

        let big3 = Val::Record(vec![Val::U8(1), Val::U8(2), Val::U8(3), Val::U8(4)]);
        let big2 = Val::Record(vec![big3.clone(), big3.clone(), big3.clone(), big3.clone()]);
        let big = Val::Record(vec![big2.clone(), big2.clone(), big2.clone(), big2.clone()]);

        let ret = Self::call_import_async(
            wit,
            Some("a:b/x"),
            "indirect-params",
            &[big.clone(), big.clone()],
        )
        .await;
        assert_eq!(ret, None);

        let ret = Self::call_import_async(
            wit,
            Some("a:b/x"),
            "indirect-params-and-result",
            &[big.clone()],
        )
        .await;
        assert_eq!(ret, Some(big.clone()));

        for s in ["", "a", "abcdefg", "hi!", ""] {
            let ret = Self::call_import_async(
                wit,
                Some("a:b/x"),
                "echo-string",
                &[Val::String(s.to_string())],
            )
            .await;
            assert_eq!(ret, Some(Val::String(s.to_string())));
        }

        None
    }
}
