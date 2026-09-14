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

        let Val::Own(handle) =
            Self::call_import(wit, Some("a:b/x"), "[constructor]a", &[]).unwrap()
        else {
            unreachable!()
        };

        assert!(handle.handle() != 100);

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "[method]a.frob",
            &[Val::Borrow(handle.borrow())],
        );
        assert!(ret.is_none());

        None
    }
}
