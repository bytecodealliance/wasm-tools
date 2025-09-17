use test_programs::*;

export_test!(struct MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        wit: Wit,
        func: Function,
        args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), None);
        assert_eq!(func.name(), "run");
        assert_eq!(func.params().len(), 0);
        assert!(func.result().is_none());
        assert_eq!(args.len(), 0);

        let ret = Self::call_import(wit, Some("a:b/x"), "invert8", &[Val::Flags(0x14)]);
        assert_eq!(ret, Some(Val::Flags(0xeb)));
        let ret = Self::call_import(wit, Some("a:b/x"), "invert16", &[Val::Flags(0x1234)]);
        assert_eq!(ret, Some(Val::Flags(0xedcb)));
        let ret = Self::call_import(wit, Some("a:b/x"), "invert32", &[Val::Flags(0x12345678)]);
        assert_eq!(ret, Some(Val::Flags(0xedcba987)));

        let ret = Self::call_import(wit, Some("a:b/x"), "ret3-v1", &[]);
        assert_eq!(
            ret,
            Some(Val::Tuple(vec![
                Val::Flags(0x12),
                Val::Flags(0x3456),
                Val::Flags(0x789abcde),
            ]))
        );

        let ret = Self::call_import(wit, Some("a:b/x"), "ret3-v2", &[]);
        assert_eq!(
            ret,
            Some(Val::Tuple(vec![
                Val::Flags(0x789abcde),
                Val::Flags(0x3456),
                Val::Flags(0x12),
            ]))
        );

        None
    }
}
