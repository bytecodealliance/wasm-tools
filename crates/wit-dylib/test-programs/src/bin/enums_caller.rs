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

        let ret = Self::call_import(wit, Some("a:b/x"), "echo-e1", &[Val::Enum(0)]);
        assert_eq!(ret, Some(Val::Enum(0)));

        let ret = Self::call_import(wit, Some("a:b/x"), "echo-e2", &[Val::Enum(1)]);
        assert_eq!(ret, Some(Val::Enum(1)));

        let ret = Self::call_import(wit, Some("a:b/x"), "echo-e3", &[Val::Enum(2)]);
        assert_eq!(ret, Some(Val::Enum(2)));

        let ret = Self::call_import(wit, Some("a:b/x"), "echo-e257", &[Val::Enum(102)]);
        assert_eq!(ret, Some(Val::Enum(102)));

        None
    }
}
