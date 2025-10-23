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
            "[constructor]a" => {
                assert_eq!(func.params().len(), 0);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 0);
                let Some(Type::Own(ty)) = func.result() else {
                    unreachable!();
                };

                Some(Val::Own(Own::new(ty, 100)))
            }
            "[method]a.frob" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_none());
                assert_eq!(args.len(), 1);
                let Val::Borrow(Borrow::Rep(handle)) = args.next().unwrap() else {
                    unreachable!();
                };
                assert_eq!(handle, 100);
                None
            }
            other => panic!("unknown function {other:?}"),
        }
    }

    fn resource_dtor(ty: Resource, handle: usize) {
        assert_eq!(ty.interface(), Some("a:b/x"));
        assert_eq!(ty.name(), "a");
        assert_eq!(handle, 100);
    }
}
