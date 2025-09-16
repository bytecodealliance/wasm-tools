use test_programs::test_util::*;
use test_programs::*;

struct MyInterpreter;

export!(MyInterpreter);

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

        {
            let _guard = alloc::Guard::new();
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "set",
                &[Val::String("hello".to_string())],
            );
            assert!(ret.is_none());
        }

        {
            let _guard = alloc::Guard::new();
            let ret = Self::call_import(wit, Some("a:b/x"), "get", &[]);
            assert_eq!(ret, Some(Val::String("world".to_string())));
        }

        None
    }
}
