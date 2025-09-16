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

        let f = wit
            .iter_funcs()
            .filter(|f| {
                f.interface() == Some("a:b/x") && f.name() == "f" && f.import_impl().is_some()
            })
            .next()
            .unwrap();

        let mut params = f.params();
        let Type::Alias(p1) = params.next().unwrap() else {
            panic!()
        };
        let Type::Tuple(p2) = params.next().unwrap() else {
            panic!()
        };
        let Type::Alias(p3) = params.next().unwrap() else {
            panic!()
        };
        assert!(params.next().is_none());

        assert_eq!(p1.name(), "t1");
        assert_eq!(p2.name(), Some("t2"));
        assert_eq!(p3.name(), "t3");

        assert_eq!(p1.ty(), Type::U32);
        assert_eq!(p3.ty(), Type::String);

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "f",
            &[
                Val::U32(0),
                Val::Tuple(vec![Val::U32(1), Val::U32(2)]),
                Val::String("x".to_string()),
            ],
        );
        assert_eq!(ret, None);

        None
    }
}
