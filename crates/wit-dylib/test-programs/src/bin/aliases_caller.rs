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
        let Type::Own(p4) = params.next().unwrap() else {
            panic!()
        };
        let Type::Own(p5) = params.next().unwrap() else {
            panic!()
        };
        assert!(params.next().is_none());

        assert_eq!(p1.name(), "t1");
        assert_eq!(p2.name(), Some("t2"));
        assert_eq!(p3.name(), "t3");
        assert_eq!(p4.name(), "t4");
        assert_eq!(p5.name(), "t4");

        let resource_ty = wit.resource(0);

        assert_eq!(p1.ty(), Type::U32);
        assert_eq!(p3.ty(), Type::String);
        assert_eq!(p4, resource_ty);
        assert_eq!(p5, resource_ty);

        let Val::Own(r1) = Self::call_import(wit, Some("a:b/x"), "[constructor]t4", &[]).unwrap()
        else {
            panic!()
        };
        let Val::Own(r2) = Self::call_import(wit, Some("a:b/x"), "[constructor]t4", &[]).unwrap()
        else {
            panic!()
        };

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "f",
            &[
                Val::U32(0),
                Val::Tuple(vec![Val::U32(1), Val::U32(2)]),
                Val::String("x".to_string()),
                Val::Own(r1),
                Val::Own(r2),
            ],
        );
        assert_eq!(ret, None);

        None
    }
}
