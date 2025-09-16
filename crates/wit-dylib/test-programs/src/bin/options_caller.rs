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

        let ret = Self::call_import(wit, Some("a:b/x"), "echo-u32", &[Val::Option(None)]);
        assert_eq!(ret, Some(Val::Option(None)));
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u32",
            &[Val::Option(Some(Box::new(Val::U32(100))))],
        );
        assert_eq!(ret, Some(Val::Option(Some(Box::new(Val::U32(100))))));

        let ret = Self::call_import(wit, Some("a:b/x"), "echo-u64", &[Val::Option(None)]);
        assert_eq!(ret, Some(Val::Option(None)));
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-u64",
            &[Val::Option(Some(Box::new(Val::U64(800))))],
        );
        assert_eq!(ret, Some(Val::Option(Some(Box::new(Val::U64(800))))));

        let ret = Self::call_import(wit, Some("a:b/x"), "echo-f32", &[Val::Option(None)]);
        assert_eq!(ret, Some(Val::Option(None)));
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-f32",
            &[Val::Option(Some(Box::new(Val::F32(212.))))],
        );
        assert_eq!(ret, Some(Val::Option(Some(Box::new(Val::F32(212.))))));

        let ret = Self::call_import(wit, Some("a:b/x"), "echo-f64", &[Val::Option(None)]);
        assert_eq!(ret, Some(Val::Option(None)));
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-f64",
            &[Val::Option(Some(Box::new(Val::F64(200.))))],
        );
        assert_eq!(ret, Some(Val::Option(Some(Box::new(Val::F64(200.))))));

        let ret = Self::call_import(wit, Some("a:b/x"), "echo-string", &[Val::Option(None)]);
        assert_eq!(ret, Some(Val::Option(None)));
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-string",
            &[Val::Option(Some(Box::new(Val::String(
                "hello".to_string(),
            ))))],
        );
        assert_eq!(
            ret,
            Some(Val::Option(Some(Box::new(Val::String(
                "hello".to_string()
            )))))
        );

        None
    }
}
