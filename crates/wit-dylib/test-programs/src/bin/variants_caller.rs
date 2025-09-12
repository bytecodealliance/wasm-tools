use test_programs::test_util::*;
use test_programs::*;

struct MyInterpreter;

export!(MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(wit: Wit, func: Function, args: OwnVals<'_, Self>) -> Option<Box<Val>> {
        assert_eq!(func.interface(), None);
        assert_eq!(func.name(), "run");
        assert_eq!(func.params().len(), 0);
        assert!(func.result().is_none());
        assert_eq!(args.len(), 0);

        // v1
        let ret = Self::call_import(wit, Some("a:b/x"), "echo-v1", &[Val::Variant(0, None)]);
        assert_eq!(ret, Some(Box::new(Val::Variant(0, None))));

        // v2
        let ret = Self::call_import(wit, Some("a:b/x"), "echo-v2", &[Val::Variant(0, None)]);
        assert_eq!(ret, Some(Box::new(Val::Variant(0, None))));
        let ret = Self::call_import(wit, Some("a:b/x"), "echo-v2", &[Val::Variant(1, None)]);
        assert_eq!(ret, Some(Box::new(Val::Variant(1, None))));

        // v3
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-v3",
            &[Val::Variant(
                0,
                Some(Box::new(Val::String("hello".to_string()))),
            )],
        );
        assert_eq!(
            ret,
            Some(Box::new(Val::Variant(
                0,
                Some(Box::new(Val::String("hello".to_string()))),
            )))
        );

        // v4
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-v4",
            &[Val::Variant(
                0,
                Some(Box::new(Val::String("hello".to_string()))),
            )],
        );
        assert_eq!(
            ret,
            Some(Box::new(Val::Variant(
                0,
                Some(Box::new(Val::String("hello".to_string()))),
            )))
        );
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-v4",
            &[Val::Variant(1, Some(Box::new(Val::F32(1.0))))],
        );
        assert_eq!(
            ret,
            Some(Box::new(Val::Variant(1, Some(Box::new(Val::F32(1.0))),)))
        );

        // v5.a
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-v5",
            &[Val::Variant(
                0,
                Some(Box::new(Val::String("hello".to_string()))),
            )],
        );
        assert_eq!(
            ret,
            Some(Box::new(Val::Variant(
                0,
                Some(Box::new(Val::String("hello".to_string()))),
            )))
        );

        // v5.b
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-v5",
            &[Val::Variant(1, Some(Box::new(Val::F32(1.0))))],
        );
        assert_eq!(
            ret,
            Some(Box::new(Val::Variant(1, Some(Box::new(Val::F32(1.0))),)))
        );

        // v5.c
        let ret = Self::call_import(wit, Some("a:b/x"), "echo-v5", &[Val::Variant(2, None)]);
        assert_eq!(ret, Some(Box::new(Val::Variant(2, None))));

        // v5.d
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-v5",
            &[Val::Variant(
                3,
                Some(Box::new(Val::Tuple(vec![
                    Box::new(Val::String("hello".to_string())),
                    Box::new(Val::String("world".to_string())),
                ]))),
            )],
        );
        assert_eq!(
            ret,
            Some(Box::new(Val::Variant(
                3,
                Some(Box::new(Val::Tuple(vec![
                    Box::new(Val::String("hello".to_string())),
                    Box::new(Val::String("world".to_string())),
                ]))),
            )))
        );

        // v5.e
        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "echo-v5",
            &[Val::Variant(4, Some(Box::new(Val::Variant(0, None))))],
        );
        assert_eq!(
            ret,
            Some(Box::new(Val::Variant(
                4,
                Some(Box::new(Val::Variant(0, None))),
            )))
        );

        None
    }
}
