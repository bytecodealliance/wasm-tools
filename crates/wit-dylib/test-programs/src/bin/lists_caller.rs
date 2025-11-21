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

        let _guard = CalleeAllocGuard::new(wit);
        let _guard = alloc::Guard::new();

        {
            let ret =
                Self::call_import(wit, Some("a:b/x"), "echo-u8", &[Val::ByteList(Vec::new())]);
            assert_eq!(ret, Some(Val::ByteList(Vec::new())));

            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-u8",
                &[Val::ByteList(b"abc".to_vec())],
            );
            assert_eq!(ret, Some(Val::ByteList(b"abc".to_vec())));
        }

        {
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-u32",
                &[Val::GenericList(Vec::new())],
            );
            assert_eq!(ret, Some(Val::GenericList(Vec::new())));
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-u32",
                &[Val::GenericList(vec![Val::U32(1), Val::U32(2)])],
            );
            assert_eq!(ret, Some(Val::GenericList(vec![Val::U32(1), Val::U32(2),])));
        }

        {
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-string",
                &[Val::GenericList(Vec::new())],
            );
            assert_eq!(ret, Some(Val::GenericList(Vec::new())));
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "echo-string",
                &[Val::GenericList(vec![
                    Val::String("1".to_string()),
                    Val::String("2".to_string()),
                ])],
            );
            assert_eq!(
                ret,
                Some(Val::GenericList(vec![
                    Val::String("1".to_string()),
                    Val::String("2".to_string()),
                ]))
            );
        }

        {
            let ret = Self::call_import(
                wit,
                Some("a:b/x"),
                "list-of-variants",
                &[Val::GenericList(vec![
                    Val::GenericList(vec![Val::Option(None)]),
                    Val::GenericList(vec![Val::Option(None)]),
                ])],
            );
            assert_eq!(ret, None);
        }

        None
    }
}

struct CalleeAllocGuard {
    wit: Wit,
    prev: u32,
}

impl CalleeAllocGuard {
    fn new(wit: Wit) -> CalleeAllocGuard {
        let Val::U32(prev) =
            MyInterpreter::call_import(wit, Some("a:b/x"), "allocated-bytes", &[]).unwrap()
        else {
            panic!()
        };
        CalleeAllocGuard { prev, wit }
    }
}

impl Drop for CalleeAllocGuard {
    fn drop(&mut self) {
        let Val::U32(cur) =
            MyInterpreter::call_import(self.wit, Some("a:b/x"), "allocated-bytes", &[]).unwrap()
        else {
            panic!()
        };
        assert_eq!(self.prev, cur);
    }
}
