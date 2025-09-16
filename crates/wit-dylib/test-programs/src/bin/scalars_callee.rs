use test_programs::test_util::*;
use test_programs::*;

struct MyInterpreter;

export!(MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        _wit: Wit,
        func: Function,
        mut args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), Some("a:b/x"));

        match func.name() {
            "add-u8" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                let Val::U8(a) = args.next().unwrap() else {
                    panic!()
                };
                let Val::U8(b) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::U8(a.wrapping_add(b)))
            }
            "add-s8" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                let Val::S8(a) = args.next().unwrap() else {
                    panic!()
                };
                let Val::S8(b) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::S8(a.wrapping_add(b)))
            }
            "add-u16" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                let Val::U16(a) = args.next().unwrap() else {
                    panic!()
                };
                let Val::U16(b) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::U16(a.wrapping_add(b)))
            }
            "add-s16" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                let Val::S16(a) = args.next().unwrap() else {
                    panic!()
                };
                let Val::S16(b) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::S16(a.wrapping_add(b)))
            }
            "add-u32" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                let Val::U32(a) = args.next().unwrap() else {
                    panic!()
                };
                let Val::U32(b) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::U32(a.wrapping_add(b)))
            }
            "add-s32" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                let Val::S32(a) = args.next().unwrap() else {
                    panic!()
                };
                let Val::S32(b) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::S32(a.wrapping_add(b)))
            }
            "add-u64" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                let Val::U64(a) = args.next().unwrap() else {
                    panic!()
                };
                let Val::U64(b) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::U64(a.wrapping_add(b)))
            }
            "add-s64" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                let Val::S64(a) = args.next().unwrap() else {
                    panic!()
                };
                let Val::S64(b) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::S64(a.wrapping_add(b)))
            }
            "add-f32" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                let Val::F32(a) = args.next().unwrap() else {
                    panic!()
                };
                let Val::F32(b) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::F32(a + b))
            }
            "add-f64" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                let Val::F64(a) = args.next().unwrap() else {
                    panic!()
                };
                let Val::F64(b) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::F64(a + b))
            }
            "and-bool" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                let Val::Bool(a) = args.next().unwrap() else {
                    panic!()
                };
                let Val::Bool(b) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::Bool(a & b))
            }
            "echo-char" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);

                let Val::Char(a) = args.next().unwrap() else {
                    panic!()
                };

                Some(Val::Char(a))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
