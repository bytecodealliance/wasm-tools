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

        let ret = Self::call_import(wit, Some("a:b/x"), "add-u8", &[Val::U8(1), Val::U8(2)]);
        assert_eq!(ret, Some(Val::U8(3)));

        let ret = Self::call_import(wit, Some("a:b/x"), "add-s8", &[Val::S8(-1), Val::S8(2)]);
        assert_eq!(ret, Some(Val::S8(1)));

        let ret = Self::call_import(wit, Some("a:b/x"), "add-u16", &[Val::U16(1), Val::U16(2)]);
        assert_eq!(ret, Some(Val::U16(3)));

        let ret = Self::call_import(wit, Some("a:b/x"), "add-s16", &[Val::S16(-1), Val::S16(2)]);
        assert_eq!(ret, Some(Val::S16(1)));

        let ret = Self::call_import(wit, Some("a:b/x"), "add-u32", &[Val::U32(1), Val::U32(2)]);
        assert_eq!(ret, Some(Val::U32(3)));

        let ret = Self::call_import(wit, Some("a:b/x"), "add-s32", &[Val::S32(-1), Val::S32(2)]);
        assert_eq!(ret, Some(Val::S32(1)));

        let ret = Self::call_import(wit, Some("a:b/x"), "add-u64", &[Val::U64(1), Val::U64(2)]);
        assert_eq!(ret, Some(Val::U64(3)));

        let ret = Self::call_import(wit, Some("a:b/x"), "add-s64", &[Val::S64(-1), Val::S64(2)]);
        assert_eq!(ret, Some(Val::S64(1)));

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "add-f32",
            &[Val::F32(-1.), Val::F32(2.)],
        );
        assert_eq!(ret, Some(Val::F32(1.)));

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "add-f64",
            &[Val::F64(-1.), Val::F64(2.)],
        );
        assert_eq!(ret, Some(Val::F64(1.)));

        let ret = Self::call_import(
            wit,
            Some("a:b/x"),
            "and-bool",
            &[Val::Bool(true), Val::Bool(false)],
        );
        assert_eq!(ret, Some(Val::Bool(false)));

        let ret = Self::call_import(wit, Some("a:b/x"), "echo-char", &[Val::Char('x')]);
        assert_eq!(ret, Some(Val::Char('x')));

        None
    }
}
