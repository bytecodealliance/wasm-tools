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

        let ret = Self::call_import(wit, Some("a:b/x"), "invert8", &[Val::Flags(0x14)]);
        assert_eq!(ret, Some(Box::new(Val::Flags(0xeb))));
        let ret = Self::call_import(wit, Some("a:b/x"), "invert16", &[Val::Flags(0x1234)]);
        assert_eq!(ret, Some(Box::new(Val::Flags(0xedcb))));
        let ret = Self::call_import(wit, Some("a:b/x"), "invert32", &[Val::Flags(0x12345678)]);
        assert_eq!(ret, Some(Box::new(Val::Flags(0xedcba987))));

        let ret = Self::call_import(wit, Some("a:b/x"), "ret3-v1", &[]);
        assert_eq!(
            ret,
            Some(Box::new(Val::Tuple(vec![
                Box::new(Val::Flags(0x12)),
                Box::new(Val::Flags(0x3456)),
                Box::new(Val::Flags(0x789abcde)),
            ])))
        );

        let ret = Self::call_import(wit, Some("a:b/x"), "ret3-v2", &[]);
        assert_eq!(
            ret,
            Some(Box::new(Val::Tuple(vec![
                Box::new(Val::Flags(0x789abcde)),
                Box::new(Val::Flags(0x3456)),
                Box::new(Val::Flags(0x12)),
            ])))
        );

        None
    }
}
