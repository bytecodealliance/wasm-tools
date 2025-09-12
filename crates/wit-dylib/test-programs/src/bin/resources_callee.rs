#![allow(unsafe_code)]

use test_programs::test_util::*;
use test_programs::*;

struct MyInterpreter;

export!(MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(_wit: Wit, func: Function, mut args: OwnVals<'_, Self>) -> Option<Box<Val>> {
        assert_eq!(func.interface(), Some("a:b/x"));
        match func.name() {
            "[constructor]a" => {
                assert_eq!(func.params().len(), 0);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 0);
                let Some(Type::Own(ty)) = func.result() else {
                    unreachable!();
                };

                let ret = unsafe { ty.new().unwrap()(100) };
                Some(Box::new(Val::Own(ret)))
            }
            "[method]a.frob" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_none());
                assert_eq!(args.len(), 1);
                let Val::Borrow(handle) = *args.next().unwrap() else {
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
