#![allow(clippy::allow_attributes_without_reason)]
#![allow(unsafe_code)]

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

        let ty = wit.iter_resources().find(|r| r.name() == "a").unwrap();
        let Val::Own(handle) =
            Self::call_import(wit, Some("a:b/x"), "[constructor]a", &[]).unwrap()
        else {
            unreachable!()
        };

        assert!(handle != 100);

        let ret = Self::call_import(wit, Some("a:b/x"), "[method]a.frob", &[Val::Borrow(handle)]);
        assert!(ret.is_none());

        unsafe {
            ty.drop()(handle);
        }

        None
    }
}
