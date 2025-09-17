#![allow(clippy::allow_attributes_without_reason)]
#![allow(unsafe_code)]

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
            "f" => {
                let mut params = func.params();
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

                assert_eq!(p1.ty(), Type::U32);
                assert_eq!(p3.ty(), Type::String);

                assert!(func.result().is_none());
                assert_eq!(args.len(), 5);
                let Val::U32(0) = args.next().unwrap() else {
                    panic!()
                };
                let Val::Tuple(tuple) = args.next().unwrap() else {
                    panic!()
                };
                assert_eq!(tuple[0], Val::U32(1));
                assert_eq!(tuple[1], Val::U32(2));
                assert_eq!(tuple.len(), 2);
                let Val::String(s) = args.next().unwrap() else {
                    panic!()
                };
                let Val::Own(r1) = args.next().unwrap() else {
                    panic!()
                };
                let Val::Own(r2) = args.next().unwrap() else {
                    panic!()
                };
                unsafe {
                    p4.drop()(r1);
                    p5.drop()(r2);
                }
                assert_eq!(s, "x");
                None
            }
            "[constructor]t4" => {
                let Type::Own(ty) = func.result().unwrap() else {
                    panic!()
                };
                unsafe { Some(Val::Own(ty.new().unwrap()(100))) }
            }
            other => panic!("unknown function {other:?}"),
        }
    }

    fn resource_dtor(_ty: Resource, handle: usize) {
        assert_eq!(handle, 100);
    }
}
