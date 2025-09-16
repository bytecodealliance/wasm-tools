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
            name if name.starts_with("echo-") => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                let arg = args.next().unwrap();
                match arg {
                    Val::GenericList(_) | Val::ByteList(_) => {}
                    _ => panic!(),
                }
                Some(arg)
            }

            "allocated-bytes" => {
                assert_eq!(func.params().len(), 0);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 0);
                let bytes = alloc::get();
                Some(Val::U32(bytes.try_into().unwrap()))
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
