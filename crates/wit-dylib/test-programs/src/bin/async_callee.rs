use test_programs::*;

export_test!(struct MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        _wit: Wit,
        _func: Function,
        _args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        unreachable!()
    }

    async fn call_export_async(
        _wit: Wit,
        func: Function,
        mut args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), Some("a:b/x"));

        let big3 = Val::Record(vec![Val::U8(1), Val::U8(2), Val::U8(3), Val::U8(4)]);
        let big2 = Val::Record(vec![big3.clone(), big3.clone(), big3.clone(), big3.clone()]);
        let big = Val::Record(vec![big2.clone(), big2.clone(), big2.clone(), big2.clone()]);

        match func.name() {
            "[async]f" => {
                assert_eq!(func.params().len(), 0);
                assert!(func.result().is_none());
                assert_eq!(args.len(), 0);

                for _ in 0..10 {
                    wit_bindgen::yield_async().await;
                }
                None
            }
            "[async]f-scalar-param" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_none());
                assert_eq!(args.len(), 1);

                assert_eq!(args.next(), Some(Val::U32(101)));
                assert_eq!(args.next(), None);
                None
            }
            "[async]f-scalar-result" => {
                assert_eq!(func.params().len(), 0);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 0);

                Some(Val::U32(202))
            }
            "[async]aggregates" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 2);

                assert_eq!(
                    args.next(),
                    Some(Val::Record(vec![
                        Val::Record(vec![Val::U32(2000), Val::Char('y')]),
                        Val::F32(32.0),
                    ]))
                );
                assert_eq!(
                    args.next(),
                    Some(Val::Record(vec![Val::U32(1000), Val::Char('x')]),)
                );
                assert_eq!(args.next(), None);

                Some(Val::Record(vec![
                    Val::Record(vec![Val::U32(3000), Val::Char('z')]),
                    Val::F32(64.0),
                ]))
            }
            "[async]indirect-params" => {
                assert_eq!(func.params().len(), 2);
                assert!(func.result().is_none());
                assert_eq!(args.len(), 2);

                assert_eq!(args.next(), Some(big.clone()));
                assert_eq!(args.next(), Some(big.clone()));
                assert_eq!(args.next(), None);

                None
            }
            "[async]indirect-params-and-result" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                assert_eq!(args.len(), 1);
                assert_eq!(args.next(), Some(big.clone()));
                assert_eq!(args.next(), None);

                Some(big.clone())
            }
            "[async]echo-string" => {
                assert_eq!(func.params().len(), 1);
                assert!(func.result().is_some());
                args.next()
            }
            other => panic!("unknown function {other:?}"),
        }
    }
}
