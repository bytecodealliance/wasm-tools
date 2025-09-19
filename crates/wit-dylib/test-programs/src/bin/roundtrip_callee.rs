use rand::Rng;
use std::sync::Mutex;
use test_programs::*;

export_test!(struct MyInterpreter);

static RNG: Mutex<Option<generate::Generator>> = Mutex::new(None);

impl TestCase for MyInterpreter {
    fn call_export(
        wit: Wit,
        func: Function,
        mut args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        if func.interface() == Some("wit-dylib:roundtrip-test/alloc") {
            match func.name() {
                "set-seed" => {
                    let Val::U64(seed) = args.next().unwrap() else {
                        panic!()
                    };
                    assert_eq!(args.next(), None);
                    dprintln!("callee: seed: {seed}");
                    let mut rng = RNG.lock().unwrap();
                    assert!(rng.is_none());
                    *rng = Some(generate::Generator::new(wit, seed));
                    None
                }

                "checkpoint" => {
                    assert_eq!(args.next(), None);
                    let mut rng = RNG.lock().unwrap();
                    let rng = rng.as_mut().unwrap();
                    Some(rng.generate(Type::U32))
                }

                "allocated-bytes" => Some(Val::U32(alloc::get().try_into().unwrap())),

                other => panic!("unknown function: {other:?}"),
            }
        } else {
            dprintln!("callee: invoke({:?}, {:?})", func.interface(), func.name());
            assert_eq!(func.params().len(), args.len());

            if func.name().starts_with("[constructor]") {
                assert_eq!(func.params().len(), 1);
                let Some(Val::U32(arg)) = args.next() else {
                    panic!()
                };
                assert_eq!(args.next(), None);
                let Some(Type::Own(ty)) = func.result() else {
                    panic!()
                };
                Some(Val::Own(Own::new(ty, arg as usize)))
            } else {
                run_export(func, args)
            }
        }
    }

    async fn call_export_async(
        _wit: Wit,
        func: Function,
        args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        // Conditionally yield to inject some async-ness sometimes.
        {
            let mut rng = RNG.lock().unwrap();
            if rng.as_mut().unwrap().rng().clone().random() {
                wit_bindgen::yield_async().await;
            }
        }

        run_export(func, args)
    }

    fn resource_dtor(_: Resource, _: usize) {}
}

fn run_export(func: Function, args: impl ExactSizeIterator<Item = Val>) -> Option<Val> {
    let mut rng = RNG.lock().unwrap();
    let rng = rng.as_mut().unwrap();
    rng.reset_remaining();
    for (i, (ty, arg)) in func.params().zip(args).enumerate() {
        dprintln!("callee: generate: {ty:?}");
        dprintln!("callee:   expect: {arg:?}");
        match (i, arg) {
            (0, Val::Borrow(Borrow::Rep(arg))) => {
                assert_eq!(rng.generate(Type::U32), Val::U32(arg as u32))
            }
            (_, arg) => assert_eq!(rng.generate(ty), arg),
        }
    }
    func.result().map(|ty| {
        dprintln!("callee: generate: {ty:?}");
        rng.generate(ty)
    })
}
