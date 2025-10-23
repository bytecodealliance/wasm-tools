use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use test_programs::*;

export_test!(struct MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        _wit: Wit,
        _func: ExportFunction,
        _args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        unreachable!()
    }

    async fn call_export_async(
        wit: Wit,
        func: ExportFunction,
        mut args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), None);
        assert_eq!(func.name(), "[async]run");
        assert_eq!(func.params().len(), 2);
        assert!(func.result().is_none());
        assert_eq!(args.len(), 2);

        let Val::U32(iters) = args.next().unwrap() else {
            panic!()
        };
        let Val::U64(seed) = args.next().unwrap() else {
            panic!()
        };
        assert_eq!(args.next(), None);

        let result = Self::call_import(
            wit,
            Some("wit-dylib:roundtrip-test/alloc"),
            "set-seed",
            &[Val::U64(seed)],
        );
        assert_eq!(result, None);

        let callee_allocated_bytes =
            wit.unwrap_import(Some("wit-dylib:roundtrip-test/alloc"), "allocated-bytes");

        dprintln!("caller: seed: {seed}");

        let mut rng = generate::Generator::new(wit, seed);
        let mut import_rng = SmallRng::seed_from_u64(seed);

        let imports = wit
            .iter_import_funcs()
            // don't test various intrinsics we use throughout this roundtrip
            // test, aka anything from our special interface or any
            // resource constructor/rep inspection.
            .filter(|f| f.interface() != Some("wit-dylib:roundtrip-test/alloc"))
            .filter(|f| !f.name().starts_with("[constructor]"))
            .filter(|f| !(f.name().starts_with("[method]") && f.name().ends_with(".rep")))
            .collect::<Vec<_>>();

        if imports.is_empty() {
            return None;
        }

        // Each iteration of the loop below is allows to leak at most 200 bytes
        // (in theory). To ensure it's not 200-per-iteration also install a
        // guard out here. This is currently because the first async call will
        // allocate some storage space in our task but after that we shouldn't
        // continue having any allocation. Our own task should get cleaned up
        // properly, so it's mostly on us to ensure that we're just not slowly
        // increasing the amount of leaked memory.
        let slop = 200;
        let _my_guard = alloc::Guard::sloppy(slop);

        for i in 0..iters {
            let _my_guard = alloc::Guard::sloppy(slop);
            let _callee_guard = CalleeAllocGuard::new(callee_allocated_bytes);
            dprintln!("caller: iter: {i}");
            let i = import_rng.random_range(0..imports.len());
            let import = &imports[i];
            dprintln!("caller: calling {import:?}");

            let mut args = Vec::new();
            rng.reset_remaining();
            let mut resource = None;
            for (i, ty) in import.params().enumerate() {
                dprintln!("caller: generate {ty:?}");
                args.push(match (i, ty) {
                    (0, Type::Borrow(ty)) => {
                        Val::Borrow(resource.get_or_insert_with(|| rng.own(ty)).borrow())
                    }
                    _ => rng.generate(ty),
                });
            }
            let result = if import.is_async() {
                Self::call_import_func_async(*import, &args).await
            } else {
                Self::call_import_func(*import, &args)
            };
            match (import.result(), result) {
                (Some(ty), Some(val)) => {
                    dprintln!("caller: expect {ty:?}");
                    dprintln!("caller:    val {val:?}");
                    assert_eq!(rng.generate(ty), val);
                }
                (None, None) => {}
                _ => unreachable!(),
            }

            dprintln!("verifying rngs still in sync after iteration");
            let checkpoint = Self::call_import(
                wit,
                Some("wit-dylib:roundtrip-test/alloc"),
                "checkpoint",
                &[],
            );
            assert_eq!(checkpoint, Some(rng.generate(Type::U32)));
        }

        None
    }
}

struct CalleeAllocGuard {
    allocated_bytes: ImportFunction,
    prev: u32,
}

impl CalleeAllocGuard {
    fn new(allocated_bytes: ImportFunction) -> CalleeAllocGuard {
        let Val::U32(prev) = MyInterpreter::call_import_func(allocated_bytes, &[]).unwrap() else {
            panic!()
        };
        CalleeAllocGuard {
            prev,
            allocated_bytes,
        }
    }
}

impl Drop for CalleeAllocGuard {
    fn drop(&mut self) {
        let Val::U32(cur) = MyInterpreter::call_import_func(self.allocated_bytes, &[]).unwrap()
        else {
            panic!()
        };
        assert_eq!(self.prev, cur);
    }
}
