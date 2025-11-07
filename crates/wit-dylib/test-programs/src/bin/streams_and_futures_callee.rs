#![allow(
    unsafe_code,
    reason = "needed to create streams and futures from vtables"
)]

use std::collections::HashMap;
use std::sync::{
    Mutex,
    atomic::{AtomicUsize, Ordering::Relaxed},
};
use test_programs::*;
use wit_bindgen::{FutureReader, StreamReader, StreamResult, rt::async_support};

static NEXT_THING: AtomicUsize = AtomicUsize::new(1);
static THINGS: Mutex<Option<HashMap<usize, String>>> = Mutex::new(None);

export_test!(struct MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        _wit: Wit,
        func: ExportFunction,
        mut args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), Some("a:b/x"));

        match func.name() {
            "[constructor]thing" => {
                assert_eq!(func.params().len(), 1);
                assert!(matches!(func.params().next(), Some(Type::String)));
                assert!(matches!(func.result(), Some(Type::Own(_))));
                assert_eq!(args.len(), 1);

                let Some(Type::Own(ty)) = func.result() else {
                    unreachable!();
                };

                let Some(Val::String(value)) = args.next() else {
                    unreachable!()
                };

                let rep = NEXT_THING.fetch_add(1, Relaxed);

                THINGS
                    .lock()
                    .unwrap()
                    .get_or_insert_default()
                    .insert(rep, value);

                Some(Val::Own(Own::new(ty, rep)))
            }

            other => panic!("unknown function {other:?}"),
        }
    }

    async fn call_export_async(
        _wit: Wit,
        func: ExportFunction,
        mut args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), Some("a:b/x"));

        match func.name() {
            "[async]echo-stream-u8" => {
                assert_eq!(func.params().len(), 1);
                assert!(matches!(func.params().next(), Some(Type::Stream(_))));
                assert!(matches!(func.result(), Some(Type::Stream(_))));
                assert_eq!(args.len(), 1);

                let Some(Type::Stream(ty)) = func.params().next() else {
                    unreachable!()
                };

                let vtable = get_stream_vtable::<u8>(ty);

                let (mut tx, result) = unsafe { async_support::stream_new(vtable) };

                let Some(Val::Stream(rx)) = args.next() else {
                    unreachable!()
                };

                let mut rx = StreamReader::new(rx, vtable);

                async_support::spawn(async move {
                    let mut chunk = Vec::with_capacity(1024);
                    loop {
                        let (status, buf) = rx.read(chunk).await;
                        chunk = buf;
                        match status {
                            StreamResult::Complete(_) => {
                                chunk = tx.write_all(chunk).await;
                                assert!(chunk.is_empty());
                            }
                            StreamResult::Dropped => break,
                            StreamResult::Cancelled => unreachable!(),
                        }
                    }
                });

                Some(Val::Stream(result.take_handle()))
            }

            "[async]echo-future-string" => {
                assert_eq!(func.params().len(), 1);
                assert!(matches!(func.params().next(), Some(Type::Future(_))));
                assert!(matches!(func.result(), Some(Type::Future(_))));
                assert_eq!(args.len(), 1);

                let Some(Type::Future(ty)) = func.params().next() else {
                    unreachable!()
                };

                let vtable = get_future_vtable::<String>(ty);

                let (tx, result) = unsafe { async_support::future_new(|| unreachable!(), vtable) };

                let Some(Val::Future(rx)) = args.next() else {
                    unreachable!()
                };

                let rx = FutureReader::new(rx, vtable);

                async_support::spawn(async move { tx.write(rx.await).await.unwrap() });

                Some(Val::Future(result.take_handle()))
            }

            "[async method]thing.get" => {
                assert_eq!(func.params().len(), 1);
                assert!(matches!(func.params().next(), Some(Type::Borrow(_))));
                assert!(matches!(func.result(), Some(Type::String)));
                assert_eq!(args.len(), 1);

                let Some(Val::Borrow(Borrow::Rep(rep))) = args.next() else {
                    unreachable!()
                };

                let value = THINGS
                    .lock()
                    .unwrap()
                    .as_ref()
                    .unwrap()
                    .get(&usize::try_from(rep).unwrap())
                    .unwrap()
                    .clone();
                Some(Val::String(value))
            }

            "[async]short-reads" => {
                assert_eq!(func.params().len(), 1);
                assert!(matches!(func.params().next(), Some(Type::Stream(_))));
                assert!(matches!(func.result(), Some(Type::Stream(_))));
                assert_eq!(args.len(), 1);

                let Some(Type::Stream(ty)) = func.params().next() else {
                    unreachable!()
                };

                let vtable = get_stream_vtable::<Val>(ty);

                let (mut tx, result) = unsafe { async_support::stream_new(vtable) };

                let Some(Val::Stream(rx)) = args.next() else {
                    unreachable!()
                };

                let mut rx = StreamReader::new(rx, vtable);

                async_support::spawn(async move {
                    // Read only one item at a time, forcing the sender to
                    // retake ownership of any unwritten items.
                    let mut received_things = Vec::new();
                    loop {
                        let (status, buffer) = rx.read(Vec::with_capacity(1)).await;
                        received_things.extend(buffer);
                        match status {
                            StreamResult::Complete(_) => {}
                            StreamResult::Dropped => break,
                            StreamResult::Cancelled => unreachable!(),
                        }
                    }

                    // Write the items all at once.  The receiver will only read
                    // them one at a time, forcing us to retake ownership of the
                    // unwritten items between writes.
                    let buffer = tx.write_all(received_things).await;
                    assert!(buffer.is_empty());
                });

                Some(Val::Stream(result.take_handle()))
            }

            other => panic!("unknown function {other:?}"),
        }
    }

    fn resource_dtor(_: Resource, handle: usize) {
        assert!(
            THINGS
                .lock()
                .unwrap()
                .as_mut()
                .unwrap()
                .remove(&handle)
                .is_some()
        );
    }
}
