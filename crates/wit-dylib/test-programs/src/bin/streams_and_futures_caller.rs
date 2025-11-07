#![allow(
    unsafe_code,
    reason = "needed to create streams and futures from vtables"
)]

use test_programs::*;
use wit_bindgen::{FutureReader, StreamReader, StreamResult, rt::async_support};

export_test!(struct MyInterpreter);

impl TestCase for MyInterpreter {
    fn call_export(
        wit: Wit,
        func: ExportFunction,
        args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        let _ = (wit, func, args);
        unreachable!()
    }

    async fn call_export_async(
        wit: Wit,
        func: ExportFunction,
        args: impl ExactSizeIterator<Item = Val>,
    ) -> Option<Val> {
        assert_eq!(func.interface(), None);
        assert_eq!(func.name(), "[async]run");
        assert_eq!(func.params().len(), 0);
        assert!(func.result().is_none());
        assert_eq!(args.len(), 0);

        {
            let Some(Type::Stream(ty)) = wit
                .unwrap_import(Some("a:b/x"), "[async]echo-stream-u8")
                .params()
                .next()
            else {
                unreachable!();
            };

            let vtable = get_stream_vtable(ty);

            let (mut tx, rx) = unsafe { async_support::stream_new(vtable) };

            let Some(Val::Stream(rx)) = Self::call_import_async(
                wit,
                Some("a:b/x"),
                "[async]echo-stream-u8",
                &[Val::Stream(rx.take_handle())],
            )
            .await
            else {
                unreachable!()
            };

            let mut rx = StreamReader::new(rx, vtable);

            let data = b"Beware the Jubjub bird, and shun\n\tThe frumious Bandersnatch!";
            tx.write_all(data.to_vec()).await;

            let (_, buffer) = rx.read(Vec::with_capacity(data.len())).await;

            assert_eq!(&data[..], &buffer);
        }

        {
            let Some(Type::Future(ty)) = wit
                .unwrap_import(Some("a:b/x"), "[async]echo-future-string")
                .params()
                .next()
            else {
                unreachable!();
            };

            let vtable = get_future_vtable(ty);

            let (tx, rx) = unsafe { async_support::future_new(|| unreachable!(), vtable) };

            let Some(Val::Future(rx)) = Self::call_import_async(
                wit,
                Some("a:b/x"),
                "[async]echo-future-string",
                &[Val::Future(rx.take_handle())],
            )
            .await
            else {
                unreachable!()
            };

            let rx = FutureReader::new(rx, vtable);

            let data = "Beware the Jubjub bird, and shun\n\tThe frumious Bandersnatch!";
            tx.write(data.to_string()).await.unwrap();

            let received = rx.await;

            assert_eq!(data, &received);
        }

        {
            let strings = ["a", "b", "c", "d", "e"];

            let things = strings
                .iter()
                .map(|&s| {
                    let Val::Own(handle) = Self::call_import(
                        wit,
                        Some("a:b/x"),
                        "[constructor]thing",
                        &[Val::String(s.into())],
                    )
                    .unwrap() else {
                        unreachable!()
                    };
                    Val::Own(handle)
                })
                .collect::<Vec<_>>();

            let Some(Type::Stream(ty)) = wit
                .unwrap_import(Some("a:b/x"), "[async]short-reads")
                .params()
                .next()
            else {
                unreachable!();
            };

            let vtable = get_stream_vtable(ty);

            let (mut tx, rx) = unsafe { async_support::stream_new(vtable) };

            let Some(Val::Stream(rx)) = Self::call_import_async(
                wit,
                Some("a:b/x"),
                "[async]short-reads",
                &[Val::Stream(rx.take_handle())],
            )
            .await
            else {
                unreachable!()
            };

            let mut rx = StreamReader::new(rx, vtable);

            // Write the items all at once.  The receiver will only read them
            // one at a time, forcing us to retake ownership of the unwritten
            // items between writes.
            tx.write_all(things).await;
            drop(tx);

            // Read only one item at a time, forcing the sender to retake
            // ownership of any unwritten items.
            let mut received_things = Vec::with_capacity(strings.len());
            loop {
                let (status, buffer) = rx.read(Vec::with_capacity(1)).await;
                received_things.extend(buffer);
                match status {
                    StreamResult::Complete(_) => {}
                    StreamResult::Dropped => break,
                    StreamResult::Cancelled => unreachable!(),
                }
            }

            let mut received_strings = Vec::with_capacity(received_things.len());
            for thing in received_things {
                let Val::Own(handle) = thing else {
                    unreachable!()
                };
                let Val::String(value) = Self::call_import_async(
                    wit,
                    Some("a:b/x"),
                    "[async method]thing.get",
                    &[Val::Borrow(handle.borrow())],
                )
                .await
                .unwrap() else {
                    unreachable!()
                };
                received_strings.push(value);
            }

            assert_eq!(
                &strings[..],
                &received_strings
                    .iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
            );
        }

        None
    }
}
