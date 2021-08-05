#![no_main]

use libfuzzer_sys::*;
use wasmparser::*;
use Payload::*;

// Simulate receiving chunks of data by fuzzing over a `Vec<Vec<u8>>` where each
// element of the outer vec is a chunk of data we received.
//
// The assertion here is that parsing everything in one go should always produce
// the exact same results as an incremental parse.
fuzz_target!(|data: Vec<Vec<u8>>| {
    drop(env_logger::try_init());

    // Concatenate everything together, create our expected iterator of
    // payloads, and then write out `input.wasm` if debugging is enabled.
    let everything = data.iter().flat_map(|a| a).copied().collect::<Vec<_>>();
    let mut expected = Parser::new(0).parse_all(&everything);
    if log::log_enabled!(log::Level::Debug) {
        std::fs::write("input.wasm", &everything).unwrap();
    }

    // Create our parser as well as a stack of nested parsers for parsing nested
    // modules.
    let mut stack = Vec::new();
    let mut parser = Parser::new(0);

    // We'll be parsing data from `buf` starting at `pos`, and we translate
    // `data` into an iterator of chunks so when requested we'll take another
    // chunk of data and feed it in.
    let mut pos = 0;
    let mut buf = Vec::new();
    let mut data = data.into_iter().peekable();
    loop {
        log::debug!("parsing {}..{}", pos, buf.len());
        let payload = match parser.parse(&buf[pos..], data.peek().is_none()) {
            // If more data is requested then we're guaranteed that `data`
            // should have another element in its iterato, so pull that off and
            // add it to the end of the `buf`.
            Ok(Chunk::NeedMoreData(_n)) => {
                buf.extend(data.next().unwrap());
                continue;
            }
            Ok(Chunk::Parsed { consumed, payload }) => {
                log::debug!("parsed {} bytes", consumed);
                pos += consumed;
                payload
            }

            // On failure we should receive the same failure as if we did a full
            // parse.
            Err(actual) => {
                let expected = expected
                    .next()
                    .expect("full parse stopped early")
                    .err()
                    .expect("full parse didn't return an error");
                assert_eq!(expected.offset(), actual.offset());
                assert_eq!(expected.message(), actual.message());
                break;
            }
        };
        log::debug!("parsed payload {:?}", payload);
        let expected_payload = expected
            .next()
            .expect("full parse stopped early")
            .expect("full parse failed but incremental succeeded");
        match (payload, expected_payload) {
            (End, End) => match stack.pop() {
                Some(p) => parser = p,
                None => {
                    log::debug!("no more parsers");
                    assert!(expected.next().is_none());
                    break;
                }
            },
            (Version { num: a, range: ar }, Version { num: b, range: br }) => {
                assert_eq!(a, b);
                assert_eq!(ar, br);
            }

            (TypeSection(a), TypeSection(b)) => assert_eq!(a.range(), b.range()),
            (ImportSection(a), ImportSection(b)) => assert_eq!(a.range(), b.range()),
            (AliasSection(a), AliasSection(b)) => assert_eq!(a.range(), b.range()),
            (InstanceSection(a), InstanceSection(b)) => assert_eq!(a.range(), b.range()),
            (FunctionSection(a), FunctionSection(b)) => assert_eq!(a.range(), b.range()),
            (TableSection(a), TableSection(b)) => assert_eq!(a.range(), b.range()),
            (MemorySection(a), MemorySection(b)) => assert_eq!(a.range(), b.range()),
            (GlobalSection(a), GlobalSection(b)) => assert_eq!(a.range(), b.range()),
            (ExportSection(a), ExportSection(b)) => assert_eq!(a.range(), b.range()),
            (TagSection(a), TagSection(b)) => assert_eq!(a.range(), b.range()),
            (StartSection { func: a, range: ar }, StartSection { func: b, range: br }) => {
                assert_eq!(a, b);
                assert_eq!(ar, br);
            }
            (ElementSection(a), ElementSection(b)) => assert_eq!(a.range(), b.range()),
            (
                DataCountSection {
                    count: a,
                    range: ar,
                },
                DataCountSection {
                    count: b,
                    range: br,
                },
            ) => {
                assert_eq!(a, b);
                assert_eq!(ar, br);
            }
            (DataSection(a), DataSection(b)) => assert_eq!(a.range(), b.range()),
            (
                CustomSection {
                    name: a,
                    data_offset: ado,
                    data: ad,
                    range: ar,
                },
                CustomSection {
                    name: b,
                    data_offset: bdo,
                    data: bd,
                    range: br,
                },
            ) => {
                assert_eq!(a, b);
                assert_eq!(ad, bd);
                assert_eq!(ado, bdo);
                assert_eq!(ar, br);
            }
            (
                CodeSectionStart {
                    count: a,
                    range: ar,
                    size: asz,
                },
                CodeSectionStart {
                    count: b,
                    range: br,
                    size: bsz,
                },
            )
            | (
                ModuleSectionStart {
                    count: a,
                    range: ar,
                    size: asz,
                },
                ModuleSectionStart {
                    count: b,
                    range: br,
                    size: bsz,
                },
            ) => {
                assert_eq!(a, b);
                assert_eq!(ar, br);
                assert_eq!(asz, bsz);
            }

            (CodeSectionEntry(a), CodeSectionEntry(b)) => {
                assert_eq!(a.get_binary_reader().range(), b.get_binary_reader().range());
            }
            (
                ModuleSectionEntry {
                    range: ar,
                    parser: ap,
                },
                ModuleSectionEntry { range: br, .. },
            ) => {
                assert_eq!(ar, br);
                stack.push(parser);
                parser = ap;
            }
            (
                UnknownSection {
                    id: a,
                    contents: ac,
                    range: ar,
                },
                UnknownSection {
                    id: b,
                    contents: bc,
                    range: br,
                },
            ) => {
                assert_eq!(a, b);
                assert_eq!(ar, br);
                assert_eq!(ac, bc);
            }

            (a, b) => {
                panic!("expected {:?}\ngot {:?}", b, a);
            }
        }
    }
});
