use arbitrary::{Result, Unstructured};
use std::path::Path;
use wit_component as wit_component_new;
use wit_parser as wit_parser_new;

pub fn run(u: &mut Unstructured<'_>) -> Result<()> {
    let wasm = u.arbitrary().and_then(|config| {
        log::debug!("config: {config:#?}");
        wit_smith::smith(&config, u)
    })?;
    write_file("doc.wasm", &wasm);
    let r1 = wit_component_old::decode(&wasm).unwrap();
    let r1 = r1.resolve();
    let r2 = wit_component_new::decode(&wasm).unwrap();
    let r2 = r2.resolve();

    let mut sa32 = wit_parser_old::SizeAlign::new(wit_parser_old::AddressSize::Wasm32);
    let mut sa64 = wit_parser_old::SizeAlign::new(wit_parser_old::AddressSize::Wasm64);

    sa32.fill(r1);
    sa64.fill(r1);

    let mut alt = wit_parser_new::SizeAlign::default();
    alt.fill(r2);

    for ((t1, _), (t2, _)) in r1.types.iter().zip(r2.types.iter()) {
        let t1 = &wit_parser_old::Type::Id(t1);
        let t2 = &wit_parser_new::Type::Id(t2);
        let (s32, a32) = (sa32.size(t1), sa32.align(t1));
        let (s64, a64) = (sa64.size(t1), sa64.align(t1));
        let (salt, aalt) = (alt.size(t2), alt.align(t2));

        assert!(s32 <= s64);
        assert!(a32 <= a64);

        assert_eq!(a32, aalt.align_wasm32());
        assert_eq!(a64, aalt.align_wasm64());

        assert_eq!(s32, salt.size_wasm32());
        assert_eq!(s64, salt.size_wasm64());

        match (t1, t2) {
            (wit_parser_old::Type::Id(id1), wit_parser_new::Type::Id(id2)) => {
                let tp1 = &r1.types[*id1];
                let tp2 = &r2.types[*id2];
                match (&tp1.kind, &tp2.kind) {
                    (
                        wit_parser_old::TypeDefKind::Record(r1),
                        wit_parser_new::TypeDefKind::Record(r2),
                    ) => {
                        let offsets32 = sa32.field_offsets(r1.fields.iter().map(|f| &f.ty));
                        let offsets64 = sa64.field_offsets(r1.fields.iter().map(|f| &f.ty));
                        let offsetsalt = alt.field_offsets(r2.fields.iter().map(|f| &f.ty));
                        for ((fd32, fd64), fdalt) in offsets32
                            .iter()
                            .zip(offsets64.iter())
                            .zip(offsetsalt.iter())
                        {
                            assert_eq!(fd32.0, fdalt.0.size_wasm32());
                            assert_eq!(fd64.0, fdalt.0.size_wasm64());
                        }
                    }
                    (
                        wit_parser_old::TypeDefKind::Tuple(t1),
                        wit_parser_new::TypeDefKind::Tuple(t2),
                    ) => {
                        let offsets32 = sa32.field_offsets(t1.types.iter());
                        let offsets64 = sa64.field_offsets(t1.types.iter());
                        let offsetsalt = alt.field_offsets(t2.types.iter());
                        for ((fd32, fd64), fdalt) in offsets32
                            .iter()
                            .zip(offsets64.iter())
                            .zip(offsetsalt.iter())
                        {
                            assert_eq!(fd32.0, fdalt.0.size_wasm32());
                            assert_eq!(fd64.0, fdalt.0.size_wasm64());
                        }
                    }
                    (
                        wit_parser_old::TypeDefKind::Variant(v1),
                        wit_parser_new::TypeDefKind::Variant(v2),
                    ) => {
                        let offset32 =
                            sa32.payload_offset(v1.tag(), v1.cases.iter().map(|f| f.ty.as_ref()));
                        let offset64 =
                            sa64.payload_offset(v1.tag(), v1.cases.iter().map(|f| f.ty.as_ref()));
                        let offsetalt =
                            alt.payload_offset(v2.tag(), v2.cases.iter().map(|f| f.ty.as_ref()));
                        assert_eq!(offset32, offsetalt.size_wasm32());
                        assert_eq!(offset64, offsetalt.size_wasm64());
                    }
                    _ => (),
                }
            }
            _ => (),
        }
    }

    Ok(())
}

fn write_file(path: &str, contents: impl AsRef<[u8]>) {
    if !log::log_enabled!(log::Level::Debug) {
        return;
    }
    log::debug!("writing file {path}");
    let contents = contents.as_ref();
    let path = Path::new(path);
    std::fs::write(path, contents).unwrap();
    if path.extension().and_then(|s| s.to_str()) == Some("wasm") {
        let path = path.with_extension("wat");
        log::debug!("writing file {}", path.display());
        std::fs::write(path, wasmprinter::print_bytes(&contents).unwrap()).unwrap();
    }
}
