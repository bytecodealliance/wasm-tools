#[test]
fn no_panic() {
    let bytes = wat::parse_str(
        r#"
            (module
                (data (br 4294967295) "")
            )
        "#,
    )
    .unwrap();
    wasmprinter::print_bytes(&bytes).unwrap();
}

#[test]
fn code_section_overflow() {
    let bytes = wat::parse_str(
        r#"
            (module binary
                "\00asm"
                "\01\00\00\00"
                "\0a\10\01"
            )
        "#,
    )
    .unwrap();
    wasmprinter::print_bytes(&bytes).unwrap_err();
}
