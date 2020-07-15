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
    let err = wasmprinter::print_bytes(&bytes).unwrap_err();
    assert!(
        err.to_string().contains("invalid code section"),
        "{:?}",
        err
    );
}

#[test]
fn locals_overflow() {
    let bytes = wat::parse_str(
        r#"
            (module binary
                "\00asm" "\01\00\00\00"     ;; module header

                "\01"           ;; type section
                "\04"           ;; size of section
                "\01"           ;; one type
                "\60\00\00"     ;; function, no parameters or results

                "\03"   ;; function section
                "\02"   ;; size of function section
                "\01"   ;; one function
                "\00"   ;; type 0

                "\0a"   ;; code section
                "\09"   ;; size of code section
                "\01"   ;; 1 function
                "\07"   ;; size of function
                "\01"   ;; one local
                "\ff\ff\ff\ff\00"   ;; lots of this type
                "\70"   ;; type
            )
        "#,
    )
    .unwrap();
    let err = wasmprinter::print_bytes(&bytes).unwrap_err();
    assert!(
        err.to_string().contains("maximum number of locals"),
        "{:?}",
        err
    );
}
