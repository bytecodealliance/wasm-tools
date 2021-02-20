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

    let bytes = wat::parse_str(
        r#"
            (module
                (func end)
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
    assert!(err.to_string().contains("Unexpected EOF"), "{:?}", err);
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

#[test]
fn memarg_too_big() {
    let bytes = wat::parse_str(
        r#"
            (module binary
                "\00asm" "\01\00\00\00"     ;; module header

                "\0b"           ;; data section
                "\07"           ;; size of section
                "\01"           ;; number of segments
                "\00"           ;; flags=active
                "\2e"           ;; i32.load16_s
                "\3f"           ;; alignment
                "\00"           ;; offset
                "\0b"           ;; end
                "\00"           ;; data size
            )
        "#,
    )
    .unwrap();
    let err = wasmprinter::print_bytes(&bytes).unwrap_err();
    assert!(
        err.to_string().contains("alignment in memarg too large"),
        "{:?}",
        err
    );
}

#[test]
fn no_panic_dangling_else() {
    let bytes = wat::parse_str(
        r#"
            (module
                (func else)
            )
        "#,
    )
    .unwrap();
    wasmprinter::print_bytes(&bytes).unwrap();
}

#[test]
fn module_section_too_large() {
    let bytes = wat::parse_str(
        r#"
            (module binary
                "\00asm" "\01\00\00\00"     ;; module header

                "\0e"           ;; module section
                "\08"           ;; size of section
                "\00"           ;; 0 modules
                ;; intentionally missing the rest of the section
            )
        "#,
    )
    .unwrap();
    let err = wasmprinter::print_bytes(&bytes).unwrap_err();
    assert!(
        err.to_string()
            .contains("unexpected eof reading module section"),
        "{:?}",
        err
    );
}
