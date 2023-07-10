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
    assert!(
        err.to_string().contains("unexpected end-of-file"),
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
fn dangling_if() {
    let bytes = wat::parse_str(
        r#"
            (module
                (func if)
            )
        "#,
    )
    .unwrap();
    let wat = wasmprinter::print_bytes(&bytes).unwrap();
    wat::parse_str(&wat).unwrap();
}

#[test]
fn no_oom() {
    // Whatever is printed here, it shouldn't take more than 500MB to print
    // since it's only 20k functions.
    let mut s = String::new();
    s.push_str("(module\n");
    for _ in 0..20_000 {
        s.push_str("(func if)\n");
    }
    s.push(')');
    let bytes = wat::parse_str(&s).unwrap();
    let wat = wasmprinter::print_bytes(&bytes).unwrap();
    assert!(wat.len() < 500_000_000);
}

#[test]
fn dont_reserve_the_world() {
    let bytes = wat::parse_str(
        r#"
            (module binary
                "\00asm" "\01\00\00\00"     ;; module header

                "\03"               ;; function section
                "\05"               ;; section size
                "\ff\ff\ff\ff\0f"   ;; number of functions (u32::MAX)
            )
        "#,
    )
    .unwrap();
    let err = wasmprinter::print_bytes(&bytes).unwrap_err();
    assert!(
        err.to_string()
            .contains("functions which exceeds the limit"),
        "{:?}",
        err
    );
}

#[test]
fn label_shadowing_block() {
    const MODULE: &str = r#"
      (module
        (type (;0;) (func))
        (func (;0;) (type 0)
          block $a ;; label = @1
            br 0 (;@1;)
          end
          block $a ;; label = @1
            br 0 (;@1;)
          end
        )
      )
    "#;
    let bytes = wat::parse_str(MODULE).unwrap();
    let result = wasmprinter::print_bytes(&bytes).unwrap();
    assert_eq!(
        result.replace(" ", "").trim(),
        MODULE.replace(" ", "").trim()
    );
}

#[test]
fn label_shadowing_block_confusion() {
    // Make sure we don’t refer to a shadowed label via a name.
    const MODULE: &str = r#"
      (module
        (type (;0;) (func))
        (func (;0;) (type 0)
          block $a ;; label = @1
            block $a ;; label = @2
              br 1 (;@1;)
            end
          end
        )
      )
    "#;
    let bytes = wat::parse_str(MODULE).unwrap();
    let result = wasmprinter::print_bytes(&bytes).unwrap();
    assert_eq!(
        result.replace(" ", "").trim(),
        MODULE.replace(" ", "").trim()
    );
}

#[test]
fn label_shadowing_locals() {
    const MODULE: &str = r#"
      (module
        (type (;0;) (func (param i32) (result i32)))
        (func (;0;) (type 0) (param $l i32) (result i32)
          (local $#local1<l> (@name "l") i32) (local $#local2<l> (@name "l") i32)
          local.get $l
        )
      )
    "#;
    let bytes = wat::parse_str(MODULE).unwrap();
    let result = wasmprinter::print_bytes(&bytes).unwrap();
    assert_eq!(
        result.replace(" ", "").trim(),
        MODULE.replace(" ", "").trim()
    );
}

#[test]
fn offsets_and_lines_smoke_test() {
    const MODULE: &str = r#"
        (;@0     ;) (module
        (;@b     ;)   (type (;0;) (func (param i32) (result i32)))
        (;@1f    ;)   (func (;0;) (type 0) (param i32) (result i32)
        (;@20    ;)     local.get 0
                      )
        (;@17    ;)   (export "f" (func 0))
                    )
    "#;
    let bytes = wat::parse_str(MODULE).unwrap();

    let mut printer = wasmprinter::Printer::new();
    let actual: Vec<_> = printer.offsets_and_lines(&bytes).unwrap().collect();

    #[rustfmt::skip]
    let expected = vec![
        (Some(0),    "(module\n"),
        (Some(0xb),  "  (type (;0;) (func (param i32) (result i32)))\n"),
        (Some(0x1f), "  (func (;0;) (type 0) (param i32) (result i32)\n"),
        (Some(0x20), "    local.get 0\n"),
        (None,       "  )\n"),
        (Some(0x17), "  (export \"f\" (func 0))\n"),
        (None,       ")"),
    ];

    assert_eq!(actual, expected);
}

#[test]
fn no_panic_non_func_type() {
    let bytes = wat::parse_str(
        "(module
            (type (struct))
            (func (type 0))
        )",
    )
    .unwrap();
    wasmprinter::print_bytes(&bytes).unwrap();
}
