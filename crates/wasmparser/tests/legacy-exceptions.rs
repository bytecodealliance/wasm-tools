// Tests for validate legacy-exceptions

#[test]
#[cfg(feature = "legacy-exceptions")]
fn check_legacy_exception_handling_opcodes() {
    let _binary_data = [0, 97, 115, 109, 1, 0, 0, 0, 1, 4, 1, 96, 0, 0, 3, 2, 1, 0, 13, 3, 1, 0, 0, 10, 21, 1, 19, 0, 6, 64, 6, 64, 6, 64, 8, 0, 25, 9, 0, 11, 24, 0, 7, 0, 11, 11];
    let _text_format_data = "
      (module
        (type (;0;) (func))
        (func (;0;)     (type 0)
          try ;; label = @1
            try ;; label = @2
              try ;; label = @3
                throw 0
              catch_all
                rethrow 0 (;@3;)
              end
            delegate 0 (;@2;)
          catch 0
          end
        )
        (tag (;0;) (type 0))
      )";

    // TODO: Use `_binary_data` or `_text_format_data`
}
