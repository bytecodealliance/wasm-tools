(component
  (import "log" (func $log (param string)))
  (module $libc
    (memory (export "memory") 1)
    (func (export "canonical_abi_realloc") (param i32 i32 i32 i32) (result i32)
      unreachable)
    (func (export "canonical_abi_free") (param i32 i32 i32)
      unreachable)
  )

  (module $my_module
    (import "env" "log-utf8" (func $log_utf8 (param i32 i32)))
    (import "env" "log-utf16" (func $log_utf16 (param i32 i32)))
    (import "env" "log-compact-utf16" (func $log_compact_utf16 (param i32 i32)))

    (func (export "log-utf8") (param i32 i32)
      local.get 0
      local.get 1
      call $log_utf8
    )
    (func (export "log-utf16") (param i32 i32)
      local.get 0
      local.get 1
      call $log_utf16
    )
    (func (export "log-compact-utf16") (param i32 i32)
      local.get 0
      local.get 1
      call $log_compact_utf16
    )
  )

  (instance $libc (instantiate (module $libc)))

  (func $log_lower_utf8 (canon.lower string=utf8 (into $libc) (func $log)))
  (func $log_lower_utf16 (canon.lower string=utf16 (into $libc) (func $log)))
  (func $log_lower_compact_utf16 (canon.lower string=latin1+utf16 (into $libc) (func $log)))

  (instance $my_instance (instantiate (module $my_module)
    (with "libc" (instance $libc))
    (with "env" (instance
      (export "log-utf8" (func $log_lower_utf8))
      (export "log-utf16" (func $log_lower_utf16))
      (export "log-compact-utf16" (func $log_lower_compact_utf16))
    ))
  ))

  (func (export "log1")
    (canon.lift (func (param string)) string=utf8 (into $libc)
      (func $my_instance "log-utf8")))
  (func (export "log2")
    (canon.lift (func (param string)) string=utf16 (into $libc)
      (func $my_instance "log-utf16")))
  (func (export "log3")
    (canon.lift (func (param string)) string=latin1+utf16 (into $libc)
      (func $my_instance "log-compact-utf16")))
)
