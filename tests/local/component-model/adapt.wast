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

(assert_invalid
  (component
    (import "" (func $f))
    (func (canon.lower string=utf8 string=utf16 (func $f)))
  )
  "canonical option `utf8` conflicts with option `utf16`")

(assert_invalid
  (component
    (import "" (func $f))
    (func (canon.lower string=utf8 string=latin1+utf16 (func $f)))
  )
  "canonical option `utf8` conflicts with option `compact-utf16`")

(assert_invalid
  (component
    (import "" (func $f))
    (func (canon.lower string=utf16 string=latin1+utf16 (func $f)))
  )
  "canonical option `utf16` conflicts with option `compact-utf16`")

(assert_invalid
  (component
    (import "" (func $f))
    (func (canon.lower (into 0) (func $f)))
  )
  "instance index out of bounds")

(assert_invalid
  (component
    (import "" (func $f))
    (import "i" (instance $i))
    (func (canon.lower (into $i) (func $f)))
  )
  "not a module instance")

(assert_invalid
  (component
    (import "" (func $f))
    (module $m)
    (instance $i (instantiate (module $m)))
    (func (canon.lower (into $i) (into $i) (func $f)))
  )
  "`into` is specified more than once")

(assert_invalid
  (component
    (module $m
      (func (export "") (param i32 i32))
    )
    (instance $i (instantiate (module $m)))
    (func (canon.lift (func (param (list u8))) (func $i "")))
  )
  "canonical option `into` is required")

(assert_invalid
  (component
    (module $m
      (func (export "") (param i32 i32))
    )
    (instance $i (instantiate (module $m)))
    (func (canon.lift (func (param (list u8))) (into $i) (func $i "")))
  )
  "does not export a memory")

(assert_invalid
  (component
    (module $m
      (func (export "") (param i32 i32))
      (memory (export "memory") 0)
    )
    (instance $i (instantiate (module $m)))
    (func (canon.lift (func (param (list u8))) (into $i) (func $i "")))
  )
  "does not export a function named `canonical_abi_realloc`")

(assert_invalid
  (component
    (module $m
      (func (export "") (param i32 i32))
      (memory (export "memory") 0)
      (func (export "canonical_abi_realloc"))
    )
    (instance $i (instantiate (module $m)))
    (func (canon.lift (func (param (list u8))) (into $i) (func $i "")))
  )
  "wrong signature")

(assert_invalid
  (component
    (module $m
      (func (export "") (param i32 i32))
      (memory (export "memory") 0)
      (func (export "canonical_abi_realloc") (param i32 i32 i32 i32) (result i32)
        unreachable)
    )
    (instance $i (instantiate (module $m)))
    (func (canon.lift (func (param (list u8))) (into $i) (func $i "")))
  )
  "does not export a function named `canonical_abi_free`")

(assert_invalid
  (component
    (module $m
      (func (export "") (param i32 i32))
      (memory (export "memory") 0)
      (func (export "canonical_abi_realloc") (param i32 i32 i32 i32) (result i32)
        unreachable)
      (func (export "canonical_abi_free"))
    )
    (instance $i (instantiate (module $m)))
    (func (canon.lift (func (param (list u8))) (into $i) (func $i "")))
  )
  "wrong signature")

(assert_invalid
  (component
    (module $m
      (func (export ""))
    )
    (instance $i (instantiate (module $m)))
    (func (canon.lower (func $i "")))
  )
  "not a component function")

(assert_invalid
  (component
    (module $m (func (export "foo") (param i32)))
    (instance $i (instantiate (module $m)))
    (func (export "foo")
      (canon.lift (func) (func $i "foo")))
  )
  "lowered parameter types `[]` do not match parameter types `[I32]`")

(assert_invalid
  (component
    (module $m (func (export "foo") (result i32)))
    (instance $i (instantiate (module $m)))
    (func (export "foo")
      (canon.lift (func) (func $i "foo")))
  )
  "lowered result types `[]` do not match result types `[I32]`")

(assert_invalid
  (component
    (type $f string)
    (module $m (func (export "foo")))
    (instance $i (instantiate (module $m)))
    (func (export "foo")
      (canon.lift (type $f) (func $i "foo")))
  )
  "not a function type")

(assert_invalid
  (component
    (import "" (func $f))
    (func (export "foo")
      (canon.lift (func) (func $f)))
  )
  "not a core WebAssembly function")
