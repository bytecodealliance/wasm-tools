(component
  (type (;0;)
    (instance
      (type (;0;) (func (param "code" u32)))
      (export (;0;) "proc-exit" (func (type 0)))
    )
  )
  (import "foo:foo/my-wasi" (instance (;0;) (type 0)))
  (core module (;0;)
    (type (;0;) (func (param i32)))
    (type (;1;) (func))
    (import "wasi-snapshot-preview1" "proc_exit" (func $j (;0;) (type 0)))
    (memory (;0;) 1)
    (export "memory" (memory 0))
    (func (;1;) (type 1)
      i32.const 42
      call $j
      i32.const 42
      call $j
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module (;1;)
    (type (;0;) (func (param i32)))
    (import "foo:foo/my-wasi" "proc-exit" (func $proc_exit (;0;) (type 0)))
    (export "proc_exit" (func 1))
    (func (;1;) (type 0) (param i32)
      local.get 0
      call $proc_exit
    )
  )
  (core module (;2;)
    (type (;0;) (func (param i32)))
    (table (;0;) 1 1 funcref)
    (export "0" (func $adapt-wasi-snapshot-preview1-proc_exit))
    (export "$imports" (table 0))
    (func $adapt-wasi-snapshot-preview1-proc_exit (;0;) (type 0) (param i32)
      local.get 0
      i32.const 0
      call_indirect (type 0)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module (;3;)
    (type (;0;) (func (param i32)))
    (import "" "0" (func (;0;) (type 0)))
    (import "" "$imports" (table (;0;) 1 1 funcref))
    (elem (;0;) (i32.const 0) func 0)
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance (;0;) (instantiate 2))
  (alias core export 0 "0" (core func (;0;)))
  (core instance (;1;)
    (export "proc_exit" (func 0))
  )
  (core instance (;2;) (instantiate 0
      (with "wasi-snapshot-preview1" (instance 1))
    )
  )
  (alias core export 2 "memory" (core memory (;0;)))
  (alias export 0 "proc-exit" (func (;0;)))
  (core func (;1;) (canon lower (func 0)))
  (core instance (;3;)
    (export "proc-exit" (func 1))
  )
  (core instance (;4;) (instantiate 1
      (with "foo:foo/my-wasi" (instance 3))
    )
  )
  (alias core export 0 "$imports" (core table (;0;)))
  (alias core export 4 "proc_exit" (core func (;2;)))
  (core instance (;5;)
    (export "$imports" (table 0))
    (export "0" (func 2))
  )
  (core instance (;6;) (instantiate 3
      (with "" (instance 5))
    )
  )
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
