(component
  (type (;0;)
    (instance
      (type (;0;) (func (param "code" u32)))
      (export (;0;) "proc-exit" (func (type 0)))
    )
  )
  (import "foo:foo/my-wasi" (instance (;0;) (type 0)))
  (type (;1;) (func))
  (import "f" (func (;0;) (type 1)))
  (type (;2;) (func (result u32)))
  (import "f2" (func (;1;) (type 2)))
  (import "g" (func (;2;) (type 1)))
  (import "g2" (func (;3;) (type 2)))
  (core module (;0;)
    (type (;0;) (func (param i32)))
    (type (;1;) (func))
    (type (;2;) (func (result i32)))
    (import "wasi-snapshot-preview1" "proc_exit" (func $exit1 (;0;) (type 0)))
    (import "wasi-snapshot-preview1" "proc_exit [v2]" (func $exit2 (;1;) (type 0)))
    (import "cm32p2" "f" (func $f_v1 (;2;) (type 1)))
    (import "cm32p2" "f2" (func $f2 (;3;) (type 2)))
    (import "cm32p2" "f [v2]" (func $f_v2 (;4;) (type 1)))
    (import "cm32p2" "f [v3]" (func $f_v3 (;5;) (type 1)))
    (import "cm32p2" "g" (func $g_v1 (;6;) (type 1)))
    (import "cm32p2" "g [v2]" (func $g_v2 (;7;) (type 1)))
    (import "cm32p2" "g2" (func $g2 (;8;) (type 2)))
    (memory (;0;) 1)
    (export "memory" (memory 0))
    (func (;9;) (type 1)
      call $f_v1
      call $f_v2
      call $f_v3
      call $f2
      drop
      call $g_v1
      call $g_v2
      call $g2
      drop
      i32.const 42
      call $exit1
      i32.const 42
      call $exit2
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
    (export "proc_exit [v2]" (func 0))
  )
  (core func (;1;) (canon lower (func 0)))
  (core func (;2;) (canon lower (func 1)))
  (core func (;3;) (canon lower (func 0)))
  (core func (;4;) (canon lower (func 0)))
  (core func (;5;) (canon lower (func 2)))
  (core func (;6;) (canon lower (func 2)))
  (core func (;7;) (canon lower (func 3)))
  (core instance (;2;)
    (export "f" (func 1))
    (export "f2" (func 2))
    (export "f [v2]" (func 3))
    (export "f [v3]" (func 4))
    (export "g" (func 5))
    (export "g [v2]" (func 6))
    (export "g2" (func 7))
  )
  (core instance (;3;) (instantiate 0
      (with "wasi-snapshot-preview1" (instance 1))
      (with "cm32p2" (instance 2))
    )
  )
  (alias core export 3 "memory" (core memory (;0;)))
  (alias export 0 "proc-exit" (func (;4;)))
  (core func (;8;) (canon lower (func 4)))
  (core instance (;4;)
    (export "proc-exit" (func 8))
  )
  (core instance (;5;) (instantiate 1
      (with "foo:foo/my-wasi" (instance 4))
    )
  )
  (alias core export 0 "$imports" (core table (;0;)))
  (alias core export 5 "proc_exit" (core func (;9;)))
  (core instance (;6;)
    (export "$imports" (table 0))
    (export "0" (func 9))
  )
  (core instance (;7;) (instantiate 3
      (with "" (instance 6))
    )
  )
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
