(component
  (type (;0;)
    (instance
      (type (;0;) (func (param "s" string) (result string)))
      (export (;0;) "foo" (func (type 0)))
    )
  )
  (import "foo:foo/bar" (instance (;0;) (type 0)))
  (type (;1;) (func (param "s" string) (result string)))
  (import "foo" (func (;0;) (type 1)))
  (core module (;0;)
    (type (;0;) (func (param i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32 i32 i32)))
    (type (;2;) (func (param i32 i32 i32 i32) (result i32)))
    (import "$root" "[async-lower]foo" (func (;0;) (type 0)))
    (import "foo:foo/bar" "[async-lower]foo" (func (;1;) (type 0)))
    (import "$root" "foo" (func (;2;) (type 1)))
    (import "foo:foo/bar" "foo" (func (;3;) (type 1)))
    (memory (;0;) 1)
    (export "memory" (memory 0))
    (export "cabi_realloc" (func 4))
    (func (;4;) (type 2) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module (;1;)
    (type (;0;) (func (param i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32 i32 i32)))
    (table (;0;) 4 4 funcref)
    (export "0" (func $"indirect-$root-[async-lower]foo"))
    (export "1" (func $indirect-$root-foo))
    (export "2" (func $"indirect-foo:foo/bar-[async-lower]foo"))
    (export "3" (func $indirect-foo:foo/bar-foo))
    (export "$imports" (table 0))
    (func $"indirect-$root-[async-lower]foo" (;0;) (type 0) (param i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      i32.const 0
      call_indirect (type 0)
    )
    (func $indirect-$root-foo (;1;) (type 1) (param i32 i32 i32)
      local.get 0
      local.get 1
      local.get 2
      i32.const 1
      call_indirect (type 1)
    )
    (func $"indirect-foo:foo/bar-[async-lower]foo" (;2;) (type 0) (param i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      i32.const 2
      call_indirect (type 0)
    )
    (func $indirect-foo:foo/bar-foo (;3;) (type 1) (param i32 i32 i32)
      local.get 0
      local.get 1
      local.get 2
      i32.const 3
      call_indirect (type 1)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module (;2;)
    (type (;0;) (func (param i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32 i32 i32)))
    (import "" "0" (func (;0;) (type 0)))
    (import "" "1" (func (;1;) (type 1)))
    (import "" "2" (func (;2;) (type 0)))
    (import "" "3" (func (;3;) (type 1)))
    (import "" "$imports" (table (;0;) 4 4 funcref))
    (elem (;0;) (i32.const 0) func 0 1 2 3)
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance (;0;) (instantiate 1))
  (alias core export 0 "0" (core func (;0;)))
  (alias core export 0 "1" (core func (;1;)))
  (core instance (;1;)
    (export "[async-lower]foo" (func 0))
    (export "foo" (func 1))
  )
  (alias core export 0 "2" (core func (;2;)))
  (alias core export 0 "3" (core func (;3;)))
  (core instance (;2;)
    (export "[async-lower]foo" (func 2))
    (export "foo" (func 3))
  )
  (core instance (;3;) (instantiate 0
      (with "$root" (instance 1))
      (with "foo:foo/bar" (instance 2))
    )
  )
  (alias core export 3 "memory" (core memory (;0;)))
  (alias core export 0 "$imports" (core table (;0;)))
  (alias core export 3 "cabi_realloc" (core func (;4;)))
  (core func (;5;) (canon lower (func 0) (memory 0) (realloc 4) string-encoding=utf8 async))
  (core func (;6;) (canon lower (func 0) (memory 0) (realloc 4) string-encoding=utf8))
  (alias export 0 "foo" (func (;1;)))
  (core func (;7;) (canon lower (func 1) (memory 0) (realloc 4) string-encoding=utf8 async))
  (alias export 0 "foo" (func (;2;)))
  (core func (;8;) (canon lower (func 2) (memory 0) (realloc 4) string-encoding=utf8))
  (core instance (;4;)
    (export "$imports" (table 0))
    (export "0" (func 5))
    (export "1" (func 6))
    (export "2" (func 7))
    (export "3" (func 8))
  )
  (core instance (;5;) (instantiate 2
      (with "" (instance 4))
    )
  )
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
