(component
  (type $ty-foo:foo/foo (;0;)
    (instance
      (type (;0;) (list string))
      (type (;1;) (func (param "x" 0)))
      (export (;0;) "b" (func (type 1)))
      (type (;2;) (func (result string)))
      (export (;1;) "l" (func (type 2)))
      (type (;3;) (list u32))
      (type (;4;) (func (result 3)))
      (export (;2;) "m" (func (type 4)))
    )
  )
  (import "foo:foo/foo" (instance $foo:foo/foo (;0;) (type $ty-foo:foo/foo)))
  (core module $main (;0;)
    (type (;0;) (func (param i64 i64)))
    (type (;1;) (func (param i64)))
    (type (;2;) (func (param i64 i64 i64 i64) (result i64)))
    (import "foo:foo/foo" "b" (func (;0;) (type 0) (param i64 i64)))
    (import "foo:foo/foo" "l" (func (;1;) (type 1) (param i64)))
    (import "foo:foo/foo" "m" (func (;2;) (type 1) (param i64)))
    (memory (;0;) i64 1)
    (export "memory" (memory 0))
    (export "cabi_realloc" (func 3))
    (func (;3;) (type 2) (param i64 i64 i64 i64) (result i64)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module $wit-component-shim-module (;1;)
    (type (;0;) (func (param i64 i64)))
    (type (;1;) (func (param i64)))
    (global (;0;) (mut (ref 0)) ref.func $"trap stub before initialization")
    (global (;1;) (mut (ref 1)) ref.func $"#func2 trap stub before initialization")
    (global (;2;) (mut (ref 1)) ref.func $"#func2 trap stub before initialization")
    (export "g0" (global 0))
    (export "0" (func $indirect-foo:foo/foo-b))
    (export "g1" (global 1))
    (export "1" (func $indirect-foo:foo/foo-l))
    (export "g2" (global 2))
    (export "2" (func $indirect-foo:foo/foo-m))
    (func $"trap stub before initialization" (;0;) (type 0) (param i64 i64)
      unreachable
    )
    (func $indirect-foo:foo/foo-b (;1;) (type 0) (param i64 i64)
      local.get 0
      local.get 1
      global.get 0
      return_call_ref 0
    )
    (func $"#func2 trap stub before initialization" (@name "trap stub before initialization") (;2;) (type 1) (param i64)
      unreachable
    )
    (func $indirect-foo:foo/foo-l (;3;) (type 1) (param i64)
      local.get 0
      global.get 1
      return_call_ref 1
    )
    (func $indirect-foo:foo/foo-m (;4;) (type 1) (param i64)
      local.get 0
      global.get 2
      return_call_ref 1
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (alias core export $wit-component-shim-instance "0" (core func $indirect-foo:foo/foo-b (;0;)))
  (alias core export $wit-component-shim-instance "1" (core func $indirect-foo:foo/foo-l (;1;)))
  (alias core export $wit-component-shim-instance "2" (core func $indirect-foo:foo/foo-m (;2;)))
  (core instance $foo:foo/foo (;1;)
    (export "b" (func $indirect-foo:foo/foo-b))
    (export "l" (func $indirect-foo:foo/foo-l))
    (export "m" (func $indirect-foo:foo/foo-m))
  )
  (core instance $main (;2;) (instantiate $main
      (with "foo:foo/foo" (instance $foo:foo/foo))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (core module $wit-component-fixup (;2;)
    (type (;0;) (func (param i64 i64)))
    (type (;1;) (func (param i64)))
    (type (;2;) (func))
    (import "actual" "0" (func $0 (;0;) (type 0) (param i64 i64)))
    (import "shim" "g0" (global $g0 (;0;) (mut (ref 0))))
    (import "actual" "1" (func $1 (;1;) (type 1) (param i64)))
    (import "shim" "g1" (global $g1 (;1;) (mut (ref 1))))
    (import "actual" "2" (func $2 (;2;) (type 1) (param i64)))
    (import "shim" "g2" (global $g2 (;2;) (mut (ref 1))))
    (start $start)
    (elem (;0;) declare func $0 $1 $2)
    (func $start (;3;) (type 2)
      ref.func $0
      global.set $g0
      ref.func $1
      global.set $g1
      ref.func $2
      global.set $g2
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (alias export $foo:foo/foo "b" (func $b (;0;)))
  (core func $"#core-func3 indirect-foo:foo/foo-b" (@name "indirect-foo:foo/foo-b") (;3;) (canon lower (func $b) (memory $memory) string-encoding=utf8))
  (alias export $foo:foo/foo "l" (func $l (;1;)))
  (alias core export $main "cabi_realloc" (core func $cabi_realloc (;4;)))
  (core func $"#core-func5 indirect-foo:foo/foo-l" (@name "indirect-foo:foo/foo-l") (;5;) (canon lower (func $l) (memory $memory) (realloc $cabi_realloc) string-encoding=utf8))
  (alias export $foo:foo/foo "m" (func $m (;2;)))
  (core func $"#core-func6 indirect-foo:foo/foo-m" (@name "indirect-foo:foo/foo-m") (;6;) (canon lower (func $m) (memory $memory) (realloc $cabi_realloc)))
  (core instance $actual (;3;)
    (export "0" (func $"#core-func3 indirect-foo:foo/foo-b"))
    (export "1" (func $"#core-func5 indirect-foo:foo/foo-l"))
    (export "2" (func $"#core-func6 indirect-foo:foo/foo-m"))
  )
  (core instance $fixup (;4;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
