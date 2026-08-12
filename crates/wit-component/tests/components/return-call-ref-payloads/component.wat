(component
  (type (;0;) (stream u8))
  (type (;1;) (func async (param "x" 0)))
  (import "foo" (func $foo (;0;) (type 1)))
  (core module $main (;0;)
    (type (;0;) (func (param i32) (result i32)))
    (type (;1;) (func (result i64)))
    (type (;2;) (func (param i32 i32 i32) (result i32)))
    (type (;3;) (func (param i32)))
    (type (;4;) (func (param i32 i32 i32 i32) (result i32)))
    (import "$root" "[async-lower]foo" (func (;0;) (type 0)))
    (import "$root" "[stream-new-0]foo" (func (;1;) (type 1)))
    (import "$root" "[stream-read-0]foo" (func (;2;) (type 2)))
    (import "$root" "[stream-write-0]foo" (func (;3;) (type 2)))
    (import "$root" "[stream-drop-readable-0]foo" (func (;4;) (type 3)))
    (import "$root" "[stream-drop-writable-0]foo" (func (;5;) (type 3)))
    (memory (;0;) 1)
    (export "memory" (memory 0))
    (export "cabi_realloc" (func 6))
    (func (;6;) (type 4) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module $wit-component-shim-module (;1;)
    (type (;0;) (func (param i32 i32 i32) (result i32)))
    (global (;0;) (mut (ref 0)) ref.func $"trap stub before initialization")
    (global (;1;) (mut (ref 0)) ref.func $"trap stub before initialization")
    (export "g0" (global 0))
    (export "0" (func $"$root-[stream-read-0]foo"))
    (export "g1" (global 1))
    (export "1" (func $"$root-[stream-write-0]foo"))
    (func $"trap stub before initialization" (;0;) (type 0) (param i32 i32 i32) (result i32)
      unreachable
    )
    (func $"$root-[stream-read-0]foo" (;1;) (type 0) (param i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      global.get 0
      return_call_ref 0
    )
    (func $"$root-[stream-write-0]foo" (;2;) (type 0) (param i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      global.get 1
      return_call_ref 0
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (core func $foo (;0;) (canon lower (func $foo) async))
  (core func $stream.new (;1;) (canon stream.new 0))
  (alias core export $wit-component-shim-instance "0" (core func $"$root-[stream-read-0]foo" (;2;)))
  (alias core export $wit-component-shim-instance "1" (core func $"$root-[stream-write-0]foo" (;3;)))
  (core func $stream.drop-readable (;4;) (canon stream.drop-readable 0))
  (core func $stream.drop-writable (;5;) (canon stream.drop-writable 0))
  (core instance $$root (;1;)
    (export "[async-lower]foo" (func $foo))
    (export "[stream-new-0]foo" (func $stream.new))
    (export "[stream-read-0]foo" (func $"$root-[stream-read-0]foo"))
    (export "[stream-write-0]foo" (func $"$root-[stream-write-0]foo"))
    (export "[stream-drop-readable-0]foo" (func $stream.drop-readable))
    (export "[stream-drop-writable-0]foo" (func $stream.drop-writable))
  )
  (core instance $main (;2;) (instantiate $main
      (with "$root" (instance $$root))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (core module $wit-component-fixup (;2;)
    (type (;0;) (func (param i32 i32 i32) (result i32)))
    (type (;1;) (func))
    (import "actual" "0" (func (;0;) (type 0)))
    (import "shim" "g0" (global (;0;) (mut (ref 0))))
    (import "actual" "1" (func (;1;) (type 0)))
    (import "shim" "g1" (global (;1;) (mut (ref 0))))
    (start $start)
    (elem (;0;) declare func 0 1)
    (func $start (;2;) (type 1)
      ref.func 0
      global.set 0
      ref.func 1
      global.set 1
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core func $stream.read (;6;) (canon stream.read 0 (memory $memory)))
  (core func $stream.write (;7;) (canon stream.write 0 (memory $memory)))
  (core instance $actual (;3;)
    (export "0" (func $stream.read))
    (export "1" (func $stream.write))
  )
  (core instance $fixup (;4;) (instantiate $wit-component-fixup
      (with "shim" (instance $wit-component-shim-instance))
      (with "actual" (instance $actual))
    )
  )
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
