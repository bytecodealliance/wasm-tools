(component
  (type $ty-new (;0;)
    (instance
      (type (;0;) (func (param "s" string)))
      (export (;0;) "log" (func (type 0)))
    )
  )
  (import "new" (instance $new (;0;) (type $ty-new)))
  (core module $main (;0;)
    (type (;0;) (func (param i32 i32)))
    (import "old" "log" (func (;0;) (type 0)))
    (memory (;0;) 1)
    (export "memory" (memory 0))
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module $wit-component:adapter:old (;1;)
    (type (;0;) (func (param i32 i32)))
    (import "new" "log" (func $log (;0;) (type 0)))
    (export "log" (func $log))
  )
  (core module $wit-component-shim-module (;2;)
    (type (;0;) (func (param i32 i32)))
    (type (;1;) (func (param i32 i32)))
    (global (;0;) (mut (ref 0)) ref.func $"trap stub before initialization")
    (global (;1;) (mut (ref 1)) ref.func $"#func2 trap stub before initialization")
    (export "g0" (global 0))
    (export "0" (func $adapt-old-log))
    (export "g1" (global 1))
    (export "1" (func $indirect-new-log))
    (func $"trap stub before initialization" (;0;) (type 0) (param i32 i32)
      unreachable
    )
    (func $adapt-old-log (;1;) (type 0) (param i32 i32)
      local.get 0
      local.get 1
      global.get 0
      return_call_ref 0
    )
    (func $"#func2 trap stub before initialization" (@name "trap stub before initialization") (;2;) (type 1) (param i32 i32)
      unreachable
    )
    (func $indirect-new-log (;3;) (type 1) (param i32 i32)
      local.get 0
      local.get 1
      global.get 1
      return_call_ref 1
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (alias core export $wit-component-shim-instance "0" (core func $adapt-old-log (;0;)))
  (core instance $old (;1;)
    (export "log" (func $adapt-old-log))
  )
  (core instance $main (;2;) (instantiate $main
      (with "old" (instance $old))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (alias core export $wit-component-shim-instance "1" (core func $indirect-new-log (;1;)))
  (core instance $new (;3;)
    (export "log" (func $indirect-new-log))
  )
  (core instance $"#core-instance4 old" (@name "old") (;4;) (instantiate $wit-component:adapter:old
      (with "new" (instance $new))
    )
  )
  (core module $wit-component-fixup (;3;)
    (type (;0;) (func (param i32 i32)))
    (type (;1;) (func))
    (import "actual" "0" (func $0 (;0;) (type 0)))
    (import "shim" "g0" (global $g0 (;0;) (mut (ref 0))))
    (import "actual" "1" (func $1 (;1;) (type 0)))
    (import "shim" "g1" (global $g1 (;1;) (mut (ref 0))))
    (start $start)
    (elem (;0;) declare func $0 $1)
    (func $start (;2;) (type 1)
      ref.func $0
      global.set $g0
      ref.func $1
      global.set $g1
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (alias core export $"#core-instance4 old" "log" (core func $log (;2;)))
  (alias export $new "log" (func $log (;0;)))
  (core func $"#core-func3 indirect-new-log" (@name "indirect-new-log") (;3;) (canon lower (func $log) (memory $memory) string-encoding=utf8))
  (core instance $actual (;5;)
    (export "0" (func $log))
    (export "1" (func $"#core-func3 indirect-new-log"))
  )
  (core instance $fixup (;6;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
