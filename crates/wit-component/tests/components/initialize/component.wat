(component
  (core module $main (;0;)
    (type (;0;) (func))
    (export "a" (func 0))
    (export "_initialize" (func 1))
    (func (;0;) (type 0)
      unreachable
    )
    (func (;1;) (type 0)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core instance $main (;0;) (instantiate $main))
  (core module $wit-component-fixup (;1;)
    (type (;0;) (func))
    (import "main" "_initialize" (func $_initialize (;0;) (type 0)))
    (start $_initialize)
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $fixup (;1;) (instantiate $wit-component-fixup
      (with "main" (instance $main))
    )
  )
  (type (;0;) (func))
  (alias core export $main "a" (core func $a (;0;)))
  (func $a (;0;) (type 0) (canon lift (core func $a)))
  (export $"#func1 a" (@name "a") (;1;) "a" (func $a))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
