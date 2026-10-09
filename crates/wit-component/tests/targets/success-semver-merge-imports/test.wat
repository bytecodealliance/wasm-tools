(component
  (import "test:foo/foo@0.2.0" (instance (export "f" (func))))
  (import "test:foo/foo@0.2.3" (instance (export "f" (func)) (export "g" (func))))
)
