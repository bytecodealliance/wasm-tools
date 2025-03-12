;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async

;; async lower
(component
  (import "foo" (func $foo (param "p1" u32) (result u32)))
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core func $foo (canon lower (func $foo) async (memory $libc "memory")))
  (core module $m
    (func (import "" "foo") (param i32 i32) (result i32))
  )
  (core instance $i (instantiate $m (with "" (instance (export "foo" (func $foo))))))
)

;; async lower; with incorrectly-typed core function
(assert_invalid
  (component
    (import "foo" (func $foo (param "p1" u32) (result u32)))
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core func $foo (canon lower (func $foo) async (memory $libc "memory")))
    (core module $m
      (func (import "" "foo") (param i32) (result i32))
    )
    (core instance $i (instantiate $m (with "" (instance (export "foo" (func $foo))))))
  )
  "type mismatch for export `foo` of module instantiation argument ``"
)

;; async lower; missing memory
(assert_invalid
  (component
    (import "foo" (func $foo (param "p1" u32) (result u32)))
    (core func $foo (canon lower (func $foo) async))
    (core module $m
      (func (import "" "foo") (param i32) (result i32))
    )
    (core instance $i (instantiate $m (with "" (instance (export "foo" (func $foo))))))
  )
  "canonical option `memory` is required"
)
