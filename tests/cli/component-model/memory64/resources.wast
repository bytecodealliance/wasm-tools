;; RUN: wast --assert default --snapshot tests/snapshots -f cm64 %

(component
  (type $x (resource (rep i64)))
)

(component
  (type $x (resource (rep i64)))

  (core func (canon resource.new $x))
  (core func (canon resource.rep $x))
  (core func (canon resource.drop $x))
)

(component
  (core module $m
    (func (export "dtor") (param i64))
  )
  (core instance $m (instantiate $m))
  (type $x (resource (rep i64) (dtor (core func $m "dtor"))))
  (core func (canon resource.new $x))
)

(component
  (type $x (resource (rep i64)))
  (core func $f1 (canon resource.new $x))
  (core func $f2 (canon resource.rep $x))
  (core func $f3 (canon resource.drop $x))

  (core module $m
    (import "" "f1" (func (param i64) (result i32)))
    (import "" "f2" (func (param i32) (result i64)))
    (import "" "f3" (func (param i32)))
  )

  (core instance (instantiate $m
    (with "" (instance
      (export "f1" (func $f1))
      (export "f2" (func $f2))
      (export "f3" (func $f3))
    ))
  ))
)

(assert_invalid
  (component
  (core module $m
    (func (export "dtor") (param i32))
  )
  (core instance $m (instantiate $m))
  (type $x (resource (rep i64) (dtor (core func $m "dtor"))))
  (core func (canon resource.new $x))
)
  "wrong signature for a destructor")

(assert_invalid
  (component
  (core module $m
    (func (export "dtor") (param i64))
  )
  (core instance $m (instantiate $m))
  (type $x (resource (rep i32) (dtor (core func $m "dtor"))))
  (core func (canon resource.new $x))
)
  "wrong signature for a destructor")

  (assert_invalid
  (component
    (type $x (resource (rep v128)))
  )
  "resources can only be represented by `i32` or `i64`")

;; A `borrow` of a locally-defined `(rep i64)` resource is passed as its `i64`
;; representation to a lifted function.
(component
  (type $x (resource (rep i64)))
  (core module $m
    (func (export "f") (param i64))
    (func (export "g") (param i32 i64))
    (func (export "h") (param i32 i64 i32))
  )
  (core instance $m (instantiate $m))
  (func (param "x" (borrow $x))
    (canon lift (core func $m "f")))
  (func (param "x" (option (borrow $x)))
    (canon lift (core func $m "g")))
  (func (param "x" (tuple u32 (borrow $x) (own $x)))
    (canon lift (core func $m "h")))
)

(assert_invalid
  (component
    (type $x (resource (rep i64)))
    (core module $m
      (func (export "f") (param i32))
    )
    (core instance $m (instantiate $m))
    (func (param "x" (borrow $x))
      (canon lift (core func $m "f")))
  )
  "lowered parameter types `[I64]` do not match parameter types `[I32]`")
