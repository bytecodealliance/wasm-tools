;; RUN: wast --assert default --snapshot tests/snapshots -f cm64 %

(component
  (core module $m
    (memory (export "m") i64 1)
    (func (export "f") (result i32) unreachable)
    (func (export "realloc") (param i64 i64 i64 i64) (result i64) unreachable)
  )
  (core instance $i (instantiate $m))
  (func (result string)
    (canon lift (core func $i "f")
      (memory $i "m")
      (realloc (func $i "realloc"))
    )
  )
)

(assert_invalid
  (component
    (core module $m
      (memory (export "m") i64 1)
      (func (export "f") (param i32 i32))
      (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
    )
    (core instance $i (instantiate $m))
    (func (param "p1" (list u8))
      (canon lift (core func $i "f")
        (memory $i "m")
        (realloc (func $i "realloc"))
      )
    )
  )
  "canonical option `realloc` uses a core function with an incorrect signature")

(assert_invalid
  (component
    (core module $m
      (memory (export "m") i32 1)
      (func (export "f") (param i32 i32))
      (func (export "realloc") (param i64 i64 i64 i64) (result i64) unreachable)
    )
    (core instance $i (instantiate $m))
    (func (param "p1" (list u8))
      (canon lift (core func $i "f")
        (memory $i "m")
        (realloc (func $i "realloc"))
      )
    )
  )
  "canonical option `realloc` uses a core function with an incorrect signature")