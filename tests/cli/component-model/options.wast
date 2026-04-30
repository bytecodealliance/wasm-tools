;; RUN: wast --assert default --snapshot tests/snapshots % -f custom-page-sizes

(component
  (core module $m
    (func (export "r") (param i32 i32 i32 i32) (result i32) unreachable)
    (memory (export "m") 0)

    (func (export "f"))
  )
  (core instance $i (instantiate $m))

  (func (canon lift (core func $i "f")))
  (func (canon lift (core func $i "f") (memory $i "m")))
  (func (canon lift (core func $i "f") (memory $i "m") (realloc (func $i "r"))))
  (func (canon lift (core func $i "f") (realloc (func $i "r")) (memory $i "m")))
)

;; `realloc` requires `memory`
(assert_invalid
  (component
    (core module $m
      (func (export "r") (param i32 i32 i32 i32) (result i32) unreachable)
      (func (export "f"))
    )
    (core instance $i (instantiate $m))
    (func (canon lift (core func $i "f") (realloc (func $i "r"))))
  )
  "canonical option `realloc` requires `memory` to also be specified")

;; even if `realloc` isn't needed, it's still validated
(assert_invalid
  (component
    (core module $m
      (func (export "r"))
      (memory (export "m") 0)
      (func (export "f"))
    )
    (core instance $i (instantiate $m))
    (func (canon lift (core func $i "f") (realloc (func $i "r")) (memory $i "m")))
  )
  "canonical option `realloc` uses a core function with an incorrect signature")

;; even if `memory` isn't needed, it's still validated
(assert_invalid
  (component
    (core module $m
      (memory (export "m") 0 (pagesize 1))
      (func (export "f"))
    )
    (core instance $i (instantiate $m))
    (func (canon lift (core func $i "f") (memory $i "m")))
  )
  "mismatch in page size for memories")
(assert_invalid
  (component
    (core module $m
      (memory (export "m") 0 1 shared)
      (func (export "f"))
    )
    (core instance $i (instantiate $m))
    (func (canon lift (core func $i "f") (memory $i "m")))
  )
  "mismatch in the shared flag for memories")
(assert_invalid
  (component
    (core module $m
      (memory (export "m") i64 0)
      (func (export "f"))
    )
    (core instance $i (instantiate $m))
    (func (canon lift (core func $i "f") (memory $i "m")))
  )
  "64-bit memories require the `cm64` feature to be enabled")
