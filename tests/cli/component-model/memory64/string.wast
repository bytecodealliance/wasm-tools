;; RUN: wast --assert default --snapshot tests/snapshots -f cm64 %

(component
  (core module $m
    (func (export "f") (param i64 i64) (result i64) unreachable)
    (func (export "realloc") (param i64 i64 i64 i64) (result i64) i64.const 0)
    (memory (export "memory") i64 1)
  )
  (core instance $m (instantiate $m))
  (func (export "a") (param "a" string) (result string)
    (canon lift (core func $m "f") (realloc (func $m "realloc")) (memory $m "memory"))
  )
)
