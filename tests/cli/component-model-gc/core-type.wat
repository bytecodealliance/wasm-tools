;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

(component
  (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))
  (core type $ty (func (param i32 i32) (result i32)))
  (core func (canon lower (func $f) (core-type $ty)))
)

(assert_invalid
  (component
    (core module $m
      (memory (export "memory") 1)
      (func (export "f") (result i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (core type $ty (func (result i32)))
    (func (export "f") (result u32)
      (canon lift (core func $i "f") (core-type $ty))
    )
  )
  "canonical option `core-type` is not allowed in `canon lift`"
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))
    (core type $ty (func (param i64 i32) (result i32)))
    (core func (canon lower (func $f) (core-type $ty)))
  )
  "declared core type has `[I64, I32]` parameter types, but actual lowering has `[I32, I32]` parameter types"
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))
    (core type $ty (func (param i32 i32) (result i64)))
    (core func (canon lower (func $f) (core-type $ty)))
  )
  "declared core type has `[I64]` result types, but actual lowering has `[I32]` result types"
)
