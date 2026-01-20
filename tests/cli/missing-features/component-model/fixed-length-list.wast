;; RUN: wast % --assert default --snapshot tests/snapshots

(assert_invalid
  (component
    (core module $m
      (memory (export "memory") 1)
      (func (export "ret-list") (result i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "ret-list") (result (list u32 4))
      (canon lift (core func $i "ret-list") (memory $i "memory"))
    )
  )
  "Fixed size lists require the component model fixed-length list feature (at offset 0x54)"
)

(assert_invalid
  (component
    (core module $m
      (func (export "param-list") (param i32 i32 i32 i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "param-list") (param "l" (list u32 4))
      (canon lift (core func $i "param-list"))
    )
  )
  "Fixed size lists require the component model fixed-length list feature"
)
