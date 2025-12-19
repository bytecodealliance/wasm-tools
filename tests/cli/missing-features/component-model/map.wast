;; RUN: wast % --assert default --snapshot tests/snapshots

(assert_invalid
  (component
    (core module $m
      (memory (export "memory") 1)
      (func (export "ret-map") (result i32 i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "ret-map") (result (map string u32))
      (canon lift (core func $i "ret-map") (memory $i "memory"))
    )
  )
  "Maps require the component model map feature (at offset 0x54)"
)

(assert_invalid
  (component
    (core module $m
      (func (export "param-map") (param i32 i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "param-map") (param "m" (map string u32))
      (canon lift (core func $i "param-map"))
    )
  )
  "Maps require the component model map feature"
)

(assert_invalid
  (component
    (type $map-type (map u32 string))
    (func (export "f") (param "x" $map-type))
  )
  "Maps require the component model map feature"
)

