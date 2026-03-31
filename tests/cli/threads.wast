;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_invalid
  (module
    (memory 1)
    (func (param i32) (result i32)
      local.get 0
      i32.atomic.load align=1
    )
  )
  "must always specify maximum alignment")

(assert_invalid
  (module
    (memory 1)
    (func (param i32) (result i32)
      local.get 0
      i32.atomic.load offset=0x1_0000_0000
    )
  )
  "offset out of range")
