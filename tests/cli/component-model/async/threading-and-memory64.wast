;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm-threading,cm64

(assert_invalid
  (component
    (core func (canon context.set i32 0))
    (core func (canon context.get i64 1))
  )
  "type must match previous context type")

(assert_invalid
  (component
    (core func (canon context.get i64 0))
    (core func (canon context.set i32 1))
  )
  "type must match previous context type")
