(assert_invalid
  (module
    (global (shared i32) (i32.const 0))
  )
  "shared globals require the shared-everything-threads proposal")

(assert_invalid
  (module
    (global (import "spectest" "global_i64") (shared mut i64))
  )
  "shared globals require the shared-everything-threads proposal")

(assert_invalid
  (module
    (global $a (import "spectest" "global_i32") i32)
    (func (result i32) (global.atomic.get seq_cst $a))
  )
  "shared-everything-threads support is not enabled")
