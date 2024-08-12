;; This relies on the GC proposal (plus reference types) to check that shared
;; structs are not allowed without the shared-everything-threads proposal.
(assert_invalid
  (module
    (type $s (struct (field i32)))
    (func (param $x (ref null $s)) (result i32)
      local.get $x
      struct.atomic.get seq_cst $s 0)
  )
  "shared-everything-threads support is not enabled")
