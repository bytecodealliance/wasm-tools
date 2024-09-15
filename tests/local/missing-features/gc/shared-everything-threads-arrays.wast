;; This relies on the GC proposal (plus reference types) to check that shared
;; arrays are not allowed without the shared-everything-threads proposal.
(assert_invalid
  (module
    (type $a (array i32))
    (func (param $x (ref null $a)) (param $y i32) (result i32)
      local.get $x
      local.get $y
      array.atomic.get seq_cst $a)
  )
  "shared-everything-threads support is not enabled")
