(assert_invalid
  (module
    (type $ty (func (param (ref null $ty))))
  )
  "unknown type 0: type index out of bounds because the GC proposal is disabled")
