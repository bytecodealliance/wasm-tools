(assert_malformed
  (module
    (type (func (result i32)))
    (func $main (result i32)
      i32.const 0
      call_indirect (type 0)
    )
    (table (;0;) 1 externref)
  )
  "indirect calls must go through a table of funcref"
)
