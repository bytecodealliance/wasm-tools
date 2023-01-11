(assert_invalid
  (module
    (memory 1)
    (func (param i32) (result i32)
      local.get 0
      i32.atomic.load align=1
    )
  )
  "must always specify maximum alignment")
