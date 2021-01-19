(module
  (func
    atomic.fence)
)

(module
  (memory 1)
  (func (result i32)
    i32.const 0
    i32.const 0
    memory.atomic.notify (memory 0) offset=0
  )
)

(assert_invalid
  (module
    (memory 1)
    (func (result i32)
      i32.const 0
      i32.const 0
      i64.const 0
      memory.atomic.wait32 (memory 1) offset=0
    )
  )
  "unknown memory 1")
