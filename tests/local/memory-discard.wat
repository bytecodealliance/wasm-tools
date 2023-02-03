(module
  (memory $m i32 2)

  (func
    (memory.discard (i32.const 0) (i32.const 65536))
  )
  (func
    (memory.discard $m (i32.const 0) (i32.const 65536))
  )

  (func
    (memory.discard 0 (i32.const 0) (i32.const 65536))
  )
)
