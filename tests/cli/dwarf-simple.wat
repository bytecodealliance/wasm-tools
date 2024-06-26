;; RUN: print --generate-dwarf lines %

(module
  (func)
  (func $b)
  (func $"c d")
  (func
    i32.const 0
    drop
  )
)
