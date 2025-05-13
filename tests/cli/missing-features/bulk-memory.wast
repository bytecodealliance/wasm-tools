;; RUN: wast % -f=-bulk-memory

(assert_invalid
  (module
    (memory 1)
    (func (param i32 i32 i32)
      local.get 0
      local.get 1
      local.get 2
      memory.copy))
  "bulk memory support is not enabled")

(assert_invalid
  (module
    (memory 1)
    (func (param i32 i32 i32)
      local.get 0
      local.get 1
      local.get 2
      memory.fill))
  "bulk memory support is not enabled")
