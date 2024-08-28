;; RUN: print --skeleton %

(module
  (memory 0)
  (func $f unreachable)
  (data (i32.const 0) "1234")
  (table 1 funcref)
  (elem (i32.const 0) func $f)
  (@custom "hello" (after data) "data")
)
