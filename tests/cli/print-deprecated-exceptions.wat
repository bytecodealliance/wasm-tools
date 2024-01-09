;; RUN: print %

(module
  (type (func (param i32 i64)))
  (type (func (param i32)))
  (tag (import "m" "t") (type 0))
  (tag (type 1))
  (func $check-throw
    i32.const 1
    i64.const 2
    throw 0
  )
  (func $check-try-catch-rethrow
    try (result i32 i64)
      call $check-throw
      unreachable
    catch 0
      ;; the exception arguments are on the stack at this point
    catch 1
      i64.const 2
    catch_all
      rethrow 0
    end
    drop
    drop
  )
  (func $try-with-params
    i32.const 0
    try (param i32) (result i32 i64)
      i32.popcnt
      drop
      call $check-throw
      unreachable
    catch 1
      i64.const 2
    catch_all
      i32.const 0
      i64.const 2
    end
    drop
    drop
  )
  (func $mix-old-and-new
    try_table
      try
      catch_all
      end
    end
  )
)
