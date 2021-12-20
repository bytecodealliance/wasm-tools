;; --enable-exceptions --enable-multi-value
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
)

(assert_invalid
  (module
    (type (func))
    (func throw 0))
  "unknown tag 0: tag index out of bounds")

(assert_invalid
  (module
    (func try catch_all catch_all end))
  "only one catch_all allowed per `try` block")

(assert_invalid
  (module
    (func try catch_all catch 0 end))
  "catch found outside of an `try` block")

(assert_invalid
  (module
    (func block try catch_all rethrow 1 end end))
  "target was not a `catch` block")
