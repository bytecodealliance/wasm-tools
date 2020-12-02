;; --enable-exceptions --enable-multi-value
(module 
  (type (func (param i32 i64)))
  (event (type 0))
  (func $check-br_on_exn-rethrow (param exnref)
    block $l (result i32 i64)
        local.get 0
        ;; exnref $e is on the stack at this point
        br_on_exn $l 0 ;; branch to $l with $e's arguments
        rethrow
    end
    drop
    drop
  )
  (func $check-throw
    i32.const 1
    i64.const 2
    throw 0
  )
  (func $check-try-w-calls (result i32)
    try (result i32)
      call $check-throw
      i32.const 0
    catch
      call $check-br_on_exn-rethrow
      i32.const 1
    end
  )
)

(assert_invalid
  (module
    (type (func))
    (func throw 0))
  "unknown event: event index out of bounds")

(assert_invalid
  (module
    (type (func))
    (func (param exnref)
      local.get 0
      br_on_exn 0 0))
  "unknown event: event index out of bounds")
