;; Test try-catch blocks.

(module
  (tag $e0 (export "e0"))
  (func (export "throw") (throw $e0))
)

(register "test")

(module
  (tag $imported-e0 (import "test" "e0"))
  (func $imported-throw (import "test" "throw"))
  (tag $e0)
  (tag $e1)
  (tag $e2)
  (tag $e-i32 (param i32))
  (tag $e-f32 (param f32))
  (tag $e-i64 (param i64))
  (tag $e-f64 (param f64))

  (func $throw-if (param i32) (result i32)
    (local.get 0)
    (i32.const 0) (if (i32.ne) (then (throw $e0)))
    (i32.const 0)
  )

  (func (export "empty-catch")
    try
    catch $e0
    end
  )

  (func (export "simple-throw-catch") (param i32) (result i32)
    try (result i32)
      local.get 0
      i32.eqz
      if
        throw 1
      end
      i32.const 42
    catch 1
      i32.const 23
    end
  )

  (func (export "unreachable-not-caught") try unreachable catch_all end)

  (func $div (param i32 i32) (result i32)
    (local.get 0) (local.get 1) (i32.div_u)
  )
  (func (export "trap-in-callee") (param i32 i32) (result i32)
    try (result i32)
      local.get 0
      local.get 1
      call 5
    catch_all
      i32.const 11
    end
  )

  (func (export "catch-complex-1") (param i32) (result i32)
    try (result i32)
      try (result i32)
        local.get 0
        i32.eqz
        if
          throw 1
        else
          local.get 0
          i32.const 1
          i32.eq
          if
            throw 2
          else
            throw 3
          end
        end
        i32.const 2
      catch 1
        i32.const 3
      end
    catch 2
      i32.const 4
    end
  )

  (func (export "catch-complex-2") (param i32) (result i32)
    try (result i32)
      local.get 0
      i32.eqz
      if
        throw 1
      else
        local.get 0
        i32.const 1
        i32.eq
        if
          throw 2
        else
          throw 3
        end
      end
      i32.const 2
    catch 1
      i32.const 3
    catch 2
      i32.const 4
    end
  )

  (func (export "throw-catch-param-i32") (param i32) (result i32)
    try (result i32)
      local.get 0
      throw 4
      i32.const 2
    catch 4
      return
    end
  )

  (func (export "throw-catch-param-f32") (param f32) (result f32)
    try (result f32)
      local.get 0
      throw 5
      f32.const 0
    catch 5
      return
    end
  )

  (func (export "throw-catch-param-i64") (param i64) (result i64)
    try (result i64)
      local.get 0
      throw 6
      i64.const 2
    catch 6
      return
    end
  )

  (func (export "throw-catch-param-f64") (param f64) (result f64)
    try (result f64)
      local.get 0
      throw 7
      f64.const 0
    catch 7
      return
    end
  )

  (func $throw-param-i32 (param i32) (local.get 0) (throw $e-i32))
  (func (export "catch-param-i32") (param i32) (result i32)
    try (result i32)
      i32.const 0
      local.get 0
      call 13
    catch 4
    end
  )

  (func (export "catch-imported") (result i32)
    try (result i32)
      i32.const 1
      call 0
    catch 0
      i32.const 2
    end
  )

  (func (export "catchless-try") (param i32) (result i32)
    try (result i32)
      try (result i32)
        local.get 0
        call 1
      end
    catch 1
      i32.const 1
    end
  )

  (func $throw-void (throw $e0))
  (func (export "return-call-in-try-catch")
    try
      return_call 17
    catch 1
    end
  )

  (table funcref (elem $throw-void))
  (func (export "return-call-indirect-in-try-catch")
    try
      i32.const 0
      return_call_indirect (type 0)
    catch 1
    end
  )

  (func (export "break-try-catch")
    try
      br 0
    catch 1
    end
  )

  (func (export "break-try-catch_all")
    try
      br 0
    catch_all
    end
  )
)

(assert_return (invoke "empty-catch"))

(assert_return (invoke "simple-throw-catch" (i32.const 0)) (i32.const 23))
(assert_return (invoke "simple-throw-catch" (i32.const 1)) (i32.const 42))

(assert_trap (invoke "unreachable-not-caught") "unreachable")

(assert_return (invoke "trap-in-callee" (i32.const 7) (i32.const 2)) (i32.const 3))
(assert_trap (invoke "trap-in-callee" (i32.const 1) (i32.const 0)) "integer divide by zero")

(assert_return (invoke "catch-complex-1" (i32.const 0)) (i32.const 3))
(assert_return (invoke "catch-complex-1" (i32.const 1)) (i32.const 4))
(assert_exception (invoke "catch-complex-1" (i32.const 2)))

(assert_return (invoke "catch-complex-2" (i32.const 0)) (i32.const 3))
(assert_return (invoke "catch-complex-2" (i32.const 1)) (i32.const 4))
(assert_exception (invoke "catch-complex-2" (i32.const 2)))

(assert_return (invoke "throw-catch-param-i32" (i32.const 0)) (i32.const 0))
(assert_return (invoke "throw-catch-param-i32" (i32.const 1)) (i32.const 1))
(assert_return (invoke "throw-catch-param-i32" (i32.const 10)) (i32.const 10))

(assert_return (invoke "throw-catch-param-f32" (f32.const 5.0)) (f32.const 5.0))
(assert_return (invoke "throw-catch-param-f32" (f32.const 10.5)) (f32.const 10.5))

(assert_return (invoke "throw-catch-param-i64" (i64.const 5)) (i64.const 5))
(assert_return (invoke "throw-catch-param-i64" (i64.const 0)) (i64.const 0))
(assert_return (invoke "throw-catch-param-i64" (i64.const -1)) (i64.const -1))

(assert_return (invoke "throw-catch-param-f64" (f64.const 5.0)) (f64.const 5.0))
(assert_return (invoke "throw-catch-param-f64" (f64.const 10.5)) (f64.const 10.5))

(assert_return (invoke "catch-param-i32" (i32.const 5)) (i32.const 5))

(assert_return (invoke "catch-imported") (i32.const 2))

(assert_return (invoke "catchless-try" (i32.const 0)) (i32.const 0))
(assert_return (invoke "catchless-try" (i32.const 1)) (i32.const 1))

(assert_exception (invoke "return-call-in-try-catch"))
(assert_exception (invoke "return-call-indirect-in-try-catch"))

(assert_return (invoke "break-try-catch"))
(assert_return (invoke "break-try-catch_all"))

(module
  (func $imported-throw (import "test" "throw"))
  (tag $e0)

  (func (export "imported-mismatch") (result i32)
    try (result i32)  ;; label = @1
      try (result i32)  ;; label = @2
        i32.const 1
        call 0
      catch 0
        i32.const 2
      end
    catch_all
      i32.const 3
    end
  )
)

(assert_return (invoke "imported-mismatch") (i32.const 3))

;; Ignore per https://github.com/bytecodealliance/wasm-tools/issues/1671
;;
;; (assert_malformed
;;  (module quote "(module (func (catch_all)))")
;;  "unexpected token"
;; )

;; (assert_malformed
;;  (module quote "(module (tag $e) (func (catch $e)))")
;;  "unexpected token"
;; )

;; (assert_malformed
;;  (module quote
;;    "(module (func try catch_all catch_all end))"
;;  )
;;  "unexpected token"
;; )

(assert_invalid (module (func (result i32) try (result i32) end))
                "type mismatch: instruction requires [i32] but stack has []")
(assert_invalid (module (func (result i32) try (result i32) i64.const 42 end))
                "type mismatch: instruction requires [i32] but stack has [i64]")
(assert_invalid (module (tag) (func try catch 0 i32.const 42 end))
                "type mismatch: block requires [] but stack has [i32]")
(assert_invalid (module
                  (tag (param i64))
                  (func (result i32)
                    try (result i32) i32.const 42 catch 0 end))
                "type mismatch: instruction requires [i32] but stack has [i64]")
(assert_invalid (module (func try catch_all i32.const 32 end))
                "type mismatch: block requires [] but stack has [i32]")
