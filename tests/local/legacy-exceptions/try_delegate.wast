;; Test try-delegate blocks.

(module
  (tag $e0)
  (tag $e1)

  (func (export "delegate-no-throw") (result i32)
    try (result i32)  ;; label = @1
      try (result i32)  ;; label = @2
        i32.const 1
      delegate 0
    catch 0
      i32.const 2
    end
  )

  (func $throw-if (param i32)
    (local.get 0)
    (if (then (throw $e0)) (else))
  )

  (func (export "delegate-throw") (param i32) (result i32)
    try (result i32)  ;; label = @1
      try (result i32)  ;; label = @2
        local.get 0
        call 1
        i32.const 1
      delegate 0
    catch 0
      i32.const 2
    end
  )

  (func (export "delegate-skip") (result i32)
    try (result i32)  ;; label = @1
      try (result i32)  ;; label = @2
        try (result i32)  ;; label = @3
          throw 0
          i32.const 1
        delegate 1
      catch 0
        i32.const 2
      end
    catch 0
      i32.const 3
    end
  )

  (func (export "delegate-to-block") (result i32)
    try (result i32)  ;; label = @1
      block  ;; label = @2
        try  ;; label = @3
          throw 0
        delegate 0
      end
      i32.const 0
    catch_all
      i32.const 1
    end
  )

  (func (export "delegate-to-catch") (result i32)
    try (result i32)  ;; label = @1
      try  ;; label = @2
        throw 0
      catch 0
        try  ;; label = @3
          rethrow 1 (;@2;)
        delegate 0
      end
      i32.const 0
    catch_all
      i32.const 1
    end
  )

  (func (export "delegate-to-caller-trivial")
    try  ;; label = @1
      throw 0
    delegate 0)

  (func (export "delegate-to-caller-skipping")
    try  ;; label = @1
      try  ;; label = @2
        throw 0
      delegate 1
    catch_all
    end
  )

  (func $select-tag (param i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
        end
        return
      end
      throw 0
    end
    throw 1
  )

  (func (export "delegate-merge") (param i32 i32) (result i32)
    try (result i32)  ;; label = @1
      local.get 0
      call 8
      try (result i32)  ;; label = @2
        local.get 1
        call 8
        i32.const 1
      delegate 0
    catch 0
      i32.const 2
    end
  )

  (func (export "delegate-throw-no-catch") (result i32)
    try (result i32)  ;; label = @1
      try (result i32)  ;; label = @2
        throw 0
        i32.const 1
      delegate 0
    catch 1
      i32.const 2
    end
  )

  (func (export "delegate-correct-targets") (result i32)
    try (result i32)  ;; label = @1
      try  ;; label = @2
        try  ;; label = @3
          try  ;; label = @4
            try  ;; label = @5
              try  ;; label = @6
                throw 0
              delegate 1
            catch_all
              unreachable
            end
          delegate 1
        catch_all
          unreachable
        end
      catch_all
        try  ;; label = @3
          throw 0
        delegate 0
      end
      unreachable
    catch_all
      i32.const 1
    end)

  (func $throw-void (throw $e0))
  (func (export "return-call-in-try-delegate")
    try  ;; label = @1
      try  ;; label = @2
        return_call 12
      delegate 0
    catch 0
    end
  )

  (table funcref (elem $throw-void))
  (func (export "return-call-indirect-in-try-delegate")
    try  ;; label = @1
      try  ;; label = @2
        i32.const 0
        return_call_indirect (type 0)
      delegate 0
    catch 0
    end
  )

  (func (export "break-try-delegate")
    try  ;; label = @1
      br 0 (;@1;)
    delegate 0)

  (func (export "break-and-call-throw") (result i32)
    try (result i32)  ;; label = @1
      try (result i32)  ;; label = @2
        block  ;; label = @3
          try  ;; label = @4
            br 1 (;@3;)
          delegate 2
        end
        call 12
        i32.const 0
      catch 0
        i32.const 1
      end
    catch 0
      i32.const 2
    end
  )

  (func (export "break-and-throw") (result i32)
    try (result i32)  ;; label = @1
      try (result i32)  ;; label = @2
        block  ;; label = @3
          try  ;; label = @4
            br 1 (;@3;)
          delegate 2
        end
        throw 0
        i32.const 0
      catch 0
        i32.const 1
      end
    catch 0
      i32.const 2
    end
  )
)

(assert_return (invoke "delegate-no-throw") (i32.const 1))

(assert_return (invoke "delegate-throw" (i32.const 0)) (i32.const 1))
(assert_return (invoke "delegate-throw" (i32.const 1)) (i32.const 2))

(assert_exception (invoke "delegate-throw-no-catch"))

(assert_return (invoke "delegate-merge" (i32.const 1) (i32.const 0)) (i32.const 2))
(assert_exception (invoke "delegate-merge" (i32.const 2) (i32.const 0)))
(assert_return (invoke "delegate-merge" (i32.const 0) (i32.const 1)) (i32.const 2))
(assert_exception (invoke "delegate-merge" (i32.const 0) (i32.const 2)))
(assert_return (invoke "delegate-merge" (i32.const 0) (i32.const 0)) (i32.const 1))

(assert_return (invoke "delegate-skip") (i32.const 3))

(assert_return (invoke "delegate-to-block") (i32.const 1))
(assert_return (invoke "delegate-to-catch") (i32.const 1))

(assert_exception (invoke "delegate-to-caller-trivial"))
(assert_exception (invoke "delegate-to-caller-skipping"))

(assert_return (invoke "delegate-correct-targets") (i32.const 1))

(assert_exception (invoke "return-call-in-try-delegate"))
(assert_exception (invoke "return-call-indirect-in-try-delegate"))

(assert_return (invoke "break-try-delegate"))

(assert_return (invoke "break-and-call-throw") (i32.const 1))
(assert_return (invoke "break-and-throw") (i32.const 1))

;; Ignore per https://github.com/bytecodealliance/wasm-tools/issues/1671
;;
;; (assert_malformed
;;   (module quote "(module (func (delegate 0)))")
;;   "unexpected token"
;; )

;; (assert_malformed
;;   (module quote "(module (tag $e) (func try catch $e delegate 0))")
;;   "unexpected token"
;; )

;; (assert_malformed
;;   (module quote "(module (func try catch_all delegate 0))")
;;   "unexpected token"
;; )

;; (assert_malformed
;;   (module quote "(module (func try delegate delegate 0))")
;;   "unexpected token"
;; )

(assert_invalid
  (module (func try delegate 1))
  "unknown label"
)
