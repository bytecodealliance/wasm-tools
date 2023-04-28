;; --enable-gc

(module
  (func $f (result i32)
    (local $a (ref i31))
    (local $b (ref null i31))
    (local $c i31ref)

    unreachable

    ;; (local.set $a (i31.new (i32.const 42)))
    ;; (i31.get_u (local.get $a))
  )
)
