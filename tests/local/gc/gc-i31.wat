;; --enable-gc

(module
  (func $f (result i32 i32)
    (local $a (ref i31))
    (local $b (ref null i31))
    (local $c i31ref)

    (local.set $a (ref.i31 (i32.const 42)))
    (local.set $b (ref.i31 (i32.const 0)))
    (i31.get_u (local.get $a))
    (i31.get_s (local.get $b))
  )
)
