(module
  (import "" "" (func (param i32)))
  (alias 0 "" (func))
  (func
    i32.const 0
    call 1)
)

(module
  (import "" "" (func))
  (module $m
    (import "" (instance (export "" (func)))))
  (instance (instantiate $m (import "" (instance 0))))
)

(module
  (import "" "a" (instance $i (export "a" (func))))
  (import "" "" (func))

  (module $m1
    (import "" (instance (export "" (func)))))
  (module $m2
    (import "" (instance (export "a" (func)))))

  ;; implicit instance should satisfy m1
  (instance (instantiate $m1 (import "" (instance 0))))
  ;; explicit instance should also still work
  (instance (instantiate $m2 (import "" (instance $i))))
)
