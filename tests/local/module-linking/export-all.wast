(module
  (import "" (instance $f1_instance (export "f1" (func))))
  (import "" (module $f1_module (export "f1" (func))))

  (module $require_f1
    (import "" (instance
      (export "f1" (func))
    ))
  )

  (instance (instantiate $require_f1 (instance $f1_instance)))

  (module $a
    (import "i" (instance $i
      (export "f1" (func))
    ))
    (export $i)
  )
  (instance $a (instantiate $a (instance $f1_instance)))
  (instance (instantiate $require_f1 (instance $a)))

  (module $b
    (import "i" (module $m
      (export "f1" (func))
    ))
    (instance $i (instantiate $m))
    (export $i)
  )
  (instance $b (instantiate $b (module $f1_module)))
  (instance (instantiate $require_f1 (instance $b)))

  (module $c
    (module $m
      (func (export "f1"))
    )
    (instance $i (instantiate $m))
    (export $i)
  )
  (instance $c (instantiate $c))
  (instance (instantiate $require_f1 (instance $c)))
)

(module
  (type $i (instance (export "a" (func))))
  (type $m (module (export $i)))

  (import "" (module $imported (type $m)))
  (import "" (module $to_instantiate
    (import "" (module (export "a" (func))))
  ))

  (instance (instantiate $to_instantiate (module $imported)))
)

(module
  (type $i (instance (export "a" (func))))
  (module
    (type $m (module (export $i)))

    (import "" (module $imported (type $m)))
    (import "" (module $to_instantiate
      (import "" (module (export "a" (func))))
    ))

    (instance (instantiate $to_instantiate (module $imported)))
  )
)
