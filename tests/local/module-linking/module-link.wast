(module
  (type $Wasi (instance))
  (module $B)
  (module $B_wrap
    ;; TODO: alias sugar
    (alias parent $Wasi (type $Wasi))
    (import "wasi" (instance $wasi (type $Wasi)))
    ;; TODO: alias sugar
    (alias parent $B (module $B))
    (instance $b (instantiate $B))
  )
)
(module
  (type $Wasi (instance))
  (import "wasi" (instance $wasi (type $Wasi)))

  (module $A
    (type $Wasi (instance))
    (import "wasi" (instance (type $Wasi)))
    (func (export "a"))
  )

  (module $B
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (import "A:1.x" (module $A
      (import "wasi" (instance (type $Wasi)))
      (export "a" (func))
    ))
    (instance $a (instantiate $A (arg "wasi" (instance $wasi))))
    (func (export "b"))
  )
  (module $B_wrap
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    ;; TODO: alias sugar
    (alias parent $B (module $B))
    (alias parent $A (module $A))
    (instance $b (instantiate $B
      (arg "wasi" (instance $wasi))
      (arg "A:1.x" (module $A)))
    )
    ;; TODO: alias sugar
    (alias $b "b" (func $b))
    (export "b" (func $b))
  )

  (module $C
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (import "B:1.x" (module $B
      (import "wasi" (instance $wasi (type $Wasi)))
      (export "b" (func))
    ))
    (instance $b (instantiate $B (arg "wasi" (instance $wasi))))
    (func (export "c"))
  )
  (module $C_wrap
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    ;; TODO: alias sugar
    (alias parent $C (module $C))
    (alias parent $B_wrap (module $B_wrap))
    (instance $c (instantiate $C
      (arg "wasi" (instance $wasi))
      (arg "B:1.x" (module $B_wrap))
    ))
    ;; TODO: alias sugar
    (alias $c "c" (func $c))
    (export "c" (func $c))
  )

  (module $D
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (import "C:1.x" (module $C
      (import "wasi" (instance $wasi (type $Wasi)))
      (export "c" (func))
    ))
    (instance $c (instantiate $C (arg "wasi" (instance $wasi))))
    (func (export "d"))
  )

  (instance $d (instantiate $D
    (arg "wasi" (instance $wasi))
    (arg "C:1.x" (module $C_wrap))
  ))

  ;; TODO: alias sugar
  (alias $d "d" (func $d))
  (export "d" (func $d))
)
