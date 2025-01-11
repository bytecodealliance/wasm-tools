(module
  (type $Wasi (instance))
  (module $B)
  (module $B_wrap
    (import "wasi" (instance $wasi (type outer 0 $Wasi)))
    (instance $b (instantiate (module outer 0 $B)))
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
    (instance $a (instantiate $A (import "wasi" (instance $wasi))))
    (func (export "b"))
  )
  (module $B_wrap
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (instance $b (instantiate (module outer 0 $B)
      (import "wasi" (instance $wasi))
      (import "A:1.x" (module outer 0 $A)))
    )
    (export "b" (func $b "b"))
  )

  (module $C
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (import "B:1.x" (module $B
      (import "wasi" (instance $wasi (type $Wasi)))
      (export "b" (func))
    ))
    (instance $b (instantiate $B (import "wasi" (instance $wasi))))
    (func (export "c"))
  )
  (module $C_wrap
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (instance $c (instantiate (module outer 0 $C)
      (import "wasi" (instance $wasi))
      (import "B:1.x" (module outer 0 $B_wrap))
    ))
    (export "c" (func $c "c"))
  )

  (module $D
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (import "C:1.x" (module $C
      (import "wasi" (instance $wasi (type $Wasi)))
      (export "c" (func))
    ))
    (instance $c (instantiate $C (import "wasi" (instance $wasi))))
    (func (export "d"))
  )

  (instance $d (instantiate $D
    (import "wasi" (instance $wasi))
    (import "C:1.x" (module $C_wrap))
  ))

  (export "d" (func $d "d"))
)
