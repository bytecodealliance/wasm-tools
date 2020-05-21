(module
  (type $Wasi (instance))
  (module $B)
  (module $B_wrap
    (import "wasi" (instance $wasi (type $Wasi)))
    (instance $b (instantiate $B))
    (export $b)
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
    (instance $a (instantiate $A (instance $wasi)))
    (func (export "b"))
  )
  (module $B_wrap
    (import "wasi" (instance $wasi (type $Wasi)))
    (instance $b (instantiate $B (instance $wasi) (module $A)))
    (export $b)
  )

  (module $C
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (import "B:1.x" (module $B
      (import "wasi" (instance $wasi (type $Wasi)))
      (export "b" (func))
    ))
    (instance $b (instantiate $B (instance $wasi)))
    (func (export "c"))
  )
  (module $C_wrap
    (import "wasi" (instance $wasi (type $Wasi)))
    (instance $c (instantiate $C (instance $wasi) (module $B_wrap)))
    (export $c)
  )

  (module $D
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (import "C:1.x" (module $C
      (import "wasi" (instance $wasi (type $Wasi)))
      (export "c" (func))
    ))
    (instance $c (instantiate $C (instance $wasi)))
    (func (export "d"))
  )

  (instance $d (instantiate $D (instance $wasi) (module $C_wrap)))
  (export $d)
)
