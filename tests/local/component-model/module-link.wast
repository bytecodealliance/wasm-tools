(component
  (type $Wasi (instance))
  (component $B)
  (component $B_wrap
    (import "wasi" (instance $wasi (type $Wasi)))
    (instance $b (instantiate $B))
  )
)

(component
  (type $Wasi (instance))
  (import "wasi" (instance $wasi (type $Wasi)))

  (component $A
    (type $Wasi (instance))
    (import "wasi" (instance (type $Wasi)))

    (core module $m
      (func (export "a"))
    )

    (core instance $i (instantiate $m))
    (func (export "a")
      (canon lift (core func $i "a"))
    )
  )

  (component $B
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (import "a1-x" (component $A
      (import "wasi" (instance (type $Wasi)))
      (export "a" (func))
    ))
    (instance $a (instantiate $A (with "wasi" (instance $wasi))))

    (core func $lower (canon lower (func $a "a")))
    (core module $b
      (import "a" "a" (func))
      (func (export "b"))
    )
    (core instance $b (instantiate $b
      (with "a" (instance (export "a" (func $lower))))
    ))
    (func (export "b")
      (canon lift (core func $b "b"))
    )
  )
  (component $B_wrap
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (instance $b (instantiate $B
      (with "wasi" (instance $wasi))
      (with "a1-x" (component $A)))
    )
    (export "b" (func $b "b"))
  )

  (component $C 
    (type $Wasi (instance)) 
    (import "wasi" (instance $wasi (type $Wasi))) 
    (import "b1-x" (component $B 
      (import "wasi" (instance $wasi (type $Wasi))) 
      (export "b" (func)) 
    )) 
    (instance $b (instantiate $B (with "wasi" (instance $wasi)))) 
    (export "c" (func $b "b")) 
  ) 
  (component $C_wrap 
    (type $Wasi (instance)) 
    (import "wasi" (instance $wasi (type $Wasi))) 
    (instance $c (instantiate $C
      (with "wasi" (instance $wasi)) 
      (with "b1-x" (component $B_wrap)) 
    )) 
    (export "c" (func $c "c")) 
  ) 

  (component $D 
    (type $Wasi (instance)) 
    (import "wasi" (instance $wasi (type $Wasi))) 
    (import "c1-x" (component $C 
      (import "wasi" (instance $wasi (type $Wasi))) 
      (export "c" (func)) 
    )) 
    (instance $c (instantiate $C (with "wasi" (instance $wasi)))) 
    (export "d" (func $c "c")) 
  ) 

  (instance $d (instantiate $D 
    (with "wasi" (instance $wasi)) 
    (with "c1-x" (component $C_wrap)) 
  )) 

  (export "d" (func $d "d")) 
)
