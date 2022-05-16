(component
  (type $Wasi (instance))
  (component $B)
  (component $B_wrap
    (import "wasi" (instance $wasi (type $Wasi)))
    (instance $b (instantiate (component $B)))
  )
)

;; FIXME(#588) this should be valid, or at least it ideally should be assuming
;; the rest of this test is updated. This is a pretty hard test to update while
;; that issue isn't implemented though.
(assert_invalid
(component
  (type $Wasi (instance))
  (import "wasi" (instance $wasi (type $Wasi)))

  (component $A
    (type $Wasi (instance))
    (import "wasi" (instance (type $Wasi)))

    (module $m
      (func (export "a"))
    )

    (instance $i (instantiate (module $m)))
    (func (export "a")
      (canon.lift (func) (func $i "a"))
    )
  )

  (component $B
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (import "A:1.x" (component $A
      (import "wasi" (instance (type $Wasi)))
      (export "a" (func))
    ))
    (instance $a (instantiate (component $A) (with "wasi" (instance $wasi))))

    (func $lower (canon.lower (func $a "a")))
    (module $b
      (import "a" "a" (func))
      (func (export "b"))
    )
    (instance $b (instantiate (module $b)
      (with "a" (instance (export "" (func $lower))))
    ))
    (func (export "b")
      (canon.lift (func) (func $b "a"))
    )
  )
  (component $B_wrap
    (type $Wasi (instance))
    (import "wasi" (instance $wasi (type $Wasi)))
    (instance $b (instantiate (component $B)
      (with "wasi" (instance $wasi))
      (with "A:1.x" (component $A)))
    )
    (export "b" (func $b "b"))
  )

  (; (component $C ;)
  (;   (type $Wasi (instance)) ;)
  (;   (import "wasi" (instance $wasi (type $Wasi))) ;)
  (;   (import "B:1.x" (component $B ;)
  (;     (import "wasi" (instance $wasi (type $Wasi))) ;)
  (;     (export "b" (func)) ;)
  (;   )) ;)
  (;   (instance $b (instantiate $B (import "wasi" (instance $wasi)))) ;)
  (;   (func (export "c")) ;)
  (; ) ;)
  (; (component $C_wrap ;)
  (;   (type $Wasi (instance)) ;)
  (;   (import "wasi" (instance $wasi (type $Wasi))) ;)
  (;   (instance $c (instantiate (component $C) ;)
  (;     (import "wasi" (instance $wasi)) ;)
  (;     (import "B:1.x" (component $B_wrap)) ;)
  (;   )) ;)
  (;   (export "c" (func $c "c")) ;)
  (; ) ;)

  (; (component $D ;)
  (;   (type $Wasi (instance)) ;)
  (;   (import "wasi" (instance $wasi (type $Wasi))) ;)
  (;   (import "C:1.x" (component $C ;)
  (;     (import "wasi" (instance $wasi (type $Wasi))) ;)
  (;     (export "c" (func)) ;)
  (;   )) ;)
  (;   (instance $c (instantiate $C (import "wasi" (instance $wasi)))) ;)
  (;   (func (export "d")) ;)
  (; ) ;)

  (; (instance $d (instantiate $D ;)
  (;   (import "wasi" (instance $wasi)) ;)
  (;   (import "C:1.x" (component $C_wrap)) ;)
  (; )) ;)

  (; (export "d" (func $d "d")) ;)
)
"instance 1 is not a module instance")
