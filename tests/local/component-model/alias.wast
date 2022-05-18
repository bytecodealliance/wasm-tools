;; FIXME(#588) should be valid
(assert_invalid
(component
  (import "i" (instance $i
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param string)))
  ))
  (export "run" (func $i "f1"))
)
"instance 0 is not a module instance")

;; FIXME(#588) should be valid
(assert_invalid
(component
  (import "i" (component $c
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param string)))
  ))
  (instance $i (instantiate (component $c)))
  (export "run" (func $i "f1"))
)
"instance 0 is not a module instance")

(component
  (import "i" (module $m
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param i32)))
  ))
  (instance $i (instantiate (module $m)))

  (module $m2 (import "" "" (func)))

  (instance (instantiate (module $m2) (with "" (instance (export "" (func $i "f1"))))))
)

(component
  (import "" (module $libc
    (export "memory" (memory 1))
    (export "table" (table 0 funcref))
    (export "func" (func))
    (export "global" (global i32))
    (export "global mut" (global (mut i64)))
  ))
  (instance $libc (instantiate (module $libc)))
  (alias export $libc "memory" (memory $mem))
  (alias export $libc "table" (table $tbl))
  (alias export $libc "func" (func $func))
  (alias export $libc "global" (global $global))
  (alias export $libc "global mut" (global $global_mut))

  (import "x" (module $needs_libc
    (import "" "memory" (memory 1))
    (import "" "table" (table 0 funcref))
    (import "" "func" (func))
    (import "" "global" (global i32))
    (import "" "global mut" (global (mut i64)))
  ))

  (instance (instantiate (module $needs_libc) (with "" (instance
    (export "memory" (memory $mem))
    (export "table" (table $tbl))
    (export "func" (func $func))
    (export "global" (global $global))
    (export "global mut" (global $global_mut))
  ))))
)

;; FIXME(#588) should be valid
(assert_invalid
(component
  (import "" (instance $i
    (export "1" (func))
    (export "2" (module))
    (export "3" (instance))
  ))
  (export "1" (func $i "1"))
  (export "2" (module $i "2"))
  (export "3" (instance $i "3"))
)
"instance 0 is not a module instance")

(component
  (import "" (module $libc
    (export "memory" (memory 1))
    (export "table" (table 0 funcref))
    (export "func" (func))
    (export "global" (global i32))
    (export "global mut" (global (mut i64)))
  ))

  (import "x" (module $needs_libc
    (import "" "memory" (memory 1))
    (import "" "table" (table 0 funcref))
    (import "" "func" (func))
    (import "" "global" (global i32))
    (import "" "global mut" (global (mut i64)))
  ))

  (instance $libc (instantiate (module $libc)))
  (instance (instantiate (module $needs_libc) (with "" (instance
    (export "memory" (memory $libc "memory"))
    (export "table" (table $libc "table"))
    (export "func" (func $libc "func"))
    (export "global" (global $libc "global"))
    (export "global mut" (global $libc "global mut"))
  ))))
)

(assert_invalid
  (component $outer
    (import "" (instance $i (export "a" (func))))

    (import "a" (component $m
      (import "" (component (export "a" (func))))
    ))

    (component $local
      (alias outer $outer $i (instance))
    )

    (instance (instantiate (component $m) (with "" (component $local))))
  )
  "invalid leading byte (0xff) for outer alias")

(assert_invalid
  (component
    (import "" (instance (export "" (func))))
    (export "" (module 0 ""))
  )
  "export `` for instance 0 is not a module")

(assert_invalid
  (component
    (component
      (component (export ""))
    )
    (instance (instantiate (component 0)))
    (export "" (module 0 ""))
  )
  "export `` for instance 0 is not a module")

(assert_invalid
  (component
    (import "" (module))
    (instance (instantiate (module 0)))
    (alias export 0 "" (func))
  )
  "instance 0 has no export named ``")

(assert_invalid
  (component
    (module)
    (instance (instantiate (module 0)))
    (alias export 0 "" (func))
  )
  "instance 0 has no export named ``")

;; FIXME(#588) should have a different error
(assert_invalid
  (component
    (import "" (component))
    (instance (instantiate (component 0)))
    (alias export 0 "" (func))
  )
  "instance 0 is not a module instance")
  (; "instance 0 has no export named ``") ;)

(assert_invalid
  (component
    (import "a" (module $a (export "" (func))))
    (import "b" (module $b (import "" "" (func (param i32)))))

    (instance $a (instantiate (module $a)))
    (instance $b (instantiate (module $b) (with "" (instance $a))))
  )
  "type mismatch")

;; aliasing various items works

(component $PARENT
  (type $t (func (result string)))
  (component
    (import "" (func (type $t)))
  )
  (component
    (alias outer $PARENT $t (type $my_type))
    (alias outer 0 $my_type (type $my_type_again))
    (import "" (func (type $my_type_again)))
  )
)

(component
  (type $a (func (result string)))
  (component
    (type $b (func (result u32)))
    (component
      (type $c (func (result s32)))

      (component
        (import "a" (func $a (type $a)))
        (import "b" (func $b (type $b)))
        (import "c" (func $c (type $c)))

        (import "" (component $C
          (import "a" (func (result string)))
          (import "b" (func (result u32)))
          (import "c" (func (result s32)))
        ))

        (instance (instantiate (component $C)
          (with "a" (func $a))
          (with "b" (func $b))
          (with "c" (func $c))
        ))
      )
    )
  )
)

;; multiple projections in alias sugar
;; FIXME(#588) should be valid
(assert_invalid
(component $a
  (import "" (instance $a
    (export "b" (instance
      (export "c" (instance
        (export "d" (instance
          (export "f" (func))
        ))
      ))
    ))
  ))

  (import "b" (component $b (import "" (func))))

  (instance (instantiate (component $b)
    (with "" (func $a "b" "c" "d" "f"))
  ))
)
"instance 3 is not a module instance")

;; alias some constructs
(component
  (import "" (instance $foo (export "v" (value s32))))
  (export "v" (value $foo "v"))
)

(component
  (import "" (instance $foo (export "v" (component))))
  (export "v" (component $foo "v"))
)

(component
  (import "" (instance $foo (export "v" (module))))
  (export "v" (module $foo "v"))
)

(component $C
  (module $m)
  (alias outer $C $m (module $target))
  (export "v" (module $target))
)

(component $C
  (component $m)
  (alias outer $C $m (component $target))
  (export "v" (component $target))
)

(assert_invalid
  (component (alias outer 100 0 (module)))
  "invalid outer alias count of 100")

(assert_invalid
  (component (alias outer 0 0 (module)))
  "index out of bounds")

(assert_invalid
  (component (alias outer 100 0 (component)))
  "invalid outer alias count of 100")

(assert_invalid
  (component (alias outer 0 0 (component)))
  "index out of bounds")
