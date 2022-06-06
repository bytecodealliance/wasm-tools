(component
  (import "i" (instance $i
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param string)))
  ))
  (export "run" (func $i "f1"))
)

(component
  (import "i" (component $c
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param string)))
  ))
  (instance $i (instantiate $c))
  (export "run" (func $i "f1"))
)

(component
  (import "i" (core module $m
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param i32)))
  ))
  (core instance $i (instantiate $m))

  (core module $m2 (import "" "" (func)))

  (core instance (instantiate $m2 (with "" (instance (export "" (func $i "f1"))))))
)

(component
  (import "" (core module $libc
    (export "memory" (memory 1))
    (export "table" (table 0 funcref))
    (export "func" (func))
    (export "global" (global i32))
    (export "global mut" (global (mut i64)))
  ))
  (core instance $libc (instantiate $libc))
  (core alias export $libc "memory" (memory $mem))
  (core alias export $libc "table" (table $tbl))
  (core alias export $libc "func" (func $func))
  (core alias export $libc "global" (global $global))
  (core alias export $libc "global mut" (global $global_mut))

  (import "x" (core module $needs_libc
    (import "" "memory" (memory 1))
    (import "" "table" (table 0 funcref))
    (import "" "func" (func))
    (import "" "global" (global i32))
    (import "" "global mut" (global (mut i64)))
  ))

  (core instance (instantiate $needs_libc (with "" (instance
    (export "memory" (memory $mem))
    (export "table" (table $tbl))
    (export "func" (func $func))
    (export "global" (global $global))
    (export "global mut" (global $global_mut))
  ))))
)

(component
  (import "" (instance $i
    (export "1" (func))
    (export "2" (core module))
    (export "3" (instance))
  ))
  (export "1" (func $i "1"))
  (export "2" (core module $i "2"))
  (export "3" (instance $i "3"))
)

(component
  (import "" (core module $libc
    (export "memory" (memory 1))
    (export "table" (table 0 funcref))
    (export "func" (func))
    (export "global" (global i32))
    (export "global mut" (global (mut i64)))
  ))

  (import "x" (core module $needs_libc
    (import "" "memory" (memory 1))
    (import "" "table" (table 0 funcref))
    (import "" "func" (func))
    (import "" "global" (global i32))
    (import "" "global mut" (global (mut i64)))
  ))

  (core instance $libc (instantiate $libc))
  (core instance (instantiate $needs_libc (with "" (instance
    (export "memory" (memory $libc "memory"))
    (export "table" (table $libc "table"))
    (export "func" (func $libc "func"))
    (export "global" (global $libc "global"))
    (export "global mut" (global $libc "global mut"))
  ))))
)

(assert_invalid
  (component
    (import "" (instance (export "" (func))))
    (export "" (core module 0 ""))
  )
  "export `` for instance 0 is not a module")

(assert_invalid
  (component
    (component
      (component (export ""))
    )
    (instance (instantiate 0))
    (export "" (core module 0 ""))
  )
  "export `` for instance 0 is not a module")

(assert_invalid
  (component
    (import "" (core module))
    (core instance (instantiate 0))
    (core alias export 0 "" (func))
  )
  "core instance 0 has no export named ``")

(assert_invalid
  (component
    (core module)
    (core instance (instantiate 0))
    (core alias export 0 "" (func))
  )
  "core instance 0 has no export named ``")

(assert_invalid
  (component
    (import "" (component))
    (instance (instantiate 0))
    (alias export 0 "" (func))
  )
  "instance 0 has no export named ``")

(assert_invalid
  (component
    (import "a" (core module $a (export "" (func))))
    (import "b" (core module $b (import "" "" (func (param i32)))))

    (core instance $a (instantiate $a))
    (core instance $b (instantiate $b (with "" (instance $a))))
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

        (instance (instantiate $C
          (with "a" (func $a))
          (with "b" (func $b))
          (with "c" (func $c))
        ))
      )
    )
  )
)

;; multiple projections in alias sugar
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

  (instance (instantiate $b
    (with "" (func $a "b" "c" "d" "f"))
  ))
)

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
  (import "" (instance $foo (export "v" (core module))))
  (export "v" (core module $foo "v"))
)

(component $C
  (core module $m)
  (alias outer $C $m (core module $target))
  (export "v" (core module $target))
)

(component $C
  (component $m)
  (alias outer $C $m (component $target))
  (export "v" (component $target))
)

(assert_invalid
  (component (alias outer 100 0 (core module)))
  "invalid outer alias count of 100")

(assert_invalid
  (component (alias outer 0 0 (core module)))
  "index out of bounds")

(assert_invalid
  (component (alias outer 100 0 (component)))
  "invalid outer alias count of 100")

(assert_invalid
  (component (alias outer 0 0 (component)))
  "index out of bounds")
