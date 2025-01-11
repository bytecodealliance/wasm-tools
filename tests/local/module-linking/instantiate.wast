(module
  (import "" (module $m))
  (instance $a (instantiate $m))
)

(module
  (import "a" (func))
  (import "b" (global i32))
  (import "c" (table 1 funcref))
  (import "d" (instance))
  (import "e" (memory 1))
  (import "f" (module $empty))

  (import "" (module $m
    (import "a" (module))
    (import "b" (func))
    (import "c" (global i32))
    (import "d" (table 1 funcref))
    (import "e" (instance))
    (import "f" (memory 1))
  ))

  (instance $a (instantiate $m
    (import "a" (module $empty))
    (import "b" (func 0))
    (import "c" (global 0))
    (import "d" (table 0))
    (import "e" (instance 0))
    (import "f" (memory 0))
  ))
)

(module
  (import "a" (module $m
    (import "a" (module))
    (import "b" (func))
    (import "c" (global i32))
    (import "d" (table 1 funcref))
    (import "e" (instance))
    (import "f" (memory 1))
  ))
  (import "b" (module $m2))
  (import "c" (instance $b))
  (func $f (import "d"))
  (global $g (import "e") i32)
  (memory $mem (import "f") 1)
  (table $table (import "g") 1 funcref)

  (instance $a
    (instantiate $m
      (import "a" (module $m2))
      (import "b" (func $f))
      (import "c" (global $g))
      (import "d" (table $table))
      (import "e" (instance $b))
      (import "f" (memory $mem))
    )
  )

  ;; inline exports/imports
  (instance $c (export "i") (instantiate $m2))
  (type $empty (instance))
  (instance $d (import "i1") (type $empty))
  (instance (import "i2"))
  (instance (import "i3")
    (export "x" (func)))
  (instance (export "a") (export "b") (instantiate $m2))
  (instance (export "c") (export "d") (import "x"))
)

(assert_invalid
  (module
    (instance (instantiate 0))
  )
  "unknown module")
(assert_invalid
  (module
    (import "" (module))
    (instance (instantiate 1))
  )
  "unknown module")

(module
  (import "a" (func $f))
  (import "b" (module $m))
  (instance (instantiate $m (import "a" (func $f))))
)
(assert_invalid
  (module
    (import "" (module $m (import "" (func))))
    (instance (instantiate $m))
  )
  "no import named ``")

(assert_invalid
  (module
    (import "" (module $m
      (import "" (func))
    ))
    (import "i" (global i32))
    (instance $i (instantiate $m (import "" (global 0))))
  )
  "item type mismatch")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (func))
    ))
    (import "i" (func (result i32)))
    (instance $i (instantiate $m (import "" (func 0))))
  )
  "func type mismatch")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (func))
    ))
    (import "i" (func (param i32)))
    (instance $i (instantiate $m (import "" (func 0))))
  )
  "func type mismatch")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (global i32))
    ))
    (import "i" (global i64))
    (instance $i (instantiate $m (import "" (global 0))))
  )
  "global type mismatch")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (table 1 externref))
    ))
    (import "i" (table 2 funcref))
    (instance $i (instantiate $m (import "" (table 0))))
  )
  "table type mismatch")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (table 1 2 funcref))
    ))
    (import "i" (table 2 funcref))
    (instance $i (instantiate $m (import "" (table 0))))
  )
  "table type mismatch")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (table 2 2 funcref))
    ))
    (import "i" (table 1 funcref))
    (instance $i (instantiate $m (import "" (table 0))))
  )
  "table type mismatch")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (table 2 2 funcref))
    ))
    (import "i" (table 2 3 funcref))
    (instance $i (instantiate $m (import "" (table 0))))
  )
  "table type mismatch")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (memory 1 2 shared))
    ))
    (import "i" (memory 1))
    (instance $i (instantiate $m (import "" (memory 0))))
  )
  "memory type mismatch")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (memory 1))
    ))
    (import "i" (memory 0))
    (instance $i (instantiate $m (import "" (memory 0))))
  )
  "memory type mismatch")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (module
        (import "" (func))
      ))
    ))
    (import "i" (module $i
      (import "" (global i32))
    ))
    (instance $i (instantiate $m (import "" (module $i))))
  )
  "item type mismatch")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (module))
    ))
    (import "i" (module $i
      (import "foobar" (global i32))
    ))
    (instance $i (instantiate $m (import "" (module $i))))
  )
  "no import named `foobar`")

;; it's ok to give a module with fewer imports
(module
  (import "" (module $m
    (import "" (module
      (import "" (global i32))
      (import "f" (func))
    ))
  ))
  (import "i" (module $i
    (import "" (global i32))
  ))
  (instance $i (instantiate $m (import "" (module $i))))
)

;; export subsets
(module
  (import "" (module $m
    (import "" (module
      (export "" (func))
    ))
  ))
  (import "i" (module $i
    (export "" (func))
    (export "a" (func))
  ))
  (instance $i (instantiate $m (import "" (module $i))))
)
(module
  (import "" (module $m
    (import "" (instance
      (export "" (func))
    ))
  ))
  (import "a" (instance $i
    (export "" (func))
    (export "a" (func))
  ))
  (instance (instantiate $m (import "" (instance $i))))
)

(assert_invalid
  (module
    (instance (instantiate 0))
  )
  "unknown module")

(module
  (module $m
    (module $sub (export "module")
      (func $f (export "") (result i32)
        i32.const 5))
  )
  (instance $a (instantiate $m))
  (instance $b (instantiate (module $a "module")))

  (func (export "get") (result i32)
    call (func $b ""))
)
