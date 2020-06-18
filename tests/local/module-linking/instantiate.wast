(module
  (import "" (module $m))
  (instance $a (instantiate $m))
)

(module
  (import "" (func))
  (import "" (global i32))
  (import "" (table 1 funcref))
  (import "" (instance))
  (import "" (memory 1))
  (import "" (module $empty))

  (import "" (module $m
    (import "" (module))
    (import "" (func))
    (import "" (global i32))
    (import "" (table 1 funcref))
    (import "" (instance))
    (import "" (memory 1))
  ))

  (instance $a (instantiate $m
    (module $empty)
    (func 0)
    (global 0)
    (table 0)
    (instance 0)
    (memory 0)
  ))
)

(module
  (import "" (module $m
    (import "" (module))
    (import "" (func))
    (import "" (global i32))
    (import "" (table 1 funcref))
    (import "" (instance))
    (import "" (memory 1))
  ))
  (import "" (module $m2))
  (import "" (instance $b))
  (func $f (import ""))
  (global $g (import "") i32)
  (memory $mem (import "") 1)
  (table $table (import "") 1 funcref)

  (instance $a
    (instantiate $m
      (module $m2)
      (func $f)
      (global $g)
      (table $table)
      (instance $b)
      (memory $mem)
    )
  )

  ;; inline exports/imports
  (instance $c (export "i") (instantiate $m2))
  (type $empty (instance))
  (instance $d (import "i") (type $empty))
  (instance (import "i"))
  (instance (import "i")
    (export "x" (func)))
  (instance (export "a") (export "b") (instantiate $m2))
  (instance (export "c") (export "d") (import "x"))
)

(assert_invalid
  (module
    (instance (instantiate 0))
  )
  "module is not defined")
(assert_invalid
  (module
    (import "" (module))
    (instance (instantiate 1))
  )
  "module is not defined")

(assert_invalid
  (module
    (import "" (func $f))
    (import "" (module $m))
    (instance (instantiate $m (func $f)))
  )
  "wrong number of imports provided")
(assert_invalid
  (module
    (import "" (module $m (import "" (func))))
    (instance (instantiate $m))
  )
  "wrong number of imports provided")


(assert_invalid
  (module
    (import "" (module $m
      (import "" (func))
    ))
    (import "" (global i32))
    (instance $i (instantiate $m (global 0)))
  )
  "wrong kind of item used for instantiate")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (func))
    ))
    (import "" (func (result i32)))
    (instance $i (instantiate $m (func 0)))
  )
  "function provided for instantiation has wrong type")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (func))
    ))
    (import "" (func (param i32)))
    (instance $i (instantiate $m (func 0)))
  )
  "function provided for instantiation has wrong type")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (global i32))
    ))
    (import "" (global i64))
    (instance $i (instantiate $m (global 0)))
  )
  "global provided for instantiation has wrong type")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (table 1 externref))
    ))
    (import "" (table 2 funcref))
    (instance $i (instantiate $m (table 0)))
  )
  "table provided for instantiation has wrong type")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (table 1 2 funcref))
    ))
    (import "" (table 2 funcref))
    (instance $i (instantiate $m (table 0)))
  )
  "table provided for instantiation has wrong type")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (table 2 2 funcref))
    ))
    (import "" (table 1 funcref))
    (instance $i (instantiate $m (table 0)))
  )
  "table provided for instantiation has wrong type")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (table 2 2 funcref))
    ))
    (import "" (table 2 3 funcref))
    (instance $i (instantiate $m (table 0)))
  )
  "table provided for instantiation has wrong type")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (memory 1 2 shared))
    ))
    (import "" (memory 1))
    (instance $i (instantiate $m (memory 0)))
  )
  "memory provided for instantiation has wrong type")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (memory 1))
    ))
    (import "" (memory 0))
    (instance $i (instantiate $m (memory 0)))
  )
  "memory provided for instantiation has wrong type")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (module
        (import "" (func))
      ))
    ))
    (import "" (module $i
      (import "" (global i32))
    ))
    (instance $i (instantiate $m (module $i)))
  )
  "wrong kind of item used for instantiate")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (module))
    ))
    (import "" (module $i
      (import "" (global i32))
    ))
    (instance $i (instantiate $m (module $i)))
  )
  "mismatched number of module imports")
(assert_invalid
  (module
    (import "" (module $m
      (import "" (module
        (import "" (global i32))
        (import "" (func))
      ))
    ))
    (import "" (module $i
      (import "" (global i32))
    ))
    (instance $i (instantiate $m (module $i)))
  )
  "mismatched number of module imports")

;; export subsets
(module
  (import "" (module $m
    (import "" (module
      (export "" (func))
    ))
  ))
  (import "" (module $i
    (export "" (func))
    (export "a" (func))
  ))
  (instance $i (instantiate $m (module $i)))
)
(module
  (import "" (module $m
    (import "" (instance
      (export "" (func))
    ))
  ))
  (import "" (instance $i
    (export "" (func))
    (export "a" (func))
  ))
  (instance (instantiate $m (instance $i)))
)
