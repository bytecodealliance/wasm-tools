(component
  (import "" (module $m))
  (instance $a (instantiate (module $m)))
)

(component
  (import "a" (func $i))
  (import "" (component $c (import "a" (func))))
  (instance (instantiate (component $c) (with "a" (func $i))))
)

(component
  (import "a" (value $i string))
  (import "" (component $c (import "a" (value string))))
  (instance (instantiate (component $c) (with "a" (value $i))))
)

(component
  (import "a" (component $i))
  (import "" (component $c (import "a" (component))))
  (instance (instantiate (component $c) (with "a" (component $i))))
)

(component
  (import "a" (module $i))
  (import "" (component $c (import "a" (module))))
  (instance (instantiate (component $c) (with "a" (module $i))))
)

(component
  (import "a" (instance $i))
  (import "" (component $c (import "a" (instance))))
  (instance (instantiate (component $c) (with "a" (instance $i))))
)

(component
  (import "a" (module $m
    (import "" "a" (func))
    (import "" "b" (global i32))
    (import "" "c" (table 1 funcref))
    (import "" "d" (memory 1))
  ))
  (import "b" (module $m2
    (export "a" (func))
    (export "b" (global i32))
    (export "c" (table 1 funcref))
    (export "d" (memory 1))
  ))
  (instance $x (instantiate (module $m2)))
  (instance (instantiate (module $m) (with "" (instance $x))))
)

(component
  (import "a" (module $m
    (import "" "d" (func))
    (import "" "c" (global i32))
    (import "" "b" (table 1 funcref))
    (import "" "a" (memory 1))
  ))
  (import "b" (module $m2
    (export "a" (func))
    (export "b" (global i32))
    (export "c" (table 1 funcref))
    (export "d" (memory 1))
  ))
  (instance $x (instantiate (module $m2)))

  (instance (instantiate (module $m) (with "" (instance
    (export "d" (func $x "a"))
    (export "c" (global $x "b"))
    (export "b" (table $x "c"))
    (export "a" (memory $x "d"))
  ))))
)

(component
  (import "a" (component $m
    (import "" (instance
      (export "a" (module))
    ))
  ))
  (import "b" (component $m2
    (export "b" (module))
  ))
  (instance $x (instantiate (component $m2)))

  (instance (instantiate (component $m) (with "" (instance
    (export "a" (module $x "b"))
  ))))
)

;; FIXME(#588) this should actually validate but it does not right now
(assert_invalid
(component
  (import "a" (component $c
    (import "a" (module))
    (import "b" (func))
    (import "c" (component))
    (import "d" (instance))
    (import "e" (value string))
  ))
  (module $m (import "b"))
  (func $f (import "c"))
  (component $c2 (import "d"))
  (instance $i (import "e"))
  (import "f" (value $v string))

  (instance
    (instantiate (component $c)
      (with "a" (module $m))
      (with "b" (func $f))
      (with "c" (component $c2))
      (with "d" (instance $i))
      (with "e" (value $v))
    )
  )

  ;; inline exports/imports
  (instance $c (export "i") (instantiate (module $m)))
  (type $empty (instance))
  (instance $d (import "i1") (type $empty))
  (instance (import "i2"))
  (instance (import "i3")
    (export "x" (func)))
  (instance (export "a") (export "b") (instantiate (module $m)))
  (instance (export "c") (export "d") (import "x"))
)
"instance 2 is not a component instance")

(assert_invalid
  (component
    (instance (instantiate (module 0)))
  )
  "unknown module")
(assert_invalid
  (component
    (instance (instantiate (component 0)))
  )
  "unknown component")
(assert_invalid
  (component
    (import "" (module))
    (instance (instantiate (module 1)))
  )
  "unknown module")

(component
  (import "a" (func $f))
  (import "b" (component $c))
  (instance (instantiate (component $c) (with "a" (func $f))))
)
(assert_invalid
  (component
    (import "" (module $m (import "" "" (func))))
    (instance (instantiate (module $m)))
  )
  "missing module instantiation argument")
(assert_invalid
  (component
    (import "" (component $m (import "" (func))))
    (instance (instantiate (component $m)))
  )
  "missing component instantiation argument")

(assert_invalid
  (component
    (import "" (component $m
      (import "" (func))
    ))
    (import "i" (component $c))
    (instance $i (instantiate (component $m) (with "" (component $c))))
  )
  "to be of type `function`")

;; FIXME(#587) this should be an invalid module
;; (assert_invalid
  (component
    (import "" (component $m
      (import "" (func))
    ))
    (import "i" (func $f (result string)))
    (instance $i (instantiate (component $m) (with "" (func $f))))
  )
;;  "func type mismatch")

(assert_invalid
  (component
    (import "" (component $m
      (import "" (func))
    ))
    (import "i" (func (param string)))
    (instance $i (instantiate (component $m) (with "" (func 0))))
  )
  "function type mismatch")

(assert_invalid
  (component
    (import "" (component $m
      (import "" (module
        (import "" "" (func))
      ))
    ))
    (import "i" (module $i
      (import "" "" (global i32))
    ))
    (instance $i (instantiate (component $m) (with "" (module $i))))
  )
  "module type mismatch")

(assert_invalid
  (component
    (import "" (component $m
      (import "" (module))
    ))
    (import "i" (module $i
      (import "" "foobar" (global i32))
    ))
    (instance $i (instantiate (component $m) (with "" (module $i))))
  )
  "module type mismatch")

;; it's ok to give a module with fewer imports
(component
  (import "" (component $m
    (import "" (module
      (import "" "" (global i32))
      (import "" "f" (func))
    ))
  ))
  (import "i" (module $i
    (import "" "" (global i32))
  ))
  (instance $i (instantiate (component $m) (with "" (module $i))))
)

;; export subsets
(component
  (import "" (component $m
    (import "" (module
      (export "" (func))
    ))
  ))
  (import "i" (module $i
    (export "" (func))
    (export "a" (func))
  ))
  (instance $i (instantiate (component $m) (with "" (module $i))))
)
(component
  (import "" (component $m
    (import "" (instance
      (export "" (func))
    ))
  ))
  (import "a" (instance $i
    (export "" (func))
    (export "a" (func))
  ))
  (instance (instantiate (component $m) (with "" (instance $i))))
)


;; ============================================================================
;; core wasm type checking

(assert_invalid
  (component
    (import "m1" (module $m1 (import "" "" (func))))
    (import "m2" (module $m2 (export "" (func (param i32)))))
    (instance $i (instantiate (module $m2)))
    (instance (instantiate (module $m1) (with "" (instance $i))))
  )
  "function type mismatch")
(assert_invalid
  (component
    (import "m1" (module $m1 (import "" "" (func))))
    (import "m2" (module $m2 (export "" (func (result i32)))))
    (instance $i (instantiate (module $m2)))
    (instance (instantiate (module $m1) (with "" (instance $i))))
  )
  "function type mismatch")
(assert_invalid
  (component
    (import "m1" (module $m1 (import "" "" (global i32))))
    (import "m2" (module $m2 (export "" (global i64))))
    (instance $i (instantiate (module $m2)))
    (instance (instantiate (module $m1) (with "" (instance $i))))
  )
  "global type mismatch")
(assert_invalid
  (component
    (import "m1" (module $m1 (import "" "" (table 1 funcref))))
    (import "m2" (module $m2 (export "" (table 2 externref))))
    (instance $i (instantiate (module $m2)))
    (instance (instantiate (module $m1) (with "" (instance $i))))
  )
  "table type mismatch")
(assert_invalid
  (component
    (import "m1" (module $m1 (import "" "" (table 1 2 funcref))))
    (import "m2" (module $m2 (export "" (table 2 funcref))))
    (instance $i (instantiate (module $m2)))
    (instance (instantiate (module $m1) (with "" (instance $i))))
  )
  "table type mismatch")
(assert_invalid
  (component
    (import "m1" (module $m1 (import "" "" (table 2 2 funcref))))
    (import "m2" (module $m2 (export "" (table 1 funcref))))
    (instance $i (instantiate (module $m2)))
    (instance (instantiate (module $m1) (with "" (instance $i))))
  )
  "table type mismatch")
(assert_invalid
  (component
    (import "m1" (module $m1 (import "" "" (table 2 2 funcref))))
    (import "m2" (module $m2 (export "" (table 2 3 funcref))))
    (instance $i (instantiate (module $m2)))
    (instance (instantiate (module $m1) (with "" (instance $i))))
  )
  "table type mismatch")
(assert_invalid
  (component
    (import "m1" (module $m1 (import "" "" (memory 1 2 shared))))
    (import "m2" (module $m2 (export "" (memory 1))))
    (instance $i (instantiate (module $m2)))
    (instance (instantiate (module $m1) (with "" (instance $i))))
  )
  "memory type mismatch")
(assert_invalid
  (component
    (import "m1" (module $m1 (import "" "" (memory 1))))
    (import "m2" (module $m2 (export "" (memory 0))))
    (instance $i (instantiate (module $m2)))
    (instance (instantiate (module $m1) (with "" (instance $i))))
  )
  "memory type mismatch")

(assert_invalid
  (component
    (instance (instantiate (module 0)))
  )
  "unknown module")

(component
  (component $m
    (module $sub (export "module")
      (func $f (export "") (result i32)
        i32.const 5))
  )
  (instance $a (instantiate (component $m)))
  (alias export $a "module" (module $sub))
  (instance $b (instantiate (module $sub)))

  (module $final
    (import "" "" (func $b (result i32)))
    (func (export "get") (result i32)
      call $b))

  (instance (instantiate (module $final) (with "" (instance $b))))
)

(assert_invalid
  (component (instance $i (export "" (func 0))))
  "function index out of bounds")

(assert_invalid
  (component (instance $i (export "" (instance 0))))
  "index out of bounds")

(assert_invalid
  (component (instance $i (export "" (component 0))))
  "index out of bounds")

(assert_invalid
  (component (instance $i (export "" (instance 0))))
  "index out of bounds")

(assert_invalid
  (component (instance $i (export "" (module 0))))
  "index out of bounds")

(assert_invalid
  (component (instance $i (export "" (value 0))))
  "index out of bounds")

(assert_invalid
  (component (instance core (export "" (func 0))))
  "index out of bounds")

(assert_invalid
  (component (instance core (export "" (table 0))))
  "index out of bounds")

(assert_invalid
  (component (instance core (export "" (global 0))))
  "index out of bounds")

(assert_invalid
  (component (instance core (export "" (memory 0))))
  "index out of bounds")

(assert_invalid
  (component
    (import "" (instance $i))
    (module $m)
    (instance (instantiate (module $m) (with "" (instance $i))))
  )
  "not a module instance")

;; FIXME(#605) I think this should be invalid
(component
  (module $m)
  (instance $i (instantiate (module $m)))
  (instance (instantiate (module $m)
    (with "" (instance $i))
    (with "" (instance $i))
  ))
)

(assert_invalid
  (component
    (module $m (func (export "")))
    (instance $i (instantiate (module $m)))
    (instance (instantiate (module $m)
      (with "" (instance $i))
      (with "" (instance $i))
    ))
  )
  "duplicate instantiation argument name")

(assert_invalid
  (component
    (module $m1 (func (export "")))
    (module $m2 (import "" "" (global i32)))
    (instance $i (instantiate (module $m1)))
    (instance (instantiate (module $m2)
      (with "" (instance $i))
    ))
  )
  "expected module instantiation argument `::` to be of type `global`")

(assert_invalid
  (component
    (module $m)
    (instance $i (instantiate (module $m)))
    (component $c)
    (instance (instantiate (component $c) (with "" (instance $i))))
  )
  "not a component instance")

(assert_invalid
  (component
    (component $m)
    (instance $i (instantiate (component $m)))
    (instance (instantiate (component $m)
      (with "" (instance $i))
      (with "" (instance $i))
    ))
  )
  "name `` already defined")

(assert_invalid
  (component
    (component $c (import "" (func)))
    (instance (instantiate (component $c)
      (with "" (component $c))
    ))
  )
  "expected component instantiation argument `` to be of type `function`")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate (component $c)
      (with "" (module 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate (component $c)
      (with "" (value 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate (component $c)
      (with "" (instance 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate (component $c)
      (with "" (func 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate (component $c)
      (with "" (component 100))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance
      (export "" (component $c))
      (export "" (component $c))
    )
  )
  "export name `` already defined")

(component
  (import "1" (instance $i))
  (import "2" (func $f))
  (import "3" (component $c))
  (import "4" (module $m))
  (import "5" (value $v string))
  (instance
    (export "1" (instance $i))
    (export "2" (func $f))
    (export "3" (component $c))
    (export "4" (module $m))
    (export "5" (value $v))
  )
)

(component
  (module $m
    (func (export "1"))
    (memory (export "2") 1)
    (table (export "3") 1 funcref)
    (global (export "4") i32 i32.const 0)
  )
  (instance $i (instantiate (module $m)))
  (instance core
    (export "a" (func $i "1"))
    (export "b" (memory $i "2"))
    (export "c" (table $i "3"))
    (export "d" (global $i "4"))
  )
)

(assert_invalid
  (component
    (module $m (func (export "")))
    (instance $i (instantiate (module $m)))
    (instance core
      (export "" (func $i ""))
      (export "" (func $i ""))
    )
  )
  "export name `` already defined")

(assert_invalid
  (component
    (component $c)
    (instance $i (instantiate (component $c)))
    (export "" (instance $i ""))
  )
  "no export named ``")

(assert_invalid
  (component
    (export "" (instance 100 ""))
  )
  "index out of bounds")
