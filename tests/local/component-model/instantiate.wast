(component
  (import "" (core module $m))
  (core instance $a (instantiate $m))
)

(component
  (import "a" (func $i))
  (import "" (component $c (import "a" (func))))
  (instance (instantiate $c (with "a" (func $i))))
)

(component
  (import "a" (value $i string))
  (import "" (component $c (import "a" (value string))))
  (instance (instantiate $c (with "a" (value $i))))
)

(component
  (import "a" (component $i))
  (import "" (component $c (import "a" (component))))
  (instance (instantiate $c (with "a" (component $i))))
)

(component
  (import "a" (core module $i))
  (import "" (component $c (import "a" (core module))))
  (instance (instantiate $c (with "a" (core module $i))))
)

(component
  (import "a" (instance $i))
  (import "" (component $c (import "a" (instance))))
  (instance (instantiate $c (with "a" (instance $i))))
)

(component
  (import "a" (core module $m
    (import "" "a" (func))
    (import "" "b" (global i32))
    (import "" "c" (table 1 funcref))
    (import "" "d" (memory 1))
  ))
  (import "b" (core module $m2
    (export "a" (func))
    (export "b" (global i32))
    (export "c" (table 1 funcref))
    (export "d" (memory 1))
  ))
  (core instance $x (instantiate $m2))
  (core instance (instantiate $m (with "" (instance $x))))
)

(component
  (import "a" (core module $m
    (import "" "d" (func))
    (import "" "c" (global i32))
    (import "" "b" (table 1 funcref))
    (import "" "a" (memory 1))
  ))
  (import "b" (core module $m2
    (export "a" (func))
    (export "b" (global i32))
    (export "c" (table 1 funcref))
    (export "d" (memory 1))
  ))
  (core instance $x (instantiate $m2))

  (core instance (instantiate $m (with "" (instance
    (export "d" (func $x "a"))
    (export "c" (global $x "b"))
    (export "b" (table $x "c"))
    (export "a" (memory $x "d"))
  ))))
)

(component
  (import "a" (component $m
    (import "" (instance
      (export "a" (core module))
    ))
  ))
  (import "b" (component $m2
    (export "b" (core module))
  ))
  (instance $x (instantiate $m2))

  (instance (instantiate $m (with "" (instance
    (export "a" (core module $x "b"))
  ))))
)

(component
  (import "a" (component $c
    (import "a" (core module))
    (import "b" (func))
    (import "c" (component))
    (import "d" (instance))
    (import "e" (value string))
  ))
  (core module $m (import "b"))
  (func $f (import "c"))
  (component $c2 (import "d"))
  (instance $i (import "e"))
  (import "f" (value $v string))

  (instance
    (instantiate $c
      (with "a" (core module $m))
      (with "b" (func $f))
      (with "c" (component $c2))
      (with "d" (instance $i))
      (with "e" (value $v))
    )
  )

  (core instance $c (instantiate $m))
  (core instance (instantiate $m))
  
  ;; inline exports/imports
  (type $empty (instance))
  (instance $d (import "i1") (type $empty))
  (instance (import "i2"))
  (instance (import "i3")
    (export "x" (func)))
  (instance (export "c") (export "d") (import "x"))
)

(assert_invalid
  (component
    (core instance (instantiate 0))
  )
  "unknown module")
(assert_invalid
  (component
    (instance (instantiate 0))
  )
  "unknown component")
(assert_invalid
  (component
    (import "" (core module))
    (core instance (instantiate 1))
  )
  "unknown module")

(component
  (import "a" (func $f))
  (import "b" (component $c))
  (instance (instantiate $c (with "a" (func $f))))
)
(assert_invalid
  (component
    (import "" (core module $m (import "" "" (func))))
    (core instance (instantiate $m))
  )
  "missing module instantiation argument")
(assert_invalid
  (component
    (import "" (component $m (import "" (func))))
    (instance (instantiate $m))
  )
  "missing component instantiation argument")

(assert_invalid
  (component
    (import "" (component $m
      (import "" (func))
    ))
    (import "i" (component $c))
    (instance $i (instantiate $m (with "" (component $c))))
  )
  "to be of type `function`")

(component
  (import "" (component $m
    (import "" (func))
  ))
  (import "i" (func $f (result string)))
  (instance $i (instantiate $m (with "" (func $f))))
)

(assert_invalid
  (component
    (import "" (component $m
      (import "" (func))
    ))
    (import "i" (func (param string)))
    (instance $i (instantiate $m (with "" (func 0))))
  )
  "function type mismatch")

(assert_invalid
  (component
    (import "" (component $m
      (import "" (core module
        (import "" "" (func))
      ))
    ))
    (import "i" (core module $i
      (import "" "" (global i32))
    ))
    (instance $i (instantiate $m (with "" (core module $i))))
  )
  "module type mismatch")

(assert_invalid
  (component
    (import "" (component $m
      (import "" (core module))
    ))
    (import "i" (core module $i
      (import "" "foobar" (global i32))
    ))
    (instance $i (instantiate $m (with "" (core module $i))))
  )
  "module type mismatch")

;; it's ok to give a module with fewer imports
(component
  (import "" (component $m
    (import "" (core module
      (import "" "" (global i32))
      (import "" "f" (func))
    ))
  ))
  (import "i" (core module $i
    (import "" "" (global i32))
  ))
  (instance $i (instantiate $m (with "" (core module $i))))
)

;; export subsets
(component
  (import "" (component $m
    (import "" (core module
      (export "" (func))
    ))
  ))
  (import "i" (core module $i
    (export "" (func))
    (export "a" (func))
  ))
  (instance $i (instantiate $m (with "" (core module $i))))
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
  (instance (instantiate $m (with "" (instance $i))))
)


;; ============================================================================
;; core wasm type checking

(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (func))))
    (import "m2" (core module $m2 (export "" (func (param i32)))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "function type mismatch")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (func))))
    (import "m2" (core module $m2 (export "" (func (result i32)))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "function type mismatch")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (global i32))))
    (import "m2" (core module $m2 (export "" (global i64))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "global type mismatch")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (table 1 funcref))))
    (import "m2" (core module $m2 (export "" (table 2 externref))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "table type mismatch")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (table 1 2 funcref))))
    (import "m2" (core module $m2 (export "" (table 2 funcref))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "table type mismatch")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (table 2 2 funcref))))
    (import "m2" (core module $m2 (export "" (table 1 funcref))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "table type mismatch")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (table 2 2 funcref))))
    (import "m2" (core module $m2 (export "" (table 2 3 funcref))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "table type mismatch")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (memory 1 2 shared))))
    (import "m2" (core module $m2 (export "" (memory 1))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "memory type mismatch")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (memory 1))))
    (import "m2" (core module $m2 (export "" (memory 0))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "memory type mismatch")

(assert_invalid
  (component
    (core instance (instantiate 0))
  )
  "unknown module")

(component
  (component $m
    (core module $sub (export "module")
      (func $f (export "") (result i32)
        i32.const 5))
  )
  (instance $a (instantiate $m))
  (alias export $a "module" (core module $sub))
  (core instance $b (instantiate $sub))

  (core module $final
    (import "" "" (func $b (result i32)))
    (func (export "get") (result i32)
      call $b))

  (core instance (instantiate $final (with "" (instance $b))))
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
  (component (instance $i (export "" (core module 0))))
  "index out of bounds")

(assert_invalid
  (component (instance $i (export "" (value 0))))
  "index out of bounds")

(assert_invalid
  (component (core instance (export "" (func 0))))
  "index out of bounds")

(assert_invalid
  (component (core instance (export "" (table 0))))
  "index out of bounds")

(assert_invalid
  (component (core instance (export "" (global 0))))
  "index out of bounds")

(assert_invalid
  (component (core instance (export "" (memory 0))))
  "index out of bounds")

(assert_invalid
  (component
    (core module $m)
    (core instance $i (instantiate $m))
    (core instance (instantiate $m
      (with "" (instance $i))
      (with "" (instance $i))
    ))
  )
  "duplicate module instantiation argument named ``"
)

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (core instance (instantiate $m
      (with "" (instance $i))
      (with "" (instance $i))
    ))
  )
  "duplicate module instantiation argument named ``")

(assert_invalid
  (component
    (core module $m1 (func (export "")))
    (core module $m2 (import "" "" (global i32)))
    (core instance $i (instantiate $m1))
    (core instance (instantiate $m2
      (with "" (instance $i))
    ))
  )
  "module instantiation argument `` exports an item named `` but it is not a global")

(assert_invalid
  (component
    (component $m)
    (instance $i (instantiate $m))
    (instance (instantiate $m
      (with "" (instance $i))
      (with "" (instance $i))
    ))
  )
  "duplicate component instantiation argument named ``")

(assert_invalid
  (component
    (component $c (import "" (func)))
    (instance (instantiate $c
      (with "" (component $c))
    ))
  )
  "expected component instantiation argument `` to be of type `function`")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate $c
      (with "" (core module 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate $c
      (with "" (value 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate $c
      (with "" (instance 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate $c
      (with "" (func 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate $c
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
  (import "4" (core module $m))
  (import "5" (value $v string))
  (instance
    (export "1" (instance $i))
    (export "2" (func $f))
    (export "3" (component $c))
    (export "4" (core module $m))
    (export "5" (value $v))
  )
)

(component
  (core module $m
    (func (export "1"))
    (memory (export "2") 1)
    (table (export "3") 1 funcref)
    (global (export "4") i32 i32.const 0)
  )
  (core instance $i (instantiate $m))
  (core instance
    (export "a" (func $i "1"))
    (export "b" (memory $i "2"))
    (export "c" (table $i "3"))
    (export "d" (global $i "4"))
  )
)

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (core instance
      (export "" (func $i ""))
      (export "" (func $i ""))
    )
  )
  "export name `` already defined")

(assert_invalid
  (component
    (component $c)
    (instance $i (instantiate $c))
    (export "" (instance $i ""))
  )
  "no export named ``")

(assert_invalid
  (component
    (export "" (instance 100 ""))
  )
  "index out of bounds")

(assert_invalid
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

    (core instance
      (instantiate $needs_libc
        (with "" (instance (export "memory" (memory $mem))))
      )
    )
  )
  "module instantiation argument `` does not export an item named `table`")
