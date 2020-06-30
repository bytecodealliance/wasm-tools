(assert_invalid
  (module
    (type (module
      (import "" (func (type 1)))
    ))
    (type (func))
  )
  "type index out of bounds")

(assert_invalid
  (module
    (func $f)
    (module
      (func
        call $f)
    )
  )
  "only parent types/modules can be aliased")


;; not enough imports/exports
(assert_invalid
  (module
    (type $m (module (export "" (func))))
    (module (type $m))
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module (import "" (func))))
    (module (type $m))
  )
  "inline module type does not match")

;; too many imports/exports
(assert_invalid
  (module
    (type $m (module))
    (module (type $m)
      (func (export ""))
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module (export "" (func))))
    (module (type $m)
      (func (export ""))
      (table (export "a") 1 funcref)
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module))
    (module (type $m)
      (func (import ""))
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module (import "" (func))))
    (module (type $m)
      (func (import ""))
      (table (import "") 1 funcref)
    )
  )
  "inline module type does not match")

;; mismatched funcs
(assert_invalid
  (module
    (type $m (module (import "" (func))))
    (module (type $m)
      (func (import "") (param i32))
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module (export "" (func))))
    (module (type $m)
      (func (export "") (param i32))
    )
  )
  "inline module type does not match")

;; mismatched tables
(assert_invalid
  (module
    (type $m (module (export "" (table 1 funcref))))
    (module (type $m)
      (table (export "") 2 funcref)
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module (import "" (table 1 funcref))))
    (module (type $m)
      (table (import "") 2 funcref)
    )
  )
  "inline module type does not match")

;; mismatched globals
(assert_invalid
  (module
    (type $m (module (export "" (global i32))))
    (module (type $m)
      (global (export "") i64 (i64.const 0))
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module (import "" (global i32))))
    (module (type $m)
      (global (import "") i64)
    )
  )
  "inline module type does not match")

;; mismatched memories
(assert_invalid
  (module
    (type $m (module (export "" (memory 1))))
    (module (type $m)
      (memory (export "") 2)
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module (import "" (memory 1))))
    (module (type $m)
      (memory (import "") 2)
    )
  )
  "inline module type does not match")

;; mismatched modules
(assert_invalid
  (module
    (type $m (module (export "" (module))))
    (module (type $m)
      (module (export "")
        (func (export ""))
      )
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module (import "" (module))))
    (module (type $m)
      (import "" (module (export "" (func))))
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module
      (import "" (module
        (import "" (func))
      ))
    ))
    (module (type $m)
      (import "" (module
        (import "" (func (param i32)))
      ))
    )
  )
  "inline module type does not match")

;; mismatched instances
(assert_invalid
  (module
    (type $m (module (export "" (instance))))
    (module (type $m)
      (module $m
        (func (export ""))
      )
      (instance (export "") (instantiate $m))
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module (export "" (instance))))
    (module $a
      (func (export ""))
    )
    (module (type $m)
      (instance (export "") (instantiate $a))
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module
      (import "" (instance (export "" (func))))
      (export "" (instance))
    ))
    (module (type $m)
      (import "" (instance $i (export "" (func))))
      (export "" (instance $i))
    )
  )
  "inline module type does not match")
(assert_invalid
  (module
    (type $m (module
      (export "" (instance
        (export "" (func (param i32)))
      ))
    ))
    (module $a
      (func (export ""))
    )
    (module (type $m)
      (instance (export "") (instantiate $a))
    )
  )
  "inline module type does not match")
