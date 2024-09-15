;; Check that shared functions are valid WAT.
(module
  (type $f (shared (func)))
  (func (import "spectest" "shared-func") (type $f))
  (func (type $f))
)

;; Check that unshared functions cannot be called from shared functions.
(assert_invalid
  (module
    (type $shared (shared (func)))
    (type $unshared (func))
    (func (type $shared)
      call $unshared)
    (func $unshared (type $unshared))
  )
  "shared functions cannot access unshared functions")

;; Check that unshared globals cannot be accessed from shared functions.
(assert_invalid
  (module
    (global $unshared_global i32 (i32.const 0))
    (type $shared_func (shared (func)))
    (func (type $shared_func)
      global.get $unshared_global
      drop)
    )
  "shared functions cannot access unshared globals")

;; Check that unshared tables cannot be accessed from shared functions.
(assert_invalid
  (module
    (table $unshared_table 1 anyref)
    (type $shared_func (shared (func)))
    (func (type $shared_func)
      i32.const 0
      table.get $unshared_table
      drop)
    )
  "shared functions cannot access unshared tables")

;; Check that unshared arrays cannot be accessed from shared functions.
(assert_invalid
  (module
    (type $unshared_array (array anyref))
    (type $shared_func (shared (func)))
    (func (type $shared_func)
      (array.new $unshared_array (ref.null any) (i32.const 0))
      (array.get $unshared_array (i32.const 0))
      drop)
    )
  "shared functions cannot access unshared arrays")

;; Check that unshared structs cannot be accessed from shared functions.
(assert_invalid
  (module
    (type $unshared_struct (struct (field i32)))
    (type $shared_func (shared (func)))
    (func (type $shared_func)
      (struct.new $unshared_struct (i32.const 0))
      (struct.get $unshared_struct 0)
      drop)
    )
  "shared functions cannot access unshared structs")
