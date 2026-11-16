;; RUN: wast --assert default --snapshot tests/snapshots % -f shared-everything-threads

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
    (type $unshared_array (array i32))
    (type $shared_func (shared (func)))
    (func (type $shared_func)
      (array.new $unshared_array (i32.const 0) (i32.const 0))
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

;; Check that unshared memories cannot be accessed from shared functions.
(assert_invalid
  (module
    (memory 1)
    (type $shared_func (shared (func)))
    (func (type $shared_func)
      (drop (i32.load (i32.const 0)))
    )
  )
  "shared functions cannot access unshared memories")
(assert_invalid
  (module
    (memory 1)
    (type $shared_func (shared (func)))
    (func (type $shared_func)
      (drop (memory.size))
    )
  )
  "shared functions cannot access unshared memories")

;; Check that unshared functions cannot be referenced from shared functions.
(assert_invalid
  (module
    (type $unshared_func (func))
    (type $shared_func (shared (func)))
    (func $unshared (type $unshared_func))
    (elem declare func $unshared)
    (func (type $shared_func)
      (drop (ref.func $unshared))
    )
  )
  "shared functions cannot access unshared functions")

;; Check that unshared tags cannot be used from shared functions.
(assert_invalid
  (module
    (type $unshared_func (func))
    (type $shared_func (shared (func)))
    (tag $t (type $unshared_func))
    (func (type $shared_func)
      (throw $t)
    )
  )
  "shared functions cannot access unshared tags")

;; Check that shared functions cannot use unshared value types.
(assert_invalid
  (module
    (type $shared_func (shared (func)))
    (func (type $shared_func) (local anyref))
  )
  "shared functions cannot use unshared value types")
(assert_invalid
  (module
    (type $shared_func (shared (func)))
    (func (type $shared_func)
      (drop (ref.null any))
    )
  )
  "shared functions cannot use unshared value types")
(assert_invalid
  (module
    (type $shared_func (shared (func)))
    (func (type $shared_func)
      (drop (ref.i31 (i32.const 0)))
    )
  )
  "shared functions cannot use unshared value types")
(assert_invalid
  (module
    (type $shared_func (shared (func)))
    (func (type $shared_func)
      (drop (block (result anyref) (unreachable)))
    )
  )
  "shared functions cannot use unshared value types")

;; Block types need not be shared in shared functions.
(module
  (type $unshared_block (func (param i32) (result i32)))
  (type $shared_func (shared (func)))
  (func (type $shared_func)
    i32.const 0
    block (type $unshared_block)
    end
    drop
  )
)

;; Shared functions can make indirect calls through shared tables.
(module
  (type $shared_func (shared (func)))
  (table $t shared 1 (ref null (shared func)))
  (func (type $shared_func)
    (call_indirect $t (type $shared_func) (i32.const 0))
    (return_call_indirect $t (type $shared_func) (i32.const 0))
  )
)

;; In unreachable code, `any.convert_extern` pushes a reference of unknown
;; sharedness (`bot-share`), which may flow into a shared type.
(module
  (type $shared_func (shared (func)))
  (func (type $shared_func)
    (drop (block (result (ref null (shared any)))
      unreachable
      any.convert_extern
    ))
  )
)
