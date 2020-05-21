(module
  (import "i" (instance $i
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param i32)))
  ))
  (func (export "run")
    call $i.$f1
  )
)
(assert_malformed
  (module quote
  "  (type $t (instance"
  "    (export \"f1\" (func $f1))"
  "  ))"
  "  (import \"i\" (instance $i (type $t)))"
  "  (func (export \"run\")"
  "    call $i.$f1"
  "  )"
  )
  "failed to find func named `$i.$f1`")

(module
  (import "i" (module $m
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param i32)))
  ))
  (instance $i (instantiate $m))
  (func (export "run")
    call $i.$f1
  )
)
(assert_malformed
  (module quote
    "(type $t (module "
    "  (export \"f1\" (func $f1))"
    "))"
    "(import \"i\" (module $m (type $t)))"
    "(instance $i (instantiate $m))"
    "(func (export \"run\")"
    "  call $i.$f1"
    ")"
  )
  "failed to find func named `$i.$f1`")

(module
  (module $m
    (func $f1 (export "f1"))
    (func $f2 (export "f2") (param i32))
  )
  (instance $i (instantiate $m))
  (func (export "run")
    call $i.$f1
  )
)


(module
  (import "libc" (instance $libc
    (export "memory" (memory $mem 1))
    (export "table" (table $tbl 0 funcref))
  ))
  (alias (instance $libc) (memory $mem))
  (alias (instance $libc) (table $tbl))
)

(module
  (import "libc" (instance $libc
    (export "memory" (memory $mem 1))
    (export "table" (table $tbl 0 funcref))
    (export "global" (global $glbl i32))
  ))

  (func $table_get
    i32.const 0
    table.get $libc.$tbl
    drop)

  (func $global_get
    global.get $libc.$glbl
    drop)
)

;; make sure auto-expanded aliases can't shadow already-defined names
(module
  (import "" (instance $i
    (export "1" (func $func (param i32)))
    (export "2" (func $memory))
    (export "3" (func $table))
    (export "4" (func $global))
    (export "5" (func $module))
    (export "6" (func $instance))
  ))
  (import "" (instance $i2
    (export "1" (func $func (param i32)))
    (export "2" (func $memory))
    (export "3" (func $table))
    (export "4" (func $global))
    (export "5" (func $module))
    (export "6" (func $instance))
  ))

  (import "" (instance $other
    (export "1" (func $func))
    (export "2" (memory $memory 1))
    (export "3" (global $global i32))
    (export "4" (table $table 1 funcref))
    (export "5" (module $module))
    (export "6" (instance $instance))
  ))

  (func $i.$func (import ""))
  (memory $i.$memory (import "") 1)
  (global $i.$global (import "") i32)
  (table $i.$table (import "") 1 funcref)
  (module $i.$module (import ""))
  (instance $i.$instance (import ""))

  (alias $i2.$func (instance $other) (func $func))
  (alias $i2.$global (instance $other) (global $global))
  (alias $i2.$table (instance $other) (table $table))
  (alias $i2.$memory (instance $other) (memory $memory))
  (alias $i2.$instance (instance $other) (instance $instance))
  (alias $i2.$module (instance $other) (module $module))

  (module $m
    (import "" (func))
    (import "" (memory 1))
    (import "" (global i32))
    (import "" (table 1 funcref))
    (import "" (module))
    (import "" (instance))
  )

  (instance (instantiate $m
    (func $i.$func)
    (memory $i.$memory)
    (global $i.$global)
    (table $i.$table)
    (module $i.$module)
    (instance $i.$instance)
  ))
  (instance (instantiate $m
    (func $i2.$func)
    (memory $i2.$memory)
    (global $i2.$global)
    (table $i2.$table)
    (module $i2.$module)
    (instance $i2.$instance)
  ))
)

;; auto-expansion should visit everywhere
(module
  (import "" (instance $i
    (export "" (global $global i32))
  ))

  (func
    global.get $i.$global
    drop)
)
(module
  (import "" (instance $i
    (export "" (global $global (mut i32)))
  ))

  (func
    i32.const 0
    global.set $i.$global)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    i32.const 0
    table.get $i.$table
    drop)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    i32.const 0
    ref.null func
    table.set $i.$table)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    i32.const 0
    call_indirect $i.$table)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    table.size $i.$table
    drop)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    ref.null func
    i32.const 0
    table.grow $i.$table
    drop)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    i32.const 0
    ref.null func
    i32.const 0
    table.fill $i.$table)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    i32.const 0
    i32.const 0
    i32.const 0
    table.init $i.$table 0)
  (elem func 0)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    i32.const 0
    i32.const 0
    i32.const 0
    table.copy $i.$table $i.$table)
)

(module
  (import "" (instance $i
    (export "" (func $func))
  ))
  (func
    return_call $i.$func)
)

(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))
  (func
    i32.const 0
    return_call_indirect $i.$table)
)

(module
  (import "" (instance $i
    (export "" (func $func))
  ))
  (global funcref (ref.func $i.$func))
)
(module
  (import "" (instance $i
    (export "" (func $func))
  ))
  (start $i.$func)
)
(module
  (import "" (instance $i
    (export "a" (table $table 1 funcref))
    (export "b" (func $func))
  ))
  (elem (table $i.$table) (i32.const 0) funcref (ref.func $i.$func))
)
(module
  (import "" (instance $i
    (export "a" (table $table 1 funcref))
    (export "b" (func $func))
  ))
  (elem (table $i.$table) (i32.const 0) func $i.$func)
)
(module
  (import "" (instance $i
    (export "" (memory $memory 1))
  ))
  (data (memory $i.$memory) (i32.const 0) "")
)

(module
  (import "" (instance $i
    (export "1" (func $func))
    (export "2" (memory $memory 1))
    (export "3" (table $table 1 funcref))
    (export "4" (global $global i32))
    (export "5" (module $module))
    (export "6" (instance $instance))
  ))
  (export "1" (func $i.$func))
  (export "2" (memory $i.$memory))
  (export "3" (table $i.$table))
  (export "4" (global $i.$global))
  (export "5" (module $i.$module))
  (export "6" (instance $i.$instance))
)

(module
  (import "" (instance $i
    (export "1" (func $func))
    (export "2" (memory $memory 1))
    (export "3" (table $table 1 funcref))
    (export "4" (global $global i32))
    (export "5" (module $module))
    (export "6" (instance $instance))
  ))

  (module $m
    (import "" (func))
    (import "" (memory 1))
    (import "" (global i32))
    (import "" (table 1 funcref))
    (import "" (module))
    (import "" (instance))
  )

  (instance (instantiate $m
    (func $i.$func)
    (memory $i.$memory)
    (global $i.$global)
    (table $i.$table)
    (module $i.$module)
    (instance $i.$instance)
  ))
)

(module
  (module $m
    (func $f (export ""))
  )

  (instance $i (instantiate $m))

  (func
    call $i.$f)
)

(module
  (import "" (instance $i (export "a" (func))))

  (import "" (module $m
    (import "" (module (export "a" (func))))
  ))

  (module $local
    (export $i))

  (instance (instantiate $m (module $local)))
)

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (export \"\" (func $f))"
    ")"
    "(func $f)"
  )
  "reference to func before item is defined")

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (import \"\" (instance))"
    "  (alias (instance 0) (func 0))"
    "  (export \"\" (func 0))"
    ")"
  )
  "aliased from an export that does not exist")

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (import \"\" (instance (export \"\" (global i32))))"
    "  (alias (instance 0) (func 0))"
    "  (export \"\" (func 0))"
    ")"
  )
  "alias points to export of wrong kind of item")

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (import \"\" (module))"
    "  (instance (instantiate 0))"
    "  (alias (instance 0) (func 0))"
    "  (export \"\" (func 0))"
    ")"
  )
  "aliased from an export that does not exist")

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (import \"\" (module (export \"\" (global i32))))"
    "  (instance (instantiate 0))"
    "  (alias (instance 0) (func 0))"
    "  (export \"\" (func 0))"
    ")"
  )
  "alias points to export of wrong kind of item")

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (module)"
    "  (instance (instantiate 0))"
    "  (alias (instance 0) (func 0))"
    "  (export \"\" (func 0))"
    ")"
  )
  "aliased from an export that does not exist")

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (module (global (export \"\") i32 (i32.const 0)))"
    "  (instance (instantiate 0))"
    "  (alias (instance 0) (func 0))"
    "  (export \"\" (func 0))"
    ")"
  )
  "alias points to export of wrong kind of item")

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (type (func))"
    "  (import \"\" (instance (type 0)))"
    "  (alias (instance 0) (func 0))"
    "  (export \"\" (func 0))"
    ")"
  )
  "aliased from an instance/module that is listed with the wrong type")

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (instance (instantiate 100))"
    "  (alias (instance 0) (func 0))"
    "  (export \"\" (func 0))"
    ")"
  )
  "reference to module is out of bounds")
