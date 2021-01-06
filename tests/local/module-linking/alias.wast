(module
  (import "i" (instance $i
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param i32)))
  ))
  (func (export "run")
    call (func $i "f1")
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
    call (func $i "f1")
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
    call (func $i "f1")
  )
)


(module
  (import "libc" (instance $libc
    (export "memory" (memory $mem 1))
    (export "table" (table $tbl 0 funcref))
  ))
  (alias $libc "memory" (memory $mem))
  (alias $libc "table" (table $tbl))
)

(module
  (import "libc" (instance $libc
    (export "memory" (memory $mem 1))
    (export "table" (table $tbl 0 funcref))
    (export "global" (global $glbl i32))
  ))

  (func $table_get
    i32.const 0
    table.get (table $libc "table")
    drop)

  (func $global_get
    global.get (global $libc "global")
    drop)
)

;; auto-expansion should visit everywhere
(module
  (import "" (instance $i
    (export "" (global $global i32))
  ))

  (func
    global.get (global $i "")
    drop)
)
(module
  (import "" (instance $i
    (export "" (global $global (mut i32)))
  ))

  (func
    i32.const 0
    global.set (global $i ""))
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    i32.const 0
    table.get (table $i "")
    drop)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    i32.const 0
    ref.null func
    table.set (table $i ""))
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    i32.const 0
    call_indirect (table $i ""))
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    table.size (table $i "")
    drop)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    ref.null func
    i32.const 0
    table.grow (table $i "")
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
    table.fill (table $i ""))
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  (func
    i32.const 0
    i32.const 0
    i32.const 0
    table.init (table $i "") 0)
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
    table.copy (table $i "") (table $i ""))
)

(module
  (import "" (instance $i
    (export "" (func $func))
  ))
  (func
    return_call (func $i ""))
)

(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))
  (func
    i32.const 0
    return_call_indirect (table $i ""))
)

(module
  (import "" (instance $i
    (export "" (func $func))
  ))
  (global funcref (ref.func (func $i "")))
)
(module
  (import "" (instance $i
    (export "" (func $func))
  ))
  (start (func $i ""))
)
(module
  (import "" (instance $i
    (export "a" (table $table 1 funcref))
    (export "b" (func $func))
  ))
  (elem (table $i "a") (i32.const 0) funcref (ref.func (func $i "b")))
)
(module
  (import "" (instance $i
    (export "a" (table $table 1 funcref))
    (export "b" (func $func))
  ))
  (elem (table $i "a") (i32.const 0) func (func $i "b"))
)
(module
  (import "" (instance $i
    (export "" (memory $memory 1))
  ))
  (data (memory $i "") (i32.const 0) "")
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
  (export "1" (func $i "1"))
  (export "2" (memory $i "2"))
  (export "3" (table $i "3"))
  (export "4" (global $i "4"))
  (export "5" (module $i "5"))
  (export "6" (instance $i "6"))
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
    (import "1" (func))
    (import "2" (memory 1))
    (import "3" (global i32))
    (import "4" (table 1 funcref))
    (import "5" (module))
    (import "6" (instance))
  )

  (instance (instantiate $m
    (arg "1" (func $i "1"))
    (arg "2" (memory $i "2"))
    (arg "3" (global $i "4"))
    (arg "4" (table $i "3"))
    (arg "5" (module $i "5"))
    (arg "6" (instance $i "6"))
  ))
)

(module
  (module $m
    (func $f (export ""))
  )

  (instance $i (instantiate $m))

  (func
    call (func $i ""))
)

(assert_invalid
  (module
    (import "" (instance $i (export "a" (func))))

    (import "a" (module $m
      (import "" (module (export "a" (func))))
    ))

    (module $local
      (alias parent $i (instance))
    )

    (instance (instantiate $m (arg "" (module $local))))
  )
  "invalid external kind in alias")

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (export \"\" (func $f))"
    ")"
    "(func $f)"
  )
  "failed to find func named `$f`")

(assert_malformed
  (module
    (func)
    (module
      (import "" (instance))
      (alias 0 "" (func))
      (export "" (func 0))
    )
  )
  "aliased name `` does not exist")

(assert_invalid
  (module
    (func)
    (module
      (import "" (instance (export "" (global i32))))
      (alias 0 "" (func))
      (export "" (func 0))
    )
  )
  "alias kind mismatch")

(assert_invalid
  (module
    (func)
    (module
      (import "" (module))
      (instance (instantiate 0))
      (alias 0 "" (func))
      (export "" (func 0))
    )
  )
  "aliased name `` does not exist")

(assert_invalid
  (module
    (func)
    (module
      (import "" (module (export "" (global i32))))
      (instance (instantiate 0))
      (alias 0 "" (func))
      (export "" (func 0))
    )
  )
  "alias kind mismatch with export kind")

(assert_invalid
  (module
    (func)
    (module
      (module)
      (instance (instantiate 0))
      (alias 0 "" (func))
      (export "" (func 0))
    )
  )
  "aliased name `` does not exist in instance")

(assert_invalid
  (module
    (func)
    (module
      (module (global (export "") i32 (i32.const 0)))
      (instance (instantiate 0))
      (alias 0 "" (func))
      (export "" (func 0))
    )
  )
  "alias kind mismatch with export kind")

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (type (func))"
    "  (import \"\" (instance (type 0)))"
    "  (alias 0 \"\" (func))"
    "  (export \"\" (func 0))"
    ")"
  )
  "type index is not an instance")

(assert_malformed
  (module quote
    "(func)"
    "(module"
    "  (instance (instantiate 100))"
    "  (alias 0 \"\" (func))"
    "  (export \"\" (func 0))"
    ")"
  )
  "unknown module")

(assert_invalid
  (module
    (import "" (instance $i))
    (alias $i "foo" (func))
  )
  "aliased name `foo` does not exist in instance")

(assert_invalid
  (module
    (import "" (instance $i
      (export "" (memory 1))
    ))
    (alias $i "" (func))
  )
  "alias kind mismatch with export kind")

(assert_invalid
  (module
    (import "" (instance $i
      (export "" (func))
    ))
    (alias $i "" (func))

    (func
      i32.const 0
      call 0)
  )
  "type mismatch")

;; aliasing various items works
(module
  (import "" (instance $i
    (export "" (global $g (mut i32)))
  ))
  (alias $i "" (global $g))

  (func
    global.get $g
    global.set $g)
)

(module
  (import "" (instance $i
    (export "" (table $t 1 funcref))
  ))
  (alias $i "" (table $t))

  (func
    i32.const 0
    table.get $t
    drop)
)

(module
  (import "" (instance $i
    (export "" (memory $m 1))
  ))
  (alias $i "" (memory $m))

  (func
    i32.const 0
    i32.load
    drop)
)

(module
  (import "" (instance $i
    (export "" (func $f))
  ))
  (alias $i "" (func $f))

  (func
    call $f)
)

(module
  (import "" (instance $i
    (export "" (instance $i2
      (export "" (func))
    ))
  ))
  (alias $i "" (instance $i2))
  (alias $i2 "" (func $f))

  (func
    call $f)
)

(module
  (import "" (instance $i
    (export "" (module $m))
  ))
  (alias $i "" (module $m))
  (instance (instantiate $m))
)

(assert_malformed
  (module quote
    "(alias 0 \"\" (module))"
  )
  "instance index out of bounds")

(assert_invalid
  (module binary
    "\00asm" "\01\00\00\00"
    "\10\05"        ;; alias section
    "\01"           ;; 1 alias
    "\00\00\00\00"  ;; (alias (instance 0) (func 0))
  )
  "unknown instance")

(module
  (import "" (module $m
    (export "" (func $f))
  ))
  (instance $i (instantiate $m))
  (alias $i "" (func $f))

  (func
    call $f)
)

(module
  (module $m
    (func $f (export ""))
  )
  (instance $i (instantiate $m))
  (alias $i "" (func $f))

  (func
    call $f)
)

(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  (func unreachable i32.load (memory $i "x") unreachable)
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  (func unreachable memory.init $data (memory $i "x"))
  (data $data "x")
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  (func unreachable memory.copy (memory $i "x") (memory $i "x"))
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  (func unreachable memory.fill (memory $i "x"))
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  (func unreachable memory.size (memory $i "x") unreachable)
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  (func unreachable memory.grow (memory $i "x") unreachable)
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  (func unreachable f64.load (memory $i "x") unreachable)
)

(module
  (module $m
    (module $m
      (func (export "")))
    (instance $a (export "") (instantiate $m))
  )
  (instance $i (instantiate $m))
  (alias (instance $i "") "" (func))
)

(assert_invalid
  (module
    (module
      (alias parent 0 (module))
    )
  )
  "alias to module not defined in parent")
(assert_invalid
  (module
    (type (module))
    (module
      (alias parent $f (type))
    )
    (type $f (func))
  )
  "alias to type not defined in parent")
