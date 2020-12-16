(module
  (import "i" (instance $i
    (export "f1" (func $f1))
    (export "f2" (func $f2 (param i32)))
  ))
  ;; TODO: alias sugar
  (alias $i "f1" (func $i.$f1))
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
  ;; TODO: alias sugar
  (alias $i "f1" (func $i.$f1))
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
  ;; TODO: alias sugar
  (alias $i "f1" (func $i.$f1))
  (func (export "run")
    call $i.$f1
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

  ;; TODO: alias sugar
  (alias $libc "table" (table $libc.$tbl))
  (alias $libc "global" (global $libc.$glbl))
  (func $table_get
    i32.const 0
    table.get $libc.$tbl
    drop)

  (func $global_get
    global.get $libc.$glbl
    drop)
)

;; make sure auto-expanded aliases can't shadow already-defined names
;; TODO: reenable with alias sugar
(; (module ;)
(;   (import "a" (instance $i ;)
(;     (export "1" (func $func (param i32))) ;)
(;     (export "2" (func $memory)) ;)
(;     (export "3" (func $table)) ;)
(;     (export "4" (func $global)) ;)
(;     (export "5" (func $module)) ;)
(;     (export "6" (func $instance)) ;)
(;   )) ;)
(;   (import "b" (instance $i2 ;)
(;     (export "1" (func $func (param i32))) ;)
(;     (export "2" (func $memory)) ;)
(;     (export "3" (func $table)) ;)
(;     (export "4" (func $global)) ;)
(;     (export "5" (func $module)) ;)
(;     (export "6" (func $instance)) ;)
(;   )) ;)

(;   (import "c" (instance $other ;)
(;     (export "1" (func $func)) ;)
(;     (export "2" (memory $memory 1)) ;)
(;     (export "3" (global $global i32)) ;)
(;     (export "4" (table $table 1 funcref)) ;)
(;     (export "5" (module $module)) ;)
(;     (export "6" (instance $instance)) ;)
(;   )) ;)

(;   (func $i.$func (import "d")) ;)
(;   (memory $i.$memory (import "e") 1) ;)
(;   (global $i.$global (import "f") i32) ;)
(;   (table $i.$table (import "g") 1 funcref) ;)
(;   (module $i.$module (import "h")) ;)
(;   (instance $i.$instance (import "i")) ;)

(;   (alias $i2.$func (instance $other) (func $func)) ;)
(;   (alias $i2.$global (instance $other) (global $global)) ;)
(;   (alias $i2.$table (instance $other) (table $table)) ;)
(;   (alias $i2.$memory (instance $other) (memory $memory)) ;)
(;   (alias $i2.$instance (instance $other) (instance $instance)) ;)
(;   (alias $i2.$module (instance $other) (module $module)) ;)

(;   (module $m ;)
(;     (import "" (func)) ;)
(;     (import "" (memory 1)) ;)
(;     (import "" (global i32)) ;)
(;     (import "" (table 1 funcref)) ;)
(;     (import "" (module)) ;)
(;     (import "" (instance)) ;)
(;   ) ;)

(;   (instance (instantiate $m ;)
(;     (func $i.$func) ;)
(;     (memory $i.$memory) ;)
(;     (global $i.$global) ;)
(;     (table $i.$table) ;)
(;     (module $i.$module) ;)
(;     (instance $i.$instance) ;)
(;   )) ;)
(;   (instance (instantiate $m ;)
(;     (func $i2.$func) ;)
(;     (memory $i2.$memory) ;)
(;     (global $i2.$global) ;)
(;     (table $i2.$table) ;)
(;     (module $i2.$module) ;)
(;     (instance $i2.$instance) ;)
(;   )) ;)
(; ) ;)

;; auto-expansion should visit everywhere
(module
  (import "" (instance $i
    (export "" (global $global i32))
  ))

  ;; TODO: alias sugar
  (alias $i "" (global $i.$global))
  (func
    global.get $i.$global
    drop)
)
(module
  (import "" (instance $i
    (export "" (global $global (mut i32)))
  ))

  ;; TODO: alias sugar
  (alias $i "" (global $i.$global))
  (func
    i32.const 0
    global.set $i.$global)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  ;; TODO: alias sugar
  (alias $i "" (table $i.$table))
  (func
    i32.const 0
    table.get $i.$table
    drop)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  ;; TODO: alias sugar
  (alias $i "" (table $i.$table))
  (func
    i32.const 0
    ref.null func
    table.set $i.$table)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  ;; TODO: alias sugar
  (alias $i "" (table $i.$table))
  (func
    i32.const 0
    call_indirect $i.$table)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  ;; TODO: alias sugar
  (alias $i "" (table $i.$table))
  (func
    table.size $i.$table
    drop)
)
(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))

  ;; TODO: alias sugar
  (alias $i "" (table $i.$table))
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

  ;; TODO: alias sugar
  (alias $i "" (table $i.$table))
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

  ;; TODO: alias sugar
  (alias $i "" (table $i.$table))
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

  ;; TODO: alias sugar
  (alias $i "" (table $i.$table))
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
  ;; TODO: alias sugar
  (alias $i "" (func $i.$func))
  (func
    return_call $i.$func)
)

(module
  (import "" (instance $i
    (export "" (table $table 1 funcref))
  ))
  ;; TODO: alias sugar
  (alias $i "" (table $i.$table))
  (func
    i32.const 0
    return_call_indirect $i.$table)
)

(module
  (import "" (instance $i
    (export "" (func $func))
  ))
  ;; TODO: alias sugar
  (alias $i "" (func $i.$func))
  (global funcref (ref.func $i.$func))
)
(module
  (import "" (instance $i
    (export "" (func $func))
  ))
  ;; TODO: alias sugar
  (alias $i "" (func $i.$func))
  (start $i.$func)
)
(module
  (import "" (instance $i
    (export "a" (table $table 1 funcref))
    (export "b" (func $func))
  ))
  ;; TODO: alias sugar
  (alias $i "a" (table $i.$table))
  (alias $i "b" (func $i.$func))
  (elem (table $i.$table) (i32.const 0) funcref (ref.func $i.$func))
)
(module
  (import "" (instance $i
    (export "a" (table $table 1 funcref))
    (export "b" (func $func))
  ))
  ;; TODO: alias sugar
  (alias $i "a" (table $i.$table))
  (alias $i "b" (func $i.$func))
  (elem (table $i.$table) (i32.const 0) func $i.$func)
)
(module
  (import "" (instance $i
    (export "" (memory $memory 1))
  ))
  ;; TODO: alias sugar
  (alias $i "" (memory $i.$memory))
  (data (memory $i.$memory) (i32.const 0) "")
)

;; TODO: reenable with alias sugar
(; (module ;)
(;   (import "" (instance $i ;)
(;     (export "1" (func $func)) ;)
(;     (export "2" (memory $memory 1)) ;)
(;     (export "3" (table $table 1 funcref)) ;)
(;     (export "4" (global $global i32)) ;)
(;     (export "5" (module $module)) ;)
(;     (export "6" (instance $instance)) ;)
(;   )) ;)
(;   (export "1" (func $i.$func)) ;)
(;   (export "2" (memory $i.$memory)) ;)
(;   (export "3" (table $i.$table)) ;)
(;   (export "4" (global $i.$global)) ;)
(;   (export "5" (module $i.$module)) ;)
(;   (export "6" (instance $i.$instance)) ;)
(; ) ;)

;; TODO: reenable with alias sugar
(; (module ;)
(;   (import "" (instance $i ;)
(;     (export "1" (func $func)) ;)
(;     (export "2" (memory $memory 1)) ;)
(;     (export "3" (table $table 1 funcref)) ;)
(;     (export "4" (global $global i32)) ;)
(;     (export "5" (module $module)) ;)
(;     (export "6" (instance $instance)) ;)
(;   )) ;)

(;   (module $m ;)
(;     (import "" (func)) ;)
(;     (import "" (memory 1)) ;)
(;     (import "" (global i32)) ;)
(;     (import "" (table 1 funcref)) ;)
(;     (import "" (module)) ;)
(;     (import "" (instance)) ;)
(;   ) ;)

(;   (instance (instantiate $m ;)
(;     (func $i.$func) ;)
(;     (memory $i.$memory) ;)
(;     (global $i.$global) ;)
(;     (table $i.$table) ;)
(;     (module $i.$module) ;)
(;     (instance $i.$instance) ;)
(;   )) ;)
(; ) ;)

;; TODO: reenable with alias sugar
(; (module ;)
(;   (module $m ;)
(;     (func $f (export "")) ;)
(;   ) ;)

(;   (instance $i (instantiate $m)) ;)

(;   (func ;)
(;     call $i.$f) ;)
(; ) ;)

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
  ;; TODO: alias sugar
  (alias $i "x" (memory $i.$m))
  (func unreachable i32.load $i.$m unreachable)
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  ;; TODO: alias sugar
  (alias $i "x" (memory $i.$m))
  (func unreachable memory.init $data $i.$m)
  (data $data "x")
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  ;; TODO: alias sugar
  (alias $i "x" (memory $i.$m))
  (func unreachable memory.copy $i.$m $i.$m)
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  ;; TODO: alias sugar
  (alias $i "x" (memory $i.$m))
  (func unreachable memory.fill $i.$m)
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  ;; TODO: alias sugar
  (alias $i "x" (memory $i.$m))
  (func unreachable memory.size $i.$m unreachable)
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  ;; TODO: alias sugar
  (alias $i "x" (memory $i.$m))
  (func unreachable memory.grow $i.$m unreachable)
)
(module
  (module $m (memory $m (export "x") 1))
  (instance $i (instantiate $m))
  ;; TODO: alias sugar
  (alias $i "x" (memory $i.$m))
  (func unreachable f64.load $i.$m unreachable)
)

(module
  (module $m
    (module $m
      (func (export "")))
    (instance $a (export "") (instantiate $m))
  )
  (instance $i (instantiate $m))
  ;; TODO: remove with alias sugar
  (alias $i "" (instance $i.$a))
  (alias $i.$a "" (func))
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
