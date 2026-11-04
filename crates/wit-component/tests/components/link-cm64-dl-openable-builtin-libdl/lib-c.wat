;;! use-built-in-libdl = true

;; module name: libc.so
(module
  (@dylink.0
    (mem-info (memory 0 4))
  )
  (type (func))
  (type (func (param i64) (result i64)))
  (import "GOT.mem" "__heap_base" (global $__heap_base (mut i64)))
  (import "GOT.mem" "__heap_end" (global $__heap_end (mut i64)))
  (global $heap (mut i64) i64.const 0)
  (func $start (type 0)
    global.get $__heap_base
    global.set $heap
  )
  (func $malloc (type 1) (param i64) (result i64)
    global.get $heap
    global.get $heap
    local.get 0
    i64.add
    global.set $heap
  )
  (export "malloc" (func $malloc))
  (start $start)
)
