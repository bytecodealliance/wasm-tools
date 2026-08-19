(module
  (@dylink.0
    (mem-info (memory 0 4))
  )
  (type $void (func))
  (type $malloc (func (param i32) (result i32)))
  (type $realloc (func (param i32 i32 i32 i32) (result i32)))
  (import "GOT.mem" "__heap_base" (global $__heap_base (mut i32)))
  (global $heap (mut i32) i32.const 0)
  (func $start (type $void)
    global.get $__heap_base
    global.set $heap
  )
  (func $malloc (type $malloc)
    global.get $heap
    global.get $heap
    local.get 0
    i32.add
    global.set $heap
  )
  (func $cabi_realloc (type $realloc)
    local.get 3
    call $malloc
  )
  (func $abort (type $void)
    unreachable
  )
  (export "malloc" (func $malloc))
  (export "cabi_realloc" (func $cabi_realloc))
  (export "abort" (func $abort))
  (start $start)
)
