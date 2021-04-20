(module $copy_between_memories
  (memory $m64 i64 1)
  (memory $m32 i32 1)

  (func
    i32.const 0 i32.const 0 i32.const 0 memory.copy $m32 $m32
    i64.const 0 i32.const 0 i32.const 0 memory.copy $m64 $m32
    i32.const 0 i64.const 0 i32.const 0 memory.copy $m32 $m64
    i64.const 0 i64.const 0 i64.const 0 memory.copy $m64 $m64
  )
)

(module $copy_between_memories
  (memory $a (data "..."))
  (memory $b i32 (data "..."))
  (memory $c i64 (data "..."))

  (func
    i32.const 0 (i32.load (memory $a)) drop
    i32.const 0 (i32.load (memory $b)) drop
    i64.const 0 (i32.load (memory $c)) drop
  )
)

