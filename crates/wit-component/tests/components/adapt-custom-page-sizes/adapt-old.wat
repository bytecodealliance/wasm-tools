(module
  (import "new" "thunk" (func $thunk))
  (import "env" "memory" (memory 0))

  (func (export "thunk")
    ;; read from memory to keep the import live
    (drop (i32.load (i32.const 0)))
    call $thunk
  )
)
