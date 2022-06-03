;; With this, we can define a component that imports a string and computes a new exported string, all at instantiation time:

(component
  (import "name" (value $name string))
  (import "libc" (core module $Libc
    (export "memory" (memory 1))
    (export "realloc" (func (param i32 i32 i32 i32) (result i32)))
    (export "free" (func (param i32 i32 i32)))
    (export "canonical_abi_realloc" (func (param i32 i32 i32 i32) (result i32)))
  ))
  (core instance $libc (instantiate $Libc))
  (core module $Main
    (import "libc" "memory" (memory 1))
    (func (export "start") (param i32 i32) (result i32)
      ;;... general-purpose compute
      unreachable
    )
    (func (export "start-post-return") (param i32))
  )
  (core instance $main (instantiate $Main (with "libc" (instance $libc))))
  (core alias export $main "start" (func $main_func))
  (func $start (param string) (result string)
    (canon lift (core func $main_func)
      (memory (core memory $libc "memory"))
      (realloc (core func $libc "canonical_abi_realloc"))
      (post-return (core func $main "start-post-return"))
    )
  )
  (start $start (value $name) (result (value $greeting)))
  (export "greeting" (value $greeting))
)
