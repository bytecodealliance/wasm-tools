;; With this, we can define a component that imports a string and computes a new exported string, all at instantiation time:


(component
  (import "name" (value $name string))
  (import "libc" (module $Libc
    (export "memory" (memory 1))
    (export "realloc" (func (param i32 i32 i32 i32) (result i32)))
    (export "free" (func (param i32 i32 i32)))
    (export "canonical_abi_realloc" (func (param i32 i32 i32 i32) (result i32)))
    (export "canonical_abi_free" (func (param i32 i32 i32)))
  ))
  (instance $libc (instantiate (module $Libc)))
  (module $Main
    (import "libc" "memory" (memory 1))
    (func (export "start") (param i32 i32) (result i32)
      ;;... general-purpose compute
      unreachable
    )
  )
  (instance $main (instantiate (module $Main) (with "libc" (instance $libc))))
  (alias export $main "start" (func $main_func))
  (func $start
    (canon.lift (func (param string) (result string)) (into $libc) (func $main_func))
  )
  (start $start (value $name) (result (value $greeting)))
  (export "greeting" (value $greeting))
)
