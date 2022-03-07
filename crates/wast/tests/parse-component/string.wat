;; With this, we can define a component that imports a string and computes a new exported string, all at instantiation time:

(component
  (import "name" (value $name string))
  (import "libc" (module $Libc
    (export "memory" (memory 1))
    (export "realloc" (func (param i32 i32 i32 i32) (result i32)))
    (export "free" (func (param i32 i32 i32)))
  ))
  (instance $libc (instantiate (module $Libc)))
  (module $Main
    ;; (import "libc" ...)
    (func (export "start") (param i32 i32) (result i32 i32)
      ;;... general-purpose compute
      unreachable
    )
  )
  (instance $main (instantiate (module $Main) (import "libc" (instance $libc))))
  (func $start
    (canon.lift (func (param string) (result string)) (into $libc) (func $main "start"))
  )
  (start $start (value $name) (result (value $greeting)))
  (export "greeting" (value $greeting))
)
