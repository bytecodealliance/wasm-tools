;; With this, we can finally write a non-trivial component that takes a string, does some logging, then returns a string.

;; FIXME(#588) this should actuall be a valid module
(assert_invalid
(component
  (import "wasi:logging" (instance $logging
    (export "log" (func (param string)))
  ))
  (import "libc" (module $Libc
    (export "memory" (memory 1))
    (export "realloc" (func (param i32 i32) (result i32)))
    (export "free" (func (param i32)))
  ))
  (instance $libc (instantiate (module $Libc)))
  (func $log
    (canon.lower (into $libc) (func $logging "log"))
  )
  (module $Main
    (import "libc" "memory" (memory 1))
    (import "libc" "realloc" (func (param i32 i32) (result i32)))
    (import "libc" "free" (func (param i32)))
    (import "wasi:logging" "log" (func $log (param i32 i32)))
    (func (export "run") (param i32 i32) (result i32 i32)
      ;; ...
      (call $log)
      ;; ...
      (unreachable)
    )
  )
  (instance $main (instantiate (module $Main)
    (with "libc" (instance $libc))
    (with "wasi:logging" (instance (export "log" (func $log))))
  ))
  (func (export "run")
    (canon.lift (func (param string) (result string)) (into $libc) (func $main "run"))
  )
)
"not a module instance")
