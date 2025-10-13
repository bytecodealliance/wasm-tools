;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-values

(component
  (import "one" (func))

  (import "two" (value $v string))
  (import "three" (instance
    (export "four" (instance
      (export "five" (core module
        (import "six" "a" (func))
        (import "six" "b" (func))
      ))
    ))
  ))

  (export "four" (value $v))
  ;; ...
)

(component
  (component
    (import "a" (instance $foo (export "v" (value s32))))
    (export "v" (value $foo "v"))
  )
)

(assert_invalid
  (component (export "" (value 0)))
  "index out of bounds")

(component
  (component
    (import "d" (value $v string))
    (export "i" (value $v))
  )
)

(assert_invalid
  (component
    (import "a" (value $v string))
    (export "b" (value $v))
    (export "c" (value $v))
  )
  "cannot be used more than once")

(assert_invalid
  (component
    (import "a" (value string))
  )
  "value index 0 was not used as part of an instantiation, start function, or export")

(component
  (component
    (import "a" (value string))
    (export "b" (value 0))
  )
)

(component
  (component
    (import "a" (value $i string))
    (import "b" (component $c (import "a" (value string))))
    (instance (instantiate $c (with "a" (value $i))))
  )
)

(component definition
  (type $t string)
  (import "a" (value (type $t)))
  (component $c (import "a" (value string)) (export "b" (value 0)))
  (instance (instantiate $c (with "a" (value 0))))
)

(component
  (component
    (import "a" (component $c
      (import "e" (value string))
    ))
    (import "f" (value $v string))

    (instance
      (instantiate $c
        (with "e" (value $v))
      )
    )
  )
)

(assert_invalid
  (component (instance $i (export "" (value 0))))
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate $c
      (with "" (value 0))
    ))
  )
  "index out of bounds")

(component
  (component
    (import "e" (value $v string))
    (instance
      (export "e" (value $v))
    )
  )
)

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
  (alias core export $main "start" (core func $main_func))
  (func $start (param "p1" string) (result string)
    (canon lift (core func $main_func)
      (memory $libc "memory")
      (realloc (func $libc "canonical_abi_realloc"))
      (post-return (func $main "start-post-return"))
    )
  )
  (start $start (value $name) (result (value $greeting)))
  (export "greeting" (value $greeting))
)
