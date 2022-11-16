(component
  (import "log" (func $log (param "msg" string)))
  (core module $libc
    (memory (export "memory") 1)
    (func (export "canonical_abi_realloc") (param i32 i32 i32 i32) (result i32)
      unreachable)
  )

  (core module $my_module
    (import "env" "log-utf8" (func $log_utf8 (param i32 i32)))
    (import "env" "log-utf16" (func $log_utf16 (param i32 i32)))
    (import "env" "log-compact-utf16" (func $log_compact_utf16 (param i32 i32)))

    (func (export "log-utf8") (param i32 i32)
      local.get 0
      local.get 1
      call $log_utf8
    )
    (func (export "log-utf16") (param i32 i32)
      local.get 0
      local.get 1
      call $log_utf16
    )
    (func (export "log-compact-utf16") (param i32 i32)
      local.get 0
      local.get 1
      call $log_compact_utf16
    )
  )

  (core instance $libc (instantiate $libc))

  (alias core export $libc "canonical_abi_realloc" (core func $realloc))
  (alias core export $libc "memory" (core memory $memory))
  (core func $log_lower_utf8 (canon lower (func $log) string-encoding=utf8 (memory $memory) (realloc $realloc)))
  (core func $log_lower_utf16 (canon lower (func $log) string-encoding=utf16 (memory $memory) (realloc $realloc)))
  (core func $log_lower_compact_utf16 (canon lower (func $log) string-encoding=latin1+utf16 (memory $memory) (realloc $realloc)))

  (core instance $my_instance (instantiate $my_module
    (with "libc" (instance $libc))
    (with "env" (instance
      (export "log-utf8" (func $log_lower_utf8))
      (export "log-utf16" (func $log_lower_utf16))
      (export "log-compact-utf16" (func $log_lower_compact_utf16))
    ))
  ))

  (func (export "log1") (param "msg" string)
    (canon lift
      (core func $my_instance "log-utf8")
      string-encoding=utf8
      (memory $memory)
      (realloc $realloc)
    )
  )
  (func (export "log2") (param "msg" string)
    (canon lift
      (core func $my_instance "log-utf16")
      string-encoding=utf16
      (memory $memory)
      (realloc $realloc)
    )
  )
  (func (export "log3") (param "msg" string)
    (canon lift
      (core func $my_instance "log-compact-utf16")
      string-encoding=latin1+utf16
      (memory $memory)
      (realloc $realloc)
    )
  )
)

(assert_invalid
  (component
    (import "i" (func $f))
    (core func (canon lower (func $f) string-encoding=utf8 string-encoding=utf16))
  )
  "canonical encoding option `utf8` conflicts with option `utf16`")

(assert_invalid
  (component
    (import "i" (func $f))
    (core func (canon lower (func $f) string-encoding=utf8 string-encoding=latin1+utf16))
  )
  "canonical encoding option `utf8` conflicts with option `latin1-utf16`")

(assert_invalid
  (component
    (import "i" (func $f))
    (core func (canon lower (func $f) string-encoding=utf16 string-encoding=latin1+utf16))
  )
  "canonical encoding option `utf16` conflicts with option `latin1-utf16`")

(assert_invalid
  (component
    (import "i" (func $f))
    (core func (canon lower (func $f) (memory 0)))
  )
  "memory index out of bounds")

(assert_invalid
  (component
    (import "i" (func $f))
    (core module $m (memory (export "memory") 1))
    (core instance $i (instantiate $m))
    (core func (canon lower (func $f) (memory $i "memory") (memory $i "memory")))
  )
  "`memory` is specified more than once")

(assert_invalid
  (component
    (core module $m
      (func (export "f") (param i32 i32))
    )
    (core instance $i (instantiate $m))
    (func (param "p1" (list u8)) (canon lift (core func $i "f")))
  )
  "canonical option `memory` is required")

(assert_invalid
  (component
    (core module $m
      (memory (export "m") 1)
      (func (export "f") (param i32 i32))
    )
    (core instance $i (instantiate $m))
    (func (param "p1" (list u8))
      (canon lift (core func $i "f")
        (memory $i "m")
      )
    )
  )
  "canonical option `realloc` is required")

(assert_invalid
  (component
    (core module $m
      (memory (export "m") 1)
      (func (export "f") (param i32 i32))
      (func (export "r") (param i32 i32 i32 i32) (result i32))
    )
    (core instance $i (instantiate $m))
    (func (param "p1" (list u8))
      (canon lift (core func $i "f")
        (memory $i "m")
        (realloc (func $i "r"))
        (realloc (func $i "r"))
      )
    )
  )
  "canonical option `realloc` is specified more than once")

(assert_invalid
  (component
    (core module $m
      (memory (export "m") 1)
      (func (export "f") (param i32 i32))
      (func (export "r"))
    )
    (core instance $i (instantiate $m))
    (func (param "p1" (list u8))
      (canon lift (core func $i "f")
        (memory $i "m")
        (realloc (func $i "r"))
      )
    )
  )
  "canonical option `realloc` uses a core function with an incorrect signature")

(assert_invalid
  (component
    (core module $m
      (memory (export "m") 1)
      (func (export "f") (result i32))
      (func (export "r") (param i32 i32 i32 i32) (result i32))
      (func (export "p"))
    )
    (core instance $i (instantiate $m))
    (func (result string)
      (canon lift (core func $i "f")
        (memory $i "m")
        (realloc (func $i "r"))
        (post-return (func $i "p"))
      )
    )
  )
  "canonical option `post-return` uses a core function with an incorrect signature")

(assert_invalid
  (component
    (core module $m
      (memory (export "m") 1)
      (func (export "f") (result i32))
      (func (export "r") (param i32 i32 i32 i32) (result i32))
      (func (export "p") (param i32))
    )
    (core instance $i (instantiate $m))
    (func (result string)
      (canon lift (core func $i "f")
        (memory $i "m")
        (realloc (func $i "r"))
        (post-return (func $i "p"))
        (post-return (func $i "p"))
      )
    )
  )
  "canonical option `post-return` is specified more than once")

(assert_invalid
  (component
    (import "i" (func $f (param "p1" string)))
    (core module $m
      (memory (export "m") 1)
      (func (export "f") (result i32))
      (func (export "r") (param i32 i32 i32 i32) (result i32))
      (func (export "p") (param i32))
    )
    (core instance $i (instantiate $m))
    (core func
      (canon lower (func $f)
        (memory $i "m")
        (realloc (func $i "r"))
        (post-return (func $i "p"))
      )
    )
  )
  "canonical option `post-return` cannot be specified for lowerings")

(component
  (core module $m
    (memory (export "m") 1)
    (func (export "f") (result i32) unreachable)
    (func (export "r") (param i32 i32 i32 i32) (result i32) unreachable)
    (func (export "p") (param i32))
  )
  (core instance $i (instantiate $m))
  (func (result string)
    (canon lift (core func $i "f")
      (memory $i "m")
      (realloc (func $i "r"))
      (post-return (func $i "p"))
    )
  )
)

(assert_invalid
  (component
    (core module $m
      (func (export ""))
    )
    (core instance $i (instantiate $m))
    (core func (canon lower (func $i "")))
  )
  "unknown instance: failed to find name `$i`")

(assert_invalid
  (component
    (core module $m (func (export "foo") (param i32)))
    (core instance $i (instantiate $m))
    (func (export "foo") (canon lift (core func $i "foo")))
  )
  "lowered parameter types `[]` do not match parameter types `[I32]`")

(assert_invalid
  (component
    (core module $m (func (export "foo") (result i32)))
    (core instance $i (instantiate $m))
    (func (export "foo") (canon lift (core func $i "foo")))
  )
  "lowered result types `[]` do not match result types `[I32]`")

(assert_invalid
  (component
    (type $f string)
    (core module $m (func (export "foo")))
    (core instance $i (instantiate $m))
    (func (export "foo") (type $f) (canon lift (core func $i "foo")))
  )
  "not a function type")

(assert_invalid
  (component
    (import "a" (func $f))
    (func (export "foo") (canon lift (core func $f)))
  )
  "unknown core func: failed to find name `$f`")
