;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm-async-builtins

;; future.new
(component
  (core module $m
    (import "" "future.new" (func $future-new (result i32)))
  )
  (type $future-type (future u8))
  (core func $future-new (canon future.new $future-type))
  (core instance $i (instantiate $m (with "" (instance (export "future.new" (func $future-new))))))
)

;; future.new; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "future.new" (func $future-new (param i32) (result i32)))
    )
    (type $future-type (future u8))
    (core func $future-new (canon future.new $future-type))
    (core instance $i (instantiate $m (with "" (instance (export "future.new" (func $future-new))))))
  )
  "type mismatch for export `future.new` of module instantiation argument ``"
)

;; future.read
(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core module $m
    (import "" "future.read" (func $future-read (param i32 i32) (result i32)))
  )
  (type $future-type (future u8))
  (core func $future-read (canon future.read $future-type async (memory $libc "memory")))
  (core instance $i (instantiate $m (with "" (instance (export "future.read" (func $future-read))))))
)

;; future.read; with realloc
(component
  (core module $libc
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
    (memory (export "memory") 1)
  )
  (core instance $libc (instantiate $libc))
  (core module $m
    (import "" "future.read" (func $future-read (param i32 i32) (result i32)))
  )
  (type $future-type (future string))
  (core func $future-read (canon future.read $future-type async (memory $libc "memory") (realloc (func $libc "realloc"))))
  (core instance $i (instantiate $m (with "" (instance (export "future.read" (func $future-read))))))
)

;; future.read; incorrect type
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "future.read" (func $future-read (param i32) (result i32)))
    )
    (type $future-type (future u8))
    (core func $future-read (canon future.read $future-type async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "future.read" (func $future-read))))))
  )
  "type mismatch for export `future.read` of module instantiation argument ``"
)

;; future.read; incorrect type argument
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "future.read" (func $future-read (param i32 i32) (result i32)))
    )
    (type $string-type string)
    (core func $future-read (canon future.read $string-type async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "future.read" (func $future-read))))))
  )
  "`future.read` requires a future type"
)

;; future.read; missing realloc
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "future.read" (func $future-read (param i32 i32) (result i32)))
    )
    (type $future-type (future string))
    (core func $future-read (canon future.read $future-type async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "future.read" (func $future-read))))))
  )
  "canonical option `realloc` is required"
)

;; future.write
(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core module $m
    (import "" "future.write" (func $future-write (param i32 i32) (result i32)))
  )
  (type $future-type (future u8))
  (core func $future-write (canon future.write $future-type async (memory $libc "memory")))
  (core instance $i (instantiate $m (with "" (instance (export "future.write" (func $future-write))))))
)

;; future.write; incorrect type
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "future.write" (func $future-write (param i32) (result i32)))
    )
    (type $future-type (future u8))
    (core func $future-write (canon future.write $future-type async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "future.write" (func $future-write))))))
  )
  "type mismatch for export `future.write` of module instantiation argument ``"
)

;; future.cancel-read
(component
  (core module $m
    (import "" "future.cancel-read" (func $future-cancel-read (param i32) (result i32)))
  )
  (type $future-type (future u8))
  (core func $future-cancel-read (canon future.cancel-read $future-type async))
  (core instance $i (instantiate $m (with "" (instance (export "future.cancel-read" (func $future-cancel-read))))))
)

;; future.cancel-read; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "future.cancel-read" (func $future-cancel-read (param i32 i32) (result i32)))
    )
    (type $future-type (future u8))
    (core func $future-cancel-read (canon future.cancel-read $future-type async))
    (core instance $i (instantiate $m (with "" (instance (export "future.cancel-read" (func $future-cancel-read))))))
  )
  "type mismatch for export `future.cancel-read` of module instantiation argument ``"
)

;; future.cancel-write
(component
  (core module $m
    (import "" "future.cancel-write" (func $future-cancel-write (param i32) (result i32)))
  )
  (type $future-type (future u8))
  (core func $future-cancel-write (canon future.cancel-write $future-type async))
  (core instance $i (instantiate $m (with "" (instance (export "future.cancel-write" (func $future-cancel-write))))))
)

;; future.cancel-write; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "future.cancel-write" (func $future-cancel-write (param i32 i32) (result i32)))
    )
    (type $future-type (future u8))
    (core func $future-cancel-write (canon future.cancel-write $future-type async))
    (core instance $i (instantiate $m (with "" (instance (export "future.cancel-write" (func $future-cancel-write))))))
  )
  "type mismatch for export `future.cancel-write` of module instantiation argument ``"
)

;; future.close-readable
(component
  (core module $m
    (import "" "future.close-readable" (func $future-close-readable (param i32 i32)))
  )
  (type $future-type (future u8))
  (core func $future-close-readable (canon future.close-readable $future-type))
  (core instance $i (instantiate $m (with "" (instance (export "future.close-readable" (func $future-close-readable))))))
)

;; future.close-readable; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "future.close-readable" (func $future-close-readable (param i32) (result i32)))
    )
    (type $future-type (future u8))
    (core func $future-close-readable (canon future.close-readable $future-type))
    (core instance $i (instantiate $m (with "" (instance (export "future.close-readable" (func $future-close-readable))))))
  )
  "type mismatch for export `future.close-readable` of module instantiation argument ``"
)

;; future.close-writable
(component
  (core module $m
    (import "" "future.close-writable" (func $future-close-writable (param i32 i32)))
  )
  (type $future-type (future u8))
  (core func $future-close-writable (canon future.close-writable $future-type))
  (core instance $i (instantiate $m (with "" (instance (export "future.close-writable" (func $future-close-writable))))))
)

;; future.close-writable; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "future.close-writable" (func $future-close-writable (param i32 i32) (result i32)))
    )
    (type $future-type (future u8))
    (core func $future-close-writable (canon future.close-writable $future-type))
    (core instance $i (instantiate $m (with "" (instance (export "future.close-writable" (func $future-close-writable))))))
  )
  "type mismatch for export `future.close-writable` of module instantiation argument ``"
)
