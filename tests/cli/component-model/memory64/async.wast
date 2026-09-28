;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm-error-context,cm64

;; async lower: retptr parameter
(component
  (core module $libc (memory (export "memory") i64 1))
  (core instance $libc (instantiate $libc))
  (import "f" (func $f async (param "x" u32) (result u32)))
  (core func $lowered
    (canon lower (func $f) (memory (core memory $libc "memory")) async))
  (core module $m (import "" "f" (func (param i32 i64) (result i32))))
  (core instance
    (instantiate $m (with "" (instance (export "f" (func $lowered))))))
)

(assert_invalid
  (component
    (core module $libc (memory (export "memory") i64 1))
    (core instance $libc (instantiate $libc))
    (import "f" (func $f async (param "x" u32) (result u32)))
    (core func $lowered
      (canon lower (func $f) (memory (core memory $libc "memory")) async))
    (core module $m (import "" "f" (func (param i32 i32) (result i32))))
    (core instance
      (instantiate $m (with "" (instance (export "f" (func $lowered))))))
  )
  "type mismatch for export `f` of module instantiation argument ``"
)

;; stream.read / stream.write
(component
  (core module $libc
    (memory (export "memory") i64 1)
    (func (export "realloc") (param i64 i64 i64 i64) (result i64) unreachable)
  )
  (core instance $libc (instantiate $libc))
  (type $stream-type (stream u8))
  (core func $stream-read
    (canon stream.read $stream-type async
      (memory (core memory $libc "memory"))
      (realloc (core func $libc "realloc"))))
  (core func $stream-write
    (canon stream.write $stream-type async
      (memory (core memory $libc "memory"))))
  (core module $m
    (import "" "stream.read" (func (param i32 i64 i64) (result i64)))
    (import "" "stream.write" (func (param i32 i64 i64) (result i64)))
  )
  (core instance (instantiate $m (with "" (instance
    (export "stream.read" (func $stream-read))
    (export "stream.write" (func $stream-write))
  ))))
)

(assert_invalid
  (component
    (core module $libc
      (memory (export "memory") i64 1)
      (func (export "realloc") (param i64 i64 i64 i64) (result i64) unreachable)
    )
    (core instance $libc (instantiate $libc))
    (type $stream-type (stream u8))
    (core func $stream-read
      (canon stream.read $stream-type async
        (memory (core memory $libc "memory"))
        (realloc (core func $libc "realloc"))))
    (core module $m
      (import "" "stream.read" (func (param i32 i32 i32) (result i32))))
    (core instance (instantiate $m (with "" (instance
      (export "stream.read" (func $stream-read))
    ))))
  )
  "type mismatch for export `stream.read` of module instantiation argument ``"
)

;; future.read / future.write
(component
  (core module $libc
    (memory (export "memory") i64 1)
    (func (export "realloc") (param i64 i64 i64 i64) (result i64) unreachable)
  )
  (core instance $libc (instantiate $libc))
  (type $future-type (future u8))
  (core func $future-read
    (canon future.read $future-type async
      (memory (core memory $libc "memory"))
      (realloc (core func $libc "realloc"))))
  (core func $future-write
    (canon future.write $future-type async
      (memory (core memory $libc "memory"))))
  (core module $m
    (import "" "future.read" (func (param i32 i64) (result i32)))
    (import "" "future.write" (func (param i32 i64) (result i32)))
  )
  (core instance (instantiate $m (with "" (instance
    (export "future.read" (func $future-read))
    (export "future.write" (func $future-write))
  ))))
)

(assert_invalid
  (component
    (core module $libc (memory (export "memory") i64 1))
    (core instance $libc (instantiate $libc))
    (type $future-type (future u8))
    (core func $future-write
      (canon future.write $future-type async
        (memory (core memory $libc "memory"))))
    (core module $m
      (import "" "future.write" (func (param i32 i32) (result i32))))
    (core instance (instantiate $m (with "" (instance
      (export "future.write" (func $future-write))
    ))))
  )
  "type mismatch for export `future.write` of module instantiation argument ``"
)

;; error-context.new / error-context.debug-message
(component
  (core module $libc
    (memory (export "memory") i64 1)
    (func (export "realloc") (param i64 i64 i64 i64) (result i64) unreachable)
  )
  (core instance $libc (instantiate $libc))
  (core func $new
    (canon error-context.new (memory (core memory $libc "memory"))))
  (core func $debug-message
    (canon error-context.debug-message
      (memory (core memory $libc "memory"))
      (realloc (core func $libc "realloc"))))
  (core module $m
    (import "" "error-context.new" (func (param i64 i64) (result i32)))
    (import "" "error-context.debug-message" (func (param i32 i64)))
  )
  (core instance (instantiate $m (with "" (instance
    (export "error-context.new" (func $new))
    (export "error-context.debug-message" (func $debug-message))
  ))))
)

(assert_invalid
  (component
    (core module $libc (memory (export "memory") i64 1))
    (core instance $libc (instantiate $libc))
    (core func $new
      (canon error-context.new (memory (core memory $libc "memory"))))
    (core module $m
      (import "" "error-context.new" (func (param i32 i32) (result i32))))
    (core instance (instantiate $m (with "" (instance
      (export "error-context.new" (func $new))
    ))))
  )
  "type mismatch for export `error-context.new` of module instantiation argument ``"
)
