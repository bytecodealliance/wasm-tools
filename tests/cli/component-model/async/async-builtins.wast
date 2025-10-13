;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm-async-builtins

;; stream.cancel-read
(component
  (core module $m
    (import "" "stream.cancel-read" (func $stream-cancel-read (param i32) (result i32)))
  )
  (type $stream-type (stream u8))
  (core func $stream-cancel-read (canon stream.cancel-read $stream-type async))
  (core instance $i (instantiate $m (with "" (instance (export "stream.cancel-read" (func $stream-cancel-read))))))
)

;; stream.cancel-read; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "stream.cancel-read" (func $stream-cancel-read (param i32 i32) (result i32)))
    )
    (type $stream-type (stream u8))
    (core func $stream-cancel-read (canon stream.cancel-read $stream-type async))
    (core instance $i (instantiate $m (with "" (instance (export "stream.cancel-read" (func $stream-cancel-read))))))
  )
  "type mismatch for export `stream.cancel-read` of module instantiation argument ``"
)

;; stream.cancel-write
(component
  (core module $m
    (import "" "stream.cancel-write" (func $stream-cancel-write (param i32) (result i32)))
  )
  (type $stream-type (stream u8))
  (core func $stream-cancel-write (canon stream.cancel-write $stream-type async))
  (core instance $i (instantiate $m (with "" (instance (export "stream.cancel-write" (func $stream-cancel-write))))))
)

;; stream.cancel-write; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "stream.cancel-write" (func $stream-cancel-write (param i32 i32) (result i32)))
    )
    (type $stream-type (stream u8))
    (core func $stream-cancel-write (canon stream.cancel-write $stream-type async))
    (core instance $i (instantiate $m (with "" (instance (export "stream.cancel-write" (func $stream-cancel-write))))))
  )
  "type mismatch for export `stream.cancel-write` of module instantiation argument ``"
)

(component
  (core func (canon subtask.cancel async))
  (canon subtask.cancel async (core func))

  (type $r (resource (rep i32)))
  (core func (canon resource.drop $r async))
  (canon resource.drop $r async (core func))

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
