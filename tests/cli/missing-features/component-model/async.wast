;; RUN: wast --assert default --snapshot tests/snapshots % -f=-cm-async

;; async lift
(assert_invalid
  (component
    (core module $m
      (func (export "foo") (param i32) (result i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "foo") (param "p1" u32) (result u32)
      (canon lift (core func $i "foo") async)
    )
  )
  "canonical option `async` requires the component model async feature"
)

;; async lower
(assert_invalid
  (component
    (import "foo" (func $foo (param "p1" u32) (result u32)))
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core func $foo (canon lower (func $foo) async (memory $libc "memory")))
    (core module $m
      (func (import "" "foo") (param i32 i32) (result i32))
    )
    (core instance $i (instantiate $m (with "" (instance (export "foo" (func $foo))))))
  )
  "canonical option `async` requires the component model async feature"
)

;; backpressure.set
(assert_invalid
  (component
    (core module $m
      (import "" "backpressure.set" (func $backpressure-set (param i32)))
    )
    (core func $backpressure.set (canon backpressure.set))
    (core instance $i (instantiate $m (with "" (instance (export "backpressure.set" (func $backpressure.set))))))
  )
  "`backpressure.set` requires the component model async feature"
)

;; task.return
(assert_invalid
  (component
    (core module $m
      (import "" "task.return" (func $task-return (param i32)))
    )
    (core func $task-return (canon task.return (result u32)))
    (core instance $i (instantiate $m (with "" (instance (export "task.return" (func $task-return))))))
  )
  "`task.return` requires the component model async feature"
)

;; waitable-set.new
(assert_invalid
  (component (core func (canon waitable-set.new)))
  "`waitable-set.new` requires the component model async feature"
)

;; waitable-set.wait
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "waitable-set.wait" (func $waitable-set-wait (param i32) (result i32)))
    )
    (core func $waitable-set-wait (canon waitable-set.wait async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.wait" (func $waitable-set-wait))))))
  )
  "`waitable-set.wait` requires the component model async feature"
)

;; waitable-set.poll
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "waitable-set.poll" (func $waitable-set-poll (param i32) (result i32)))
    )
    (core func $waitable-set-poll (canon waitable-set.poll async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.poll" (func $waitable-set-poll))))))
  )
  "`waitable-set.poll` requires the component model async feature"
)

;; waitable-set.drop
(assert_invalid
  (component (core func (canon waitable-set.drop)))
  "`waitable-set.drop` requires the component model async feature"
)

;; waitable.join
(assert_invalid
  (component (core func (canon waitable.join)))
  "`waitable.join` requires the component model async feature"
)

;; yield
(assert_invalid
  (component
    (core module $m
      (import "" "yield" (func $yield))
    )
    (core func $yield (canon yield async))
    (core instance $i (instantiate $m (with "" (instance (export "yield" (func $yield))))))
  )
  "`yield` requires the component model async feature"
)

;; subtask.drop
(assert_invalid
  (component
    (core module $m
      (import "" "subtask.drop" (func $subtask-drop (param i32)))
    )
    (core func $subtask-drop (canon subtask.drop))
    (core instance $i (instantiate $m (with "" (instance (export "subtask.drop" (func $subtask-drop))))))
  )
  "`subtask.drop` requires the component model async feature"
)

;; stream.new
(assert_invalid
  (component
    (core module $m
      (import "" "stream.new" (func $stream-new (result i32)))
    )
    (type $stream-type (stream u8))
    (core func $stream-new (canon stream.new $stream-type))
    (core instance $i (instantiate $m (with "" (instance (export "stream.new" (func $stream-new))))))
  )
  "requires the component model async feature"
)

;; stream.read
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "stream.read" (func $stream-read (param i32 i32 i32) (result i32)))
    )
    (type $stream-type (stream u8))
    (core func $stream-read (canon stream.read $stream-type async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "stream.read" (func $stream-read))))))
  )
  "requires the component model async feature"
)

;; stream.write
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "stream.write" (func $stream-write (param i32 i32 i32) (result i32)))
    )
    (type $stream-type (stream u8))
    (core func $stream-write (canon stream.write $stream-type async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "stream.write" (func $stream-write))))))
  )
  "requires the component model async feature"
)

;; stream.cancel-read
(assert_invalid
  (component
    (core module $m
      (import "" "stream.cancel-read" (func $stream-cancel-read (param i32) (result i32)))
    )
    (type $stream-type (stream u8))
    (core func $stream-cancel-read (canon stream.cancel-read $stream-type async))
    (core instance $i (instantiate $m (with "" (instance (export "stream.cancel-read" (func $stream-cancel-read))))))
  )
  "requires the component model async feature"
)

;; stream.cancel-write
(assert_invalid
  (component
    (core module $m
      (import "" "stream.cancel-write" (func $stream-cancel-write (param i32) (result i32)))
    )
    (type $stream-type (stream u8))
    (core func $stream-cancel-write (canon stream.cancel-write $stream-type async))
    (core instance $i (instantiate $m (with "" (instance (export "stream.cancel-write" (func $stream-cancel-write))))))
  )
  "requires the component model async feature"
)

;; stream.close-readable
(assert_invalid
  (component
    (core module $m
      (import "" "stream.close-readable" (func $stream-close-readable (param i32)))
    )
    (type $stream-type (stream u8))
    (core func $stream-close-readable (canon stream.close-readable $stream-type))
    (core instance $i (instantiate $m (with "" (instance (export "stream.close-readable" (func $stream-close-readable))))))
  )
  "requires the component model async feature"
)

;; stream.close-writable
(assert_invalid
  (component
    (core module $m
      (import "" "stream.close-writable" (func $stream-close-writable (param i32 i32)))
    )
    (type $stream-type (stream u8))
    (core func $stream-close-writable (canon stream.close-writable $stream-type))
    (core instance $i (instantiate $m (with "" (instance (export "stream.close-writable" (func $stream-close-writable))))))
  )
  "requires the component model async feature"
)

;; future.new
(assert_invalid
  (component
    (core module $m
      (import "" "future.new" (func $future-new (result i32)))
    )
    (type $future-type (future u8))
    (core func $future-new (canon future.new $future-type))
    (core instance $i (instantiate $m (with "" (instance (export "future.new" (func $future-new))))))
  )
  "requires the component model async feature"
)

;; future.read
(assert_invalid
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
  "requires the component model async feature"
)

;; future.write
(assert_invalid
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
  "requires the component model async feature"
)

;; future.cancel-read
(assert_invalid
  (component
    (core module $m
      (import "" "future.cancel-read" (func $future-cancel-read (param i32) (result i32)))
    )
    (type $future-type (future u8))
    (core func $future-cancel-read (canon future.cancel-read $future-type async))
    (core instance $i (instantiate $m (with "" (instance (export "future.cancel-read" (func $future-cancel-read))))))
  )
  "requires the component model async feature"
)

;; future.cancel-write
(assert_invalid
  (component
    (core module $m
      (import "" "future.cancel-write" (func $future-cancel-write (param i32) (result i32)))
    )
    (type $future-type (future u8))
    (core func $future-cancel-write (canon future.cancel-write $future-type async))
    (core instance $i (instantiate $m (with "" (instance (export "future.cancel-write" (func $future-cancel-write))))))
  )
  "requires the component model async feature"
)

;; future.close-readable
(assert_invalid
  (component
    (core module $m
      (import "" "future.close-readable" (func $future-close-readable (param i32)))
    )
    (type $future-type (future u8))
    (core func $future-close-readable (canon future.close-readable $future-type))
    (core instance $i (instantiate $m (with "" (instance (export "future.close-readable" (func $future-close-readable))))))
  )
  "requires the component model async feature"
)

;; future.close-writable
(assert_invalid
  (component
    (core module $m
      (import "" "future.close-writable" (func $future-close-writable (param i32 i32)))
    )
    (type $future-type (future u8))
    (core func $future-close-writable (canon future.close-writable $future-type))
    (core instance $i (instantiate $m (with "" (instance (export "future.close-writable" (func $future-close-writable))))))
  )
  "requires the component model async feature"
)

;; error-context.new
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "error-context.new" (func $error-context-new (param i32 i32) (result i32)))
    )
    (core func $error-context-new (canon error-context.new (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "error-context.new" (func $error-context-new))))))
  )
  "`error-context.new` requires the component model async feature"
)

;; error-context.debug-message
(assert_invalid
  (component
    (core module $libc
      (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
      (memory (export "memory") 1)
    )
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "error-context.debug-message" (func $error-context-debug-message (param i32 i32)))
    )
    (core func $error-context-debug-message (canon error-context.debug-message (memory $libc "memory") (realloc (func $libc "realloc"))))
    (core instance $i (instantiate $m (with "" (instance (export "error-context.debug-message" (func $error-context-debug-message))))))
  )
  "`error-context.debug-message` requires the component model async feature"
)

;; error-context.drop
(assert_invalid
  (component
    (core module $m
      (import "" "error-context.drop" (func $error-context-drop (param i32)))
    )
    (core func $error-context-drop (canon error-context.drop))
    (core instance $i (instantiate $m (with "" (instance (export "error-context.drop" (func $error-context-drop))))))
  )
  "`error-context.drop` requires the component model async feature"
)

;; various types
(assert_invalid
  (component (type (future)))
  "requires the component model async feature"
)
(assert_invalid
  (component (type (stream)))
  "requires the component model async feature"
)
(assert_invalid
  (component (type error-context))
  "requires the component model async feature"
)
(assert_invalid
  (component
    (type $t (resource (rep i32)))
    (core func $f (canon resource.drop $t async))
  )
  "requires the component model async builtins feature"
)

(assert_invalid
  (component (import "[async]f" (func)))
  "require the component model async feature"
)

(assert_invalid
  (component (import "[async method]a.b" (func)))
  "require the component model async feature"
)

(assert_invalid
  (component (import "[async static]a.b" (func)))
  "require the component model async feature"
)
