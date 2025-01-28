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

;; task.backpressure
(assert_invalid
  (component
    (core module $m
      (import "" "task.backpressure" (func $task-backpressure (param i32)))
    )
    (core func $task-backpressure (canon task.backpressure))
    (core instance $i (instantiate $m (with "" (instance (export "task.backpressure" (func $task-backpressure))))))
  )
  "`task.backpressure` requires the component model async feature"
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

;; task.wait
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "task.wait" (func $task-wait (param i32) (result i32)))
    )
    (core func $task-wait (canon task.wait async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "task.wait" (func $task-wait))))))
  )
  "`task.wait` requires the component model async feature"
)

;; task.poll
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "task.poll" (func $task-poll (param i32) (result i32)))
    )
    (core func $task-poll (canon task.poll async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "task.poll" (func $task-poll))))))
  )
  "`task.poll` requires the component model async feature"
)

;; task.yield
(assert_invalid
  (component
    (core module $m
      (import "" "task.yield" (func $task-yield))
    )
    (core func $task-yield (canon task.yield async))
    (core instance $i (instantiate $m (with "" (instance (export "task.yield" (func $task-yield))))))
  )
  "`task.yield` requires the component model async feature"
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
  "`stream.new` requires the component model async feature"
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
  "`stream.read` requires the component model async feature"
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
  "`stream.write` requires the component model async feature"
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
  "`stream.cancel-read` requires the component model async feature"
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
  "`stream.cancel-write` requires the component model async feature"
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
  "`stream.close-readable` requires the component model async feature"
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
  "`stream.close-writable` requires the component model async feature"
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
  "`future.new` requires the component model async feature"
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
  "`future.read` requires the component model async feature"
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
  "`future.write` requires the component model async feature"
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
  "`future.cancel-read` requires the component model async feature"
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
  "`future.cancel-write` requires the component model async feature"
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
  "`future.close-readable` requires the component model async feature"
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
  "`future.close-writable` requires the component model async feature"
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
