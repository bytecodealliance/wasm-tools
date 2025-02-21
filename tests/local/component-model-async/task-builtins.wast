;; backpressure.set
(component
  (core module $m
    (import "" "backpressure.set" (func $backpressure.set (param i32)))
  )
  (core func $backpressure.set (canon backpressure.set))
  (core instance $i (instantiate $m (with "" (instance (export "backpressure.set" (func $backpressure.set))))))
)

;; backpressure.set; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "backpressure.set" (func $backpressure.set (param i32 i32)))
    )
    (core func $backpressure.set (canon backpressure.set))
    (core instance $i (instantiate $m (with "" (instance (export "backpressure.set" (func $backpressure.set))))))
  )
  "type mismatch for export `backpressure.set` of module instantiation argument ``"
)

;; task.return
(component
  (core module $m
    (import "" "task.return" (func $task-return (param i32)))
  )
  (core func $task-return (canon task.return (result u32)))
  (core instance $i (instantiate $m (with "" (instance (export "task.return" (func $task-return))))))
)

(assert_invalid
  (component (core func $task-return (canon task.return (result u32) async)))
  "cannot specify `async` option on `task.return`")

(assert_invalid
  (component
    (core func $f (canon backpressure.set))
    (core func $task-return (canon task.return (result u32) (callback $f)))
  )
  "cannot specify `callback` option on `task.return`")

(assert_invalid
  (component
    (core func $f (canon backpressure.set))
    (core func $task-return (canon task.return (result u32) (post-return $f)))
  )
  "cannot specify `post-return` option on `task.return`")

(component
  (core module $m
    (memory (export "m") 1)
    (func (export "r") (param i32 i32 i32 i32) (result i32) unreachable)
  )
  (core instance $i (instantiate $m))
  (core func (canon task.return (result u32) string-encoding=utf8))
  (core func (canon task.return (result u32) string-encoding=utf16))
  (core func (canon task.return (result u32) string-encoding=latin1+utf16))
  (core func (canon task.return (result u32) (memory $i "m")))
  (core func (canon task.return (result u32) (realloc (func $i "r"))))
)

;; task.wait
(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core module $m
    (import "" "task.wait" (func $task-wait (param i32) (result i32)))
  )
  (core func $task-wait (canon task.wait async (memory $libc "memory")))
  (core instance $i (instantiate $m (with "" (instance (export "task.wait" (func $task-wait))))))
)

;; task.wait; incorrect type
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "task.wait" (func $task-wait (param i32 i32) (result i32)))
    )
    (core func $task-wait (canon task.wait async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "task.wait" (func $task-wait))))))
  )
  "type mismatch for export `task.wait` of module instantiation argument ``"
)

;; task.poll
(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core module $m
    (import "" "task.poll" (func $task-poll (param i32) (result i32)))
  )
  (core func $task-poll (canon task.poll async (memory $libc "memory")))
  (core instance $i (instantiate $m (with "" (instance (export "task.poll" (func $task-poll))))))
)

;; task.poll; incorrect type
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "task.poll" (func $task-poll (param i32 i32) (result i32)))
    )
    (core func $task-poll (canon task.poll async (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "task.poll" (func $task-poll))))))
  )
  "type mismatch for export `task.poll` of module instantiation argument ``"
)

;; task.yield
(component
  (core module $m
    (import "" "task.yield" (func $task-yield))
  )
  (core func $task-yield (canon task.yield async))
  (core instance $i (instantiate $m (with "" (instance (export "task.yield" (func $task-yield))))))
)

;; task.yield; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "task.yield" (func $task-yield (param i32) (result i32)))
    )
    (core func $task-yield (canon task.yield async))
    (core instance $i (instantiate $m (with "" (instance (export "task.yield" (func $task-yield))))))
  )
  "type mismatch for export `task.yield` of module instantiation argument ``"
)

;; subtask.drop
(component
  (core module $m
    (import "" "subtask.drop" (func $subtask-drop (param i32)))
  )
  (core func $subtask-drop (canon subtask.drop))
  (core instance $i (instantiate $m (with "" (instance (export "subtask.drop" (func $subtask-drop))))))
)

;; subtask.drop; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "subtask.drop" (func $subtask-drop (param i32) (result i32)))
    )
    (core func $subtask-drop (canon subtask.drop))
    (core instance $i (instantiate $m (with "" (instance (export "subtask.drop" (func $subtask-drop))))))
  )
  "type mismatch for export `subtask.drop` of module instantiation argument ``"
)
