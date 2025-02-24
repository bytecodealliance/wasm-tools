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

;; waitable-set.new
(component
  (core module $m (import "" "waitable-set.new" (func (result i32))))
  (core func $waitable-set-new (canon waitable-set.new))
  (core instance $i (instantiate $m (with "" (instance (export "waitable-set.new" (func $waitable-set-new))))))
)

;; waitable-set.new; incorrect type
(assert_invalid
  (component
    (core module $m (import "" "waitable-set.new" (func (result i64))))
    (core func $waitable-set-new (canon waitable-set.new))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.new" (func $waitable-set-new))))))
  )
  "type mismatch for export `waitable-set.new` of module instantiation argument ``"
)

;; waitable-set.wait
(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core module $m
    (import "" "waitable-set.wait" (func $waitable-set-wait (param i32 i32) (result i32)))
  )
  (core func $waitable-set-wait (canon waitable-set.wait async (memory $libc "memory")))
  (core instance $i (instantiate $m (with "" (instance (export "waitable-set.wait" (func $waitable-set-wait))))))
)

;; waitable-set.wait; incorrect type
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
  "type mismatch for export `waitable-set.wait` of module instantiation argument ``"
)

;; waitable-set.poll
(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core module $m
    (import "" "waitable-set.poll" (func $waitable-set-poll (param i32 i32) (result i32)))
  )
  (core func $waitable-set-poll (canon waitable-set.poll async (memory $libc "memory")))
  (core instance $i (instantiate $m (with "" (instance (export "waitable-set.poll" (func $waitable-set-poll))))))
)

;; waitable-set.poll; incorrect type
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
  "type mismatch for export `waitable-set.poll` of module instantiation argument ``"
)

;; waitable-set.drop
(component
  (core module $m (import "" "waitable-set.drop" (func (param i32))))
  (core func $waitable-set-drop (canon waitable-set.drop))
  (core instance $i (instantiate $m (with "" (instance (export "waitable-set.drop" (func $waitable-set-drop))))))
)

;; waitable-set.drop; incorrect type
(assert_invalid
  (component
    (core module $m (import "" "waitable-set.drop" (func (param i64))))
    (core func $waitable-set-drop (canon waitable-set.drop))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.drop" (func $waitable-set-drop))))))
  )
  "type mismatch for export `waitable-set.drop` of module instantiation argument ``"
)

;; waitable.join
(component
  (core module $m (import "" "waitable.join" (func (param i32 i32))))
  (core func $waitable.join (canon waitable.join))
  (core instance $i (instantiate $m (with "" (instance (export "waitable.join" (func $waitable.join))))))
)

;; waitable.join; incorrect type
(assert_invalid
  (component
    (core module $m (import "" "waitable.join" (func (param i64))))
    (core func $waitable.join (canon waitable.join))
    (core instance $i (instantiate $m (with "" (instance (export "waitable.join" (func $waitable.join))))))
  )
  "type mismatch for export `waitable.join` of module instantiation argument ``"
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
