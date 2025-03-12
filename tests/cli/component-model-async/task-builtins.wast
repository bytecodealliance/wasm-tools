;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm-async-builtins,cm-async-stackful

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

(assert_invalid
  (component
    (core module $m
      (func (export "r") (param i32 i32 i32 i32) (result i32) unreachable)
    )
    (core instance $m (instantiate $m))
    (core func $task-return (canon task.return (result u32) (realloc (func $m "r"))))
  )
  "cannot specify `realloc` option on `task.return`")

(component
  (core module $m
    (memory (export "m") 1)
  )
  (core instance $i (instantiate $m))
  (core func (canon task.return (result u32) string-encoding=utf8))
  (core func (canon task.return (result u32) string-encoding=utf16))
  (core func (canon task.return (result u32) string-encoding=latin1+utf16))
  (core func (canon task.return (result u32) (memory $i "m")))
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

;; yield
(component
  (core module $m
    (import "" "yield" (func $yield))
  )
  (core func $yield (canon yield async))
  (core instance $i (instantiate $m (with "" (instance (export "yield" (func $yield))))))
)

;; yield; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "yield" (func $yield (param i32) (result i32)))
    )
    (core func $yield (canon yield async))
    (core instance $i (instantiate $m (with "" (instance (export "yield" (func $yield))))))
  )
  "type mismatch for export `yield` of module instantiation argument ``"
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

;; different forms of canonical intrinsics

(component
  (core func (canon backpressure.set))
  (canon backpressure.set (core func))
  (core func (canon task.return))
  (canon task.return (core func))
  (core func (canon subtask.drop))
  (canon subtask.drop (core func))

  (core module $m
    (memory (export "m") 1)
    (func (export "r") (param i32 i32 i32 i32) (result i32) unreachable)
  )
  (core instance $i (instantiate $m))
  (alias core export $i "m" (core memory $m))
  (alias core export $i "r" (core func $r))

  (type $r (resource (rep i32)))
  (core func (canon resource.drop $r async))
  (canon resource.drop $r async (core func))

  (type $s (stream))
  (type $f (future))
  (core func (canon future.new $f))
  (canon future.new $f (core func))
  (core func (canon stream.new $s))
  (canon stream.new $s (core func))
  (core func (canon future.cancel-read $f))
  (canon future.cancel-read $f (core func))
  (core func (canon stream.cancel-read $s))
  (canon stream.cancel-read $s (core func))
  (core func (canon future.cancel-write $f))
  (canon future.cancel-write $f (core func))
  (core func (canon stream.cancel-write $s))
  (canon stream.cancel-write $s (core func))
  (core func (canon future.close-readable $f))
  (canon future.close-readable $f (core func))
  (core func (canon future.close-writable $f))
  (canon future.close-writable $f (core func))
  (core func (canon stream.close-readable $s))
  (canon stream.close-readable $s (core func))
  (core func (canon stream.close-writable $s))
  (canon stream.close-writable $s (core func))
  (core func (canon future.read $f (memory $m)))
  (canon future.read $f (memory $m) (core func))
  (core func (canon future.write $f (memory $m)))
  (canon future.write $f (memory $m) (core func))
  (core func (canon stream.read $s (memory $m)))
  (canon stream.read $s (memory $m) (core func))
  (core func (canon stream.write $s (memory $m)))
  (canon stream.write $s (memory $m) (core func))

  (core func (canon error-context.new (memory $m)))
  (canon error-context.new (memory $m) (core func))
  (core func (canon error-context.debug-message (memory $m) (realloc $r)))
  (canon error-context.debug-message (memory $m) (realloc $r) (core func))
  (core func (canon error-context.drop))
  (canon error-context.drop (core func))
)
