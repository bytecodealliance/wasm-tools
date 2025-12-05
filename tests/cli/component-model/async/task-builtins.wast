;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async

;; backpressure.set -- removed from the spec
(assert_malformed
  (component quote
    "(core func (canon backpressure.set))"
  )
  "unexpected token")

;; backpressure.inc
(component
  (core module $m
    (import "" "backpressure.inc" (func $backpressure.inc))
  )
  (core func $backpressure.inc (canon backpressure.inc))
  (core instance $i (instantiate $m (with "" (instance (export "backpressure.inc" (func $backpressure.inc))))))
)

;; backpressure.inc; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "backpressure.inc" (func $backpressure.inc (param i32)))
    )
    (core func $backpressure.inc (canon backpressure.inc))
    (core instance $i (instantiate $m (with "" (instance (export "backpressure.inc" (func $backpressure.inc))))))
  )
  "type mismatch for export `backpressure.inc` of module instantiation argument ``"
)

;; backpressure.dec
(component
  (core module $m
    (import "" "backpressure.dec" (func $backpressure.dec))
  )
  (core func $backpressure.dec (canon backpressure.dec))
  (core instance $i (instantiate $m (with "" (instance (export "backpressure.dec" (func $backpressure.dec))))))
)

;; backpressure.dec; decorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "backpressure.dec" (func $backpressure.dec (param i32)))
    )
    (core func $backpressure.dec (canon backpressure.dec))
    (core instance $i (instantiate $m (with "" (instance (export "backpressure.dec" (func $backpressure.dec))))))
  )
  "type mismatch for export `backpressure.dec` of module instantiation argument ``"
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
    (core func $f (canon backpressure.inc))
    (core func $task-return (canon task.return (result u32) (callback $f)))
  )
  "cannot specify callback without async")

(assert_invalid
  (component
    (core func $f (canon backpressure.inc))
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

;; task.cancel
(component
  (core module $m
    (import "" "task.cancel" (func $task-cancel))
  )
  (core func $task-cancel (canon task.cancel))
  (core instance $i (instantiate $m (with "" (instance (export "task.cancel" (func $task-cancel))))))
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
  (core func $waitable-set-wait (canon waitable-set.wait (memory $libc "memory")))
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
    (core func $waitable-set-wait (canon waitable-set.wait (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.wait" (func $waitable-set-wait))))))
  )
  "type mismatch for export `waitable-set.wait` of module instantiation argument ``"
)

(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "waitable-set.wait" (func $waitable-set-wait (param i32 i32) (result i32)))
    )
    (core func $waitable-set-wait (canon waitable-set.wait cancellable (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.wait" (func $waitable-set-wait))))))
  )
  "requires the component model async stackful feature")

;; waitable-set.poll
(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core module $m
    (import "" "waitable-set.poll" (func $waitable-set-poll (param i32 i32) (result i32)))
  )
  (core func $waitable-set-poll (canon waitable-set.poll (memory $libc "memory")))
  (core instance $i (instantiate $m (with "" (instance (export "waitable-set.poll" (func $waitable-set-poll))))))
)
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "waitable-set.poll" (func $waitable-set-poll (param i32 i32) (result i32)))
    )
    (core func $waitable-set-poll (canon waitable-set.poll cancellable (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.poll" (func $waitable-set-poll))))))
  )
  "requires the component model async stackful feature")

;; waitable-set.poll; incorrect type
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "waitable-set.poll" (func $waitable-set-poll (param i32) (result i32)))
    )
    (core func $waitable-set-poll (canon waitable-set.poll (memory $libc "memory")))
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

;; subtask.cancel
(component
  (core module $m
    (import "" "subtask.cancel" (func $subtask-cancel (param i32) (result i32)))
  )
  (core func $subtask-cancel (canon subtask.cancel))
  (core instance $i (instantiate $m (with "" (instance (export "subtask.cancel" (func $subtask-cancel))))))
)

;; subtask.cancel; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "subtask.cancel" (func $subtask-cancel (param i32 i32) (result i32)))
    )
    (core func $subtask-cancel (canon subtask.cancel))
    (core instance $i (instantiate $m (with "" (instance (export "subtask.cancel" (func $subtask-cancel))))))
  )
  "type mismatch for export `subtask.cancel` of module instantiation argument ``"
)

;; context.{get,set}
(component
  (core func $get0 (canon context.get i32 0))
  (core func $set0 (canon context.set i32 0))

  (core module $m
    (import "" "get0" (func (result i32)))
    (import "" "set0" (func (param i32)))
  )
  (core instance (instantiate $m
    (with "" (instance
      (export "get0" (func $get0))
      (export "set0" (func $set0))
    ))
  ))
)

;; thread.yield
(component
  (core module $m
    (import "" "thread.yield" (func $thread.yield (result i32)))
  )
  (core func $thread.yield (canon thread.yield))
  (core instance $i (instantiate $m (with "" (instance (export "thread.yield" (func $thread.yield))))))
)

(assert_invalid
  (component
    (core module $m
      (import "" "thread.yield" (func $thread.yield (result i32)))
    )
    (core func $thread.yield (canon thread.yield cancellable))
    (core instance $i (instantiate $m (with "" (instance (export "thread.yield" (func $thread.yield))))))
  )
  "requires the component model async stackful feature")

;; thread.yield; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "thread.yield" (func $thread.yield (param i32) (result i32)))
    )
    (core func $thread.yield (canon thread.yield))
    (core instance $i (instantiate $m (with "" (instance (export "thread.yield" (func $thread.yield))))))
  )
  "type mismatch for export `thread.yield` of module instantiation argument ``"
)

(assert_invalid
  (component
    (core module $m (import "" "" (func (param i32) (result i32))))
    (core func $f (canon context.get i32 0))
    (core instance $i (instantiate $m (with "" (instance (export "" (func $f))))))
  )
  "found:    (func (result i32))")
(assert_invalid
  (component
    (core module $m (import "" "" (func (param i32) (result i32))))
    (core func $f (canon context.set i32 0))
    (core instance $i (instantiate $m (with "" (instance (export "" (func $f))))))
  )
  "found:    (func (param i32))")
(assert_invalid
  (component
    (core func (canon context.get i32 1)))
  "immediate must be zero: 1")
(assert_invalid
  (component
    (core func (canon context.set i32 1)))
  "immediate must be zero: 1")
(assert_invalid
  (component
    (core func (canon context.get i32 100)))
  "immediate must be zero: 100")
(assert_invalid
  (component
    (core func (canon context.set i32 100)))
  "immediate must be zero: 100")
(assert_malformed
  (component quote
    "(core func (canon context.get i64 100))")
  "expected keyword `i32`")
(assert_malformed
  (component quote
    "(core func (canon context.set i64 100))")
  "expected keyword `i32`")

(assert_malformed
  (component binary
    "\00asm" "\0d\00\01\00" ;; component header
    "\08\04"                ;; canonicals section, 4 bytes
    "\01"                   ;; 1 count
    "\0a\7e\00")            ;; context.get i64 0
  "invalid leading byte (0x7e) for context.get")
(assert_malformed
  (component binary
    "\00asm" "\0d\00\01\00" ;; component header
    "\08\04"                ;; canonicals section, 4 bytes
    "\01"                   ;; 1 count
    "\0b\7e\00")            ;; context.set i64 0
  "invalid leading byte (0x7e) for context.set")

;; different forms of canonical intrinsics

(component
  (core func (canon backpressure.inc))
  (canon backpressure.inc (core func))
  (core func (canon backpressure.dec))
  (canon backpressure.dec (core func))
  (core func (canon task.return))
  (canon task.return (core func))
  (core func (canon task.cancel))
  (canon task.cancel (core func))
  (core func (canon subtask.drop))
  (canon subtask.drop (core func))
  (core func (canon subtask.cancel))
  (canon subtask.cancel (core func))

  (core module $m
    (memory (export "m") 1)
  )
  (core instance $i (instantiate $m))
  (alias core export $i "m" (core memory $m))

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
  (core func (canon future.drop-readable $f))
  (canon future.drop-readable $f (core func))
  (core func (canon future.drop-writable $f))
  (canon future.drop-writable $f (core func))
  (core func (canon stream.drop-readable $s))
  (canon stream.drop-readable $s (core func))
  (core func (canon stream.drop-writable $s))
  (canon stream.drop-writable $s (core func))
  (core func (canon future.read $f (memory $m)))
  (canon future.read $f (memory $m) (core func))
  (core func (canon future.write $f (memory $m)))
  (canon future.write $f (memory $m) (core func))
  (core func (canon stream.read $s (memory $m)))
  (canon stream.read $s (memory $m) (core func))
  (core func (canon stream.write $s (memory $m)))
  (canon stream.write $s (memory $m) (core func))

  (core func (canon context.get i32 0))
  (canon context.get i32 0 (core func))
  (core func (canon context.set i32 0))
  (canon context.set i32 0 (core func))

  (core func (canon thread.yield))
  (canon thread.yield (core func))
)

(component
  (canon task.return (result (stream u8)) (core func))
)
