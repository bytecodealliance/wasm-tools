;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm-async-stackful,cm-fixed-length-lists

;; waitable-set.wait
(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core module $m
    (import "" "waitable-set.wait" (func $waitable-set-wait (param i32 i32) (result i32)))
  )
  (core func $waitable-set-wait (canon waitable-set.wait cancellable (memory $libc "memory")))
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
    (core func $waitable-set-wait (canon waitable-set.wait cancellable (memory $libc "memory")))
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
  (core func $waitable-set-poll (canon waitable-set.poll cancellable (memory $libc "memory")))
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
    (core func $waitable-set-poll (canon waitable-set.poll cancellable (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.poll" (func $waitable-set-poll))))))
  )
  "type mismatch for export `waitable-set.poll` of module instantiation argument ``"
)

;; thread.yield
(component
  (core module $m
    (import "" "thread.yield" (func $thread.yield (result i32)))
  )
  (core func $thread.yield (canon thread.yield cancellable))
  (core instance $i (instantiate $m (with "" (instance (export "thread.yield" (func $thread.yield))))))
)

;; thread.yield; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "thread.yield" (func $thread.yield (param i32) (result i32)))
    )
    (core func $thread.yield (canon thread.yield cancellable))
    (core instance $i (instantiate $m (with "" (instance (export "thread.yield" (func $thread.yield))))))
  )
  "type mismatch for export `thread.yield` of module instantiation argument ``"
)

;; async lift, stackful abi
(component
  ;; func()
  (core module $m1
    (func (export "f") unreachable))
  (core instance $m1 (instantiate $m1))
  (func
    (canon lift (core func $m1 "f") async))

  ;; func(x: u32)
  (core module $m2
    (func (export "f") (param i32) unreachable))
  (core instance $m2 (instantiate $m2))
  (func (param "x" u32)
    (canon lift (core func $m2 "f") async))

  ;; func() -> u32
  (core module $m3
    (func (export "f") unreachable))
  (core instance $m3 (instantiate $m3))
  (func (result u32)
    (canon lift (core func $m3 "f") async))

  ;; func(x: f32)
  (core module $m4
    (func (export "f") (param f32) unreachable))
  (core instance $m4 (instantiate $m4))
  (func (param "x" f32)
    (canon lift (core func $m4 "f") async))

  ;; func(x: f32, y: string)
  (core module $m5
    (memory (export "memory") 1)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
    (func (export "f") (param f32 i32 i32) unreachable))
  (core instance $m5 (instantiate $m5))
  (func (param "x" f32) (param "y" string)
    (canon lift (core func $m5 "f") async
      (memory $m5 "memory") (realloc (func $m5 "realloc"))))

  ;; func(x: list<string; 4>)
  (core module $m6
    (memory (export "memory") 1)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
    (func (export "f") (param i32 i32 i32 i32 i32 i32 i32 i32) unreachable))
  (core instance $m6 (instantiate $m6))
  (func (param "x" (list string 4))
    (canon lift (core func $m6 "f") async
      (memory $m6 "memory") (realloc (func $m6 "realloc"))))

  ;; func(x: list<string; 10>)
  (core module $m7
    (memory (export "memory") 1)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
    (func (export "f") (param i32) unreachable))
  (core instance $m7 (instantiate $m7))
  (func (param "x" (list string 10))
    (canon lift (core func $m7 "f") async
      (memory $m7 "memory") (realloc (func $m7 "realloc"))))
)

;; async lift; no callback
(component
  (core module $m
    (func (export "foo") (param i32) unreachable)
  )
  (core instance $i (instantiate $m))

  (func (export "foo") (param "p1" u32) (result u32)
    (canon lift (core func $i "foo") async)
  )
)
