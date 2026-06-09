;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm64

;; waitable-set.wait

(component
    (core module $libc (memory (export "memory") i64 1))
    (core instance $libc (instantiate $libc))
    (core module $m
        (import "" "waitable-set.wait" (func $waitable-set-wait (param i32 i64) (result i32)))
    )
    (core func $waitable-set-wait (canon waitable-set.wait cancellable (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.wait" (func $waitable-set-wait))))))
)

(assert_invalid
    (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
        (import "" "waitable-set.wait" (func $waitable-set-wait (param i32 i64) (result i32)))
    )
    (core func $waitable-set-wait (canon waitable-set.wait cancellable (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.wait" (func $waitable-set-wait))))))
    )
    "type mismatch for export `waitable-set.wait`"
)

(assert_invalid
    (component
    (core module $libc (memory (export "memory") i64 1))
    (core instance $libc (instantiate $libc))
    (core module $m
        (import "" "waitable-set.wait" (func $waitable-set-wait (param i32 i32) (result i32)))
    )
    (core func $waitable-set-wait (canon waitable-set.wait cancellable (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.wait" (func $waitable-set-wait))))))
    )
    "type mismatch for export `waitable-set.wait`"
)


;; waitable-set.poll

(component
    (core module $libc (memory (export "memory") i64 1))
    (core instance $libc (instantiate $libc))
    (core module $m
        (import "" "waitable-set.poll" (func $waitable-set-poll (param i32 i64) (result i32)))
    )
    (core func $waitable-set-poll (canon waitable-set.poll cancellable (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.poll" (func $waitable-set-poll))))))
)

(assert_invalid
    (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
        (import "" "waitable-set.poll" (func $waitable-set-poll (param i32 i64) (result i32)))
    )
    (core func $waitable-set-poll (canon waitable-set.poll cancellable (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.poll" (func $waitable-set-poll))))))
    )
    "type mismatch for export `waitable-set.poll`"
)

(assert_invalid
    (component
    (core module $libc (memory (export "memory") i64 1))
    (core instance $libc (instantiate $libc))
    (core module $m
        (import "" "waitable-set.poll" (func $waitable-set-poll (param i32 i32) (result i32)))
    )
    (core func $waitable-set-poll (canon waitable-set.poll cancellable (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "waitable-set.poll" (func $waitable-set-poll))))))
    )
    "type mismatch for export `waitable-set.poll`"
)