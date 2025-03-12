;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async

;; error-context.new
(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core module $m
    (import "" "error-context.new" (func $error-context-new (param i32 i32) (result i32)))
  )
  (core func $error-context-new (canon error-context.new (memory $libc "memory")))
  (core instance $i (instantiate $m (with "" (instance (export "error-context.new" (func $error-context-new))))))
)

;; error-context.new; incorrect type
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "error-context.new" (func $error-context-new (param i32) (result i32)))
    )
    (core func $error-context-new (canon error-context.new (memory $libc "memory")))
    (core instance $i (instantiate $m (with "" (instance (export "error-context.new" (func $error-context-new))))))
  )
  "type mismatch for export `error-context.new` of module instantiation argument ``"
)

;; error-context.debug-message
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

;; error-context.debug-message; incorrect type
(assert_invalid
  (component
    (core module $libc
      (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
      (memory (export "memory") 1)
    )
    (core instance $libc (instantiate $libc))
    (core module $m
      (import "" "error-context.debug-message" (func $error-context-debug-message (param i32) (result i32)))
    )
    (core func $error-context-debug-message (canon error-context.debug-message (memory $libc "memory") (realloc (func $libc "realloc"))))
    (core instance $i (instantiate $m (with "" (instance (export "error-context.debug-message" (func $error-context-debug-message))))))
  )
  "type mismatch for export `error-context.debug-message` of module instantiation argument ``"
)

;; error-context.drop
(component
  (core module $m
    (import "" "error-context.drop" (func $error-context-drop (param i32)))
  )
  (core func $error-context-drop (canon error-context.drop))
  (core instance $i (instantiate $m (with "" (instance (export "error-context.drop" (func $error-context-drop))))))
)

;; error-context.drop; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "error-context.drop" (func $error-context-drop (param i32) (result i32)))
    )
    (core func $error-context-drop (canon error-context.drop))
    (core instance $i (instantiate $m (with "" (instance (export "error-context.drop" (func $error-context-drop))))))
  )
  "type mismatch for export `error-context.drop` of module instantiation argument ``"
)

;; can define the `error-context` type
(component (type error-context))
(component (type (list error-context)))
