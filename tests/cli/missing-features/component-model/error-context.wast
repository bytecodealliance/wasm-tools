;; RUN: wast --assert default --snapshot tests/snapshots % -f=-cm-error-context

;; error-context.new
(assert_invalid
  (component
    (import "x" (func (param "x" error-context)))
  )
  "requires the component model error-context feature"
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
  "`error-context.new` requires the component model error-context feature"
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
  "`error-context.debug-message` requires the component model error-context feature"
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
  "`error-context.drop` requires the component model error-context feature"
)

;; various types
(assert_invalid
  (component (type error-context))
  "requires the component model error-context feature"
)
