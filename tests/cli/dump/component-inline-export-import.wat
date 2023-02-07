;; RUN: dump %

(component
  (component
    (export "")
    (import "")
  )
  (core module
    (export "a")
    (import "a")
  )
)
