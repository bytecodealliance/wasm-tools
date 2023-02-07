;; RUN: dump %

(component
  (import "a" (component))
  (import "b" (core module))
  (import "c" (instance))
  (import "d" (func))
)
