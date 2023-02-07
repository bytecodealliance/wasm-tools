;; RUN: dump %

(component
 (import "a" (component $c (import "a" (func))))
 (instance (instantiate $c))
)
