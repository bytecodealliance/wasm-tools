;; RUN: dump %

(component
  (component $c
    (core module (export "e"))
  )
  (component $c2 (import "i" (component (import "i" (core module)))))
  (instance $C (instantiate $c))
  (alias export $C "e" (core module $m))
  (instance (instantiate $c2 (with "i" (instance
    (export "e" (core module $m))
  ))))
)
