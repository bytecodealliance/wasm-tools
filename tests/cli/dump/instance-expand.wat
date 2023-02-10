;; RUN: dump %

(component
  (type $i (instance
    (export "" (func))
  ))

  (import "a" (instance (type $i)))
)
