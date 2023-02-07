;; RUN: dump %

(component
  (import "a" (component $c))
  (func $f (import "f"))

  (instance $a
    (instantiate $c
      (with "a" (func $f))
    )
  )
)
