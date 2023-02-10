;; RUN: dump %

(component
  (component (import "a"))

  (component)
  (component)

  (component (export "a"))

  (component
    (component)
  )

  (component
    (core module $m)
    (import "a" (func (param "p" string)))
    (export "a" (core module $m))

    (instance (export "b") (import "b")
      (export "a" (func))
    )
  )
)
