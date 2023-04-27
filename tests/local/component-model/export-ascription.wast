(component
  (import "f" (func $f))
  (export "f2" (func $f) (func))
)

;; subtyping works
(component
  (import "f" (instance $i (export "f" (func))))
  (export "f2" (instance $i) (instance))
)

;; make sure subtyping works in the right direction
(assert_invalid
  (component
    (import "f" (instance $i))
    (export "f2" (instance $i) (instance (export "f" (func))))
  )
  "ascribed type of export is not compatible")

;; make sure the type is actually changed
(assert_invalid
  (component
    (import "f" (func $f))

    (component $c
      (import "f" (instance $i (export "f" (func))))
      (export "f2" (instance $i) (instance))
    )

    (instance $c (instantiate $c (with "f" (instance (export "f" (func $f))))))

    (component $consume
      (import "arg" (instance $i (export "f" (func))))
    )

    (instance (instantiate $consume (with "arg" (instance $c "f2"))))
  )
  "missing expected export `f`")
