(assert_invalid
  (component
    (import "f" (func $f (param "x" (list u8))))
    (core func $f (canon lower (func $f)
    ))
  )
  "canonical option `memory` is required")

(assert_invalid
  (component
    (import "f" (func $f (result (list u8))))
    (core func $f (canon lower (func $f)
    ))
  )
  "canonical option `memory` is required")
