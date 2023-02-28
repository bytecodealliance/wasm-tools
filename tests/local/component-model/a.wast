;; For example, the following component:

;; a.wasm
(component
  (import "one" (func))

  (import "two" (value $v string))
  (import "three" (instance
    (export "four" (instance
      (export "five" (core module
        (import "six" "a" (func))
        (import "six" "b" (func))
      ))
    ))
  ))

  (export "four" (value $v))
  ;; ...
)
