;; For example, the following component:

;; a.wasm
(component
  (import "one" (func))

  (import "two" (value string))
  (import "three" (instance
    (export "four" (instance
      (export "five" (module
        (import "six" "a" (func))
        (import "six" "b" (func))
      ))
    ))
  ))
  ;; ...
)
