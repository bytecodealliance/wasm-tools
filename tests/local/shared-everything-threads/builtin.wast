;; Styled after ../component-model/resources.wast

(component
  (type $start (func (param "context" u32)))
  (core func $spawn (canon thread.spawn $start))
  (core func $concurrency (canon thread.hw_concurrency))
)

(component
  (type $start (func (param "context" u32)))
  (core func $spawn (canon thread.spawn $start))
  (core func $concurrency (canon thread.hw_concurrency))

  (core module $m
    ;; (type $st (func (param $context i32)))
    (import "" "spawn" (func (param (ref null func)) (param i32) (result i32)))
    (import "" "concurrency" (func (result i32)))
  )

  (core instance (instantiate $m
    (with "" (instance
      (export "spawn" (func $spawn))
      (export "concurrency" (func $concurrency))
    ))
  ))
)

(assert_invalid
  (component
    (type $start (func))
    (core func $spawn (canon thread.spawn $start))
  )
  "spawn function must take a single `u32` argument (currently)"
)
