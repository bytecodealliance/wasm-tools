;; Styled after ../component-model/resources.wast

(component
  (core type $start (shared (func (param $context i32))))
  (core func $spawn (canon thread.spawn $start))
  (core func $parallelism (canon thread.available_parallelism))
)

(component
  (core type $start (shared (func (param $context i32))))
  (core func $spawn (canon thread.spawn $start))
  (core func $parallelism (canon thread.available_parallelism))

  (core module $m
    (type $st (shared (func (param $context i32))))
    (import "" "spawn" (func (param (ref null $st)) (param i32) (result i32)))
    (import "" "parallelism" (func (result i32)))
  )

  (core instance (instantiate $m
    (with "" (instance
      (export "spawn" (func $spawn))
      (export "parallelism" (func $parallelism))
    ))
  ))
)

(assert_invalid
  (component
    (core type $start (func))
    (core func $spawn (canon thread.spawn $start))
  )
  "spawn type must be shared"
)

(assert_invalid
  (component
    (core type $start (shared (func)))
    (core func $spawn (canon thread.spawn $start))
  )
  "spawn function must take a single `i32` argument (currently)"
)
