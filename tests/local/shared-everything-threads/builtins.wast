;; Styled after ../component-model/resources.wast

(component
  (core type $start (shared (func (param $context i32))))
  (core func $spawn_ref (canon thread.spawn_ref $start))

  (core module $libc (table (export "start-table") 1 (ref null (shared func))))
  (core instance $libc (instantiate $libc))
  (core func $spawn_indirect (canon thread.spawn_indirect (table $libc "start-table")))

  (core func $concurrency (canon thread.hw_concurrency))
)

(component
  (core type $start (shared (func (param $context i32))))
  (core func $spawn_ref (canon thread.spawn_ref $start))

  (core module $libc (table (export "start-table") 1 (ref null (shared func))))
  (core instance $libc (instantiate $libc))
  (core func $spawn_indirect (canon thread.spawn_indirect (table $libc "start-table")))

  (core func $concurrency (canon thread.hw_concurrency))

  (core module $m
    (type $st (shared (func (param $context i32))))
    (import "" "spawn_ref" (func (param (ref null $st)) (param i32) (result i32)))
    (import "" "spawn_indirect" (func (param i32) (param i32) (result i32)))
    (import "" "concurrency" (func (result i32)))
  )

  (core instance (instantiate $m
    (with "" (instance
      (export "spawn_ref" (func $spawn_ref))
      (export "spawn_indirect" (func $spawn_indirect))
      (export "concurrency" (func $concurrency))
    ))
  ))
)

(assert_invalid
  (component
    (core type $start (func))
    (core func $spawn (canon thread.spawn_ref $start))
  )
  "spawn type must be shared"
)

(assert_invalid
  (component
    (core type $start (shared (func)))
    (core func $spawn_ref (canon thread.spawn_ref $start))
  )
  "spawn function must take a single `i32` argument (currently)"
)
