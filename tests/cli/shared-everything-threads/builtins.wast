;; RUN: wast --assert default --snapshot tests/snapshots % -f shared-everything-threads

;; Styled after ../component-model/resources.wast

(component
  (core type $start (shared (func (param $context i32))))
  (core func $spawn_ref (canon thread.spawn_ref $start))

  (core module $libc (table (export "start-table") shared 1 (ref null (shared func))))
  (core instance $libc (instantiate $libc))
  (core func $spawn_indirect (canon thread.spawn_indirect $start (table $libc "start-table")))

  (core func $parallelism (canon thread.available_parallelism))
)

(component
  (core type $start (shared (func (param $context i32))))
  (core func $spawn_ref (canon thread.spawn_ref $start))

  (core module $libc (table (export "start-table") shared 1 (ref null (shared func))))
  (core instance $libc (instantiate $libc))
  (core func $spawn_indirect (canon thread.spawn_indirect $start (table $libc "start-table")))

  (core func $parallelism (canon thread.available_parallelism))

  (core module $m
    (type $spawned_func_ty (shared (func (param $context i32))))
    (type $spawn_ref_ty (shared (func (param (ref null $spawned_func_ty)) (param i32) (result i32))))
    (type $spawn_indirect_ty (shared (func (param i32) (param i32) (result i32))))
    (type $parallelism_ty (shared (func (result i32))))
    (import "" "spawn_ref" (func (type $spawn_ref_ty)))
    (import "" "spawn_indirect" (func (type $spawn_indirect_ty)))
    (import "" "parallelism" (func (type $parallelism_ty)))
  )

  (core instance (instantiate $m
    (with "" (instance
      (export "spawn_ref" (func $spawn_ref))
      (export "spawn_indirect" (func $spawn_indirect))
      (export "parallelism" (func $parallelism))
    ))
  ))
)

(assert_invalid
  (component
    (core type $start (func))
    (core func $spawn_ref (canon thread.spawn_ref $start))
  )
  "spawn type must be shared"
)

(assert_invalid
  (component
    (core type $start (shared (func (param i32))))
    ;; Refer to a non-existent table type (i.e., 0); validation
    ;; for `thread.spawn_indirect` happens first.
    (core func $spawn_indirect (canon thread.spawn_indirect $start (table 0)))
  )
  "unknown table 0: table index out of bounds"
)

(assert_invalid
  (component
    (core type $start (shared (func)))
    (core func $spawn_ref (canon thread.spawn_ref $start))
  )
  "spawn function must take a single `i32` argument (currently)"
)

(assert_invalid
  (component
    (core type $start (shared (func)))
    (core module $libc (table (export "start-table") shared 1 (ref null (shared func))))
    (core instance $libc (instantiate $libc))
    (core func $spawn_indirect (canon thread.spawn_indirect $start (table $libc "start-table")))
  )
  "spawn function must take a single `i32` argument (currently)"
)

(assert_invalid
  (component
    (core type $start (shared (func (param i32))))
    (core module $libc (table (export "start-table") 1 (ref null (shared func))))
    (core instance $libc (instantiate $libc))
    (core func $spawn_indirect (canon thread.spawn_indirect $start (table $libc "start-table")))
  )
  "mismatch in the shared flag for tables"
)
