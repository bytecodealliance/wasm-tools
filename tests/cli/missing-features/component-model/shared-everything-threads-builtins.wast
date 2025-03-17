;; RUN: wast --assert default --snapshot tests/snapshots % -f=-shared-everything-threads

(assert_invalid
  (component
    ;; Refer to a non-existent function type (i.e., 0); validation for
    ;; `thread.spawn_ref` happens first.
    (core func $spawn (canon thread.spawn_ref 0))
  )
  "`thread.spawn_ref` requires the shared-everything-threads proposal")

(assert_invalid
  (component
    ;; Refer to a non-existent function and table types (i.e., 0); validation
    ;; for `thread.spawn_indirect` happens first.
    (core func $spawn_indirect (canon thread.spawn_indirect 0 (table 0)))
  )
  "`thread.spawn_indirect` requires the shared-everything-threads proposal")

(assert_invalid
  (component
    (core func $parallelism (canon thread.available_parallelism))
  )
  "`thread.available_parallelism` requires the shared-everything-threads proposal")
