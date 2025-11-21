;; RUN: wast --assert default --snapshot tests/snapshots % -f=-shared-everything-threads

(assert_invalid
  (component
    ;; Refer to a non-existent function type (i.e., 0); validation for
    ;; `thread.spawn-ref` happens first.
    (core func $spawn (canon thread.spawn-ref 0))
  )
  "`thread.spawn-ref` requires the shared-everything-threads proposal")

(assert_invalid
  (component
    ;; Refer to a non-existent function and table types (i.e., 0); validation
    ;; for `thread.spawn-indirect` happens first.
    (core func $spawn-indirect (canon thread.spawn-indirect 0 (table 0)))
  )
  "`thread.spawn-indirect` requires the shared-everything-threads proposal")

(assert_invalid
  (component
    (core func $parallelism (canon thread.available_parallelism))
  )
  "`thread.available_parallelism` requires the shared-everything-threads proposal")
