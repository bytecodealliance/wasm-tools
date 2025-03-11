;; RUN: wast --assert default --snapshot tests/snapshots % -f mvp

(assert_invalid
  (module
    (type (shared (array i8)))
  )
  "shared composite types require the shared-everything-threads proposal")
