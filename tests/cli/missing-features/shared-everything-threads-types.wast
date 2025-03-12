;; RUN: wast --assert default --snapshot tests/snapshots % -f mvp

(assert_invalid
  (module
    (type (shared (func)))
  )
  "shared composite types require the shared-everything-threads proposal")
