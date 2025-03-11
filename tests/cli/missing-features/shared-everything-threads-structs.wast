;; RUN: wast --assert default --snapshot tests/snapshots % -f mvp

(assert_invalid
  (module
    (type (shared (struct)))
  )
  "shared composite types require the shared-everything-threads proposal")
