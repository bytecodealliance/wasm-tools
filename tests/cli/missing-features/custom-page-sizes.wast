;; RUN: wast --assert default --snapshot tests/snapshots % -f mvp

(assert_invalid
  (module
    (memory 0 (pagesize 1))
  )
  "the custom page sizes proposal must be enabled to customize a memory's page size")

(assert_invalid
  (module (memory 0 (pagesize 65536)))
  "the custom page sizes proposal must be enabled to customize a memory's page size")
