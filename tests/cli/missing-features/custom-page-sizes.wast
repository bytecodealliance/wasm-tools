;; RUN: wast --assert default --snapshot tests/snapshots % -f mvp

(assert_malformed
  (module
    (memory 0 (pagesize 1))
  )
  "the custom page sizes proposal must be enabled to customize a memory's page size")
