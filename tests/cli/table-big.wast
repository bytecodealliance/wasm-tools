;; RUN: wast --assert default --snapshot tests/snapshots %

(module definition
  (table 1_000_000_000 funcref)
)
