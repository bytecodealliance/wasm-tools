;; RUN: wast --assert default --snapshot tests/snapshots %

(module
  (func
    unreachable
    i32.const 0
    select
    block (param i32)
      unreachable
    end))

