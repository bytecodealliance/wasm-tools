;; RUN: wast --assert default --snapshot tests/snapshots %

(module
  (func (result i32 i32)
    unreachable
    select
    if (result i32 i32)
      unreachable
    else
      unreachable
    end)
)
