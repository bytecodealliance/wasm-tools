;; RUN: wast --assert default --snapshot tests/snapshots %

(func (result i32) unreachable select (result i32) (result))
(func (result i32) unreachable select (result i32) (result) select)
