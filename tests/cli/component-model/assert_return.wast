;; RUN: wast --assert default --snapshot tests/snapshots %

(component
  (core module $m (func (export "") (result i32) i32.const 1))
  (core instance $m (instantiate $m))
  (func (export "a") (result u32) (canon lift (core func $m "")))
)

(assert_return (invoke "a") (u32.const 1))