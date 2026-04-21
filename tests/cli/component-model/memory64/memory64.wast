;; RUN: wast --assert default --snapshot tests/snapshots -f cm64 %

  (component
    (import "x" (func $x (param "x" string)))
    (core module $A
      (memory (export "m") i64 1))
    (core instance $A (instantiate $A))
    (alias core export $A "m" (core memory $m))
    (core func (canon lower (func $x) (memory $m)))
  )