;; RUN: wast --assert default --snapshot tests/snapshots % -f wasm1

(assert_invalid
  (module (rec))
  "requires `gc` proposal to be enabled")

(assert_invalid
  (module (rec (type (func))))
  "requires `gc` proposal to be enabled")

(assert_invalid
  (module (rec (type (func)) (type (func))))
  "requires `gc` proposal to be enabled")

(assert_invalid
  (module (type $t (func (param (ref $t)))))
  "reference types support is not enabled")

(assert_invalid
  (module (type $t (sub (func))))
  "gc proposal must be enabled")

(assert_invalid
  (module (type $t (func)) (type (sub $t (func))))
  "gc proposal must be enabled")
