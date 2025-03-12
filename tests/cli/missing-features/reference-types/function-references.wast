;; RUN: wast --assert default --snapshot tests/snapshots % -f wasm1,reference-types

(assert_invalid
  (module (type (func (param (ref func)))))
  "function references required")
(assert_invalid
  (module (type (func (param (ref extern)))))
  "function references required")
(assert_invalid
  (module
    (type $f (func))
    (type (func (param (ref $f))))
  )
  "function references required")
