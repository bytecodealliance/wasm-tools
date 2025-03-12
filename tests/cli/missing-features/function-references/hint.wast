;; RUN: wast --assert default --snapshot tests/snapshots % -f wasm1,function-references

(assert_invalid
  (module
    (type $ty (func (param (ref null $ty))))
  )
  "unknown type 0: type index out of bounds because the GC proposal is disabled")
