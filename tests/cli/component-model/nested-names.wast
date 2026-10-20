;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-nested-names

;; These are the extended import name forms that are currently supported
;; via WasmFeatures::component_model_nested_names.

(component
  (component
    (import "a:b:c:d/e" (func))
    (import "a:b-c:d-e:f-g/h-i/j-k/l-m/n/o/p@1.0.0" (func))
  )
)
