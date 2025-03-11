;; RUN: wast --assert default --snapshot tests/snapshots % -f component-model-async

(component
  (type $t (resource (rep i32)))
  (core func $f (canon resource.drop $t async))
)
