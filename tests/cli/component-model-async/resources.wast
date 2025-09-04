;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm-threading

(component
  (type $t (resource (rep i32)))
  (core func $f (canon resource.drop $t async))
)
