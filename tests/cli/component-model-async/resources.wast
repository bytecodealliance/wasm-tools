;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm-async-builtins

(component
  (type $t (resource (rep i32)))
  (core func $f (canon resource.drop $t async))
)
