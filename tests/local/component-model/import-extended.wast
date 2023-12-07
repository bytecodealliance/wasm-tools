;; These are the extended import name forms that are currently supported
;; via WasmFeatures::component_model_nested_names.

(component
  (import "a:b:c:d/e" (func))
  (import "a:b-c:d-e:f-g/h-i/j-k/l-m/n/o/p@1.0.0" (func))
)

(component
  (import "unlocked-dep=<a:b:c:d/e/f/g>" (func))
  (import "unlocked-dep=<a:b:c:d/e/f/g@*>" (func))
  (import "unlocked-dep=<a:b:c:d/e/f/g@{>=1.2.3}>" (func))
  (import "unlocked-dep=<a:b:c:d/e/f/g@{>=1.2.3-rc}>" (func))
  (import "unlocked-dep=<a:b:c:d/e/f/g@{<1.2.3}>" (func))
  (import "unlocked-dep=<a:b:c:d/e/f/g@{<1.2.3-rc}>" (func))
  (import "unlocked-dep=<a:b:c:d/e/f/g@{>=1.2.3 <1.2.3}>" (func))
  (import "unlocked-dep=<a:b:c:d/e/f/g@{>=1.2.3-rc <1.2.3}>" (func))
)

(component
  (import "locked-dep=<a:b:c:d/e/f/g>" (func))
  (import "locked-dep=<a:b:c:d/e/f/g@1.2.3>" (func))
  (import "locked-dep=<a:b:c:d/e/f/g>,integrity=<sha256-a>" (func))
  (import "locked-dep=<a:b:c:d/e/f/g@1.2.3>,integrity=<sha256-a>" (func))
)
