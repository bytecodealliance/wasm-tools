;; Referencing a core instance export whose sort has no core export kind
;; (module instances cannot be exported from core instances) must be an
;; error, not a panic. Note that this syntax predates
;; WebAssembly/component-model#655 and panicked in wasm-tools 1.253.0 and
;; prior.
(component
  (core module $m)
  (core instance $i (instantiate $m))
  (core instance (instantiate $m (with "" (instance $i "x"))))
)
