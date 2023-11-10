;; Test that we can properly push valtypes containing rec-group-local indices
;; onto the operand stack:
;;
;; * Once canonicalized, `$f`'s parameter reference to `$g` will be a
;;   rec-group-local index: `(recgroup 1)`
;;
;; * The `call_ref` function will push the parameter types to the validator's
;;   operand stack.
;;
;; * But we have the invariant that all type indices on the validator's operand
;;   stack are `CoreTypeId`s.
;;
;; * So, internally, we re-canonicalize these indices into `CoreTypeId`s. This
;;   is done by implementations of the `WasmModuleResources` trait.

(module
  (rec (type $f (func (param (ref $g))))
       (type $g (func)))

  (func (export "run") (param (ref $f) (ref $g))
    local.get 1
    local.get 0
    call_ref $f)
)
