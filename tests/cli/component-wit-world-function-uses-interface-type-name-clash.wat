;; FAIL: component wit %
;;
;; `f` uses `a:b/i`'s `t`, but the world's `use` for it would clash with the
;; function `t`.

(component
  (import "a:b/i" (instance $i (export "t" (type (sub resource)))))
  (alias export $i "t" (type $t))
  (type $o (own $t))
  (import "t" (func))
  (import "f" (func (param "x" $o)))
)
