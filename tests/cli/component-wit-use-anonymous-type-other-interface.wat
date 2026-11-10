;; FAIL: component wit %
;;
;; `x` is a list of `a:x/y`'s `t`, which `a:x/k` doesn't `use`.

(component
  (import "a:x/y" (instance $y (export "t" (type (sub resource)))))
  (alias export $y "t" (type $t))
  (type $o (own $t))
  (type $l (list $o))
  (import "f" (func (param "x" $l)))
  (import "a:x/k" (instance
    (alias outer 1 $l (type $l2))
    (export "x" (type (eq $l2)))
  ))
)
