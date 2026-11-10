;; FAIL: component wit %
;;
;; `x` reuses an owned handle to `r`, a resource the interface `a:x/k` neither
;; defines nor `use`s.

(component
  (type $pkg (component
    (type $w (component
      (import "r" (type $r (sub resource)))
      (type $o (own $r))
      (import "f" (func (param "x" $o)))
      (import "a:x/k" (instance
        (alias outer $w $o (type $o2))
        (export "x" (type (eq $o2)))
      ))
    ))
    (export "root:component/root" (component (type $w)))
  ))
  (export "root" (type $pkg))
)
