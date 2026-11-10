;; FAIL: component wit %
;;
;; One import of `a:b/i` has `f` as a method of `r`, the other as a static
;; function of `r`.

(component
    (import "a:b/i" (instance $i
        (export "r" (type $r (sub resource)))
        (export "[method]r.f" (func (param "self" (borrow $r))))
    ))
    (alias export $i "r" (type $r))
    (import "x" (implements "a:b/i") (instance
        (alias outer 1 $r (type $r2))
        (export "r" (type (eq $r2)))
        (export "[static]r.f" (func))
    ))
)
