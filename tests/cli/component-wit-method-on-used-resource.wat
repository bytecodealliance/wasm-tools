;; FAIL: component wit %

(component
    (import "a:b/d" (instance $d
        (export "r" (type (sub resource)))
    ))
    (alias export $d "r" (type $r))
    (import "a:b/e" (instance
        (export "r" (type $x (eq $r)))
        (export "[method]r.f" (func (param "self" (borrow $x))))
    ))
)
