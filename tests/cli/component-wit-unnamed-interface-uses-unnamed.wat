;; FAIL: component wit %
;;
;; `b` uses `r` from `a`, but WIT can't `use` from an unnamed interface.

(component
    (import "a" (instance $a (export "r" (type (sub resource)))))
    (alias export $a "r" (type $r))
    (import "b" (instance
        (alias outer 1 $r (type))
        (export "r" (type (eq 0)))
    ))
)
