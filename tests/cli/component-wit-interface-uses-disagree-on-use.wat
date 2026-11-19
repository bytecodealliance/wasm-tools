;; FAIL: component wit %
;;
;; One import of `a:b/bar` has its `t` from `a:b/foo@1.0.0`, the other from
;; `a:b/foo@2.0.0`.

(component
    (import "a:b/foo@1.0.0" (instance $f1 (export "t" (type (sub resource)))))
    (import "a:b/foo@2.0.0" (instance $f2 (export "t" (type (sub resource)))))
    (alias export $f1 "t" (type $t1))
    (alias export $f2 "t" (type $t2))
    (import "a:b/bar" (instance
        (alias outer 1 $t1 (type))
        (export "t" (type (eq 0)))
    ))
    (import "x" (implements "a:b/bar") (instance
        (alias outer 1 $t2 (type))
        (export "t" (type (eq 0)))
    ))
)
