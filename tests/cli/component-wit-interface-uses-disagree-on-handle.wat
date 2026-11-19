;; FAIL: component wit %
;;
;; One import of `a:b/i` has `t` as `own<a>`, the other as `own<b>`.

(component
    (import "a:b/i" (instance
        (export "a" (type $a (sub resource)))
        (export "b" (type (sub resource)))
        (type $o (own $a))
        (export "t" (type (eq $o)))
    ))
    (import "x" (implements "a:b/i") (instance
        (export "a" (type (sub resource)))
        (export "b" (type $b (sub resource)))
        (type $o (own $b))
        (export "t" (type (eq $o)))
    ))
)
