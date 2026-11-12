;; FAIL: component wit %

(component
    (type $a (instance (type (;0;) u32) (export "t" (type (eq 0)))))
    (type $b (instance (type (;0;) u32) (export "t" (type (eq 0)))))
    (import "x" (implements "a:b/c") (instance (type $a)))
    (import "y" (implements "a:b/d") (instance (type $b)))
    (import "z" (implements "a:b/d") (instance (type $a)))
)
