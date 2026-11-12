;; FAIL: component wit %

(component
    (import "x" (implements "a:b/c") (instance
        (type $l (list u8))
        (type $t (list $l))
        (export "t" (type (eq $t)))
    ))
    (import "y" (implements "a:b/c") (instance
        (type $t (list u8))
        (export "t" (type (eq $t)))
    ))
)
