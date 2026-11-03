;; FAIL: component wit %

(component
    (import "x" (implements "a:b/c") (instance
        (type $t u32)
        (export "t" (type (eq $t)))
    ))
    (import "y" (implements "a:b/c") (instance
        (type $i (instance))
        (export "t" (type (eq $i)))
    ))
)
