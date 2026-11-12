;; FAIL: component wit %

(component
    (type $c (component
        (export "a:b/c" (func))
    ))
    (export "x" (type $c))
)
