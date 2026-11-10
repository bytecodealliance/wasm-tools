;; FAIL: component wit %

(component
    (type $p (component
        (export "x:y/c" (instance))
    ))
    (export "a:b/wit" (type $p))
)
