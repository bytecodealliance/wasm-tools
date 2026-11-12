;; FAIL: component wit %

(component
    (type $p (component
        (export "foo" (instance))
    ))
    (export "a:b/wit" (type $p))
)
