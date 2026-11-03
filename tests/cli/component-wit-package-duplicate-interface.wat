;; FAIL: component wit %

(component
    (type $p (component
        (export "a:b/c@1.0.0" (instance))
        (export "a:b/c@2.0.0" (instance))
    ))
    (export "a:b/wit" (type $p))
)
