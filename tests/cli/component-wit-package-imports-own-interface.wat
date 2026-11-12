;; FAIL: component wit %

(component
    (type $p (component
        (import "a:b/c" (instance))
        (export "a:b/c" (instance))
    ))
    (export "a:b/wit" (type $p))
)
