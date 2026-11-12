;; FAIL: component wit %

(component
    (import "i" (instance
        (type (list u8))
        (export "a:b/c@1.0.0" (type (eq 0)))
    ))
)
