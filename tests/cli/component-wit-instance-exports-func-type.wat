;; FAIL: component wit %

(component
    (import "i" (instance
        (type $f (func))
        (export "t" (type (eq $f)))
    ))
)
