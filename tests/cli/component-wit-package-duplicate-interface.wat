;; FAIL: component wit %

(component
    (type $x (component
        (export "a:b/c@1.0.0" (instance))
    ))
    (type $y (component
        (export "a:b/c@2.0.0" (instance))
    ))
    (export "x" (type $x))
    (export "y" (type $y))
)
