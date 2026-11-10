;; FAIL: component wit %

(component
    (type $w (component))
    (type $x (component
        (export "a:b/w@1.0.0" (component (type $w)))
    ))
    (type $y (component
        (export "a:b/w@2.0.0" (component (type $w)))
    ))
    (export "x" (type $x))
    (export "y" (type $y))
)
