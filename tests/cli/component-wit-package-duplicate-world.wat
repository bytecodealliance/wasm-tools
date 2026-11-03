;; FAIL: component wit %

(component
    (type $w (component))
    (type $p (component
        (export "a:b/w@1.0.0" (component (type $w)))
        (export "a:b/w@2.0.0" (component (type $w)))
    ))
    (export "a:b/wit" (type $p))
)
