;; RUN: component wit %

(component
    (type (;0;) (instance
        (type (;0;) (variant (case "other" string)))
        (export "error" (type (eq 0)))
        (type (;2;) (func (result 1)))
        (export "open" (func (type 2)))
    ))
    (import "primary" (implements "a:b/store@0.1.0") (instance (type 0)))
    (import "backup" (implements "a:b/store@0.1.0") (instance (type 0)))
)
