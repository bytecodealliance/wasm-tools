;; FAIL: component wit %

(component
    (type $c (component))
    (import "t" (type (eq $c)))
)
