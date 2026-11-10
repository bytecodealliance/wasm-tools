;; FAIL: component wit %
;;
;; World `a:b/c` imports interface `a:b/c`, so package `a:b` would have an
;; interface and a world both named `c`.

(component
    (type (component
        (type (component
            (import "a:b/c" (instance))
        ))
        (export "a:b/c" (component (type 0)))
    ))
    (export "c" (type 0))
)
