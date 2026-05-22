;; FAIL: component wit %

(component
    (type (;0;) (instance (export "request" (type (sub resource)))))
    (import "impl:test/handler" (instance (;0;) (type 0)))
    (export "wasi:http/handler@0.3.0" (instance 0))
)
