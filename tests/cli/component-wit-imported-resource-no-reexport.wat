;; RUN: component wit %

(component
    (type (;0;) (instance (export "request" (type (sub resource)))))
    (import "impl:test/handler" (instance (;0;) (type 0)))
)
