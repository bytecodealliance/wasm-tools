;; Two `(implements ...)` imports of one interface share a single instance
;; type. Validation accepts this shape. Decoding must keep the repeated export
;; alias and print both imports, rather than panicking.
;;
;; RUN[validate]: validate --features all %
;; RUN[wit]: component wit %

(component
  (type (instance
    (type (variant (case "other" string)))
    (export "error" (type (eq 0)))
    (type (func (result 1)))
    (export "open" (func (type 2)))
  ))
  (import "primary" (implements "a:b/store@0.1.0") (instance (type 0)))
  (import "backup" (implements "a:b/store@0.1.0") (instance (type 0)))
)
