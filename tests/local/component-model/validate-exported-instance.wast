;; Minimal test of using an exported resource.
(component
  (type (export "x") (instance
    (export $x "x" (type (sub resource)))
    (export "f" (func (param "x" (borrow $x))))
  ))
)

;; More complicated case from the component types explainer
(component
  (type $types (component
    (type $types' (instance
      (export $file "file" (type (sub resource)))
      (export "[method]file.read" (func
        (param "self" (borrow $file)) (param "off" u32) (param "n" u32)
        (result (list u8))
      ))
      (export "[method]file.write" (func
        (param "self" (borrow $file))
        (param "bytes" (list u8))
      ))
    ))
    (export (interface "local:demo/types") (type (eq $types')))
  ))
  (export "types" (type $types))
  (type $namespace (component
    (import (interface "local:demo/types") (instance $types
      (export "file" (type (sub resource)))
    ))
    (alias export $types "file" (type $file))
    (type $interface' (instance
      (export "open" (func (param "name" string) (result (own $file))))
    ))
    (export (interface "local:demo/namespace") (type (eq $interface')))
  ))
  (export "namespace" (type $namespace))
)
