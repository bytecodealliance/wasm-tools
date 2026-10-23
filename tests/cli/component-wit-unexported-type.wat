;; FAIL: component wit %

(component
  (type $pkg (component
    (type $w (component
      (import "a:x/k" (instance (export "f" (func))))
      (import "a:x/l" (instance $l (export "t" (type (sub resource)))))
      (alias export $l "t" (type $lt))
      (export "a:x/k" (instance (export "g" (func (param "x" (own $lt))))))
    ))
    (export "root:component/root" (component (type $w)))
  ))
  (export "root" (type $pkg))
)
