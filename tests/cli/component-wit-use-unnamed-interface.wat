;; FAIL: component wit %

(component
  (type $pkg (component
    (type $w (component
      (import "foo" (instance $f (export "t" (type (sub resource)))))
      (alias export $f "t" (type $t))
      (import "a:x/k" (instance (export "t" (type (eq $t)))))
    ))
    (export "root:component/root" (component (type $w)))
  ))
  (export "root" (type $pkg))
)
