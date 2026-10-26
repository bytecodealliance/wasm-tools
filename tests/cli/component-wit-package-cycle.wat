;; FAIL: component wit %

(component
  (type $pkg (component
    (type $w (component
      (import "a:x/k" (instance $k (export "s" (type (sub resource)))))
      (alias export $k "s" (type $ks))
      (import "b:y/l" (instance $l (export "s" (type (eq $ks))) (export "t" (type (sub resource)))))
      (alias export $l "t" (type $lt))
      (export "a:x/k" (instance (export "t" (type (eq $lt)))))
    ))
    (export "root:component/root" (component (type $w)))
  ))
  (export "root" (type $pkg))
)
