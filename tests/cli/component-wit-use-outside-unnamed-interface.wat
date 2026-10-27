;; FAIL: component wit %

(component
  (type $pkg (component
    (type $w (component
      (import "t" (type $t (sub resource)))
      (import "foo" (instance (export "t" (type (eq $t)))))
    ))
    (export "root:component/root" (component (type $w)))
  ))
  (export "root" (type $pkg))
)
