;; RUN: component wit %
;;
;; A type reusing a list that has no named types in it is just that list.

(component
  (type $pkg (component
    (type $w (component
      (type $l (list u32))
      (import "f" (func (param "x" $l)))
      (import "a:x/k" (instance (export "x" (type (eq $l)))))
    ))
    (export "root:component/root" (component (type $w)))
  ))
  (export "root" (type $pkg))
)
