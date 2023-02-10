;; RUN: dump %

;; instances
(component
  (type $outer (instance
    (type $local_type (func))
    (export "c5" (component
      (type $nested_func_type (func))
      (alias outer $outer $local_type (type $my_type))
      (import "1" (func (type $nested_func_type)))
      (export "1" (func (type $my_type)))
    ))
  ))
)

