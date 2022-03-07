;; With what's defined so far, we can define component types using a mix of inline and out-of-line type definitions:

(component $C
  (type $T (list (tuple string bool)))
  (type $U (option $T))
  (type $G (func (param (list $T)) (result $U)))
  (type $D (component
    ;; fixme (alias outer $C $T (type $C_T))
    (type $L (list $C_T))
    (import "f" (func (param $L) (result (list u8))))
    (import "g" $G)
    (export "g" $G)
    (export "h" (func (result $U)))
  ))
)
