(component
  (import "c" (instance
    (type $dummy u32)
    (type $t' u32)
    (export $t "t" (type (eq $t')))
    (export "f1" (func (result $t)))
  ))
)
