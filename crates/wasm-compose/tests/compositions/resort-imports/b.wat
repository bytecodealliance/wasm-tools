(component
  (import "dep0" (instance $dep0
    (type $t' u32)
    (export $t "t" (type (eq $t')))
  ))
  (alias export $dep0 "t" (type $t-dep0))
  (import "dep2" (instance
    (export "f2" (func (result $t-dep0)))
  ))
)
