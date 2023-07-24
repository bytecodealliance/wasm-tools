(component
  (import "dep1" (instance $dep1
    (type $t' u32)
    (export $t "t" (type (eq $t')))
  ))
  (alias export $dep1 "t" (type $t-dep1))
  (import "dep2" (instance
    (export "f1" (func (result $t-dep1)))
  ))
)
