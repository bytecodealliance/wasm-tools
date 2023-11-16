(component
  (import "host-a" (instance $i
    (export "a" (type (sub resource)))
  ))
  (alias export $i "a" (type $a))
  (import "host-b" (instance
    (export "b" (type (eq $a)))
  ))

  (import "b" (instance))
)
