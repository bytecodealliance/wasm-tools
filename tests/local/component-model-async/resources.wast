(component
  (type $t (resource (rep i32)))
  (core func $f (canon resource.drop $t async))
)
