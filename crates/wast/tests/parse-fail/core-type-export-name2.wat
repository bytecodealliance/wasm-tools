;; Same as core-type-export-name.wat but through a canon built-in's core type
;; index rather than a canonical ABI option.
(component
  (core module $m)
  (core instance $i (instantiate $m))
  (core func (canon thread.spawn-ref (core type $i "t")))
)
