(assert_invalid
  (module (type (func (param i31ref))))
  "heap types not supported without the gc feature")
(assert_invalid
  (module (type (func (param anyref))))
  "heap types not supported without the gc feature")
(assert_invalid
  (module (type (func (param (ref none)))))
  "heap types not supported without the gc feature")
(assert_invalid
  (module (type (func (param (ref array)))))
  "heap types not supported without the gc feature")
(assert_invalid
  (module (type (func (param (ref struct)))))
  "heap types not supported without the gc feature")
(assert_invalid
  (module (type (func (param (ref eq)))))
  "heap types not supported without the gc feature")
(assert_invalid
  (module (type (func (param (ref noextern)))))
  "heap types not supported without the gc feature")
(assert_invalid
  (module (type (func (param (ref nofunc)))))
  "heap types not supported without the gc feature")
(assert_invalid
  (module (func (local i31ref)))
  "heap types not supported without the gc feature")
(assert_invalid
  (module (func (block (result i31ref) unreachable)))
  "heap types not supported without the gc feature")
