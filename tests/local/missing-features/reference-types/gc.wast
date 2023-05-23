(assert_invalid
  (module (type (func (param i31ref))))
  "gc proposal is not enabled")
(assert_invalid
  (module (type (func (param anyref))))
  "gc proposal is not enabled")
(assert_invalid
  (module (type (func (param (ref none)))))
  "gc proposal is not enabled")
(assert_invalid
  (module (type (func (param (ref array)))))
  "gc proposal is not enabled")
(assert_invalid
  (module (type (func (param (ref struct)))))
  "gc proposal is not enabled")
(assert_invalid
  (module (type (func (param (ref eq)))))
  "gc proposal is not enabled")
(assert_invalid
  (module (type (func (param (ref noextern)))))
  "gc proposal is not enabled")
(assert_invalid
  (module (type (func (param (ref nofunc)))))
  "gc proposal is not enabled")
(assert_invalid
  (module (func (local i31ref)))
  "gc proposal is not enabled")
(assert_invalid
  (module (func (block (result i31ref) unreachable)))
  "gc proposal is not enabled")
