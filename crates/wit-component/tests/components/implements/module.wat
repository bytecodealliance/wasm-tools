(module
  (import "a" "f" (func))
  (import "b" "f" (func))
  (import "c" "f" (func))

  (func (export "a#f"))
  (func (export "b#f"))
  (func (export "c#f"))
)
