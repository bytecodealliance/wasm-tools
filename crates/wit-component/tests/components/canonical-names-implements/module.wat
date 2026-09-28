;;! emit-canonical-names = true

(module
  (import "foo" "x" (func))

  (func (export "bar#x") unreachable)
)
