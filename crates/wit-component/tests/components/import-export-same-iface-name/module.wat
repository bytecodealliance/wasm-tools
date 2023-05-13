(module
  (import "dep/the-name" "a" (func))
  (import "foo/the-name" "a" (func))
  (func (export "foo/the-name#a"))
)
