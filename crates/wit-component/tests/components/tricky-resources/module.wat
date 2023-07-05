(module
  (import "[export]foo:bar/a" "[resource-drop-own]r" (func (param i32)))
  (import "[export]foo:bar/a" "[resource-rep]r" (func (param i32) (result i32)))

  (func (export "some-name#f") (result i32)
    unreachable)
)
