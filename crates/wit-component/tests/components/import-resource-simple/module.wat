(module
  (import "$root" "[constructor]a" (func (result i32)))
  (import "$root" "[static]a.other-new" (func (result i32)))
  (import "$root" "[resource-drop-own]a" (func (param i32)))
  (import "$root" "[resource-drop-borrow]a" (func (param i32)))
)
