(module
  (import "old" "f" (func))

  (import "a:b/c@0.1.0" "[constructor]r" (func (result i32)))
  (import "a:b/c@0.1.0" "[resource-drop]r" (func (param i32)))
  (import "a:b/c@0.1.0" "x" (func (param i32 i32)))

  ;; this module's WIT doesn't directly this import, but it's going to be
  ;; recognized through the adapter so it should in theory work.
  (import "a:b/c@0.1.1" "x" (func (param i32 i32)))

  (memory (export "memory") 1)
)
