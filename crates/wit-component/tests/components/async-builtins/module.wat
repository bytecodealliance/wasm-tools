(module
  (import "$root" "[backpressure-set]" (func (param i32)))
  (import "[export]$root" "[task-cancel]" (func))
  (import "[export]$root" "[task-return]foo" (func (param i32 i32)))
  (import "[export]foo:foo/bar" "[task-return]foo" (func (param i32 i32)))
  (import "$root" "[waitable-set-new]" (func (result i32)))
  (import "$root" "[waitable-set-wait]" (func (param i32 i32) (result i32)))
  (import "$root" "[waitable-set-poll]" (func (param i32 i32) (result i32)))
  (import "$root" "[waitable-set-drop]" (func (param i32)))
  (import "$root" "[waitable-join]" (func (param i32 i32)))
  (import "$root" "[yield]" (func (result i32)))
  (import "$root" "[subtask-drop]" (func (param i32)))
  (import "$root" "[subtask-cancel]" (func (param i32) (result i32)))
  (import "$root" "[error-context-new-utf8]" (func (param i32 i32) (result i32)))
  (import "$root" "[error-context-new-utf16]" (func (param i32 i32) (result i32)))
  (import "$root" "[error-context-new-latin1+utf16]" (func (param i32 i32) (result i32)))
  (import "$root" "[error-context-debug-message-utf8]" (func (param i32 i32)))
  (import "$root" "[error-context-debug-message-utf16]" (func (param i32 i32)))
  (import "$root" "[error-context-debug-message-latin1+utf16]" (func (param i32 i32)))
  (import "$root" "[error-context-drop]" (func (param i32)))
  (import "$root" "[context-get-0]" (func (result i32)))
  (import "$root" "[context-set-0]" (func (param i32)))
  (func (export "[async-lift-stackful]foo") (param i32 i32) unreachable)
  (func (export "[async-lift-stackful]foo:foo/bar#foo") (param i32 i32) unreachable)
  (memory (export "memory") 1)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
)
