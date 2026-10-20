(module
  (func (import "$root" "[future-forward-0]foo") (param i32 i32))
  (func (import "$root" "[stream-forward-1]foo") (param i32 i32))
  (func (import "[export]$root" "[future-forward-0]foo") (param i32 i32))
  (func (import "[export]$root" "[stream-forward-1]foo") (param i32 i32))
  (func (export "[async-lift-stackful]foo") (param i32) unreachable)
)
