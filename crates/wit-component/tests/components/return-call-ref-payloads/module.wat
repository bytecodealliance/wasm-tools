;;! return-call-ref = true

(module
  (func (import "$root" "[async-lower]foo") (param i32) (result i32))
  (func (import "$root" "[stream-new-0]foo") (result i64))
  (func (import "$root" "[stream-read-0]foo") (param i32 i32 i32) (result i32))
  (func (import "$root" "[stream-write-0]foo") (param i32 i32 i32) (result i32))
  (func (import "$root" "[stream-drop-readable-0]foo") (param i32))
  (func (import "$root" "[stream-drop-writable-0]foo") (param i32))
  (memory (export "memory") 1)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
)
