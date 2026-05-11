(module
  (@dylink.0 (needed "bar"))
  (import "env" "tag1" (tag (param i32 i32)))
  (import "env" "tag2" (tag (param i32)))
  (import "GOT.func" "bar" (global (mut i32)))
  (func (export "run") unreachable)
)
