(module
  (@dylink.0 (needed "bar"))
  (import "env" "bar" (func $bar))
  (import "env" "tag1" (tag))
  (import "env" "tag2" (tag))
  (func (export "run") unreachable)
)
