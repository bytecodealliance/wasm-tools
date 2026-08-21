;;! merge-imports-based-on-canonical-version = true

(module
  (import "a:b/c@0.1.1" "x" (func (param i32 i32)))
  (import "a:b/c@0.1.1" "y" (func))

  (memory (export "memory") 1)
)
