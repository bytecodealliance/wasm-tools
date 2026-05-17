;; RUN: addr2line --generate-dwarf lines % 24 26 30 32

(module
  (func $"dwarf(name)"
(;@18;)  i32.const 0
(;@1a;)  drop
  )

  (func $another-function
(;@1e;)  i32.const 0
(;@20;)  drop
  )
)
