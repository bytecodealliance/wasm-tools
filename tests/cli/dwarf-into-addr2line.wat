;; RUN: addr2line --generate-dwarf lines % 0x18 0x1a 0x1e 0x20

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
