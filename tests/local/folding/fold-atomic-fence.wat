;;; WABT fold-atomic-fence.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (func $f
    atomic.fence
    atomic.fence))
