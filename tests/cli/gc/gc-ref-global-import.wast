;; RUN: wast --assert default --snapshot tests/snapshots %

;; --enable-gc
(module
  (import "" "" (global (ref $a)))
  (type $a (struct))
)
