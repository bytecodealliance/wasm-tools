;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-block-labels.wat test  (Copyright 2016- WebAssembly Community Group participants)
(module
  (func
    block
      br 0
    end
    block
      br 0
    end)

  (func
    block
      br 0
      block
        br 0
      end
    end
    block
      br 0
    end)

  (func
    (i32.const 0)
    if
      br 0
    end
    (i32.const 0)
    if
      br 0
    end)

  (func
    loop
      br 0
    end
    loop
      br 0
    end)

;; wasm-tools does not parse legacy-exceptions in the text format
;;  (func
;;    try
;;      br 0
;;    catch_all
;;    end
;;    try
;;      br 0
;;    catch_all
;;    end)
  )
