;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-unreachable.txt test (Copyright 2016- WebAssembly Community Group participants)
(module
  (func (result i32)
    i32.const 1
    unreachable
    i32.add)

  (func
    br 0
    drop)

  (func
    block
      block
        block
          i32.const 1
          br_table 0 1 2 0
          i32.const 2
          i32.div_s
          return
        end
      end
    end)

  (func (result i32)
    i32.const 0
    return
    i32.eqz))
