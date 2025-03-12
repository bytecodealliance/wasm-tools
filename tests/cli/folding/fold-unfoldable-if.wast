;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

(module
  (func
    i32.const 0
    try_table (param i32)
      if
      end
    end
  )
)
