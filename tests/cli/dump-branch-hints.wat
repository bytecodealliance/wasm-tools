;; RUN: dump %

(module
  (func
    i32.const 0
    (@metadata.code.branch_hint "\00")
    if
    end

    (@metadata.code.branch_hint "\01")
    if
    end
  )
)
