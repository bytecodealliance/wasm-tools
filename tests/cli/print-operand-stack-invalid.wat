;; RUN: print --print-operand-stack %

(module
  (func (param i32)
    local.get 0
    i64.div_u
    f32.load
  )
)

