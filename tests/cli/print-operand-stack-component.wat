;; RUN: print --print-operand-stack %

(component
  (component (core module (func i32.const 0 drop)))
  (component (core module (func i32.const 0 drop)))
)
