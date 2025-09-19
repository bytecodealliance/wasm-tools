;; RUN[stack]: print -p %
;; RUN[fold]: print -p % -f
;; RUN[stack-valid]: print -p % | validate
;; RUN[fold-valid]: print -p % -f | validate

(module
  (import "" "" (func))
  (memory 1)
  (table 1 funcref)
  (global i32 i32.const 0)


  (func)
  (func i32.const 0 drop)

  (data (i32.const 0) "100")
  (elem (i32.const 0) func 0)
)
