;; RUN: dump %

(module
  (@dylink.0
    (needed "a" "b")
    (mem-info (memory 1 1) (table 1 1))
    (export-info "a" 0)
    (import-info "a" "a" 0)
    (export-info "a" 2 binding-local binding-weak 0 undefined)
  )
)
