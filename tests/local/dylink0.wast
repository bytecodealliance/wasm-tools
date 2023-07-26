(module
  (@dylink.0)
)

(module
  (@dylink.0)
  (@dylink.0)
)

(module
  (@dylink.0
    (mem-info)
    (mem-info (memory 1 1))
    (mem-info (table 1 1))
    (mem-info (memory 1 1) (table 1 1))
  )
)

(module
  (@dylink.0
    (needed)
    (needed "a" "b")
    (needed "a")
  )
)

(module
  (@dylink.0
    (export-info "a" 0)
    (export-info "b" 1)
  )
)

(module
  (@dylink.0
    (import-info "a" "a" 0)
    (import-info "b" "b" 1)
  )
)

(module
  (@dylink.0
    (mem-info (memory 1 1))
    (needed "a" "b")
    (export-info "a" 0)
    (import-info "a" "a" 0)
    (import-info "b" "b" 1)
  )
)

(module
  (@dylink.0
    (export-info "a" 0)
    (import-info "a" "a" 0)
    (export-info "a" 0)
    (export-info "a" 0)
    (import-info "a" "a" 0)
    (import-info "a" "a" 0)
    (export-info "a" 0)
    (import-info "a" "a" 0)
  )
)

(module
  (@dylink.0
    (export-info "a" 0xffffffff)
    (export-info "a" binding-local)
    (export-info "a" 2 binding-local binding-weak 0 undefined)
  )
)
