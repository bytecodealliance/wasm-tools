(module
  (module
    (global (export "g") i32 (i32.const 0)))
  (instance (instantiate 0))
  (alias 0 "g" (global))
  (global i32 (global.get 0))
)
