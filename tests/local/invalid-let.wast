(assert_invalid
  (module
    (func
      let
      else
      else
      local.get 1
      ))
  "unknown opcode: 0x17") ;; update this error once wasmparser is updated
