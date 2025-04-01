;; FAIL: validate %

(module
  (func (param i32) (result i32)
    i32.const 0
    i32.const 0
    br_if 0
    i32.const 0
    return
    end
    block
      i32.const 0
      return
    end
  )
)
