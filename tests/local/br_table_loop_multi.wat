;;; --enable-multi-value
(module
  (func (param i32) (result i32)
    (local i32)
    local.get 1
    loop (param i32)  ;; label = @1
      i32.const -458751
      br_table 0 (;@1;) 1 (;@0;) 0 (;@1;)
      unreachable
    end
    unreachable
  )
)
