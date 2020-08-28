;;; --enable-multi-value
(module
  (func (param i32 i32)
    unreachable
    local.get 1
    loop (param i32 i32)  ;; label = @1
      loop (param i32 i32)  ;; label = @2
        call 0
      end
    end
    unreachable
  )
)
