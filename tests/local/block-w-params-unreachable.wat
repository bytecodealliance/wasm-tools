;;; --enable-multi-value
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32 i32)))
  (import "" "" (table (;0;) 4278190080 funcref))
  (func (;0;) (type 1) (param i32 i32)
    call 1
    return
    local.get 1
    loop (param i32 i32)  ;; label = @1
      loop (param i32 i32)  ;; label = @2
        call 0
      end
    end
    unreachable)
  (func (;1;) (type 0) (result i32)
    unreachable)
)
