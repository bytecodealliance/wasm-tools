(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    i32.const 1
    block  ;; label = @1
      block  ;; label = @2
        unreachable
        if  ;; label = @3
          return
        end
      end
      unreachable
      select
      drop
    end
    unreachable))