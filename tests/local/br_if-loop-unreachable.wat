;;; --enable-multi-value
(module
  (type (;0;) (func))
  (type (;1;) (func (result f64 i32)))
  (func (;0;) (type 1) (result f64 i32)
    loop (result f64 i32)  ;; label = @1
      br 0 (;@1;)
      call 0
      br_if 0 (;@1;)
      i32.trunc_f64_u
      unreachable
    end
    unreachable)
)
