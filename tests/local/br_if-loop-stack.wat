(module
  (type (;0;) (func (result i32)))
  (func $main (type 0) (result i32)
    block (result i32)  ;; label = @1
      loop (result i32)  ;; label = @2
        i32.const 77
        i32.const 0
        br_if 0 (;@2;)
        br 1 (;@1;)
      end
    end
  )
  (export "main" (func $main))
)
