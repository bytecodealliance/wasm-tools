(module
  (type (;0;) (func))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (result i32)))
  (func $__wasm_nullptr (type 0)
    unreachable)
  (func $main (type 2) (result i32)
    (local i32 i32 i32 i32)
    i32.const 0
    local.tee 2
    local.set 3
    loop  ;; label = @1
      local.get 2
      i32.const 50000
      i32.eq
      (@metadata.code.branch_hint "\00") if  ;; label = @2
        i32.const 1
        local.set 3
      end
      local.get 2
      i32.const 1
      i32.add
      local.tee 2
      i32.const 100000
      i32.ne
      (@metadata.code.branch_hint "\01") br_if 0 (;@1;)
    end
    local.get 3)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 17 128)
  (global (;0;) (mut i32) (i32.const 1048576))
  (export "memory" (memory 0))
  (export "_main" (func $main))
  (elem (;0;) (i32.const 0) func $__wasm_nullptr)
)
