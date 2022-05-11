(module
  (type (;0;) (func (param i32 i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (result i32)))
  (type (;3;) (func (param i32)))
  (type (;4;) (func))
  (import "env" "DYNAMICTOP_PTR" (global (;0;) i32))
  (import "env" "tempDoublePtr" (global (;1;) i32))
  (import "env" "ABORT" (global (;2;) i32))
  (import "env" "STACKTOP" (global (;3;) i32))
  (import "env" "STACK_MAX" (global (;4;) i32))
  (import "env" "gb" (global (;5;) i32))
  (import "env" "fb" (global (;6;) i32))
  (import "global" "NaN" (global (;7;) f64))
  (import "global" "Infinity" (global (;8;) f64))
  (import "env" "memory" (memory (;0;) 256 256))
  (import "env" "table" (table (;0;) 0 0 funcref))
  (import "env" "memoryBase" (global (;9;) i32))
  (import "env" "tableBase" (global (;10;) i32))
  (func $stackAlloc (type 1) (param i32) (result i32)
    (local i32)
    block  ;; label = @1
      global.get 14
      local.set 1
      global.get 14
      local.get 0
      i32.add
      global.set 14
      global.get 14
      i32.const 15
      i32.add
      i32.const -16
      i32.and
      global.set 14
      local.get 1
      return
      unreachable
    end
    unreachable
  )
  (func $stackSave (type 2) (result i32)
    global.get 14
    return
  )
  (func $stackRestore (type 3) (param i32)
    local.get 0
    global.set 14
  )
  (func $establishStackSpace (type 0) (param i32 i32)
    block  ;; label = @1
      local.get 0
      global.set 14
      local.get 1
      global.set 15
    end
  )
  (func $setThrew (type 0) (param i32 i32)
    global.get 18
    i32.const 0
    i32.eq
    if  ;; label = @1
      local.get 0
      global.set 18
      local.get 1
      global.set 19
    end
  )
  (func $_fib (type 1) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      global.get 14
      local.set 11
      local.get 0
      i32.const 0
      i32.gt_s
      local.set 6
      local.get 6
      if  ;; label = @2
        i32.const 0
        local.set 1
        i32.const 1
        local.set 5
        i32.const 0
        local.set 8
      else
        i32.const 1
        local.set 4
        local.get 4
        return
      end
      loop  ;; label = @2
        block  ;; label = @3
          local.get 5
          local.get 1
          i32.add
          local.set 3
          local.get 8
          i32.const 1
          i32.add
          local.set 9
          local.get 9
          local.get 0
          i32.eq
          local.set 7
          local.get 7
          if  ;; label = @4
            local.get 3
            local.set 4
            br 1 (;@3;)
          else
            local.get 5
            local.set 2
            local.get 3
            local.set 5
            local.get 9
            local.set 8
            local.get 2
            local.set 1
          end
          br 1 (;@2;)
        end
      end
      local.get 4
      return
      unreachable
    end
    unreachable
  )
  (func $runPostSets (type 4)
    (local i32)
    nop
  )
  (global (;11;) (mut i32) global.get 0)
  (global (;12;) (mut i32) global.get 1)
  (global (;13;) (mut i32) global.get 2)
  (global (;14;) (mut i32) global.get 3)
  (global (;15;) (mut i32) global.get 4)
  (global (;16;) (mut i32) global.get 5)
  (global (;17;) (mut i32) global.get 6)
  (global (;18;) (mut i32) i32.const 0)
  (global (;19;) (mut i32) i32.const 0)
  (global (;20;) (mut i32) i32.const 0)
  (global (;21;) (mut i32) i32.const 0)
  (global (;22;) (mut f64) global.get 7)
  (global (;23;) (mut f64) global.get 8)
  (global (;24;) (mut i32) i32.const 0)
  (global (;25;) (mut i32) i32.const 0)
  (global (;26;) (mut i32) i32.const 0)
  (global (;27;) (mut i32) i32.const 0)
  (global (;28;) (mut f64) f64.const 0x0p+0 (;=0;))
  (global (;29;) (mut i32) i32.const 0)
  (global (;30;) (mut i32) i32.const 0)
  (global (;31;) (mut i32) i32.const 0)
  (global (;32;) (mut f64) f64.const 0x0p+0 (;=0;))
  (global (;33;) (mut i32) i32.const 0)
  (global (;34;) (mut f64) f64.const 0x0p+0 (;=0;))
  (export "setThrew" (func $setThrew))
  (export "runPostSets" (func $runPostSets))
  (export "establishStackSpace" (func $establishStackSpace))
  (export "stackSave" (func $stackSave))
  (export "stackRestore" (func $stackRestore))
  (export "_fib" (func $_fib))
  (export "stackAlloc" (func $stackAlloc))
)
