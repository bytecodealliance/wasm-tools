(assert_invalid
  (module
    (type (;0;) (func (param i32)))
    (type (;1;) (func (param i32) (result i32)))
    (type (;2;) (func (param i32 i32)))
    (type (;3;) (func))
    (func (;0;) (type 3))
    (func (;1;) (type 0) (param i32))
    (func (;2;) (type 1) (param i32) (result i32)
      i32.const 1
    )
    (func (;3;) (type 1) (param i32) (result i32)
      i32.const 2
    )
    (func (;4;) (type 2) (param i32 i32)
      local.get 0
      unreachable
      unreachable
      unreachable
      unreachable
      unreachable
      unreachable
      unreachable
      block (param i32)  ;; label = @1
        unreachable
        drop

    )
    (func (;5;) (type 3)
      (local i32)
      unreachable
      unreachable
      i32.mul
      i32.popcnt
      i32.div_u
      i32.sub
      i32.popcnt
      i32.div_u
      i32.sub
      local.tee 0
      global.set 0
      local.get 0
      i32.const 8
      i32.add
      i32.const 1024
      call 4
      local.get 0
      i32.const 8
      i32.add
      i32.const 25166864
      unreachable
      call 2097156
      call_indirect (type 0)
      i32.const 16
      i32.add
      global.set 0
    )
    (func (;6;) (type 3)
      i64.clz
    )
  )
  "control frames remain at end of function")
