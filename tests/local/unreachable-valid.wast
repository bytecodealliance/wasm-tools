(module
  (func (export "f")
    f32.const 1.0
    f32.const 2.0
    br 0
    i32.add
    drop
  )
)

(assert_trap (invoke "f") "unreachable")
