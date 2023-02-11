(assert_invalid
  (module
    (func
      unreachable
      select
      loop
        i32x4.eq
      end
    )
  )
  "expected v128 but nothing on stack")
