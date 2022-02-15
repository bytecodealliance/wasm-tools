(assert_invalid
  (module
    (func $i32_extend_8s (param i32) (result i32)
      local.get 0
      i32.extend8_s
    )
  )
  "sign extension operations support is not enabled"
)

(assert_invalid
  (module
    (func $i32_extend_16s (param i32) (result i32)
      local.get 0
      i32.extend16_s
    )
  )
  "sign extension operations support is not enabled"
)

(assert_invalid
  (module
    (func $i64_extend_8s (param i64) (result i64)
      local.get 0
      i64.extend8_s
    )
  )
  "sign extension operations support is not enabled"
)

(assert_invalid
  (module
    (func $i64_extend_16s (param i64) (result i64)
      local.get 0
      i64.extend16_s
    )
  )
  "sign extension operations support is not enabled"
)

(assert_invalid
  (module
    (func $i64_extend_32s (param i64) (result i64)
      local.get 0
      i64.extend32_s
    )
  )
  "sign extension operations support is not enabled"
)
