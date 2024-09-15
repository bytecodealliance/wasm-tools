(assert_invalid
  (module
    (global i32 (i32.const 0))
    (global i32 (global.get 0))
  )
  "global.get of locally defined global")

(assert_invalid
  (module
    (global i32 (i32.const 0))
    (table 1 funcref)
    (elem (offset global.get 0))
  )
  "global.get of locally defined global")

(assert_invalid
  (module
    (global i32 (i32.const 0))
    (memory 1)
    (data (offset global.get 0))
  )
  "global.get of locally defined global")
