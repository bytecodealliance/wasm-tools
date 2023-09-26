(module
  (tag $a (param i32))

  (func
    try_table
    end

    try_table (result i32)
      i32.const 0
    end
    drop

    try_table (catch $a 0)
    end

    try_table (catch $a 0) (catch $a 0)
    end

    try_table (catch_all 0)
    end

    try_table (catch $a 0) (catch_all 0)
    end

    try_table (catch $a 0) (catch $a 0) (catch_all 0)
    end

    try_table (result i32) (catch $a 0)
      i32.const 0
    end
    drop

    try_table (result i32) (catch $a 0) (catch $a 0)
      i32.const 0
    end
    drop

    try_table (result i32) (catch_all 0)
      i32.const 0
    end
    drop

    try_table (result i32) (catch $a 0) (catch_all 0)
      i32.const 0
    end
    drop

    try_table (result i32) (catch $a 0) (catch $a 0) (catch_all 0)
      i32.const 0
    end
    drop
  )
)
