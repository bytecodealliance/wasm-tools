(module
  (tag $a (param i32))

  (func
    (; empty try_table ;)
    try_table
    end

    (; try_table with result  ;)
    try_table (result i32)
      i32.const 0
    end
    drop

    (; try_table can catches ;)
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

    (; try_table can have results and catches ;)
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

    (; mixes of catch, catch_ref, catch_all, and catch_all_ref ;)

    try_table (catch_ref $a 0)
    end

    try_table (catch_ref $a 0) (catch_ref $a 0)
    end

    try_table (catch $a 0) (catch_ref $a 0)
    end

    try_table (catch_ref $a 0) (catch_ref $a 0)
    end

    try_table (catch_ref $a 0) (catch $a 0)
    end

    try_table (catch_all 0)
    end

    try_table (catch_all_ref 0)
    end

    try_table (catch_ref $a 0) (catch_all_ref 0)
    end

    try_table (catch_ref $a 0) (catch_ref $a 0) (catch_all_ref 0)
    end

    try_table (catch $a 0) (catch_ref $a 0) (catch_all_ref 0)
    end

    try_table (catch_ref $a 0) (catch_ref $a 0) (catch_all_ref 0)
    end

    try_table (catch_ref $a 0) (catch $a 0) (catch_all_ref 0)
    end
  )
)
