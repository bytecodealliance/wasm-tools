;; --enable-function-references

(module
  (func
    (let)

    let end

    let
    end
  )
  (func
    i32.const 0
    (let (param i32) drop)

    i32.const 0
    let (param i32) drop end

    i32.const 0
    let (param i32)
      drop
    end
  )
  (func
    (let (result i32)
      i32.const 0
    )
    drop

    let (result i32) i32.const 0 end
    drop

    let (result i32)
      i32.const 0
    end
    drop
  )
  (func
    (let (local $x i32) local.get $x drop)
    let (local $x i32) local.get $x drop end
    let (local $x i32)
      local.get $x
      drop
    end
  )
)
