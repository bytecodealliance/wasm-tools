(module
  (func
    unreachable
    ref.null (shared eq)
    ref.eq
    drop

    ref.null eq
    ref.eq
    drop
  )
  (func
    unreachable
    any.convert_extern
    ref.cast (ref i31)
    drop
  )
  (func
    unreachable
    any.convert_extern
    ref.cast (ref (shared i31))
    drop
  )
  (func
    unreachable
    ref.eq
    drop
  )
  (func
    (local $a (ref null (shared eq)))
    unreachable
    local.get $a
    ref.eq
    drop
  )
  (func
    (local $a (ref null eq))
    unreachable
    local.get $a
    ref.eq
    drop
  )
  (func
    (local $a (ref null eq))
    unreachable
    ref.cast (ref eq)
    local.get $a
    ref.eq
    drop
  )
  (func
    (local $a (ref null (shared eq)))
    unreachable
    ref.cast (ref (shared eq))
    local.get $a
    ref.eq
    drop
  )
  (func
    (local $a (ref null (shared eq)))
    unreachable
    any.convert_extern
    extern.convert_any
    any.convert_extern
    ref.cast (ref (shared eq))
    local.get $a
    ref.eq
    drop
  )
  (func
    (local $a (ref null eq))
    unreachable
    any.convert_extern
    extern.convert_any
    any.convert_extern
    ref.cast (ref eq)
    local.get $a
    ref.eq
    drop
  )
  (func
    unreachable
    any.convert_extern
    ref.cast (ref (shared array))
    array.len
    drop
  )
)

(assert_invalid
  (module
    (func
      unreachable
      any.convert_extern
      ref.null eq
      ref.eq
      drop
    )
  )
  "expected subtype of eq, found any")

(assert_invalid
  (module
    (func
      unreachable
      extern.convert_any
      ref.cast (ref (shared i31))
      drop
    )
  )
  "expected (shared anyref), found (ref (shared extern))")

(assert_invalid
  (module
    (func
      unreachable
      any.convert_extern
      ref.cast (ref extern)
      drop
    )
  )
  "expected externref, found (ref any)")
(assert_invalid
  (module
    (func
      unreachable
      any.convert_extern
      array.len
      drop
    )
  )
  "expected subtype of array, found any")
(assert_invalid
  (module
    (func
      unreachable
      ref.as_non_null
      i32.eq
      drop
    )
  )
  "expected i32, found heap type")
(assert_invalid
  (module
    (func
      block $l
        unreachable
        br_on_cast $l (ref any) (ref any)
      end
    )
  )
  "br_on_cast to label with empty types")
