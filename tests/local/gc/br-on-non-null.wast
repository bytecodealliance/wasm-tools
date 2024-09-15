(assert_invalid
  (module
    (func
      block $l (result anyref)
        ref.null func
        br_on_non_null $l
        unreachable
      end
      drop
    )
  )
  "expected anyref, found funcref")
