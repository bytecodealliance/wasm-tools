(assert_invalid
  (module
    (func
      block $l (result anyref)
        try_table (catch_all_ref $l)
        end
      end
    )
  )
  "type mismatch: catch_all_ref label must a subtype of (ref exn)")
