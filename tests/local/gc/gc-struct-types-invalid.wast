;; --enable-gc

(assert_malformed
  (module quote
    "(type (struct (field $id)))"
  )
  "unexpected token"
)
(assert_malformed
  (module quote
    "(type (struct (field $id i32 i32)))"
  )
  "expected `)`"
)
(assert_malformed
  (module quote
    "(type (struct (field i32 $id i32)))"
  )
  "unexpected token"
)
