;; --enable-exceptions

(assert_malformed
  (module quote
    "(func (try (catch)))"
  )
  "previous `try` had no `do`")

(assert_malformed
  (module quote
    "(func (try (unreachable) (catch)))"
  )
  "previous `try` had no `do`")

(assert_malformed
  (module quote
    "(func (try (do)))"
  )
  "previous `try` had no `catch`")

(assert_malformed
  (module quote
    "(func (try (do) (unreachable)))"
  )
  "previous `try` had no `catch`")

(assert_malformed
  (module quote
    "(func (try (do) (catch) drop))"
  )
  "expected `(`")

(assert_malformed
  (module quote
    "(func (try (do) (catch) (drop)))"
  )
  "too many payloads inside of `(try)`")

