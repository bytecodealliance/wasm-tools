;; --enable-exceptions

(assert_malformed
  (module quote
    "(func (try))"
  )
  "previous `try` had no `do`")

(assert_malformed
  (module quote
    "(func (try (catch $exn)))"
  )
  "previous `try` had no `do`")

(assert_malformed
  (module quote
    "(func (try (unreachable) (catch $exn)))"
  )
  "previous `try` had no `do`")

(assert_malformed
  (module quote
    "(func (try (do) (unreachable)))"
  )
  "expected a `catch`, `catch_all`, or `delegate`")

(assert_malformed
  (module quote
    "(func (try (do) (catch_all) (unreachable)))"
  )
  "too many payloads inside of `(try)`")

(assert_malformed
  (module quote
    "(func (try (do) (catch $exn) drop))"
  )
  "expected `(`")

(assert_malformed
  (module quote
    "(func (try (do) (catch $exn) (drop)))"
  )
  "unexpected items after `catch`")

(assert_malformed
  (module quote
    "(func (try (do) (delegate 0) (drop)))"
  )
  "too many payloads inside of `(try)`")

(assert_malformed
  (module quote
    "(func (try $l (do) (delegate $l)))"
  )
  "failed to find label")
