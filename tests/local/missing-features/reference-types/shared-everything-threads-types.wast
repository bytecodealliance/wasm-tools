;; This relies on the reference-types proposal to check that shared function
;; parameters are not allowed without the shared-everything-threads proposal.
(assert_invalid
  (module
    (func (param $f (ref null (shared func)))
      drop)
  )
  "shared reference types require the shared-everything-threads proposal")
