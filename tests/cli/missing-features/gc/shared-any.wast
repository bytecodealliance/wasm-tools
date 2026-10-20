;; RUN: wast --assert default --snapshot tests/snapshots % -f=-shared-everything-threads

(assert_invalid
  (module
    (func
      ref.null (shared any)
      drop
    )
  )
  "shared reference types require the shared-everything-threads proposal")

(assert_invalid
  (module
    (func (result i32)
      (ref.test (ref (shared any)) (unreachable))))
  "shared reference types require the shared-everything-threads proposal")

(assert_invalid
  (module
    (func (result anyref)
      (ref.cast (ref null (shared eq)) (unreachable))))
  "shared reference types require the shared-everything-threads proposal")

(assert_invalid
  (module
    (func (result anyref)
      (br_on_cast 0 anyref (ref (shared any)) (unreachable))))
  "shared reference types require the shared-everything-threads proposal")
